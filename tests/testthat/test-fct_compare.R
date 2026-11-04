# Uses make_scenario_matrix_df() and make_scenarios() from helper-fixtures.R.
# Baseline "All" vs "No 1" at 1st due:
#
#   call  zone  baseline         No 1             change
#   I1    1A    station 1, 1.1   station 7, 7.1   +6
#   I2    1A    station 1, 1.2   station 7, 7.2   +6
#   I3    2B    station 7, 7.3   station 7, 7.3    0
#   I4    3C    station 3, 3.4   station 3, 3.4    0

runcard <- function() prep_runcard(make_runcard_raw_df())

metric_row <- function(out, metric) out[out$metric == metric, ]

# compare_overall -------------------------------------------------------------

test_that("compare_overall gives baseline, scenario and diff per metric", {
  out <- compare_overall(make_scenarios(), make_scenario_matrix_df(), runcard())

  expect_true(all(out$scenario_name == "No 1")) # the baseline is not repeated
  mean_row <- metric_row(out, "mean_minutes")
  expect_equal(mean_row$baseline, 3.25)
  expect_equal(mean_row$scenario, 6.25)
  expect_equal(mean_row$diff, 3)
  expect_equal(mean_row$pct_diff, 3 / 3.25)

  expect_equal(metric_row(out, "median_minutes")$baseline, 2.3) # (1.2 + 3.4) / 2
  expect_equal(metric_row(out, "median_minutes")$scenario, 7.15)
  expect_equal(metric_row(out, "n_calls")$scenario, 4)
  expect_equal(metric_row(out, "mean_distance_m")$diff, 3000)
})

test_that("total_minutes sums the paired calls' minutes", {
  out <- compare_overall(make_scenarios(), make_scenario_matrix_df(), runcard())
  total <- metric_row(out, "total_minutes")

  expect_equal(total$baseline, 1.1 + 1.2 + 7.3 + 3.4)
  expect_equal(total$scenario, 7.1 + 7.2 + 7.3 + 3.4)
  # same calls on both sides, so the total diff is n_calls times the mean diff
  expect_equal(total$diff, 4 * metric_row(out, "mean_minutes")$diff)
})

test_that("compare_overall threshold metrics", {
  out <- compare_overall(make_scenarios(), make_scenario_matrix_df(), runcard(), thresholds = 4)

  # within 4 minutes: 1.1, 1.2, 3.4 in the baseline; only 3.4 in the scenario
  expect_equal(metric_row(out, "pct_within_4min")$baseline, 0.75)
  expect_equal(metric_row(out, "pct_within_4min")$scenario, 0.25)
  expect_equal(metric_row(out, "calls_within_4min")$diff, -2)
  # zone p90 over 4 minutes: 2B in the baseline; 1A and 2B in the scenario
  expect_equal(metric_row(out, "fdz_p90_over_4min")$baseline, 1)
  expect_equal(metric_row(out, "fdz_p90_over_4min")$scenario, 2)
  expect_false("pct_within_8min" %in% out$metric)
})

test_that("compare_overall counts unserved calls instead of pairing them", {
  # with only station 22 open, 2B (I3) has no responder
  scenarios <- tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "Only 22", 22, FALSE
  )

  out <- suppressMessages(compare_overall(scenarios, make_scenario_matrix_df(), runcard()))

  expect_equal(metric_row(out, "unserved_calls")$scenario, 1)
  expect_equal(metric_row(out, "unserved_zones")$scenario, 1)
  expect_equal(metric_row(out, "unserved_calls")$baseline, 0)
  expect_equal(metric_row(out, "n_calls")$baseline, 3) # I1, I2, I4 are paired
})

test_that("compare_overall works per filter set and due level", {
  night <- incident_filter("Night", time_range = c("22:00", "06:00"))
  out <- suppressMessages(compare_overall(
    make_scenarios(),
    make_scenario_matrix_df(),
    runcard(),
    filters = list(incident_filter("All calls"), night),
    due = 1:2
  ))

  n_calls <- out[out$metric == "n_calls", ]
  expect_equal(n_calls$filter_name, c("All calls", "All calls", "Night", "Night"))
  expect_equal(n_calls$due, c(1L, 2L, 1L, 2L))
  # at 2nd due, 2B's card (7, 1) has no second open station without station 1
  due2 <- out[out$filter_name == "All calls" & out$due == 2, ]
  expect_equal(metric_row(due2, "unserved_zones")$scenario, 1)
  expect_equal(metric_row(due2, "unserved_zones")$baseline, 0)
})

# Confidence intervals --------------------------------------------------------

test_that("CI columns appear only when asked for", {
  without <- compare_overall(make_scenarios(), make_scenario_matrix_df(), runcard())
  expect_false(any(c("ci_lower", "ci_upper") %in% names(without)))

  set.seed(1)
  with_ci <- compare_overall(make_scenarios(), make_scenario_matrix_df(), runcard(), ci = TRUE, n_boot = 200)
  intervals <- with_ci[!is.na(with_ci$ci_lower), ]
  expect_setequal(
    intervals$metric,
    c("mean_minutes", "median_minutes", "p90_minutes", "mean_distance_m", "pct_within_4min", "pct_within_8min")
  )
  expect_true(all(intervals$ci_lower <= intervals$ci_upper))
})

test_that("a scenario identical to the baseline has zero diffs and a zero interval", {
  scenarios <- make_scenarios()
  scenarios$stations[[2]] <- scenarios$stations[[1]]

  set.seed(1)
  out <- compare_overall(scenarios, make_scenario_matrix_df(), runcard(), ci = TRUE, n_boot = 100)
  intervals <- out[!is.na(out$ci_lower), ]

  expect_true(all(out$diff == 0))
  expect_true(all(intervals$ci_lower == 0 & intervals$ci_upper == 0))
})

test_that("bootstrap medians match quantile() on the resampled calls", {
  # draw_stats() reads order statistics from running counts; check it against
  # quantile() on an explicitly resampled set of calls
  both <- data.frame(
    fdz_id = c("a", "a", "b", "c", "c", "c"),
    baseline_minutes = c(1, 4, 2, 9, 3, 5),
    scenario_minutes = c(2, 6, 2, 9, 7, 5),
    baseline_distance_m = 1,
    scenario_distance_m = 1
  )
  set.seed(42)
  ci <- bootstrap_diffs(both, thresholds = 4, n_boot = 1, level = 0.5)

  set.seed(42)
  w <- tabulate(sample.int(3, 3, replace = TRUE), 3)
  rows <- rep(seq_len(nrow(both)), w[as.integer(factor(both$fdz_id))])
  expected <- stats::median(both$scenario_minutes[rows]) - stats::median(both$baseline_minutes[rows])

  expect_equal(ci$ci_lower[ci$metric == "median_minutes"], expected)
})

# compare_calls ---------------------------------------------------------------

test_that("compare_calls describes how individual calls change", {
  out <- compare_calls(make_scenarios(), make_scenario_matrix_df(), runcard(), slower_by = c(2, 10))
  value <- function(m) out$value[out$metric == m]

  expect_equal(value("n_calls"), 4)
  expect_equal(value("share_responder_changed"), 0.5)
  expect_equal(value("share_slower"), 0.5)
  expect_equal(value("share_unchanged"), 0.5)
  expect_equal(value("share_faster"), 0)
  expect_equal(value("mean_change"), 3)
  expect_equal(value("median_change"), 3)
  expect_equal(value("max_change"), 6)
  expect_equal(value("calls_slower_by_2min"), 2)
  expect_equal(value("calls_slower_by_10min"), 0)
  expect_equal(value("share_slower_by_2min"), 0.5)
})

# compare_by_fdz --------------------------------------------------------------

test_that("compare_by_fdz shows each zone's responders and diffs", {
  out <- compare_by_fdz(make_scenarios(), make_scenario_matrix_df(), runcard())
  means <- out[out$metric == "mean_minutes", ]

  expect_equal(means$fdz_id, c("1A", "2B", "3C"))
  expect_equal(means$baseline_source_id, c(1L, 7L, 3L))
  expect_equal(means$scenario_source_id, c(7L, 7L, 3L))
  expect_equal(means$responder_changed, c(TRUE, FALSE, FALSE))
  expect_equal(means$diff, c(6, 0, 0))
  expect_equal(out$scenario[out$metric == "n_calls"], c(2, 1, 1))
  # 1A's two calls: 1.1 + 1.2 in the baseline, 7.1 + 7.2 in the scenario
  expect_equal(out$diff[out$metric == "total_minutes"], c(12, 0, 0))
})

test_that("compare_by_fdz leaves a side's metrics NA when the zone has no responder", {
  scenarios <- tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "Only 22", 22, FALSE
  )

  out <- suppressMessages(compare_by_fdz(scenarios, make_scenario_matrix_df(), runcard()))
  zone_2b <- out[out$fdz_id == "2B" & out$metric == "mean_minutes", ]

  expect_true(is.na(zone_2b$scenario_source_id))
  expect_true(zone_2b$responder_changed)
  expect_equal(zone_2b$baseline, 7.3)
  expect_true(is.na(zone_2b$scenario))
})

# compare_by_station ----------------------------------------------------------

test_that("compare_by_station shows the workload moving between stations", {
  out <- compare_by_station(make_scenarios(), make_scenario_matrix_df(), runcard())
  get <- function(station, metric) out[out$source_id == station & out$metric == metric, ]

  # station 1 closes: its 2 calls (zone 1A) go to station 7
  expect_equal(get(1, "n_incidents")$baseline, 2)
  expect_equal(get(1, "n_incidents")$scenario, 0)
  expect_true(is.na(get(1, "median_minutes")$scenario))
  expect_equal(get(7, "n_incidents")$diff, 2)
  expect_equal(get(7, "n_fdz")$baseline, 1)
  expect_equal(get(7, "n_fdz")$scenario, 2)
  expect_equal(get(3, "n_incidents")$diff, 0)

  # drive minutes: station 1 drops its 1.1 + 1.2 to 0; station 7 goes from
  # 7.3 to 7.1 + 7.2 + 7.3
  expect_equal(get(1, "total_minutes")$scenario, 0)
  expect_equal(get(1, "total_minutes")$baseline, 2.3)
  expect_equal(get(7, "total_minutes")$scenario, 21.6)
})

# Baseline checks -------------------------------------------------------------

test_that("the scenarios need exactly one baseline", {
  matrix_df <- make_scenario_matrix_df()

  no_column <- make_scenarios()
  no_column$baseline <- NULL
  expect_error(compare_overall(no_column, matrix_df, runcard()), "baseline")

  two <- make_scenarios()
  two$baseline <- TRUE
  expect_error(compare_overall(two, matrix_df, runcard()), "exactly one")

  none <- make_scenarios()
  none$baseline <- FALSE
  expect_error(compare_calls(none, matrix_df, runcard()), "exactly one")

  expect_error(compare_by_fdz(make_scenarios()[1, ], matrix_df, runcard()), "besides the baseline")
})
