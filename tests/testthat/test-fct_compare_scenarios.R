# Scenario-vs-scenario comparisons on the mini fixtures (helper-fixtures.R).
# Baseline "All" (stations 1, 3, 7, 22); "No 1" closes station 1 and "No 3"
# station 3. Minutes:
#
#           I1   I2   I3   I4
#   All    1.1  1.2  7.3  3.4
#   No 1   7.1  7.2  7.3  3.4
#   No 3   1.1  1.2  7.3  1.4

runcard <- function() prep_runcard(make_runcard_raw_df())

# The three scenarios, plus a fourth if given (stations, name)
scenarios_with <- function(stations = NULL, name = NULL) {
  out <- tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "No 1", c(3, 7, 22), FALSE,
    3, "No 3", c(1, 7, 22), FALSE
  )
  if (!is.null(stations)) {
    out <- tibble::add_row(out, scenario_id = 4, scenario_name = name, stations = list(stations), baseline = FALSE)
  }
  out
}

paired <- function(scenarios = scenarios_with()) {
  suppressMessages(paired_calls(
    scenarios, make_scenario_matrix_df(), runcard(), list(incident_filter("All calls")), 1
  ))
}

compare_of <- function(...) compare_scenarios(paired(), baseline_name = "All", n_boot = 20, ...)

row_of <- function(out, a, b, metric = "mean_minutes") {
  out[out$scenario_a == a & out$scenario_b == b & out$metric == metric, ]
}

# compare_scenarios -----------------------------------------------------------

test_that("every pair in the order given, first minus second", {
  out <- compare_of(scenario = c("No 1", "No 3", "All"))

  expect_equal(
    paste(out$scenario_a, out$scenario_b),
    c("No 1 No 3", "No 1 All", "No 3 All")
  )
  pair <- row_of(out, "No 1", "No 3")
  expect_equal(pair$value_a, 25 / 4)
  expect_equal(pair$value_b, 11 / 4)
  expect_equal(pair$diff, 3.5)
  expect_equal(pair$n_calls, 4)
  expect_equal(pair$level, 0.95)
})

test_that("NULL takes the baseline and then every scenario, and ids work", {
  expect_equal(nrow(compare_of()), 3) # All/No 1, All/No 3, No 1/No 3
  by_id <- compare_of(scenario = c(2, 3))
  expect_equal(c(by_id$scenario_a, by_id$scenario_b), c("No 1", "No 3"))
})

test_that("a baseline pair matches compare_overall() with the sign flipped", {
  metrics <- c("mean_minutes", "median_minutes", "p90_minutes", "mean_distance_m", "pct_within_4min")
  out <- compare_of(scenario = c("All", "No 1"), metrics = metrics)
  overall <- compare_overall(scenarios_with()[1:2, ], make_scenario_matrix_df(), runcard(), thresholds = 4)

  expect_equal(out$diff, -overall$diff[match(metrics, overall$metric)])
  expect_equal(row_of(out, "All", "No 1")$diff, -3)
})

test_that("pct_within thresholds are read from the metric name", {
  out <- compare_of(scenario = c("No 1", "No 3"), metrics = c("pct_within_4min", "pct_within_3m24s"))

  # within 4 (and 3:24): only I4 for No 1; I1, I2, I4 for No 3
  expect_equal(out$value_a, c(0.25, 0.25))
  expect_equal(out$value_b, c(0.75, 0.75))
  expect_equal(out$diff, c(-0.5, -0.5))
  expect_false(anyNA(out$ci_lower))
})

test_that("threshold_minutes() undoes threshold_key()", {
  t <- c(4, 8, 8 + 58 / 60, 10 + 21 / 60)
  expect_equal(threshold_minutes(threshold_key(t)), t)
  expect_error(threshold_minutes("8.5"), "8m58s")
})

test_that("unsupported metrics and unknown scenarios are errors", {
  expect_error(compare_of(metrics = "total_minutes"), "mean_minutes")
  expect_error(compare_of(metrics = "n_calls"), "Unknown metric")
  expect_error(compare_of(scenario = c("All", "Nope")), "Unknown scenario")
  expect_error(compare_of(scenario = "All"), "at least two")
  expect_error(compare_of(filter_name = "Nope"), "Unknown filter set")
})

test_that("identical scenarios have a zero interval that does not exclude 0", {
  scenarios <- scenarios_with(c(3, 7, 22), "No 1 again")
  out <- compare_scenarios(paired(scenarios), c("No 1", "No 1 again"), baseline_name = "All", n_boot = 20)

  expect_equal(c(out$diff, out$ci_lower, out$ci_upper), c(0, 0, 0))
  expect_false(out$ci_excludes_zero)
})

test_that("a call one scenario can't serve is left out of that scenario's pairs only", {
  # with only station 22 open, 2B (I3) has no responder
  scenarios <- scenarios_with(22, "Only 22")
  out <- compare_scenarios(paired(scenarios), c("All", "No 1", "Only 22"), baseline_name = "All", n_boot = 20)

  expect_equal(row_of(out, "All", "No 1")$n_calls, 4)
  expect_equal(row_of(out, "No 1", "Only 22")$n_calls, 3)
  # I1, I2, I4: 7.1, 7.2, 3.4 against 22.1, 22.2, 22.4
  expect_equal(row_of(out, "No 1", "Only 22")$diff, (7.1 + 7.2 + 3.4 - 22.1 - 22.2 - 22.4) / 3)
})

# scenario_pairs_plot ---------------------------------------------------------

# A compare_scenarios() result by hand: the first pair clearly differs, the
# second does not
pairs_df <- function() {
  data.frame(
    filter_name = "All calls",
    due = 1,
    scenario_a = rep(c("A", "A"), 2),
    scenario_b = rep(c("B", "C"), 2),
    metric = rep(c("mean_minutes", "pct_within_8min"), each = 2),
    n_calls = 100,
    value_a = c(7, 7, 0.6, 0.6),
    value_b = c(7.5, 7.1, 0.5, 0.58),
    diff = c(-0.5, -0.1, 0.1, 0.02),
    ci_lower = c(-0.8, -0.3, 0.05, -0.01),
    ci_upper = c(-0.2, 0.1, 0.15, 0.05),
    level = 0.95,
    ci_excludes_zero = c(TRUE, FALSE, TRUE, FALSE)
  )
}

test_that("the pairs plot has a dot and an interval per pair and metric", {
  p <- scenario_pairs_plot(pairs_df())
  ranges <- ggplot2::layer_data(p, 2)
  dots <- ggplot2::layer_data(p, 3)

  expect_equal(nrow(ranges), 4)
  expect_equal(nrow(dots), 4)
  expect_equal(length(unique(dots$PANEL)), 2) # a facet per metric
  # shares are drawn in percentage points
  expect_equal(sort(dots$x[dots$PANEL == 2]), c(2, 10))
})

test_that("pairs whose interval crosses 0 are open gray dots", {
  dots <- ggplot2::layer_data(scenario_pairs_plot(pairs_df(), metrics = "mean_minutes"), 3)
  dots <- dots[order(dots$x), ] # -0.5 (clear), then -0.1 (crosses 0)

  expect_equal(dots$shape, c(19, 21))
  expect_equal(dots$colour, c(plot_colors[1], plot_ink[["muted"]]))
})

test_that("the first pair is drawn on top", {
  p <- scenario_pairs_plot(pairs_df(), metrics = "mean_minutes")
  expect_equal(levels(p$data$pair), c("A vs C", "A vs B"))
})

test_that("pair strip labels say which side of 0 favors the first scenario", {
  expect_equal(
    pair_strip_label(c("mean_minutes", "pct_within_8min")),
    c("Mean Minutes\nbelow 0 = first better", "Within 8 Min (pct pts)\nabove 0 = first better")
  )
  expect_equal(signed_labels(c(-0.5, 0, 0.5, NA)), c("-0.5", "0", "+0.5", ""))
})

# scenario_pairs_table --------------------------------------------------------

test_that("the pairs table has a row per pair and First / Second / Diff / CI per metric", {
  ft <- scenario_pairs_table(pairs_df())
  body <- ft$body$dataset

  expect_equal(body$scenario_a, c("A", "A"))
  expect_equal(body$scenario_b, c("B", "C"))
  expect_equal(body$mean_minutes_d, c("-0.50", "-0.10"))
  expect_equal(body$mean_minutes_ci, c("[-0.80, -0.20]", "[-0.30, +0.10]"))
  expect_equal(body$pct_within_8min_d, c("+10.0 pts", "+2.0 pts"))
  expect_equal(ncol(body), 2 + 4 * 2)
})
