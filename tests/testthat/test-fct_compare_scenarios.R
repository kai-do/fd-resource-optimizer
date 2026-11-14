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

test_that("against compares each scenario with every other group, scenario first", {
  everything <- c("All", "No 1", "No 3")
  one <- compare_of(scenario = "No 1", against = everything)
  two <- compare_of(scenario = c("No 3", "No 1"), against = everything)

  expect_equal(paste(one$scenario_a, one$scenario_b), c("No 1 All", "No 1 No 3"))
  # pairs within scenario first, in its order; no self or repeated pairs
  expect_equal(paste(two$scenario_a, two$scenario_b), c("No 3 No 1", "No 3 All", "No 1 All"))
  expect_equal(row_of(one, "No 1", "No 3")$diff, 3.5)

  set.seed(5)
  without <- compare_of(scenario = c("All", "No 1", "No 3"))
  set.seed(5)
  with_null <- compare_of(scenario = c("All", "No 1", "No 3"), against = NULL)
  expect_identical(without, with_null)
  expect_equal(unique(without$ci_type), "marginal")
  expect_equal(without$per_interval_level, rep(0.95, 3))
  expect_equal(without$n_comparisons, rep(1L, 3))
})

test_that("calibrate_simultaneous widens intervals only as far as needed", {
  set.seed(11)
  n <- 4000
  q <- function(x, p) stats::quantile(x, p, names = FALSE)

  # one comparison: the ordinary interval
  x <- stats::rnorm(n)
  one <- calibrate_simultaneous(x, 0.95)
  expect_equal(one$alpha, 0.025)
  expect_equal(c(one$lower, one$upper), c(q(x, 0.025), q(x, 0.975)))

  # perfectly correlated comparisons: no widening
  same <- calibrate_simultaneous(rbind(x, 2 * x, x + 1), 0.95)
  expect_equal(same$alpha, 0.025)

  # independent ones: wider, and jointly covering 95% of the resamples
  draws <- matrix(stats::rnorm(10 * n), nrow = 10)
  wide <- calibrate_simultaneous(draws, 0.95)
  inside <- colSums(draws < wide$lower | draws > wide$upper) == 0
  expect_lt(wide$alpha, 0.025)
  expect_gt(wide$alpha, 0.025 / 10 / 2) # narrower than Bonferroni, roughly 0.0025
  expect_gte(mean(inside), 0.95)
  expect_true(all(wide$lower <= apply(draws, 1, q, 0.025)))

  # a comparison that never varies is always covered and widens nothing
  with_const <- calibrate_simultaneous(rbind(x, 0), 0.95)
  expect_equal(with_const$alpha, 0.025)
  expect_equal(unname(c(with_const$lower[2], with_const$upper[2])), c(0, 0))
})

test_that("simultaneous intervals hold per metric over all pairs", {
  set.seed(3)
  metrics <- c("mean_minutes", "pct_within_4min")
  out <- compare_scenarios(
    paired(), c("All", "No 1", "No 3"),
    metrics = metrics, baseline_name = "All", n_boot = 400, simultaneous = TRUE
  )

  expect_equal(unique(out$ci_type), "simultaneous")
  expect_equal(unique(out$n_comparisons), 3L)
  expect_true(all(out$per_interval_level >= 0.95 & out$per_interval_level <= 1))
  expect_equal(nrow(out), 3 * 2)
  expect_equal(row_of(out, "No 1", "No 3")$diff, 3.5) # point values as before
  expect_true(all(out$ci_lower <= out$ci_upper))

  # identical scenarios still get a zero interval
  scenarios <- scenarios_with(c(3, 7, 22), "No 1 again")
  same <- compare_scenarios(
    paired(scenarios), c("No 1", "No 1 again"), baseline_name = "All", n_boot = 50, simultaneous = TRUE
  )
  expect_equal(c(same$ci_lower, same$ci_upper), c(0, 0))
  expect_false(same$ci_excludes_zero)
})

test_that("calls can be resampled one by one, simultaneous or not", {
  set.seed(8)
  marginal <- compare_of(scenario = c("All", "No 1", "No 3"), resample = "call")
  set.seed(8)
  sim <- compare_of(scenario = c("All", "No 1", "No 3"), resample = "call", simultaneous = TRUE)

  expect_equal(unique(marginal$resample), "call")
  expect_equal(unique(sim$resample), "call")
  expect_equal(marginal$diff, sim$diff)
  expect_true(all(marginal$ci_lower <= marginal$ci_upper))

  # four calls: a resample can draw each 0 to 4 times; No 1 minus No 3 is
  # 6 for I1, I2 and 2 for I4, 0 for I3, so the mean diff stays in [0, 6]
  pair <- row_of(marginal, "No 1", "No 3")
  expect_gte(pair$ci_lower, 0)
  expect_lte(pair$ci_upper, 6)
})

# Shared sides and parallel chunks ----------------------------------------------

# compare_scenarios()'s inputs to simultaneous_pairs(): "Only 22" can't serve
# I3, so its pairs are on 3 calls and the others on 4
pairs_input <- function() {
  groups <- c("All", "No 1", "No 3", "Only 22")
  df <- paired(scenarios_with(22, "Only 22"))
  pairs <- scenario_pairs(groups)
  calls <- stats::setNames(lapply(groups, function(g) group_calls(df, g, "All")), groups)
  list(
    boths = lapply(pairs, function(p) pair_calls(calls[[p[1]]], calls[[p[2]]])),
    pairs = pairs,
    universe = sort(unique(df$dest_id))
  )
}

test_that("sharing a scenario's side between pairs gives the same draws", {
  x <- pairs_input()
  # a resample can draw none of a 3-call pair's calls; it is left out (message)
  run <- function(...) {
    suppressMessages(simultaneous_pairs(
      x$boths, x$universe, "call", c("mean_minutes", "p90_minutes"), thresholds = 4,
      n_boot = 200, level = 0.95, groups = x$pairs, ...
    ))
  }
  set.seed(1)
  shared <- run()
  set.seed(1)
  own <- run(cache = FALSE)

  expect_identical(shared, own)
  expect_equal(vapply(x$boths, nrow, integer(1)), c(4, 4, 3, 4, 3, 3))
})

test_that("chunked resamples give the same intervals on 1 or 2 cores", {
  skip_on_cran()
  run <- function(n_cores) {
    set.seed(6)
    compare_of(scenario = c("All", "No 1", "No 3"), resample = "call", simultaneous = TRUE, n_cores = n_cores)
  }
  one <- run(1)
  two <- run(2)

  expect_identical(one$ci_lower, two$ci_lower)
  expect_identical(one$ci_upper, two$ci_upper)
  expect_equal(RNGkind()[1], "Mersenne-Twister") # the caller's generator is left as it was
  expect_error(compare_of(n_cores = 0), "n_cores")
})

test_that("overdispersion widens every interval about its difference", {
  set.seed(9)
  plain <- compare_of(scenario = c("No 1", "No 3"), resample = "call")
  set.seed(9)
  wide <- compare_of(scenario = c("No 1", "No 3"), resample = "call", overdispersion = 4)

  expect_equal(wide$ci_lower - wide$diff, 2 * (plain$ci_lower - plain$diff))
  expect_equal(wide$ci_upper - wide$diff, 2 * (plain$ci_upper - plain$diff))
  expect_equal(wide$overdispersion, 4)
  expect_error(compare_of(overdispersion = 0.5), "1 or more")
})

test_that("predict_years gives future-period ranges, and needs data_years and calls", {
  set.seed(4)
  out <- compare_of(
    scenario = c("All", "No 1", "No 3"), resample = "call", simultaneous = TRUE,
    predict_years = 1, data_years = 3
  )
  expect_equal(unique(out$predict_years), 1)
  expect_equal(pairs_ci_text(out)$label, "95% simultaneous range")
  expect_equal(pairs_ci_text(out)$column, "95% Sim. 1-Yr Range")
  expect_match(pairs_ci_text(out)$source, "simultaneous ranges for a single year per metric")

  marginal <- compare_of(scenario = c("No 1", "No 3"), resample = "call", predict_years = 1, data_years = 3)
  expect_equal(pairs_ci_text(marginal)$label, "95% range for a single year")
  expect_true(is.na(compare_of(scenario = c("No 1", "No 3"))$predict_years))

  expect_error(compare_of(resample = "call", predict_years = 1), "needs data_years")
  expect_error(compare_of(predict_years = 1, data_years = 3), "resample = \"call\"")
})

test_that("overdispersion_factor compares period-to-period swings with chance", {
  # every zone keeps its share exactly: no more than chance
  steady <- rep(c("a", "b"), times = c(40, 20))
  expect_equal(overdispersion_factor(steady, rep(1:2, 30)), 1)

  # zone a 30 then 10, zone b 10 then 30: chi-square 20 on 1 df
  zone <- rep(c("a", "a", "b", "b"), times = c(30, 10, 10, 30))
  period <- rep(c(1, 2, 1, 2), times = c(30, 10, 10, 30))
  expect_equal(overdispersion_factor(zone, period), 20)
  expect_equal(overdispersion_factor(zone, rep(1, length(zone))), 1) # one period: can't tell
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

test_that("group_first puts a block per first scenario, rows named by the second", {
  df <- rbind(pairs_df(), transform(pairs_df(), scenario_a = "B", scenario_b = rep(c("C", "D"), 2)))
  p <- scenario_pairs_plot(df, metrics = "mean_minutes", group_first = TRUE)

  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(levels(p$data$group), c("A vs", "B vs"))
  expect_equal(levels(p$data$pair), c("D", "C", "B")) # first in order on top
  expect_equal(length(unique(ggplot2::layer_data(p, 3)$PANEL)), 2)
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

test_that("simultaneous intervals are labeled as such in the plot and table", {
  sim <- pairs_df()
  sim$ci_type <- "simultaneous"
  sim$per_interval_level <- rep(c(0.996, 0.9962), each = 2)
  sim$n_comparisons <- 2L

  text <- pairs_ci_text(sim)
  expect_equal(text$label, "95% simultaneous CI")
  expect_match(text$source, "each 99.6% alone")
  expect_match(text$meaning, "all 2 CIs hold together 95%")
  expect_equal(pairs_ci_text(pairs_df())$label, "95% CI")

  p <- scenario_pairs_plot(sim)
  expect_match(p$labels$subtitle, "simultaneous")
  expect_true("Within noise (95% simultaneous CI crosses 0)" %in% levels(p$data$verdict))

  ft <- scenario_pairs_table(sim)
  footer <- ft$footer$dataset[[1]]
  expect_true(any(grepl("hold together", footer)))

  sim$resample <- "call"
  sim$overdispersion <- 1.44
  expect_match(pairs_ci_text(sim)$source, "calls resampled, widened ×1.20 for year-to-year swings")
  expect_match(pairs_ci_text(pairs_df())$source, "resampling whole FDZs")
})
