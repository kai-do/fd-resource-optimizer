# Comparison tables on the mini fixtures (helper-fixtures.R). Baseline "All"
# (stations 1, 3, 7, 22); "No 1" closes station 1, so 1A's two calls go from
# station 1 to 7 (+6 minutes each); "No 3" closes station 3, so 3C's call goes
# from station 3 to 1.

runcard <- function() prep_runcard(make_runcard_raw_df())

three_scenarios <- function() {
  tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "No 1", c(3, 7, 22), FALSE,
    3, "No 3", c(1, 7, 22), FALSE
  )
}

compare_with <- function(fun, ...) {
  suppressMessages(fun(three_scenarios(), make_scenario_matrix_df(), runcard(), ...))
}

body <- function(ft) ft$body$dataset
header_rows <- function(ft) ft$header$dataset
# The header text as displayed (labels set with set_header_labels() are not
# in the header dataset)
header_text <- function(ft) {
  chunks <- flextable::information_data_chunk(ft)
  unique(chunks$txt[chunks$.part == "header"])
}

# overall_table ---------------------------------------------------------------

test_that("overall_table shows one scenario against the baseline by default", {
  ft <- overall_table(compare_with(compare_overall, thresholds = 4), baseline_name = "Baseline Rescues")
  b <- body(ft)

  expect_s3_class(ft, "flextable")
  expect_named(b, c("metric", "baseline", "s1_scenario", "s1_diff", "s1_pct"))
  expect_equal(nrow(header_rows(ft)), 3) # title, subtitle, column labels: no spanner
  expect_true("Total Minutes" %in% b$metric)

  total <- b[b$metric == "Total Minutes", ]
  expect_equal(c(total$baseline, total$s1_scenario, total$s1_diff), c("13", "25", "+12"))
  within <- b[b$metric == "Within 4 Min", ]
  expect_equal(c(within$baseline, within$s1_scenario, within$s1_diff), c("75.0%", "25.0%", "-50.0 pts"))
})

test_that("overall_table builds a Title Case title and subtitle", {
  ft <- overall_table(compare_with(compare_overall), baseline_name = "Baseline Rescues")
  text <- header_text(ft)

  expect_true("Scenario Comparison: Overall" %in% text)
  expect_true("All calls · 1st Due · No 1 vs Baseline Rescues" %in% text)
  expect_true(all(c("Metric", "Baseline", "Scenario", "Diff", "% Diff") %in% text))
})

test_that("overall_table puts several scenarios side by side, Baseline once", {
  ft <- overall_table(compare_with(compare_overall), scenario = c("No 1", "No 3"))

  expect_equal(sum(names(body(ft)) == "baseline"), 1)
  expect_true(all(c("No 1", "No 3") %in% header_text(ft)))
  expect_equal(nrow(header_rows(ft)), 4) # spanner row added
})

test_that("overall_table shows confidence intervals when present", {
  set.seed(1)
  ft <- overall_table(compare_with(compare_overall, ci = TRUE, n_boot = 50))

  expect_true("s1_ci" %in% names(body(ft)))
  expect_true("95% CI" %in% header_text(ft))
  expect_match(body(ft)$s1_ci[body(ft)$metric == "Mean Minutes"], "^\\[.*, .*\\]$")
})

# station_table ---------------------------------------------------------------

test_that("station_table shows one row per station with Baseline / Scenario / Diff", {
  ft <- station_table(compare_with(compare_by_station))
  b <- body(ft)

  expect_true("Station Workload: Incidents" %in% header_text(ft))
  station_1 <- b[b$source_id == "1", ]
  expect_equal(c(station_1$n_incidents_b, station_1$n_incidents_s, station_1$n_incidents_d), c("2", "0", "-2"))
  station_7 <- b[b$source_id == "7", ]
  expect_equal(station_7$n_incidents_d, "+2")
})

test_that("station_table fills stations absent from a scenario with 0 calls", {
  ft <- station_table(compare_with(compare_by_station), scenario = c("No 1", "No 3"))
  b <- body(ft)

  # station 3 responds in "No 1" but closes in "No 3"
  station_3 <- b[b$source_id == "3", ]
  expect_equal(station_3$baseline, "1")
  expect_equal(station_3$s2_s, "0")
  expect_equal(station_3$s2_d, "-1")
})

test_that("station_table takes several metrics for one scenario, one for several", {
  ft <- station_table(compare_with(compare_by_station), metrics = c("n_incidents", "total_minutes"))
  expect_true(all(c("Incidents", "Total Minutes") %in% header_text(ft)))

  expect_error(
    station_table(compare_with(compare_by_station), scenario = c("No 1", "No 3"), metrics = c("n_incidents", "n_fdz")),
    "pick one metric"
  )
})

# fdz_table -------------------------------------------------------------------

test_that("fdz_table shows each zone's responders and the metric diff", {
  ft <- fdz_table(compare_with(compare_by_fdz))
  b <- body(ft)
  zone_1a <- b[b$fdz_id == "1A", ]

  expect_equal(c(zone_1a$baseline_station, zone_1a$scenario_station), c("1", "7"))
  expect_equal(zone_1a$median_minutes_d, "+6.0")
  expect_true(all(c("FDZ", "Baseline Station", "Scenario Station") %in% header_text(ft)))
})

test_that("slower zones are coloured red, unchanged ones are not", {
  ft <- fdz_table(compare_with(compare_by_fdz))
  colours <- ft$body$styles$text$color$data[, "median_minutes_d"]

  expect_equal(unname(colours[body(ft)$fdz_id == "1A"]), "#B00020")
  expect_false(identical(unname(colours[body(ft)$fdz_id == "2B"]), "#B00020"))
})

test_that("zones sort by number, then letter", {
  expect_equal(
    c("12A", "1B", "4A", "LA", "1A", "27D")[natural_order(c("12A", "1B", "4A", "LA", "1A", "27D"))],
    c("1A", "1B", "4A", "12A", "27D", "LA")
  )
})

test_that("a narrow table is widened to fit its title", {
  ft <- calls_table(compare_with(compare_calls), title = strrep("Long Title ", 6))

  expect_gte(sum(dim(ft)$widths), nchar(strrep("Long Title ", 6)) * 0.13 - 1e-6)
})

# calls_table -----------------------------------------------------------------

test_that("calls_table lists the call-level changes", {
  ft <- calls_table(compare_with(compare_calls))
  b <- body(ft)

  expect_equal(b$s1[b$metric == "Share Slower"], "50.0%")
  expect_equal(b$s1[b$metric == "Mean Change (Minutes)"], "+3.0")
  expect_true("Value" %in% header_text(ft))
})

# Selection and labels ---------------------------------------------------------

test_that("the first filter set is used by default, with a message", {
  cmp <- compare_with(
    compare_overall,
    filters = list(incident_filter("All calls"), incident_filter("Day", time_range = c("06:00", "22:00")))
  )

  expect_message(ft <- overall_table(cmp), "also available: Day")
  expect_true(any(grepl("^All calls", header_text(ft))))
  expect_no_message(overall_table(cmp, filter_name = "Day"))
})

test_that("unknown scenarios and metrics are errors naming the choices", {
  cmp <- compare_with(compare_by_station)

  expect_error(station_table(cmp, scenario = "Nope"), "Available: No 1, No 3")
  expect_error(station_table(cmp, metrics = "speed"), "Unknown metric")
})

test_that("a scenario can be picked by id", {
  ft <- station_table(compare_with(compare_by_station), scenario = 3)

  expect_true(any(grepl("No 3 vs", header_text(ft))))
})

test_that("metric labels are Title Case", {
  expect_equal(
    metric_label(c("p90_minutes", "pct_within_12.37min", "fdz_p90_over_8min", "calls_slower_by_2min", "mean_distance_m")),
    c(
      "90th Percentile Minutes",
      "Within 12.37 Min",
      "FDZs With 90th Percentile Over 8 Min",
      "Calls Slower by More Than 2 Min",
      "Mean Distance (Miles)"
    )
  )
})

# save_table ------------------------------------------------------------------

test_that("save_table writes Word and PNG files", {
  ft <- station_table(compare_with(compare_by_station))
  docx <- tempfile(fileext = ".docx")
  png <- tempfile(fileext = ".png")

  save_table(ft, docx)
  save_table(ft, png)

  expect_true(file.exists(docx) && file.size(docx) > 0)
  expect_true(file.exists(png) && file.size(png) > 0)
  expect_error(save_table(ft, tempfile(fileext = ".pdf")), "docx or .png")
})
