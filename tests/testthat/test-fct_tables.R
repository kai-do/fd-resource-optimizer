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

test_that("overall_table names a future-year range and explains it", {
  df <- compare_with(compare_overall, ci = TRUE, n_boot = 20)
  df$resample <- "call"
  df$predict_years <- 1
  ft <- overall_table(df)

  expect_true("95% 1-Yr Range" %in% header_text(ft))
  expect_true(any(grepl("would fall over a single year", ft$footer$dataset[[1]])))
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

test_that("pct_total adds each row's share of its column's total", {
  ft <- station_table(compare_with(compare_by_station), pct_total = TRUE)
  b <- body(ft)
  share <- function(x) as.numeric(sub("%", "", x))

  expect_named(b, c("source_id", "n_incidents_b", "n_incidents_bp", "n_incidents_s", "n_incidents_sp", "n_incidents_d"))
  expect_equal(sum(header_text(ft) == "% of Total"), 1) # one label text, used twice
  station_1 <- b[b$source_id == "1", ]
  expect_equal(c(station_1$n_incidents_bp, station_1$n_incidents_sp), c("50.0%", "0.0%"))
  expect_equal(sum(share(b$n_incidents_bp)), 100)
  expect_equal(sum(share(b$n_incidents_sp)), 100)
})

test_that("pct_total works with several scenarios and is off by default", {
  ft <- station_table(compare_with(compare_by_station), scenario = c("No 1", "No 3"), pct_total = TRUE)
  expect_true(all(c("baseline_pct", "s1_sp", "s2_sp") %in% names(body(ft))))

  expect_false(any(grepl("p$", names(body(station_table(compare_with(compare_by_station)))))))
})

test_that("pct_total skips metrics that are not counts", {
  expect_message(
    ft <- fdz_table(compare_with(compare_by_fdz), pct_total = TRUE),
    "only shown for counts"
  )
  expect_false("% of Total" %in% header_text(ft))
})

# fdz_table -------------------------------------------------------------------

test_that("fdz_table shows each zone's responders and the metric diff", {
  ft <- fdz_table(compare_with(compare_by_fdz))
  b <- body(ft)
  zone_1a <- b[b$fdz_id == "1A", ]

  expect_equal(c(zone_1a$baseline_station, zone_1a$scenario_station), c("1", "7"))
  expect_equal(zone_1a$median_minutes_d, "+6.00")
  expect_true(all(c("FDZ", "Baseline Station", "Scenario Station") %in% header_text(ft)))
})

test_that("slower zones are colored red, unchanged ones are not", {
  ft <- fdz_table(compare_with(compare_by_fdz))
  colors <- ft$body$styles$text$color$data[, "median_minutes_d"]

  expect_equal(unname(colors[body(ft)$fdz_id == "1A"]), "#B00020")
  expect_false(identical(unname(colors[body(ft)$fdz_id == "2B"]), "#B00020"))
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

# territory_table -------------------------------------------------------------

test_that("territory_table shows one row per territory, colored better or worse", {
  ft <- territory_table(compare_with(compare_by_territory))
  b <- body(ft)

  # territories, not responders: station 7 responds but has no territory
  expect_equal(b$station_id, c("1", "3", "51"))
  expect_equal(b$p90_minutes_d[b$station_id == "1"], "+6.00")
  expect_true("Territory" %in% header_text(ft))
  expect_match(header_text(ft)[1], "Station Territory Comparison")

  colors <- ft$body$styles$text$color$data[, "p90_minutes_d"]
  expect_equal(unname(colors[b$station_id == "1"]), "#B00020")
})

test_that("territory_table takes several metrics or several scenarios", {
  ft <- territory_table(compare_with(compare_by_territory), metrics = c("n_calls", "p90_minutes"), pct_total = TRUE)
  # calls are the same on both sides: one count and its share, no diff
  expect_equal(names(body(ft))[2:3], c("n_calls_v", "n_calls_vp"))
  expect_false(any(c("n_calls_b", "n_calls_s", "n_calls_d") %in% names(body(ft))))
  expect_equal(body(ft)$n_calls_v, c("2", "1", "1"))
  expect_equal(body(ft)$n_calls_vp, c("50.0%", "25.0%", "25.0%"))
  expect_true(all(c("Count", "% of Total", "Calls") %in% header_text(ft)))
  expect_true("p90_minutes_d" %in% names(body(ft)))

  calls_only <- territory_table(compare_with(compare_by_territory), scenario = c("No 1", "No 3"), metrics = "n_calls")
  expect_named(body(calls_only), c("station_id", "n_calls_v"))

  ft <- territory_table(compare_with(compare_by_territory), scenario = c("No 1", "No 3"))
  expect_true(all(c("s1_d", "s2_d") %in% names(body(ft))))
})

# scenario_summary_table -------------------------------------------------------

footer_text <- function(ft) {
  chunks <- flextable::information_data_chunk(ft)
  paste(chunks$txt[chunks$.part == "footer"], collapse = " ")
}

test_that("the summary has the baseline's values, then each scenario's value and diff", {
  cmp <- compare_with(compare_overall, thresholds = 4)
  ft <- scenario_summary_table(cmp, baseline_name = "Baseline Rescues")
  b <- body(ft)
  get <- function(sc, m, col) cmp[[col]][cmp$scenario_name == sc & cmp$metric == m]

  expect_equal(b$scenario, c("Baseline Rescues", "No 1", "No 3"))
  expect_named(b, c("scenario", "median_minutes", "p90_minutes", "pct_within_4min", "total_minutes"))
  expect_equal(b$total_minutes[1], "13")
  expect_equal(b$total_minutes[2], "25 (+12)") # 7.1 + 7.2 + 7.3 + 3.4, 12 more than the baseline
  expect_equal(
    b$total_minutes[3],
    paste0(
      format_values(get("No 3", "total_minutes", "scenario"), "total_minutes"),
      " (", format_values(get("No 3", "total_minutes", "diff"), "total_minutes", diff = TRUE), ")"
    )
  )
  expect_equal(b$pct_within_4min[2], "25.0% (-50.0 pts)")
  expect_true("vs Baseline Rescues" %in% unlist(strsplit(header_text(ft)[2], " · ")))
})

test_that("only the diff in parentheses is colored", {
  ft <- scenario_summary_table(compare_with(compare_overall))
  chunks <- flextable::information_data_chunk(ft)
  cell <- chunks[chunks$.part == "body" & chunks$.row_id == 2 & chunks$.col_id == "total_minutes", ]

  expect_equal(paste(cell$txt, collapse = ""), "25 (+12)")
  expect_equal(cell$color[cell$txt == "+12"], "#B00020") # No 1: 12 minutes more, worse
  expect_false(identical(cell$color[cell$txt == "25"], "#B00020"))
})

test_that("sort_by puts the best scenario first", {
  ft <- scenario_summary_table(compare_with(compare_overall), sort_by = "total_minutes")
  expect_equal(body(ft)$scenario, c("Baseline", "No 3", "No 1")) # No 1 adds 12 minutes

  expect_error(scenario_summary_table(compare_with(compare_overall), sort_by = "speed"), "must be a column")
  expect_error(scenario_summary_table(compare_with(compare_overall), metrics = "n_calls", sort_by = "n_calls"), "no better direction")
})

test_that("the best scenario in each column is bold", {
  ft <- scenario_summary_table(compare_with(compare_overall))
  bold <- ft$body$styles$text$bold$data[, "total_minutes"]

  expect_equal(unname(bold), c(FALSE, FALSE, TRUE)) # No 3, not No 1 (+12)
})

test_that("calls and station columns are added when given", {
  ft <- scenario_summary_table(
    compare_with(compare_overall),
    calls_df = compare_with(compare_calls, slower_by = c(2, 5)),
    station_df = compare_with(compare_by_station)
  )
  b <- body(ft)

  expect_true(all(c("share_slower", "calls_slower_by_5min", "busiest_station") %in% names(b)))
  expect_equal(b$share_slower[2], "50.0%")
  expect_equal(b$calls_slower_by_5min[2], "2") # 1A's two calls are 6 minutes slower
  expect_equal(b$busiest_station[1], "50.0% (1)") # baseline: station 1 takes 2 of 4 calls
  expect_match(b$busiest_station[2], "\\(7\\)$") # No 1: station 7 takes over 1A
  expect_match(footer_text(ft), "call-level and busiest station columns: scenario values only")
})

test_that("the footer only mentions the extra columns when they are shown", {
  ft <- scenario_summary_table(compare_with(compare_overall))

  expect_no_match(footer_text(ft), "call-level")
  expect_match(footer_text(ft), "First row: baseline values; other rows: scenario values, change from baseline in parentheses")
})

test_that("a subset of scenarios can be shown, and yearly counts are labeled", {
  ft <- scenario_summary_table(annualize_counts(compare_with(compare_overall), years = 2), scenario = "No 3")

  expect_equal(body(ft)$scenario, c("Baseline", "No 3"))
  expect_true("Total Minutes per Year" %in% header_text(ft))
})

# rank_scenarios --------------------------------------------------------------

test_that("scenarios are ranked best first", {
  cmp <- compare_with(compare_overall, thresholds = 4)

  expect_equal(rank_scenarios(cmp, by = "total_minutes"), c("No 3", "No 1")) # No 1 adds 12 minutes
  expect_error(rank_scenarios(cmp, by = "n_calls"), "no better direction")
  expect_error(rank_scenarios(cmp, by = "speed"), "Unknown metric")
})

# calls_table -----------------------------------------------------------------

test_that("calls_table lists the call-level changes", {
  ft <- calls_table(compare_with(compare_calls))
  b <- body(ft)

  expect_equal(b$s1[b$metric == "Share Slower"], "50.0%")
  expect_equal(b$s1[b$metric == "Mean Change (Minutes)"], "+3.00")
  expect_true("Value" %in% header_text(ft))
})

# Time formats ----------------------------------------------------------------

test_that("times show 2 decimals by default, or as many as digits asks", {
  expect_equal(format_values(7.456, "median_minutes"), "7.46")
  expect_equal(format_values(7.456, "median_minutes", digits = 1), "7.5")
  expect_equal(format_values(-0.5, "mean_change", diff = TRUE), "-0.50")
})

test_that("mmss shows times as m:ss, with a sign on diffs", {
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format = "mmss")

  expect_equal(fmt(7.5, "median_minutes"), "7:30")
  expect_equal(fmt(12.37, "p90_minutes"), "12:22")
  expect_equal(fmt(c(1.0833, -0.5, 0), "mean_change", diff = TRUE), c("+1:05", "-0:30", "0:00"))
  expect_equal(fmt(59.999, "median_minutes"), "60:00") # rounds up into the next minute
  # counts, shares and distance are unchanged
  expect_equal(fmt(1234, "total_minutes"), "1,234")
  expect_equal(fmt(0.25, "pct_within_8min"), "25.0%")
})

test_that("every table takes time_format and notes it in the footer", {
  footer <- function(ft) paste(ft$footer$dataset[[1]], collapse = " ")

  ft <- fdz_table(compare_with(compare_by_fdz), time_format = "mmss")
  expect_equal(body(ft)$median_minutes_d[body(ft)$fdz_id == "1A"], "+6:00")
  expect_match(footer(ft), "minutes:seconds")

  ft <- calls_table(compare_with(compare_calls), time_format = "mmss")
  expect_equal(body(ft)$s1[body(ft)$metric == "Mean Change (Minutes)"], "+3:00")

  expect_s3_class(overall_table(compare_with(compare_overall), time_format = "mmss"), "flextable")
  expect_s3_class(station_table(compare_with(compare_by_station), time_format = "mmss"), "flextable")
  expect_s3_class(scenario_summary_table(compare_with(compare_overall), time_format = "mmss"), "flextable")
  expect_no_match(footer(overall_table(compare_with(compare_overall))), "minutes:seconds")
  expect_error(overall_table(compare_with(compare_overall), time_format = "hms"), "should be one of")
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
    metric_label(c("p90_minutes", "pct_within_10m21s", "fdz_p90_over_8min", "calls_slower_by_2min", "mean_distance_m")),
    c(
      "90th Percentile Minutes",
      "Within 10:21 Min",
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

# p_median_solutions_table ----------------------------------------------------

make_solutions_df <- function() {
  tibble::tibble(
    rank = 1:2,
    objective = c(300, 330), # the optimizer's own totals: never shown
    added_stations = list(c(7, 4), c(4, 22)),
    move_from = list(c(1, 2), c(1, 2)),
    move_to = list(7, 22)
  )
}

# Run card scoring of the two solutions against today's engines (360 min):
# #2 scores better on the run card than #1
make_solutions_overall <- function() {
  rows <- function(sc, total, p90, pct) {
    data.frame(
      filter_name = "All calls", due = 1, scenario_name = sc,
      metric = c("total_minutes", "p90_minutes", "pct_within_8min"),
      baseline = c(360, 9, 0.80),
      scenario = c(total, p90, pct),
      diff = c(total - 360, p90 - 9, pct - 0.80)
    )
  }
  rbind(rows("P-median #1", 320, 8.5, 0.85), rows("P-median #2", 300, 8, 0.90))
}

test_that("p_median_solutions_table shows run card values, best first, after today's row", {
  ft <- p_median_solutions_table(make_solutions_df(), make_solutions_overall(), metrics = c("p90_minutes", "pct_within_8min"))
  b <- body(ft)

  expect_equal(b$rank, c("Today's engines", "#2", "#1"))
  expect_equal(b$added, c("", "4, 22", "4, 7"))
  expect_equal(b$move_to, c("", "22", "7"))
  expect_equal(b$total, c("360", "300", "320")) # run card, not the objective (300, 330)
  expect_equal(b$diff, c("", "-60", "-40"))
  expect_equal(b$pct, c("", "-16.7%", "-11.1%"))
  expect_equal(b$pct_within_8min, c("80.0%", "90.0% (+10.0 pts)", "85.0% (+5.0 pts)"))
  footer <- paste(ft$footer$dataset[[1]], collapse = " ")
  expect_match(footer, "CAD run card order")
  expect_no_match(footer, "cheapest open station")
})

test_that("p_median_solutions_table needs every solution's metrics", {
  expect_error(
    p_median_solutions_table(make_solutions_df(), make_solutions_overall(), metrics = "pct_within_10min")
  )
})
