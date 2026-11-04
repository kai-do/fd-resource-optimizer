# Uses make_scenario_matrix_df() (alarms 2024-01-01 to 2024-01-03),
# make_scenarios() and make_runcard_raw_df() from helper-fixtures.R.

runcard <- function() prep_runcard(make_runcard_raw_df())

compare_with <- function(fun, ...) {
  suppressMessages(fun(make_scenarios(), make_scenario_matrix_df(), runcard(), ...))
}

# filter_years ----------------------------------------------------------------

test_that("a date_range covers its days, both ends included", {
  years <- filter_years(incident_filter("Last 3 years", date_range = c("2023-10-01", "2026-09-30")))

  expect_equal(years$filter_name, "Last 3 years")
  expect_equal(years$start, as.Date("2023-10-01"))
  expect_equal(years$end, as.Date("2026-09-30"))
  expect_equal(years$years, 1096 / 365.25) # includes 2024-02-29
})

test_that("a filter set without a date_range covers the data's dates", {
  years <- filter_years(list(incident_filter("All calls")), make_scenario_matrix_df())

  expect_equal(years$start, as.Date("2024-01-01"))
  expect_equal(years$end, as.Date("2024-01-03"))
  expect_equal(years$years, 3 / 365.25)
})

test_that("a filter set without a date_range needs the data", {
  expect_error(filter_years(incident_filter("All calls")), "has no date_range")
})

test_that("a date_range past the data keeps its whole window, with a message", {
  f <- incident_filter("Wide", date_range = c("2023-01-01", "2024-12-31"))

  expect_message(years <- filter_years(f, make_scenario_matrix_df()), "past the data")
  expect_equal(years$years, 731 / 365.25)
})

# annualize_counts ------------------------------------------------------------

test_that("call counts are divided by the years; times and shares are not", {
  cmp <- compare_with(compare_overall, thresholds = 4)
  yearly <- annualize_counts(cmp, years = 2)
  row <- function(df, m) df[df$metric == m, ]

  calls <- row(yearly, "n_calls")
  expect_equal(c(calls$baseline, calls$scenario, calls$diff), c(4, 4, 0) / 2)
  expect_equal(row(yearly, "total_minutes")$baseline, row(cmp, "total_minutes")$baseline / 2)
  expect_equal(row(yearly, "total_minutes")$pct_diff, row(cmp, "total_minutes")$pct_diff)
  expect_equal(row(yearly, "median_minutes")$baseline, row(cmp, "median_minutes")$baseline)
  expect_equal(row(yearly, "pct_within_4min")$baseline, row(cmp, "pct_within_4min")$baseline)
  expect_equal(unique(yearly$years), 2)
})

test_that("zone counts are not call counts", {
  cmp <- compare_with(compare_by_station)
  yearly <- annualize_counts(cmp, years = 2)

  expect_equal(yearly$baseline[yearly$metric == "n_fdz"], cmp$baseline[cmp$metric == "n_fdz"])
  expect_equal(yearly$baseline[yearly$metric == "n_incidents"], cmp$baseline[cmp$metric == "n_incidents"] / 2)
})

test_that("call-level counts and the stats tables are annualized too", {
  calls <- annualize_counts(compare_with(compare_calls, slower_by = 2), years = 2)
  # closing station 1 makes 1A's two calls 6 minutes slower
  expect_equal(calls$value[calls$metric == "calls_slower_by_2min"], 2 / 2)
  expect_equal(calls$value[calls$metric == "share_slower"], 0.5)

  stats <- compare_with(stats_by_station)
  yearly <- annualize_counts(stats, years = 2)
  expect_equal(yearly$n_incidents, stats$n_incidents / 2)
  expect_equal(yearly$n_fdz, stats$n_fdz)
})

test_that("each filter set uses its own years", {
  filters <- list(
    incident_filter("All calls"),
    incident_filter("Jan 1", date_range = c("2024-01-01", "2024-01-01"))
  )
  cmp <- compare_with(compare_overall, filters = filters)
  yearly <- annualize_counts(cmp, filter_years(filters, make_scenario_matrix_df()))
  calls <- yearly[yearly$metric == "n_calls", ]

  expect_equal(calls$baseline[calls$filter_name == "All calls"], 4 / (3 / 365.25))
  expect_equal(calls$baseline[calls$filter_name == "Jan 1"], 2 / (1 / 365.25))
})

test_that("annualizing twice or with unknown filter sets is an error", {
  cmp <- compare_with(compare_overall)

  expect_error(annualize_counts(annualize_counts(cmp, 2), 2), "already yearly")
  expect_error(annualize_counts(cmp, data.frame(filter_name = "Other", years = 1)), "No years for filter set")
})

# Tables ------------------------------------------------------------------------

test_that("tables label yearly counts per Year and say so in the footer", {
  ft <- overall_table(annualize_counts(compare_with(compare_overall), years = 3))
  labels <- ft$body$dataset$metric
  chunks <- flextable::information_data_chunk(ft)
  footer <- paste(chunks$txt[chunks$.part == "footer"], collapse = "")

  expect_true(all(c("Calls per Year", "Total Minutes per Year", "Median Minutes") %in% labels))
  expect_match(footer, "yearly averages over 3.0 years")
})

test_that("station table titles carry per Year", {
  ft <- station_table(annualize_counts(compare_with(compare_by_station), years = 2))
  chunks <- flextable::information_data_chunk(ft)

  expect_true("Station Workload: Incidents per Year" %in% chunks$txt[chunks$.part == "header"])
})
