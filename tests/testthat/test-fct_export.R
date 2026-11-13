# Uses make_scenario_matrix_df(), make_scenarios() and make_runcard_raw_df()
# from helper-fixtures.R. Workbooks go to temporary files.

runcard <- function() prep_runcard(make_runcard_raw_df())

night_als <- function() {
  incident_filter("Night ALS", time_range = c("22:00", "06:00"), ems_capability = c("PARAMEDIC", NA))
}

export_to_temp <- function(...) {
  file <- tempfile(fileext = ".xlsx")
  sheets <- suppressMessages(
    export_comparison(make_scenarios(), make_scenario_matrix_df(), runcard(), file, ...)
  )
  list(file = file, sheets = sheets)
}

read_sheet <- function(file, sheet) openxlsx::read.xlsx(file, sheet = sheet)

test_that("the workbook has the eight sheets in order", {
  out <- export_to_temp()

  expected <- c("Settings", "Scenarios", "Filter Sets", "Overall", "Calls", "By FDZ", "By Station", "By Territory")
  expect_equal(openxlsx::getSheetNames(out$file), expected)
  expect_named(out$sheets, expected)
})

test_that("the call-level paired table comes back as an attribute, not a sheet", {
  out <- export_to_temp()
  paired <- attr(out$sheets, "paired")

  expect_true(all(c("baseline_minutes", "scenario_minutes", "station_id") %in% names(paired)))
  expect_false("paired" %in% openxlsx::getSheetNames(out$file))
})

test_that("the comparison sheets hold the compare_*() results", {
  out <- export_to_temp(thresholds = 4, slower_by = 2)
  args <- list(make_scenarios(), make_scenario_matrix_df(), runcard())

  direct <- list(
    "Overall" = do.call(compare_overall, c(args, thresholds = 4)),
    "Calls" = do.call(compare_calls, c(args, slower_by = 2)),
    "By FDZ" = do.call(compare_by_fdz, c(args, thresholds = 4)),
    "By Station" = do.call(compare_by_station, args),
    "By Territory" = do.call(compare_by_territory, c(args, thresholds = 4))
  )

  for (sheet in names(direct)) {
    expect_equal(
      read_sheet(out$file, sheet),
      as.data.frame(direct[[sheet]]),
      ignore_attr = TRUE,
      info = sheet
    )
  }
})

test_that("a matrix without territory stations has no By Territory sheet", {
  file <- tempfile(fileext = ".xlsx")
  matrix_df <- make_scenario_matrix_df()
  matrix_df$station_id <- NULL
  sheets <- suppressMessages(export_comparison(make_scenarios(), matrix_df, runcard(), file))

  expect_false("By Territory" %in% names(sheets))
  expect_false("By Territory" %in% openxlsx::getSheetNames(file))
})

test_that("the Scenarios sheet lists each scenario and marks the baseline", {
  scenarios <- read_sheet(export_to_temp()$file, "Scenarios")

  expect_equal(scenarios$scenario_name, c("All", "No 1"))
  expect_equal(scenarios$baseline, c("Yes", ""))
  expect_equal(scenarios$n_stations, c(4, 3))
  expect_equal(scenarios$stations, c("1, 3, 7, 22", "3, 7, 22"))
})

test_that("the Filter Sets sheet lists every active filter", {
  out <- export_to_temp(filters = list(incident_filter("All calls"), night_als()))
  filters <- read_sheet(out$file, "Filter Sets")

  expect_equal(filters$filter_name, c("All calls", "Night ALS", "Night ALS"))
  expect_equal(filters$filter, c("(none)", "time_range", "ems_capability"))
  expect_equal(filters$value, c("All incidents", "22:00 to 06:00", "PARAMEDIC, (blank)"))
})

test_that("the Settings sheet records the run's settings", {
  settings <- read_sheet(export_to_temp(due = 1:2, thresholds = c(4, 8), ci = TRUE, n_boot = 50, seed = 7)$file, "Settings")
  value <- function(s) settings$value[settings$setting == s]

  expect_equal(value("due levels"), "1, 2")
  expect_equal(value("thresholds (minutes)"), "4, 8")

  mmss <- read_sheet(export_to_temp(thresholds = c(8, 8 + 58 / 60))$file, "Settings")
  expect_equal(mmss$value[mmss$setting == "thresholds (minutes)"], "8, 8:58")
  expect_equal(value("intervals"), "95% CI")
  expect_equal(value("bootstrap resamples"), "50")
  expect_equal(value("resampled"), "whole zones")
  expect_equal(value("seed"), "7")
  expect_equal(value("calls in travel matrix"), "4")

  ranged <- read_sheet(
    export_to_temp(ci = TRUE, n_boot = 20, resample = "call", predict_years = 1)$file, "Settings"
  )
  expect_equal(ranged$value[ranged$setting == "intervals"], "95% range for a single year")
  expect_equal(ranged$value[ranged$setting == "resampled"], "calls")
})

test_that("a future-period range needs call resampling", {
  expect_error(export_to_temp(ci = TRUE, predict_years = 1), "resample = \"call\"")
})

test_that("the same seed gives the same confidence intervals", {
  first <- export_to_temp(ci = TRUE, n_boot = 100, seed = 11)$sheets$Overall
  second <- export_to_temp(ci = TRUE, n_boot = 100, seed = 11)$sheets$Overall

  expect_true("ci_lower" %in% names(first))
  expect_identical(first$ci_lower, second$ci_lower)
  expect_identical(first$ci_upper, second$ci_upper)
})

test_that("a filter set that keeps no calls gives no comparison rows", {
  # the only night call (I2) is AEMT, so Night ALS keeps nothing
  out <- export_to_temp(filters = list(incident_filter("All calls"), night_als()))

  expect_true(all(out$sheets$Overall$filter_name == "All calls"))
  expect_true("Night ALS" %in% out$sheets$`Filter Sets`$filter_name)
})

test_that("shares are formatted as percentages", {
  file <- export_to_temp()$file
  wb <- openxlsx::loadWorkbook(file)
  styles <- openxlsx::getStyles(wb)

  expect_true(any(vapply(styles, function(s) identical(s$numFmt$formatCode, "0.0%"), logical(1))))
})
