# make_scenario_matrix_df() and make_scenarios() are in helper-fixtures.R

runcard <- function() prep_runcard(make_runcard_raw_df())

# stats_by_station ------------------------------------------------------------

test_that("stats_by_station gives each responder's stats per scenario", {
  out <- stats_by_station(make_scenarios(), make_scenario_matrix_df(), runcard())

  all <- out[out$scenario_name == "All", ]
  expect_equal(all$source_id, c(1L, 3L, 7L))
  expect_equal(all$n_incidents, c(2L, 1L, 1L))
  expect_equal(all$mean_minutes, c(1.15, 3.4, 7.3))

  # with station 1 closed, 7 is 1st due in 1A as well as 2B
  no_1 <- out[out$scenario_name == "No 1", ]
  expect_equal(no_1$source_id, c(3L, 7L))
  expect_equal(no_1$n_incidents, c(1L, 3L))
  expect_equal(no_1$n_fdz, c(1L, 2L))
  expect_equal(no_1$median_minutes, c(3.4, 7.2))
})

test_that("stats_by_station has the scenario and filter columns first", {
  out <- stats_by_station(make_scenarios(), make_scenario_matrix_df(), runcard())

  expect_equal(
    names(out)[1:7],
    c("filter_name", "scenario_id", "scenario_name", "due", "source_id", "n_incidents", "n_fdz")
  )
  expect_true(all(out$filter_name == "All calls"))
})

# stats_by_fdz ----------------------------------------------------------------

test_that("stats_by_fdz equals filter_by_runcard + summarize_travel_stats", {
  matrix_df <- make_scenario_matrix_df()
  stations <- c(1, 3, 7, 22)

  out <- stats_by_fdz(make_scenarios()[1, ], matrix_df, runcard())
  direct <- filter_by_runcard(matrix_df, runcard(), stations) |>
    summarize_travel_stats(by = c("due", "fdz_id", "source_id", "station_id"))

  expect_equal(out$fdz_id, direct$fdz_id)
  expect_equal(out$source_id, direct$source_id)
  expect_equal(out$mean_minutes, direct$mean_minutes)
  expect_equal(out$station_id, c(1L, 51L, 3L))
})

test_that("stats_by_fdz gives one row per zone and due level", {
  out <- stats_by_fdz(make_scenarios()[1, ], make_scenario_matrix_df(), runcard(), due = 1:2)

  expect_equal(out$fdz_id, c("1A", "2B", "3C", "1A", "2B", "3C"))
  expect_equal(out$due, c(1L, 1L, 1L, 2L, 2L, 2L))
  expect_equal(out$source_id, c(1L, 7L, 3L, 7L, 1L, 1L))
})

# Filter sets -----------------------------------------------------------------

test_that("every scenario runs under every filter set, in the order given", {
  night <- incident_filter("Night", time_range = c("22:00", "06:00"))
  filters <- list(night, incident_filter("All calls"))

  out <- stats_by_station(make_scenarios(), make_scenario_matrix_df(), runcard(), filters = filters)

  expect_equal(unique(out$filter_name), c("Night", "All calls"))
  expect_equal(unique(out$scenario_name), c("All", "No 1"))

  # at night only I2 (1A) counts: station 1 responds in "All", station 7 in "No 1"
  night_rows <- out[out$filter_name == "Night", ]
  expect_equal(night_rows$source_id, c(1L, 7L))
  expect_equal(night_rows$n_incidents, c(1L, 1L))
  expect_equal(night_rows$mean_minutes, c(1.2, 7.2))
})

test_that("a single filter set does not need a list", {
  night <- incident_filter("Night", time_range = c("22:00", "06:00"))

  out <- stats_by_fdz(make_scenarios(), make_scenario_matrix_df(), runcard(), filters = night)

  expect_true(all(out$filter_name == "Night"))
})

test_that("filter set names must be unique", {
  f <- incident_filter("Same")

  expect_error(
    stats_by_station(make_scenarios(), make_scenario_matrix_df(), runcard(), filters = list(f, f)),
    "unique"
  )
})

# Messages and checks ---------------------------------------------------------

test_that("zones with no responder are reported with the scenario's name", {
  # with only station 22 open, 2B's card (7, 1) has no open station
  scenarios <- tibble::tribble(~scenario_id, ~scenario_name, ~stations, 1, "Only 22", 22)

  expect_message(
    stats_by_station(scenarios, make_scenario_matrix_df(), runcard()),
    "All calls / Only 22: 1 zones have no due 1 station"
  )
})

test_that("scenarios are checked", {
  matrix_df <- make_scenario_matrix_df()
  dup <- make_scenarios()
  dup$scenario_id <- 1

  expect_error(stats_by_station(dup, matrix_df, runcard()), "unique")
  expect_error(stats_by_station(make_scenarios()[, 1:2], matrix_df, runcard()), "stations")

  empty <- make_scenarios()
  empty$stations[[2]] <- numeric()
  expect_error(stats_by_station(empty, matrix_df, runcard()), "No 1")
})

test_that("a plain stations column means one station per scenario", {
  scenarios <- data.frame(scenario_id = 1:2, scenario_name = c("Only 1", "Only 7"), stations = c(1, 7))

  out <- suppressMessages(stats_by_station(scenarios, make_scenario_matrix_df(), runcard()))

  expect_equal(out$scenario_name, c("Only 1", "Only 7"))
  expect_equal(out$source_id, c(1L, 7L))
})
