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

test_that("scenario_stub makes scenario names into folder names", {
  expect_equal(
    scenario_stub(c("R26->16 +R1", "+R1", "Baseline Rescues", "R11->16 +R27")),
    c("R26_16_R1", "R1", "Baseline_Rescues", "R11_16_R27")
  )
})

test_that("als_summary_scenarios lists today's rescues, the rescues, then each engine set", {
  out <- als_summary_scenarios(
    baseline_rescues = c(26, 11),
    rescue_stations = c(16, 11, 1),
    current_engines = c(1, 2, 9),
    p_median_open = list(c(1, 11, 16, 4), list(c(1, 11, 16, 5))),
    extra = list("Call Volume" = c(8, 19)),
    label = "COA 1"
  )

  expect_equal(
    out$scenario_name,
    c(
      "Current Setup", "Baseline Rescues", "COA 1 Rescues", "COA 1 + Current ALS",
      "COA 1 + P-median #1", "COA 1 + P-median #2", "COA 1 + Call Volume"
    )
  )
  expect_equal(out$scenario_id, 1:7)
  expect_equal(out$baseline, c(TRUE, rep(FALSE, 6)))
  expect_equal(
    out$stations,
    list(c(1, 2, 9, 11, 26), c(11, 26), c(1, 11, 16), c(1, 2, 9, 11, 16), c(1, 4, 11, 16), c(1, 5, 11, 16), c(1, 8, 11, 16, 19))
  )
})

test_that("als_summary_scenarios drops a missing extra and warns on engines at rescues", {
  out <- als_summary_scenarios(c(11), c(1, 11), c(2), extra = NULL)
  expect_equal(out$scenario_name, c("Current Setup", "Baseline Rescues", "Rescues Rescues", "Rescues + Current ALS"))

  expect_warning(
    als_summary_scenarios(c(11), c(1, 11), c(2), extra = list("Call Volume" = c(11, 8)), label = "COA 2"),
    "COA 2 + Call Volume: engines at rescue stations add no location: 11",
    fixed = TRUE
  )
  expect_error(als_summary_scenarios(c(11), c(1), c(2), extra = list(c(8))), "named list")
})

test_that("als_plan_codes joins the methods that found a plan and numbers p-medians by run card rank", {
  methods <- list(
    c("Greedy Moves", "Greedy Build-Up", "P-median #1"),
    "P-median #2",
    "P-median #3",
    "Call Volume"
  )
  # optimizer #3 scores better than #2 on the run card
  codes <- als_plan_codes(methods, c(100, 103, 102, 110))

  expect_equal(codes, c("GM=GB=PM1", "PM3", "PM2", "CV"))
  # code order is fixed whatever order the methods come in
  expect_equal(als_plan_codes(list(c("P-median #4", "Greedy Build-Up")), 1), "GB=PM1")
  expect_equal(als_plan_codes(list("Night Shift"), 1), "Night Shift")
  expect_error(als_plan_codes(methods, 1:2), "one entry per plan")
})

test_that("als_scenario_names puts the method in parentheses after the rescue scenario", {
  expect_equal(
    als_scenario_names("R26->16 +R1", c("GM=GB=PM1", "CV")),
    c(
      "Baseline Rescues (CUR)", "Baseline Rescues", "R26->16 +R1", "R26->16 +R1 (CUR)",
      "R26->16 +R1 (GM=GB=PM1)", "R26->16 +R1 (CV)"
    )
  )
  expect_equal(als_scenario_names("+R1", character())[3:4], c("+R1", "+R1 (CUR)"))
})
