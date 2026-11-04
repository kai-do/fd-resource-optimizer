# A hand-made travel matrix: 2 stations x 3 incidents, with times chosen so
# every answer can be worked out by hand.
#
#   duration_min   I1  I2  I3
#   station 1       3   6   9
#   station 2       5   4   2
#
# I1 and I2 are in zone 1A (territory station 1), I3 in zone 2B (station 2).
# So the closest station is 1 for I1 and 2 for I2 and I3, and I2 is the one
# incident whose own station is not the fastest.
make_matrix_df <- function() {
  data.frame(
    source_id = rep(1:2, times = 3),
    dest_id = rep(c("I1", "I2", "I3"), each = 2),
    duration_min = c(3, 5, 6, 4, 9, 2),
    distance_m = c(3000, 5000, 6000, 4000, 9000, 2000),
    fdz_id = rep(c("1A", "1A", "2B"), each = 2),
    station_id = rep(c(1L, 1L, 2L), each = 2),
    incident_alarm_dt = rep(
      as.POSIXct(c("2024-01-01 10:00", "2024-02-01 10:00", "2024-03-01 10:00"), tz = "America/New_York"),
      each = 2
    )
  )
}

# closest_station -------------------------------------------------------------

test_that("closest_station keeps the fastest station for each incident", {
  out <- closest_station(make_matrix_df())

  expect_equal(out$dest_id, c("I1", "I2", "I3"))
  expect_equal(out$source_id, c(1L, 2L, 2L))
  expect_equal(out$duration_min, c(3, 4, 2))
})

test_that("closest_station keeps the incident columns", {
  out <- closest_station(make_matrix_df())

  expect_named(out, names(make_matrix_df()))
  expect_equal(out$fdz_id, c("1A", "1A", "2B"))
})

test_that("closest_station breaks ties by the lowest station id", {
  matrix_df <- make_matrix_df()
  matrix_df$duration_min[matrix_df$dest_id == "I1"] <- 5

  out <- closest_station(matrix_df)

  expect_equal(out$source_id[out$dest_id == "I1"], 1L)
})

test_that("closest_station skips unroutable pairs", {
  matrix_df <- make_matrix_df()
  matrix_df$duration_min[1] <- NA # station 1 to I1

  out <- closest_station(matrix_df)

  expect_equal(out$source_id[out$dest_id == "I1"], 2L)
  expect_equal(out$duration_min[out$dest_id == "I1"], 5)
})

# territory_travel ------------------------------------------------------------

test_that("territory_travel keeps each incident's own station", {
  out <- territory_travel(make_matrix_df())

  expect_equal(out$dest_id, c("I1", "I2", "I3"))
  expect_equal(out$source_id, c(1L, 1L, 2L))
  expect_equal(out$duration_min, c(3, 6, 2))
})

test_that("territory_travel drops incidents with no territory station", {
  matrix_df <- make_matrix_df()
  matrix_df$station_id[matrix_df$dest_id == "I3"] <- NA

  expect_message(out <- territory_travel(matrix_df), "^1 incidents have no territory station")
  expect_equal(out$dest_id, c("I1", "I2"))
})

test_that("territory_travel uses the column it is given", {
  matrix_df <- make_matrix_df()
  names(matrix_df)[names(matrix_df) == "station_id"] <- "station_territory"

  out <- territory_travel(matrix_df, territory = "station_territory")

  expect_equal(out$duration_min, c(3, 6, 2))
})

# summarise_travel_stats ------------------------------------------------------

test_that("summarise_travel_stats by station and zone matches hand calculation", {
  out <- summarise_travel_stats(make_matrix_df())

  expect_equal(out$source_id, c(1L, 1L, 2L, 2L))
  expect_equal(out$fdz_id, c("1A", "2B", "1A", "2B"))
  expect_equal(out$n_incidents, c(2L, 1L, 2L, 1L))
  # station 1 to zone 1A: times 3 and 6
  expect_equal(out$mean_minutes[1], 4.5)
  expect_equal(out$median_minutes[1], 4.5)
  expect_equal(out$p90_minutes[1], 3 + 0.9 * (6 - 3)) # R's default quantile type
  expect_equal(out$mean_distance_m[1], 4500)
})

test_that("summarise_travel_stats reports the first and last alarm per group", {
  out <- summarise_travel_stats(make_matrix_df())
  row <- out[out$source_id == 1 & out$fdz_id == "1A", ]

  expect_equal(format(row$min_incident_alarm_dt, "%Y-%m-%d"), "2024-01-01")
  expect_equal(format(row$max_incident_alarm_dt, "%Y-%m-%d"), "2024-02-01")
})

test_that("summarise_travel_stats groups by the columns it is given", {
  out <- summarise_travel_stats(closest_station(make_matrix_df()), by = "fdz_id")

  expect_equal(out$fdz_id, c("1A", "2B"))
  # zone 1A, closest times: I1 3 (station 1), I2 4 (station 2)
  expect_equal(out$mean_minutes, c(3.5, 2))
})

# coverage_share --------------------------------------------------------------

test_that("coverage_share counts incidents within each threshold", {
  out <- coverage_share(territory_travel(make_matrix_df()), minutes = c(3, 4, 8))

  # territory times are 3, 6 and 2
  expect_equal(out$n_incidents, 3L)
  expect_equal(out$within_3_min, 2 / 3) # a time equal to the threshold counts
  expect_equal(out$within_4_min, 2 / 3)
  expect_equal(out$within_8_min, 1)
})

test_that("coverage_share is higher from the closest station than the territory one", {
  matrix_df <- make_matrix_df()

  closest <- coverage_share(closest_station(matrix_df), minutes = 4)
  territory <- coverage_share(territory_travel(matrix_df), minutes = 4)

  expect_equal(closest$within_4_min, 1)
  expect_equal(territory$within_4_min, 2 / 3)
})

test_that("coverage_share groups by the columns it is given", {
  out <- coverage_share(territory_travel(make_matrix_df()), minutes = 4, by = "fdz_id")

  expect_equal(out$fdz_id, c("1A", "2B"))
  expect_equal(out$within_4_min, c(1 / 2, 1))
})

test_that("coverage_share counts unroutable incidents as not covered", {
  df <- closest_station(make_matrix_df())
  df$duration_min[1] <- NA

  expect_equal(coverage_share(df, minutes = 4)$within_4_min, 2 / 3)
})

test_that("coverage_share refuses the full matrix", {
  expect_error(coverage_share(make_matrix_df(), minutes = 4), "one row per incident")
})
