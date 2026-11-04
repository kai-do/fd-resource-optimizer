# make_runcard_raw_df() is in helper-fixtures.R

# prep_runcard ----------------------------------------------------------------

test_that("prep_runcard keeps Cobb responders on beat cards, in card order", {
  out <- prep_runcard(make_runcard_raw_df())
  zone_1a <- out[out$fdz_id == "1A", ]

  # MF/51 dropped and the ranks renumbered; the Location card's CF/09 ignored
  expect_equal(zone_1a$source_id, c(1L, 7L, 22L))
  expect_equal(zone_1a$rank, 1:3)
})

test_that("prep_runcard records the beat's agency and other-agency-first cards", {
  out <- prep_runcard(make_runcard_raw_df())
  first_rows <- out[!duplicated(out$fdz_id), ]

  expect_equal(first_rows$fdz_id, c("1A", "2B", "3C"))
  expect_equal(first_rows$agency, c("CF", "MF", "CF"))
  expect_equal(first_rows$other_agency_first, c(FALSE, TRUE, FALSE))
})

test_that("prep_runcard interleaves a two-card beat by sequence", {
  out <- prep_runcard(make_runcard_raw_df())
  zone_3c <- out[out$fdz_id == "3C", ]

  # sequence 1 from both cards, then sequence 2 from both, ties in file order
  expect_equal(zone_3c$source_id, c(3L, 3L, 1L, 22L))
})

test_that("prep_runcard takes the station number from responding_value", {
  out <- prep_runcard(make_runcard_raw_df())

  expect_type(out$source_id, "integer")
  expect_named(out, c("fdz_id", "source_id", "rank", "agency", "other_agency_first"))
})

# runcard_responders ----------------------------------------------------------

test_that("runcard_responders numbers the open stations into due levels", {
  out <- runcard_responders(prep_runcard(make_runcard_raw_df()), stations = c(1, 22))

  expect_equal(out$fdz_id, c("1A", "1A", "2B", "3C", "3C"))
  expect_equal(out$source_id, c(1L, 22L, 1L, 1L, 22L))
  expect_equal(out$due, c(1L, 2L, 1L, 1L, 2L))
})

test_that("runcard_responders counts a station on both cards of a beat once", {
  out <- runcard_responders(prep_runcard(make_runcard_raw_df()), stations = c(1, 3, 22))
  zone_3c <- out[out$fdz_id == "3C", ]

  expect_equal(zone_3c$source_id, c(3L, 1L, 22L))
  expect_equal(zone_3c$due, 1:3)
})

test_that("runcard_responders keeps only the requested due levels", {
  runcard <- prep_runcard(make_runcard_raw_df())

  second <- runcard_responders(runcard, stations = c(1, 22), due = 2)
  expect_equal(second$fdz_id, c("1A", "3C")) # 2B has only one open station
  expect_equal(second$source_id, c(22L, 22L))

  first_two <- runcard_responders(runcard, stations = c(1, 22), due = 1:2)
  expect_equal(nrow(first_two), 5)
})

# filter_by_runcard -----------------------------------------------------------

# A mini travel stats table: every station x zone, mean_minutes = station + a
# zone offset, so each row is recognisable
make_travel_stats_df <- function() {
  df <- expand.grid(
    source_id = c(1L, 3L, 7L, 22L),
    fdz_id = c("1A", "2B", "3C"),
    stringsAsFactors = FALSE
  )
  df$mean_minutes <- df$source_id + c("1A" = 0.1, "2B" = 0.2, "3C" = 0.3)[df$fdz_id]
  df
}

test_that("filter_by_runcard keeps each zone's 1st due station", {
  out <- filter_by_runcard(make_travel_stats_df(), prep_runcard(make_runcard_raw_df()), stations = c(1, 3, 7, 22))
  out <- out[order(out$fdz_id), ]

  expect_equal(out$fdz_id, c("1A", "2B", "3C"))
  expect_equal(out$source_id, c(1L, 7L, 3L))
  expect_equal(unname(out$mean_minutes), c(1.1, 7.2, 3.3))
  expect_equal(out$due, c(1L, 1L, 1L))
})

test_that("filter_by_runcard moves to the next open station when one is closed", {
  # station 1 closed: 1A falls to 7, 2B (7 first anyway) stays, 3C stays on 3
  out <- filter_by_runcard(make_travel_stats_df(), prep_runcard(make_runcard_raw_df()), stations = c(3, 7, 22))
  out <- out[order(out$fdz_id), ]

  expect_equal(out$source_id, c(7L, 7L, 3L))
})

test_that("filter_by_runcard reports zones with no station at a due level", {
  runcard <- prep_runcard(make_runcard_raw_df())

  expect_message(
    out <- filter_by_runcard(make_travel_stats_df(), runcard, stations = c(1, 22), due = 2),
    "^1 zones have no due 2 station"
  )
  expect_equal(sort(out$fdz_id), c("1A", "3C"))
})

test_that("filter_by_runcard keeps several due levels", {
  out <- suppressMessages( # 2B has no due 2 station
    filter_by_runcard(make_travel_stats_df(), prep_runcard(make_runcard_raw_df()), stations = c(1, 22), due = 1:2)
  )

  expect_equal(nrow(out), 5)
  expect_setequal(out$due, 1:2)
})

test_that("filter_by_runcard works on the travel matrix, one row per incident", {
  matrix_df <- data.frame(
    source_id = rep(c(1L, 7L), times = 3),
    dest_id = rep(c("I1", "I2", "I3"), each = 2),
    fdz_id = rep(c("1A", "1A", "2B"), each = 2),
    duration_min = c(3, 5, 6, 4, 9, 2)
  )

  out <- filter_by_runcard(matrix_df, prep_runcard(make_runcard_raw_df()), stations = c(1, 7))

  # 1A's 1st due is station 1, 2B's is station 7
  expect_equal(out$dest_id, c("I1", "I2", "I3"))
  expect_equal(out$source_id, c(1L, 1L, 7L))
  expect_equal(out$duration_min, c(3, 6, 2))
})

test_that("station stats come from the responder's own incidents", {
  # Station 1 is 1st due in 1A and 3C. Its incidents there take 2, 3, 10
  # (1A) and 20 (3C) minutes, so its median over its own incidents is 6.5.
  # Summarising zones first gives medians 3 (1A) and 20 (3C), and no
  # combination of those recovers 6.5: the filter has to run per incident.
  matrix_df <- data.frame(
    source_id = 1L,
    dest_id = c("I1", "I2", "I3", "I4"),
    fdz_id = c("1A", "1A", "1A", "3C"),
    duration_min = c(2, 3, 10, 20),
    distance_m = c(2000, 3000, 10000, 20000),
    incident_alarm_dt = as.POSIXct("2024-01-01 10:00", tz = "America/New_York")
  )
  responders <- filter_by_runcard(matrix_df, prep_runcard(make_runcard_raw_df()), stations = c(1, 22))

  by_station <- summarise_travel_stats(responders, by = c("due", "source_id"))
  by_fdz <- summarise_travel_stats(responders, by = c("due", "fdz_id"))

  expect_equal(by_station$n_incidents, 4L)
  expect_equal(by_station$median_minutes, 6.5)
  expect_equal(by_fdz$median_minutes, c(3, 20))
})
