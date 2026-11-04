# Four synthetic incidents. Alarm times are set in New York time and stored in
# UTC, the way they could arrive from another source, so the tests also check
# that filters use the New York clock and date.
#
#   I1  2024-03-01 23:30  PARAMEDIC  rms 321  dispatch category 09
#   I2  2024-03-02 03:00  AEMT       rms 111  dispatch category 26
#   I3  2024-03-02 12:00  PARAMEDIC  rms 321  dispatch category 09
#   I4  2023-12-31 22:00  (none)     rms 700  dispatch category 09
make_incidents_df <- function() {
  ny <- as.POSIXct(
    c("2024-03-01 23:30", "2024-03-02 03:00", "2024-03-02 12:00", "2023-12-31 22:00"),
    tz = "America/New_York"
  )
  data.frame(
    dest_id = c("I1", "I2", "I3", "I4"),
    incident_alarm_dt = lubridate::with_tz(ny, "UTC"),
    ems_unit_capability = c("PARAMEDIC", "AEMT", "PARAMEDIC", NA),
    rms_incident_type_code = c("321", "111", "321", "700"),
    dispatch_category_code = c("09", "26", "09", "09")
  )
}

kept <- function(filter, df = make_incidents_df()) {
  filter_incidents(df, filter)$dest_id
}

# incident_filter -------------------------------------------------------------

test_that("an empty filter set keeps everything", {
  df <- make_incidents_df()

  expect_identical(filter_incidents(df, incident_filter("All calls")), df)
})

test_that("incident_filter checks its inputs", {
  expect_error(incident_filter(""), "name")
  expect_error(incident_filter(c("a", "b")), "name")
  expect_error(incident_filter("x", date_range = c("2024-02-01", "2024-01-01")), "date_range")
  expect_error(incident_filter("x", date_range = "2024-01-01"), "date_range")
  expect_error(incident_filter("x", time_range = c("25:00", "06:00")), "HH:MM")
  expect_error(incident_filter("x", time_range = c("7pm", "06:00")), "HH:MM")
  expect_error(incident_filter("x", time_range = "22:00"), "two clock times")
})

test_that("incident_filter prints its active filters", {
  f <- incident_filter("Night ALS", time_range = c("22:00", "06:00"), ems_capability = "PARAMEDIC")

  expect_output(print(f), "Night ALS")
  expect_output(print(f), "time_range: 22:00 to 06:00")
  expect_output(print(f), "ems_capability: PARAMEDIC")
  expect_output(print(incident_filter("All calls")), "no filters")
})

# Code filters ----------------------------------------------------------------

test_that("code filters keep matching incidents", {
  expect_equal(kept(incident_filter("x", ems_capability = "PARAMEDIC")), c("I1", "I3"))
  expect_equal(kept(incident_filter("x", rms_type = c("111", "700"))), c("I2", "I4"))
  expect_equal(kept(incident_filter("x", dispatch_category = "09")), c("I1", "I3", "I4"))
})

test_that("code filters can keep incidents with no code", {
  expect_equal(kept(incident_filter("x", ems_capability = NA)), "I4")
})

test_that("a filter on a missing column is an error naming the column", {
  expect_error(
    filter_incidents(make_incidents_df(), incident_filter("x", cad_type = "ALS")),
    "cad_incident_type_code"
  )
})

# Dates and times -------------------------------------------------------------

test_that("date_range keeps both ends, by New York date", {
  # I1 is 23:30 on 2024-03-01 in New York but already 2024-03-02 in UTC
  f <- incident_filter("x", date_range = c("2024-01-01", "2024-03-01"))

  expect_equal(kept(f), "I1")
})

test_that("time_range wraps past midnight and keeps the start", {
  # 22:00 to 06:00: I4 at exactly 22:00 is kept
  expect_equal(kept(incident_filter("x", time_range = c("22:00", "06:00"))), c("I1", "I2", "I4"))
})

test_that("time_range drops the end time", {
  # 12:00 to 22:00: I3 at 12:00 is kept, I4 at 22:00 is not
  expect_equal(kept(incident_filter("x", time_range = c("12:00", "22:00"))), "I3")
})

test_that("equal times or 00:00 to 24:00 keep the whole day", {
  expect_equal(kept(incident_filter("x", time_range = c("08:00", "08:00"))), c("I1", "I2", "I3", "I4"))
  expect_equal(kept(incident_filter("x", time_range = c("00:00", "24:00"))), c("I1", "I2", "I3", "I4"))
})

# Combining and matrix rows ---------------------------------------------------

test_that("filters in a set combine with AND", {
  f <- incident_filter("x", time_range = c("22:00", "06:00"), ems_capability = "PARAMEDIC")

  expect_equal(kept(f), "I1")
})

test_that("on the matrix, every station row of a kept incident is kept", {
  incidents <- make_incidents_df()
  matrix_df <- merge(data.frame(source_id = c(1L, 2L)), incidents)

  out <- filter_incidents(matrix_df, incident_filter("x", ems_capability = "PARAMEDIC"))

  expect_equal(nrow(out), 4)
  expect_setequal(out$dest_id, c("I1", "I3"))
  expect_setequal(out$source_id, c(1L, 2L))
})
