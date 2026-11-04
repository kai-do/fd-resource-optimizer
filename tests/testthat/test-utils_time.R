# fread() reads CAD alarm times as POSIXct labelled UTC, but the clock digits
# are New York local time. These build values in that same shape.
as_cad_time <- function(x) as.POSIXct(x, tz = "UTC")

test_that("fix_alarm_tz keeps the clock digits and changes the zone label", {
  out <- fix_alarm_tz(as_cad_time("2024-07-04 14:30:00"))

  expect_equal(attr(out, "tzone"), "America/New_York")
  expect_equal(format(out, "%Y-%m-%d %H:%M:%S"), "2024-07-04 14:30:00")
  # 14:30 EDT is 18:30 UTC: the instant moved, the clock reading did not
  expect_equal(format(out, "%H:%M", tz = "UTC"), "18:30")
})

test_that("fix_alarm_tz keeps exact-midnight alarms", {
  # The reason ymd_hms() is not used: it formats midnight as a bare date and
  # then fails to parse it.
  out <- fix_alarm_tz(as_cad_time("2025-01-05 00:00:00"))

  expect_false(is.na(out))
  expect_equal(format(out, "%Y-%m-%d %H:%M:%S"), "2025-01-05 00:00:00")
})

test_that("fix_alarm_tz shifts spring-forward gap times forward one hour", {
  # 02:54 on 2021-03-14 does not exist in New York; the real extract has two
  # alarms like this. roll_dst = "post" moves it by the size of the gap.
  out <- fix_alarm_tz(as_cad_time("2021-03-14 02:54:00"))

  expect_false(is.na(out))
  expect_equal(format(out, "%Y-%m-%d %H:%M:%S %Z"), "2021-03-14 03:54:00 EDT")
})

test_that("fix_alarm_tz matches ymd_hms(tz = ) in the repeated fall-back hour", {
  # 01:30 on 2024-11-03 happens twice in New York. The old code parsed with
  # ymd_hms(tz = ); the new code must pick the same one of the two instants.
  x <- "2024-11-03 01:30:00"

  expect_equal(
    fix_alarm_tz(as_cad_time(x)),
    lubridate::ymd_hms(x, tz = "America/New_York")
  )
})

test_that("fix_alarm_tz uses the zone it is given", {
  out <- fix_alarm_tz(as_cad_time("2021-03-14 02:54:00"), tz = "EST")

  # EST has no daylight saving, so there is no gap to roll over
  expect_equal(format(out, "%H:%M"), "02:54")
})
