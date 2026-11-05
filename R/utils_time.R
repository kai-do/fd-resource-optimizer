#' Label CAD clock times with their time zone
#'
#' The CAD extract stores alarm times as local clock time, but `fread()` reads
#' the column as POSIXct and labels it UTC. This keeps the clock digits and
#' swaps the label for `tz`, so it is `force_tz()`, not `with_tz()`.
#'
#' Re-parsing with `ymd_hms()` is avoided on purpose: on a POSIXct it formats to
#' text first, and exact-midnight alarms ("2025-01-05") then fail to parse.
#'
#' Daylight saving: clock times in the spring-forward gap (2021-03-14 02:54)
#' do not exist in New York time. `"post"` shifts them forward by the size of
#' the gap, so 02:54 becomes 03:54 EDT (`"boundary"` would give 03:00 and
#' `"pre"` 01:54 EST). Every other time, including the repeated fall-back hour,
#' comes out the same as `ymd_hms(tz = )` gave.
#'
#' @param x POSIXct vector labeled UTC whose clock digits are local time.
#' @param tz Time zone the clock digits are really in.
#'
#' @return `x` as POSIXct in `tz`.
#' @noRd
fix_alarm_tz <- function(x, tz = "America/New_York") {
  lubridate::force_tz(x, tz, roll_dst = c("post", "post"))
}
