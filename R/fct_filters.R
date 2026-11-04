# Incident filter sets: a named, reusable set of incident filters (dates, time
# of day, incident codes) that can be applied to the travel matrix on its own
# with filter_incidents(), or passed to stats_by_station() / stats_by_fdz().
#
#   night_als <- incident_filter(
#     "Night ALS",
#     time_range = c("22:00", "06:00"),
#     ems_capability = "PARAMEDIC"
#   )
#   matrix_df |> filter_incidents(night_als)

# Code filter argument -> matrix column it applies to
filter_columns <- c(
  source_system = "rms_source_system",
  rms_category = "incident_category_code",
  rms_subcategory = "incident_subcategory_code",
  rms_type = "rms_incident_type_code",
  cad_type = "cad_incident_type_code",
  dispatch_code = "dispatch_code",
  dispatch_category = "dispatch_category_code",
  proqa_case_type = "dispatch_case_type_description",
  ems_capability = "ems_unit_capability"
)

#' Define a named set of incident filters
#'
#' Every filter left `NULL` is not applied, so `incident_filter("All calls")`
#' keeps every incident. Filters combine with AND.
#'
#' @param name Label for the filter set, used as `filter_name` in results.
#' @param date_range Two dates (Date or "YYYY-MM-DD"); incidents whose alarm
#'   falls on or between them, by New York date, are kept.
#' @param time_range Two clock times ("HH:MM"); incidents whose New York alarm
#'   time is at or after the first and before the second are kept. A start
#'   later than the end wraps past midnight ("22:00", "06:00"); equal times,
#'   or "00:00" to "24:00", keep the whole day.
#' @param source_system,rms_category,rms_subcategory,rms_type,cad_type,dispatch_code,dispatch_category,proqa_case_type,ems_capability
#'   Codes to keep, matched as text against the column each one names in
#'   `filter_columns` (e.g. `rms_type` matches `rms_incident_type_code`).
#'
#' @return An `incident_filter` object.
#' @noRd
incident_filter <- function(
  name,
  date_range = NULL,
  time_range = NULL,
  source_system = NULL,
  rms_category = NULL,
  rms_subcategory = NULL,
  rms_type = NULL,
  cad_type = NULL,
  dispatch_code = NULL,
  dispatch_category = NULL,
  proqa_case_type = NULL,
  ems_capability = NULL
) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
    stop("name must be a single, non-empty string", call. = FALSE)
  }

  if (!is.null(date_range)) {
    date_range <- as.Date(date_range)
    if (length(date_range) != 2 || anyNA(date_range) || date_range[1] > date_range[2]) {
      stop("date_range must be two dates, the first not after the second", call. = FALSE)
    }
  }

  if (!is.null(time_range)) {
    if (length(time_range) != 2) {
      stop("time_range must be two clock times, e.g. c(\"22:00\", \"06:00\")", call. = FALSE)
    }
    clock_minutes(time_range) # validates the format
  }

  codes <- list(
    source_system = source_system,
    rms_category = rms_category,
    rms_subcategory = rms_subcategory,
    rms_type = rms_type,
    cad_type = cad_type,
    dispatch_code = dispatch_code,
    dispatch_category = dispatch_category,
    proqa_case_type = proqa_case_type,
    ems_capability = ems_capability
  )
  # drop unused filters; compare codes as text so 09 and "09" mean the same
  codes <- lapply(codes[!vapply(codes, is.null, logical(1))], as.character)

  structure(
    list(name = name, date_range = date_range, time_range = time_range, codes = codes),
    class = "incident_filter"
  )
}

#' @exportS3Method base::print
print.incident_filter <- function(x, ...) {
  cat("<incident_filter> ", x$name, "\n", sep = "")
  lines <- c(
    if (!is.null(x$date_range)) paste0("  date_range: ", paste(x$date_range, collapse = " to ")),
    if (!is.null(x$time_range)) paste0("  time_range: ", paste(x$time_range, collapse = " to ")),
    vapply(
      names(x$codes),
      function(key) paste0("  ", key, ": ", paste(x$codes[[key]], collapse = ", ")),
      character(1)
    )
  )
  if (length(lines) == 0) lines <- "  (no filters: all incidents)"
  cat(lines, sep = "\n")
  invisible(x)
}

#' Keep the incidents that pass a filter set
#'
#' @param df The travel matrix (one row per station and incident) or other data
#'   with a `dest_id` column and the columns the filters use
#'   (`incident_alarm_dt` for dates and times).
#' @param filter An [incident_filter()].
#'
#' @return The rows of `df` whose incident passes every filter; for the
#'   matrix, all of a kept incident's station rows.
#' @noRd
filter_incidents <- function(df, filter) {
  if (!inherits(filter, "incident_filter")) {
    stop("filter must be made with incident_filter()", call. = FALSE)
  }

  needed <- c(
    filter_columns[names(filter$codes)],
    if (!is.null(filter$date_range) || !is.null(filter$time_range)) "incident_alarm_dt"
  )
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop(
      "Filter set '", filter$name, "' needs column(s) missing from the data: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Nothing to filter on: keep everything
  if (length(needed) == 0) {
    return(df)
  }

  # Every incident column is the same on all of an incident's station rows, so
  # filter one row per incident
  incidents <- df[!duplicated(df$dest_id), c("dest_id", unname(needed))]
  keep <- rep(TRUE, nrow(incidents))

  for (key in names(filter$codes)) {
    values <- as.character(incidents[[filter_columns[[key]]]])
    keep <- keep & values %in% filter$codes[[key]]
  }

  if (!is.null(filter$date_range) || !is.null(filter$time_range)) {
    local_dt <- lubridate::with_tz(incidents$incident_alarm_dt, "America/New_York")
  }

  if (!is.null(filter$date_range)) {
    local_date <- lubridate::as_date(local_dt)
    keep <- keep &
      local_date >= filter$date_range[1] &
      local_date <= filter$date_range[2]
  }

  if (!is.null(filter$time_range)) {
    window <- clock_minutes(filter$time_range)
    whole_day <- window[1] == window[2] || all(c(0, 1440) %in% window)
    if (!whole_day) {
      minute_of_day <- lubridate::hour(local_dt) * 60 + lubridate::minute(local_dt)
      in_window <- if (window[1] < window[2]) {
        minute_of_day >= window[1] & minute_of_day < window[2]
      } else {
        minute_of_day >= window[1] | minute_of_day < window[2]
      }
      keep <- keep & in_window
    }
  }

  df[df$dest_id %in% incidents$dest_id[which(keep)], ]
}

# "HH:MM" clock times to minutes since midnight (0 to 1440)
clock_minutes <- function(x) {
  bad <- function() {
    stop("clock times must be \"HH:MM\" between 00:00 and 24:00", call. = FALSE)
  }
  if (!is.character(x) || !all(grepl("^([01]?[0-9]|2[0-4]):[0-5][0-9]$", x))) {
    bad()
  }
  minutes <- as.integer(sub(":.*", "", x)) * 60 + as.integer(sub(".*:", "", x))
  if (any(minutes > 1440)) {
    bad()
  }
  minutes
}
