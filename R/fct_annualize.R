# Call counts as yearly averages.
#
# A count over a filter set's date window divided by the window's length in
# years: 3,600 calls over 2023-10-01 to 2026-09-30 (3 years) is 1,200 a year.
# The window is the filter's date_range, taken as the time calls could have
# happened in, whether or not any fell on its first or last day. A filter set
# without a date_range covers the whole data: the first to the last alarm date
# in the travel matrix.
#
#   years <- filter_years(filters, travel_matrix)
#   comparison$Overall |> annualize_counts(years) |> overall_table()

#' Years covered by each filter set
#'
#' @param filters One [incident_filter()] or a list of them.
#' @param matrix_df Travel matrix (or any data) with `incident_alarm_dt`. Gives
#'   the window of filter sets without a `date_range`, and a message when a
#'   `date_range` runs past the data. Only needed if a filter set has no
#'   `date_range`.
#'
#' @return One row per filter set: `filter_name`, `start`, `end` (New York
#'   dates, both included) and `years` (days / 365.25).
#' @noRd
filter_years <- function(filters, matrix_df = NULL) {
  if (inherits(filters, "incident_filter")) {
    filters <- list(filters)
  }

  data_range <- NULL
  if (!is.null(matrix_df) && "incident_alarm_dt" %in% names(matrix_df)) {
    local_dt <- lubridate::with_tz(range(matrix_df$incident_alarm_dt, na.rm = TRUE), "America/New_York")
    data_range <- lubridate::as_date(local_dt)
  }

  rows <- lapply(filters, function(f) {
    window <- f$date_range
    if (is.null(window)) {
      if (is.null(data_range)) {
        stop(
          "Filter set '", f$name, "' has no date_range; pass matrix_df ",
          "(with incident_alarm_dt) to use the data's date range",
          call. = FALSE
        )
      }
      window <- data_range
    } else if (!is.null(data_range) && (window[1] < data_range[1] || window[2] > data_range[2])) {
      message(
        "Filter set '", f$name, "' runs ", window[1], " to ", window[2],
        ", past the data (", data_range[1], " to ", data_range[2],
        "); its yearly averages still divide by the whole window"
      )
    }
    data.frame(filter_name = f$name, start = window[1], end = window[2])
  })

  out <- do.call(rbind, rows)
  out$years <- (as.numeric(out$end - out$start) + 1) / 365.25
  out
}

#' Convert call counts to yearly averages
#'
#' Divides the call counts (calls, incidents, unserved calls, calls within or
#' slower than a threshold, total minutes) by the years each row's filter set
#' covers. Times, shares and zone counts (FDZs, unserved zones, FDZs over a
#' threshold) are not counts of calls and are left as they are; so is
#' `pct_diff`, which a common divisor does not change.
#'
#' @param df A compare_*() result (long, with a `metric` column) or a
#'   [stats_by_station()] / [stats_by_fdz()] result (one column per metric),
#'   with `filter_name`.
#' @param years Output of [filter_years()], or one number of years for every
#'   row.
#'
#' @return `df` with the call counts divided and a `years` column added, which
#'   the table functions use to label the counts "per Year".
#' @noRd
annualize_counts <- function(df, years) {
  if ("years" %in% names(df)) {
    stop("df already has a years column: its counts are already yearly", call. = FALSE)
  }

  if (is.numeric(years)) {
    if (length(years) != 1 || is.na(years) || years <= 0) {
      stop("years must be one positive number or the output of filter_years()", call. = FALSE)
    }
    divisor <- rep(years, nrow(df))
  } else {
    idx <- match(df$filter_name, years$filter_name)
    if (anyNA(idx)) {
      stop(
        "No years for filter set(s): ",
        paste(unique(df$filter_name[is.na(idx)]), collapse = ", "),
        call. = FALSE
      )
    }
    divisor <- years$years[idx]
  }

  if ("metric" %in% names(df)) {
    rows <- is_call_count(df$metric)
    value_cols <- intersect(c("baseline", "scenario", "diff", "ci_lower", "ci_upper", "value"), names(df))
    for (col in value_cols) {
      df[[col]] <- as.numeric(df[[col]])
      df[[col]][rows] <- df[[col]][rows] / divisor[rows]
    }
  } else {
    for (col in names(df)[is_call_count(names(df))]) {
      df[[col]] <- df[[col]] / divisor
    }
  }

  df$years <- divisor
  df
}

# Metrics that count calls (or sum their minutes), so scale with time covered
is_call_count <- function(m) {
  m %in% c("n_calls", "n_incidents", "unserved_calls", "total_minutes") |
    grepl("^(calls_within_|calls_slower_by_)", m)
}
