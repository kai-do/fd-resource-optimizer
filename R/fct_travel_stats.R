# Travel-time statistics from the station x incident travel matrix.
#
# The matrix has one row per station per incident. closest_station() and
# territory_travel() reduce it to one row per incident; summarise_travel_stats()
# and coverage_share() then answer questions on either form. For example:
#
#   matrix_df |> closest_station() |> summarise_travel_stats(by = "fdz_id")
#   matrix_df |> territory_travel() |> coverage_share(minutes = c(4, 8))

#' The fastest station for each incident
#'
#' @param matrix_df Travel matrix with `source_id`, `dest_id` and
#'   `duration_min` (see [build_travel_matrix()]); any incident columns are
#'   kept.
#'
#' @return One row per incident (`dest_id`): its matrix row for the station
#'   with the shortest `duration_min`. Ties go to the lowest `source_id`.
#'   Unroutable pairs (`NA` duration) are used only if an incident has nothing
#'   else.
#' @noRd
closest_station <- function(matrix_df) {
  ord <- order(
    matrix_df$dest_id,
    matrix_df$duration_min,
    matrix_df$source_id,
    na.last = TRUE,
    method = "radix"
  )
  matrix_df <- matrix_df[ord, ]
  matrix_df[!duplicated(matrix_df$dest_id), ]
}

#' Each incident's travel from its own territory station
#'
#' @param matrix_df Travel matrix with `source_id`, `dest_id`, `duration_min`
#'   and a territory station column (see the step 03 and 04 outputs).
#' @param territory Name (string) of the column holding each incident's
#'   territory station id.
#'
#' @return One row per incident: its matrix row where `source_id` is its
#'   territory station. Incidents with no territory station (outside every
#'   FDZ, or a zone with no station), or whose territory station is not in the
#'   matrix, are dropped, with a message saying how many.
#' @noRd
territory_travel <- function(matrix_df, territory = "station_id") {
  is_own <- !is.na(matrix_df[[territory]]) &
    matrix_df$source_id == matrix_df[[territory]]
  out <- matrix_df[is_own, ]

  n_dropped <- length(unique(matrix_df$dest_id)) - nrow(out)
  if (n_dropped > 0) {
    message(n_dropped, " incidents have no territory station in the matrix")
  }
  out
}

#' Summarise travel times by group
#'
#' @param df Travel matrix, or a one-row-per-incident table from
#'   [closest_station()] or [territory_travel()]. Needs `dest_id`,
#'   `duration_min`, `distance_m`, `incident_alarm_dt` and the `by` columns.
#' @param by Names (strings) of the grouping columns. The default, station by
#'   zone on the full matrix, is the legacy travel_stats table.
#'
#' @return One row per group: `n_incidents`, the mean, median and 90th
#'   percentile of `duration_min`, the mean `distance_m`, and the first and
#'   last alarm time.
#' @noRd
summarise_travel_stats <- function(df, by = c("source_id", "fdz_id")) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      n_incidents = dplyr::n_distinct(dest_id),
      mean_minutes = mean(duration_min, na.rm = TRUE),
      median_minutes = stats::median(duration_min, na.rm = TRUE),
      p90_minutes = stats::quantile(duration_min, 0.9, na.rm = TRUE, names = FALSE),
      mean_distance_m = mean(distance_m, na.rm = TRUE),
      min_incident_alarm_dt = min(incident_alarm_dt, na.rm = TRUE),
      max_incident_alarm_dt = max(incident_alarm_dt, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Share of incidents reached within time thresholds
#'
#' @param df One row per incident, from [closest_station()] or
#'   [territory_travel()], with `dest_id` and `duration_min`.
#' @param minutes Thresholds in minutes; a travel time equal to the threshold
#'   counts as covered.
#' @param by Names (strings) of grouping columns, or `NULL` for one overall
#'   row.
#'
#' @return One row per group: `n_incidents` and a `within_<m>_min` column per
#'   threshold, the share (0 to 1) of incidents reached within `m` minutes.
#'   Unroutable incidents (`NA` duration) count as not covered.
#' @noRd
coverage_share <- function(df, minutes, by = NULL) {
  if (anyDuplicated(df$dest_id)) {
    stop(
      "coverage_share() needs one row per incident: ",
      "use closest_station() or territory_travel() first",
      call. = FALSE
    )
  }

  share_cols <- paste0("within_", minutes, "_min")
  for (i in seq_along(minutes)) {
    df[[share_cols[i]]] <- !is.na(df$duration_min) & df$duration_min <= minutes[i]
  }

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      n_incidents = dplyr::n(),
      dplyr::across(dplyr::all_of(share_cols), mean),
      .groups = "drop"
    )
}
