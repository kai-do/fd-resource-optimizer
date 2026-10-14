library(tidyverse)
library(arrow)

incident_travel_matrix_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet'
)

# Travel-time summaries per FDZ. If `filter_by` or `codes` is NA, no filter is applied and all incidents are used.
# Otherwise, restricted to incidents matching `codes` in the column chosen by `filter_by`.
get_travel_stats <- function(
  travel_matrix_df,
  filter_by = NA,
  codes = NA
) {
  if (!anyNA(filter_by) && !anyNA(codes)) {
    filter_by <- match.arg(
      filter_by,
      c('rms_type', 'cad_type', 'dispatch_code')
    )
    filter_col <- c(
      rms_type = 'rms_incident_type_code',
      cad_type = 'cad_incident_type_code',
      dispatch_code = 'dispatch_code'
    )[[filter_by]]

    travel_matrix_df <- travel_matrix_df |>
      filter(.data[[filter_col]] %in% codes)
  }

  travel_matrix_df |>
    group_by(source_id, fdz) |>
    summarise(
      n_incidents = n_distinct(dest_id),
      mean_minutes = mean(duration_min, na.rm = TRUE),
      median_minutes = median(duration_min, na.rm = TRUE),
      p90_minutes = quantile(duration_min, 0.9, na.rm = TRUE),
      mean_distance_m = mean(distance_m, na.rm = TRUE),
      .groups = 'drop'
    )
}

travel_stats_df <- get_travel_stats(incident_travel_matrix_df)

write_parquet(travel_stats_df, 'datasets/outputs/travel_stats.parquet')

