library(tidyverse)
library(sf)
library(data.table)
library(arrow)

source('r/helpers_optimize.R') # get_travel_stats()

incident_travel_matrix_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet'
)

fdz_df <- fread(
  'datasets/inputs/2026-10-14_fdzs.csv',
  na.strings = 'NULL',
  data.table = FALSE
) |>
  rename(station_id = station) |>
  mutate(
    fdz = paste0(station_id, fire_demand_zone),
    fdz = case_when(fdz == 'NAA' ~ 'LA', TRUE ~ fdz)
  )

compare_travel_stats <- function(
  incident_travel_matrix_df,
  filter_station,
  filter_by = NA,
  codes = NA,
  filters = NULL
) {
  df <- get_travel_stats(
    incident_travel_matrix_df,
    filter_by = filter_by,
    codes = codes,
    filters = filters
  ) |>
    left_join(distinct(fdz_df[c('station_id', 'fdz')]), by = c('fdz')) |>
    rename(station_territory = station_id) |>
    mutate(
      home_station_flag = case_when(
        source_id == station_territory ~ TRUE,
        TRUE ~ FALSE
      )
    )

  df |>
    filter(home_station_flag == TRUE) |>
    inner_join(
      (df |>
        filter(source_id == filter_station)),
      by = ('fdz')
    ) |>
    transmute(
      responding_station = source_id.y,
      fdz,
      n_incidents = n_incidents.y,
      min_incident_alarm_dt = min_incident_alarm_dt.y,
      max_incident_alarm_dt = max_incident_alarm_dt.y,
      rs_mean_travel_time_min = mean_minutes.y,
      rs_median_travel_time_min = median_minutes.y,
      rs_p90_travel_time_min = p90_minutes.y,
      rs_mean_distance_m = mean_distance_m.y,

      station_territory = source_id.x,
      st_mean_travel_time_min = mean_minutes.x,
      st_median_travel_time_min = median_minutes.x,
      st_p90_travel_time_min = p90_minutes.x,
      st_mean_distance_m = mean_distance_m.x,

      diff_mean_travel_time_min = rs_mean_travel_time_min -
        st_mean_travel_time_min,
      diff_median_travel_time_min = rs_median_travel_time_min -
        st_median_travel_time_min,
      diff_p90_travel_time_min = rs_p90_travel_time_min -
        st_p90_travel_time_min,
      diff_mean_distance_m = rs_mean_distance_m - st_mean_distance_m,
    )
}

station_8_fdz_travel_stats <- compare_travel_stats(incident_travel_matrix_df, 8, 'ems_capability', c('AEMT','EMT-I / BLS'))

write_csv(
  station_8_fdz_travel_stats,
  'datasets/outputs/station_8_fdz_travel_stats.csv'
)
