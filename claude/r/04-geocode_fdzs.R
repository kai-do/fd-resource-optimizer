library(tidyverse)
library(sf)
library(data.table)
library(arrow)

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

fdz_sf <- fdz_df |>
  st_as_sf(wkt = "shape_wkt", crs = 2240) # stay in 2240

# Same incident set the matrix was built from (see 03-generate_travel_matrix.R)
incident_address_df <- fread(
  'datasets/inputs/2026-10-13_incident_address_data.csv',
  na.strings = 'NULL',
  data.table = FALSE
)

bad_coords <- with(
  incident_address_df,
  is.na(latitude) | is.na(longitude) | latitude == 0 | longitude == 0
)

incident_fdz_df <- incident_address_df[!bad_coords, ] |>
  distinct(incident_number, .keep_all = TRUE) |>
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE) |>
  st_transform(2240) |>
  st_join(fdz_sf[c('fdz','station_id')], join = st_within, left = TRUE) |>
  st_drop_geometry() |>
  transmute(
    incident_number,
    incident_alarm_dt = ymd_hms(incident_alarm_dt, tz = 'America/New_York'),
    fdz = fdz.y,
    station_territory = station_id,
    cad_incident_type_code,
    dispatch_code,
    rms_incident_type_code = incident_type_code,
    rms_source_system = source_system
  )

# EMS unit capability needed by each dispatch code. Codes are read as text so leading zeros (01C05) are kept.
# Coded incidents the capability file does not list are 'OTHER'; incidents with no dispatch code stay NA.
ems_capabilities_df <- read_csv(
  'datasets/inputs/2026-10-14_ems_unit_capabilities.csv',
  col_types = cols(.default = col_character())
) |>
  distinct(dispatch_code, .keep_all = TRUE)

incident_fdz_df <- incident_fdz_df |>
  left_join(ems_capabilities_df, by = 'dispatch_code') |>
  mutate(
    ems_unit_capability = if_else(
      is.na(ems_unit_capability) & !is.na(dispatch_code), 'OTHER', ems_unit_capability
    )
  )

# checks
stopifnot(!anyDuplicated(incident_fdz_df$incident_number)) # no duplicated rows from overlapping FDZs
sum(is.na(incident_fdz_df$fdz)) # points that fell in no FDZ

# Attach to the existing matrix (dest_id is incident_number)
incident_travel_matrix_df <- read_parquet('datasets/outputs/incident_travel_matrix.parquet') |>
  left_join(incident_fdz_df, by = c('dest_id' = 'incident_number'))

stopifnot(!anyNA(incident_travel_matrix_df$dest_id))

write_parquet(
  incident_travel_matrix_df,
  'datasets/outputs/incident_travel_matrix_fdz.parquet',
  compression = 'zstd'
)
