library(tidyverse)
library(sf)
library(data.table)

fdz_df <- fread(
  'datasets/inputs/2026-10-14_fdzs.csv',
  na.strings = 'NULL',
  data.table = FALSE
) %>%
  rename(station_id = station) %>%
  mutate(
    fdz = paste0(station_id, fire_demand_zone),
    fdz = case_when(fdz == 'NAA' ~ 'LA', TRUE ~ fdz)
  )

fdz_sf <- fdz_df %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) # stay in 2240

# Same incident set the matrix was built from (see 02-generate_travel_matrix.R)
incident_address_df <- fread(
  'datasets/inputs/2026-10-13_incident_address_data.csv',
  na.strings = 'NULL',
  data.table = FALSE
)

bad_coords <- with(
  incident_address_df,
  is.na(latitude) | is.na(longitude) | latitude == 0 | longitude == 0
)

incident_fdz_df <- incident_address_df[!bad_coords, ] %>%
  distinct(incident_number, .keep_all = TRUE) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE) %>%
  st_transform(2240) %>%
  st_join(fdz_sf['fdz'], join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  transmute(
    incident_number,
    incident_alarm_dt = ymd_hms(incident_alarm_dt, tz = 'America/New_York'),
    fdz = fdz.y,
    cad_incident_type_code,
    dispatch_code,
    rms_incident_type_code = incident_type_code,
    rms_source_system = source_system
  )

# checks
stopifnot(!anyDuplicated(incident_fdz_df$incident_number)) # no duplicated rows from overlapping FDZs
sum(is.na(incident_fdz_df$fdz)) # points that fell in no FDZ

# Attach to the existing matrix (dest_id is incident_number)
incident_travel_matrix_df <- fread(
  'datasets/outputs/incident_travel_matrix.csv',
  data.table = FALSE
) %>%
  left_join(incident_fdz_df, by = c('dest_id' = 'incident_number'))

stopifnot(!anyNA(travel_matrix$dest_id))
