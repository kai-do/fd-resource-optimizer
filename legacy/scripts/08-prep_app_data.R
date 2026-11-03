# LEGACY — quarantined. Not owner-vetted as a whole. Do not build new code on this; copy vetted pieces into the new structure.
library(tidyverse)
library(sf)
library(data.table)
library(arrow)

# Small assets for the Shiny app so it does not depend on the raw input CSVs.
dir.create('legacy/app/data', showWarnings = FALSE, recursive = TRUE)

# FDZ polygons, same fdz key as 04-geocode_fdzs.R, one (multi)polygon per fdz, in WGS84 for leaflet
fdz_polygons_sf <- fread(
  'datasets/inputs/2026-10-14_fdzs.csv',
  na.strings = 'NULL',
  data.table = FALSE
) |>
  rename(station_id = station) |>
  mutate(
    fdz = paste0(station_id, fire_demand_zone),
    fdz = case_when(fdz == 'NAA' ~ 'LA', TRUE ~ fdz)
  ) |>
  st_as_sf(wkt = 'shape_wkt', crs = 2240) |>
  group_by(fdz) |>
  summarise(.groups = 'drop') |> # sf summarise unions the geometries
  st_transform(4326)

saveRDS(fdz_polygons_sf, 'legacy/app/data/fdz_polygons.rds')

# Station coordinates, taken from the matrix sources
stations_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet',
  col_select = c('source_id', 'source_lat', 'source_lon')
) |>
  distinct(source_id, .keep_all = TRUE) |>
  rename(lat = source_lat, lon = source_lon) |>
  arrange(source_id)

write_parquet(stations_df, 'legacy/app/data/stations.parquet')

# Incident type code descriptions for the filter dropdowns, one row per filter type and code.
# A few codes carry more than one description over the years, so keep the most common one.
code_cols <- tribble(
  ~filter_by, ~code_col, ~desc_col,
  'rms_type', 'incident_type_code', 'incident_type_description',
  'cad_type', 'cad_incident_type_code', 'cad_incident_type_description',
  'dispatch_code', 'dispatch_code', 'dispatch_code_description',
  'dispatch_category', 'dispatch_category_code', 'dispatch_category_description',
  'proqa_case_type', 'dispatch_case_type_description', 'dispatch_case_type_description', # the value is its own label
  'rms_category', 'incident_category_code', 'incident_category_description',
  'rms_subcategory', 'incident_subcategory_code', 'incident_subcategory_description'
)

incident_codes_df <- fread(
  'datasets/inputs/2026-10-13_incident_address_data.csv',
  na.strings = 'NULL',
  data.table = FALSE,
  select = unique(c(
    code_cols$code_col,
    code_cols$desc_col,
    'incident_number',
    'latitude',
    'longitude'
  ))
)

# Category and subcategory per incident, joined onto the travel matrix by the app (dest_id is incident_number).
# Same incident set as 04-geocode_fdzs.R: drop bad coordinates, then keep one row per incident_number.
incident_category_df <- incident_codes_df |>
  filter(!(is.na(latitude) | is.na(longitude) | latitude == 0 | longitude == 0)) |>
  distinct(incident_number, .keep_all = TRUE) |>
  transmute(
    incident_number,
    incident_category_code,
    incident_subcategory_code,
    dispatch_category_code = as.character(dispatch_category_code), # integer in the CSV, character like the other codes
    dispatch_case_type_description
  )

write_parquet(incident_category_df, 'legacy/app/data/incident_categories.parquet')

code_lookup_df <- pmap(code_cols, \(filter_by, code_col, desc_col) {
  incident_codes_df |>
    transmute(filter_by = filter_by, code = as.character(.data[[code_col]]), description = .data[[desc_col]]) |>
    filter(!is.na(code)) |>
    count(filter_by, code, description) |>
    group_by(filter_by, code) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(-n)
}) |>
  list_rbind()

write_parquet(code_lookup_df, 'legacy/app/data/code_lookup.parquet')

# Run card order per beat (beats are fdzs), Cobb responders only. Marietta and Austell stations are not candidates and have
# no travel times, so they are dropped from the order. Rank is the order on the card, ties broken by file order.
# agency is the beat's own agency (CF, MF or AF), used to filter zones in the app.
# other_agency_first flags beats where the first responder on the card is not a Cobb station.
runcard_beats_df <- read_csv(
  'datasets/inputs/2026-10-13_cad_run_card.csv',
  na = 'NULL',
  col_types = cols(.default = col_character(), response_sequence = col_integer())
) |>
  filter(location_type == 'Beat') |>
  group_by(location) |>
  arrange(response_sequence, .by_group = TRUE) |>
  mutate(
    beat_agency = first(agency),
    other_agency_first = !str_starts(first(responding_value), 'CF/'),
    is_cobb = str_starts(responding_value, 'CF/')
  ) |>
  filter(is_cobb) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  transmute(
    fdz = location,
    station = as.integer(str_extract(responding_value, '[0-9]+')),
    rank,
    agency = beat_agency,
    other_agency_first
  )

write_parquet(runcard_beats_df, 'legacy/app/data/runcard_beats.parquet')

# EMS unit capability needed by each dispatch code, for the app's capability filter.
# Codes are read as text so leading zeros (01C05) are kept.
ems_capabilities_df <- read_csv(
  'datasets/inputs/2026-10-14_ems_unit_capabilities.csv',
  col_types = cols(.default = col_character())
) |>
  distinct(dispatch_code, .keep_all = TRUE)

write_parquet(ems_capabilities_df, 'legacy/app/data/ems_capabilities.parquet')

# Travel matrix for the app with every filter column already attached, so the app only has to read it.
# Text columns are stored as factors (dictionary encoded) to keep it small and quick to load.
# Coded incidents the capability file does not list are 'OTHER'; incidents with no dispatch code stay NA.
app_matrix_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet',
  col_select = c(
    'source_id', 'dest_id', 'fdz', 'duration_min', 'distance_m', 'rms_source_system',
    'rms_incident_type_code', 'cad_incident_type_code', 'dispatch_code', 'incident_alarm_dt'
  )
) |>
  left_join(incident_category_df, by = c('dest_id' = 'incident_number')) |>
  left_join(ems_capabilities_df, by = 'dispatch_code') |>
  mutate(
    ems_unit_capability = if_else(
      is.na(ems_unit_capability) & !is.na(dispatch_code), 'OTHER', ems_unit_capability
    ),
    across(c(fdz, rms_source_system, ends_with('_code'), dispatch_case_type_description, ems_unit_capability), as.factor)
  )

write_parquet(app_matrix_df, 'legacy/app/data/travel_matrix_app.parquet', compression = 'zstd')

# Small list of the values each filter can take, so the app can build its dropdowns without loading the matrix.
value_cols <- c(
  'incident_category_code', 'incident_subcategory_code', 'rms_incident_type_code', 'cad_incident_type_code',
  'dispatch_case_type_description', 'ems_unit_capability', 'dispatch_category_code', 'dispatch_code'
)
filter_values <- list(
  source_system = sort(unique(as.character(na.omit(app_matrix_df$rms_source_system)))),
  columns = map(value_cols, \(col) sort(unique(as.character(na.omit(app_matrix_df[[col]]))))) |>
    set_names(value_cols),
  dispatch_capability = app_matrix_df |>
    distinct(dispatch_code, ems_unit_capability) |>
    filter(!is.na(dispatch_code)) |>
    mutate(across(everything(), as.character))
)

saveRDS(filter_values, 'legacy/app/data/filter_values.rds')

# checks
travel_stats_df <- read_parquet('datasets/outputs/travel_stats.parquet')
missing_polygons <- setdiff(na.omit(unique(travel_stats_df$fdz)), fdz_polygons_sf$fdz)
stopifnot('fdzs with no polygon' = length(missing_polygons) == 0)
