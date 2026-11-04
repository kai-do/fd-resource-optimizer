# 02 - Station-to-incident travel matrix
#
# Inputs  (data_inputs_dir):  <date>_incident_address_data.csv  (incident_address_data.sql)
#                             <date>_station_addresses.csv      (station_addresses.sql)
# Outputs (data_outputs_dir): incident_travel_matrix.parquet
#                             One row per station x incident: drive distance (m),
#                             drive time (min) and OSRM snap distances.
# Needs:  the local OSRM server running (01-start_osrm_server.ps1), started with
#         --max-table-size >= 900 + the number of stations.
#
# Run from the repo root.

pkgload::load_all()

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

incident_file <- file.path(
  cfg$data_inputs_dir,
  "2026-10-13_incident_address_data.csv"
)
station_file <- file.path(
  cfg$data_inputs_dir,
  "2026-10-13_station_addresses.csv"
)
output_file <- file.path(cfg$data_outputs_dir, "incident_travel_matrix.parquet")

# Point osrm at the local server. Without this, osrm uses its public demo
# server, which would send incident locations off the network.
options(
  osrm.server = sprintf("http://%s:%s/", cfg$osrm_host, cfg$osrm_port),
  osrm.profile = cfg$osrm_profile
)

# Stations --------------------------------------------------------------------

# Cobb stations only, without headquarters
station_df <- data.table::fread(station_file, data.table = FALSE) |>
  prep_stations(keep_dept = "Cobb", exclude_abbrev = "FHQ")

# Incidents -------------------------------------------------------------------

incident_raw_df <- data.table::fread(
  incident_file,
  na.strings = "NULL",
  data.table = FALSE
)

incident_df <- clean_incidents(incident_raw_df)

message(
  "Dropped ",
  nrow(incident_raw_df) - nrow(incident_df),
  " incident rows (missing or (0, 0) coordinates, or duplicate incident numbers)"
)

# Matrix ----------------------------------------------------------------------

message(
  "Building matrix: ",
  nrow(station_df),
  " stations x ",
  nrow(incident_df),
  " incidents = ",
  format(nrow(station_df) * nrow(incident_df), big.mark = ","),
  " rows"
)

travel_matrix <- build_travel_matrix(station_df, incident_df)

arrow::write_parquet(travel_matrix, output_file, compression = "zstd")
