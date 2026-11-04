# 03 - Assign incidents to Fire Demand Zones
#
# Inputs  (data_inputs_dir):  <date>_fdzs.csv                   (fdzs.sql)
#                             <date>_incident_address_data.csv  (incident_address_data.sql)
#                             <date>_ems_unit_capabilities.csv
# Outputs (data_outputs_dir): incident_fdz.parquet
#                             One row per incident: FDZ, station territory,
#                             alarm time (New York), incident type codes and
#                             EMS unit capability.
#
# Run from the repo root.

pkgload::load_all()

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

fdz_file <- file.path(cfg$data_inputs_dir, "2026-10-14_fdzs.csv")
incident_file <- file.path(
  cfg$data_inputs_dir,
  "2026-10-13_incident_address_data.csv"
)
ems_file <- file.path(
  cfg$data_inputs_dir,
  "2026-10-14_ems_unit_capabilities.csv"
)
output_file <- file.path(cfg$data_outputs_dir, "incident_fdz.parquet")

# FDZ polygons ----------------------------------------------------------------

fdz_sf <- data.table::fread(
  fdz_file,
  na.strings = "NULL",
  data.table = FALSE
) |>
  prep_fdz_polygons()

# Incidents -------------------------------------------------------------------

# The same incident set script 02 routes
# Code columns as text, so codes like 09 keep their leading zero
incident_df <- data.table::fread(
  incident_file,
  na.strings = "NULL",
  data.table = FALSE,
  colClasses = list(
    character = c(
      "incident_category_code",
      "incident_subcategory_code",
      "dispatch_category_code"
    )
  )
) |>
  clean_incidents()

# EMS unit capability ---------------------------------------------------------

# Every column as text, so dispatch codes keep their leading zeros (01C05)
ems_unit_capability_df <- readr::read_csv(
  ems_file,
  col_types = readr::cols(.default = readr::col_character())
)

# Assemble --------------------------------------------------------------------

# Spatially joins incidents to FDZs. Prints how many incidents fell in no FDZ
fdz_keys <- assign_incident_fdz(incident_df, fdz_sf)

incident_fdz_df <- incident_df |>
  dplyr::left_join(fdz_keys, by = "incident_number") |>
  dplyr::mutate(incident_alarm_dt = fix_alarm_tz(incident_alarm_dt)) |>
  add_ems_capability(ems_unit_capability_df) |>
  # The columns later steps use. fdz comes from the polygons, not the
  # extract's own CAD fdz column.
  dplyr::transmute(
    incident_number,
    incident_alarm_dt,
    fdz_id,
    station_id,
    cad_incident_type_code,
    dispatch_code,
    rms_incident_type_code = incident_type_code,
    rms_source_system = source_system,
    ems_unit_capability,
    # for the incident filters (see incident_filter())
    incident_category_code,
    incident_subcategory_code,
    dispatch_category_code,
    dispatch_case_type_description
  )

# Checks ----------------------------------------------------------------------

stopifnot(
  nrow(incident_fdz_df) == nrow(incident_df),
  !anyDuplicated(incident_fdz_df$incident_number),
  !anyNA(incident_fdz_df$incident_alarm_dt)
)

# Write -----------------------------------------------------------------------

arrow::write_parquet(incident_fdz_df, output_file, compression = "zstd")
