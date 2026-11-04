# 04 - Attach incident FDZ and attributes to the travel matrix
#
# Replaces the last part of legacy/scripts/04-geocode_fdzs.R.
#
# Inputs  (data_outputs_dir): incident_travel_matrix.parquet  (step 02)
#                             incident_fdz.parquet            (step 03)
# Outputs (data_outputs_dir): incident_travel_matrix_fdz.parquet
#                             The travel matrix, one row per station x
#                             incident, with each incident's FDZ, station
#                             territory, alarm time, type codes and EMS unit
#                             capability added.
#
# Run from the repo root.

pkgload::load_all()

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

matrix_file <- file.path(cfg$data_outputs_dir, "incident_travel_matrix.parquet")
incident_fdz_file <- file.path(cfg$data_outputs_dir, "incident_fdz.parquet")
output_file <- file.path(
  cfg$data_outputs_dir,
  "incident_travel_matrix_fdz.parquet"
)

# Read ------------------------------------------------------------------------

travel_matrix <- arrow::read_parquet(matrix_file)
incident_fdz_df <- arrow::read_parquet(incident_fdz_file)

# Join ------------------------------------------------------------------------

# dest_id in the matrix is the incident number
travel_matrix_fdz <- travel_matrix |>
  dplyr::left_join(
    incident_fdz_df,
    by = c("dest_id" = "incident_number"),
    relationship = "many-to-one"
  )

# Checks ----------------------------------------------------------------------

# The join adds columns only: no rows gained or lost, and every incident in
# the matrix was found (steps 02 and 03 both start from clean_incidents()).
missing <- setdiff(travel_matrix$dest_id, incident_fdz_df$incident_number)

stopifnot(
  nrow(travel_matrix_fdz) == nrow(travel_matrix),
  length(missing) == 0
)

# Write -----------------------------------------------------------------------

arrow::write_parquet(travel_matrix_fdz, output_file, compression = "zstd")
