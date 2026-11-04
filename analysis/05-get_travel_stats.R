# 05 - Travel-time statistics by station and FDZ
#
# Replaces legacy/scripts/05-get_travel_stats.R.
#
# Inputs  (data_outputs_dir): incident_travel_matrix_fdz.parquet  (step 04)
# Outputs (data_outputs_dir): travel_stats.parquet
#                             One row per station x FDZ: incident count, mean,
#                             median and 90th percentile drive time (min),
#                             mean distance (m), first and last alarm time.
#
# Run from the repo root.

pkgload::load_all()

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

matrix_file <- file.path(
  cfg$data_outputs_dir,
  "incident_travel_matrix_fdz.parquet"
)
output_file <- file.path(cfg$data_outputs_dir, "travel_stats.parquet")

# Read ------------------------------------------------------------------------

# Only the columns the summary needs: the matrix has 14M+ rows
travel_matrix <- arrow::read_parquet(
  matrix_file,
  col_select = c(
    "source_id",
    "dest_id",
    "fdz_id",
    "station_id",
    "duration_min",
    "distance_m",
    "incident_alarm_dt"
  )
)

# Summarise -------------------------------------------------------------------

travel_stats <- summarise_travel_stats(
  travel_matrix,
  by = c("source_id", "fdz_id", "station_id")
)

# Write -----------------------------------------------------------------------

arrow::write_parquet(travel_stats, output_file, compression = "zstd")
