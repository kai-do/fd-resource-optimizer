# 06 - Run card travel stats by scenario and filter set
#
# Each scenario is a set of open stations; each filter set
# (incident_filter()) says which calls count. Every scenario runs under every
# filter set: each incident goes to its zone's run card responder at the chosen
# due levels, and stats are summarised by responding station and by FDZ. Results
# for all scenarios and filter sets sit side by side in one file each.
#
# Inputs  (data_outputs_dir): incident_travel_matrix_fdz.parquet  (step 04)
#         (data_inputs_dir):  <date>_cad_run_card.csv  (cad_run_card.sql)
# Outputs (data_outputs_dir): runcard.parquet
#                             Run card order: one row per zone and Cobb
#                             responding station, in card order.
#                             runcard_stats_station.parquet
#                             One row per filter set, scenario, due level and
#                             responding station, with n_fdz (zones served).
#                             runcard_stats_fdz.parquet
#                             One row per filter set, scenario, due level and
#                             zone.
#                             compare_overall.parquet, compare_calls.parquet,
#                             compare_fdz.parquet, compare_station.parquet
#                             Each scenario minus the baseline scenario, one
#                             row per metric (see R/fct_compare.R).
#
# Run from the repo root.

pkgload::load_all()

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

matrix_file <- file.path(
  cfg$data_outputs_dir,
  "incident_travel_matrix_fdz.parquet"
)
runcard_file <- file.path(cfg$data_inputs_dir, "2026-10-13_cad_run_card.csv")

runcard_output_file <- file.path(cfg$data_outputs_dir, "runcard.parquet")
station_output_file <- file.path(
  cfg$data_outputs_dir,
  "runcard_stats_station.parquet"
)
fdz_output_file <- file.path(cfg$data_outputs_dir, "runcard_stats_fdz.parquet")

# Read ------------------------------------------------------------------------

# The matrix has 14M+ rows: only the travel columns and the ones the filter
# sets use
travel_matrix <- arrow::read_parquet(
  matrix_file,
  col_select = c(
    "source_id",
    "dest_id",
    "fdz_id",
    "station_id",
    "duration_min",
    "distance_m",
    "incident_alarm_dt",
    "rms_source_system",
    "incident_category_code",
    "incident_subcategory_code",
    "rms_incident_type_code",
    "cad_incident_type_code",
    "dispatch_code",
    "dispatch_category_code",
    "dispatch_case_type_description",
    "ems_unit_capability"
  )
)

runcard <- data.table::fread(
  runcard_file,
  na.strings = "NULL",
  data.table = FALSE
) |>
  prep_runcard()

all_stations <- sort(unique(travel_matrix$source_id))

# Scenarios -------------------------------------------------------------------

# One row per scenario: a unique id, a name, the open stations, and whether
# it is the baseline the others are compared with (exactly one TRUE)
scenarios <- tibble::tribble(
  ~scenario_id, ~scenario_name, ~stations, ~baseline,
  1, "Current", all_stations, TRUE,
  2, "7 stations", c(11, 14, 21, 23, 24, 26, 30), FALSE
)

# Filter sets -----------------------------------------------------------------

# Which calls count. Add more with incident_filter(), e.g.
#   incident_filter(
#     "Night ALS, last 3 yrs",
#     date_range = c("2023-10-01", "2026-09-30"),
#     time_range = c("22:00", "06:00"),
#     ems_capability = "PARAMEDIC"
#   )
filters <- list(
  incident_filter("All calls")
)

# Due levels: 1 for 1st due, 1:3 for 1st to 3rd due
due_levels <- 1

# Comparison settings: travel-time thresholds (minutes), and bootstrap
# confidence intervals on the overall diffs (slower; set.seed for repeatable
# intervals)
thresholds <- c(4, 8)
with_ci <- FALSE

# Stats -----------------------------------------------------------------------

# Prints, per filter set and scenario, how many zones have no open station at
# a due level
stats_station <- scenarios |>
  stats_by_station(travel_matrix, runcard, filters = filters, due = due_levels)

stats_fdz <- scenarios |>
  stats_by_fdz(travel_matrix, runcard, filters = filters, due = due_levels)

# Comparisons -----------------------------------------------------------------

set.seed(2026)
compare_overall_df <- scenarios |>
  compare_overall(
    travel_matrix,
    runcard,
    filters = filters,
    due = due_levels,
    thresholds = thresholds,
    ci = with_ci
  )

compare_calls_df <- scenarios |>
  compare_calls(travel_matrix, runcard, filters = filters, due = due_levels)

compare_fdz_df <- scenarios |>
  compare_by_fdz(
    travel_matrix,
    runcard,
    filters = filters,
    due = due_levels,
    thresholds = thresholds
  )

compare_station_df <- scenarios |>
  compare_by_station(travel_matrix, runcard, filters = filters, due = due_levels)

# Checks ----------------------------------------------------------------------

# Within each filter set, scenario and due level: every zone once, and the
# station view covers the same incidents and zones as the zone view
keys <- c("filter_name", "scenario_id", "due")

fdz_totals <- stats_fdz |>
  dplyr::summarise(
    n_incidents = sum(n_incidents),
    n_fdz = dplyr::n(),
    .by = dplyr::all_of(keys)
  )
station_totals <- stats_station |>
  dplyr::summarise(
    n_incidents = sum(n_incidents),
    n_fdz = sum(n_fdz),
    .by = dplyr::all_of(keys)
  )

stopifnot(
  !anyDuplicated(stats_fdz[c(keys, "fdz_id")]),
  isTRUE(all.equal(
    as.data.frame(fdz_totals),
    as.data.frame(station_totals),
    check.attributes = FALSE
  ))
)

# Write -----------------------------------------------------------------------

arrow::write_parquet(runcard, runcard_output_file, compression = "zstd")
arrow::write_parquet(stats_station, station_output_file, compression = "zstd")
arrow::write_parquet(stats_fdz, fdz_output_file, compression = "zstd")

for (name in c("overall", "calls", "fdz", "station")) {
  arrow::write_parquet(
    get(paste0("compare_", name, "_df")),
    file.path(cfg$data_outputs_dir, paste0("compare_", name, ".parquet")),
    compression = "zstd"
  )
}
