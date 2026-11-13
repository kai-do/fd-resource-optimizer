# 09 - Score the ALS engine placements on the run card (data step, slow)
#
# Scores step 08's p-median solutions the same way step 06 scores the rescue
# scenarios (each call to its zone's first open station on the run card), with
# today's ALS engines as the baseline. Step 13 makes the tables.
#
# A scenario here is the fixed rescues plus the engine stations: the first
# ALS unit is whichever of them comes first on the run card.
#
# Inputs  (data_outputs_dir): p_median_solutions.parquet       (step 08)
#                             runcard.parquet                  (step 06)
#                             incident_travel_matrix_fdz.parquet  (step 04)
# Outputs (data_outputs_dir): als/als_scenario_comparison.xlsx,
#                             als/compare_{overall,calls,fdz,station,
#                                          territory}.parquet
#                             als/scenarios.parquet, als/filter_years.parquet
#                             (the baseline's name and the years behind the
#                             yearly averages, for step 13)
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Keep these in sync with step 08
fixed_stations <- c(1, 11, 14, 16, 21, 23, 24, 30)
current_als_engines <- c(1, 2, 4, 5, 9, 12, 19, 21, 27)

# Keep in sync with steps 06 and 08
filter <- incident_filter(
  "Paramedic Calls, Last 3 Years",
  date_range = c("2023-10-01", "2026-09-30"),
  ems_capability = "PARAMEDIC"
)
thresholds <- c(
  8,
  8 + 58 / 60, # 8:58, EMS High First Unit
  10,
  10 + 21 / 60 # 10:21, EMS High ERF
)
due_level <- 1

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

als_dir <- file.path(cfg$data_outputs_dir, "als")
dir.create(als_dir, recursive = TRUE, showWarnings = FALSE)

# Read ------------------------------------------------------------------------

read_output <- function(file) {
  as.data.frame(arrow::read_parquet(file.path(cfg$data_outputs_dir, file)))
}

solutions <- read_output("p_median_solutions.parquet")
runcard <- read_output("runcard.parquet")

# The matrix has 14M+ rows: only the travel columns and the ones the filter
# set uses
travel_matrix <- arrow::read_parquet(
  file.path(cfg$data_outputs_dir, "incident_travel_matrix_fdz.parquet"),
  col_select = c(
    "source_id",
    "dest_id",
    "fdz_id",
    "station_id",
    "duration_min",
    "distance_m",
    "incident_alarm_dt",
    "ems_unit_capability"
  )
)

# Every solution must keep the fixed stations; if not, step 08 ran with other
# settings
stopifnot(all(vapply(
  solutions$open_stations,
  \(s) all(fixed_stations %in% unlist(s)),
  logical(1)
)))

# Scenarios -------------------------------------------------------------------

baseline_name <- "Current ALS Engines"

scenarios <- tibble::tibble(
  scenario_id = c(1L, solutions$rank + 1L),
  scenario_name = c(baseline_name, paste0("P-median #", solutions$rank)),
  stations = c(
    list(sort(union(fixed_stations, current_als_engines))),
    lapply(solutions$open_stations, \(s) sort(unlist(s)))
  ),
  baseline = c(TRUE, rep(FALSE, nrow(solutions)))
) |>
  validate_scenarios()

# Compare ---------------------------------------------------------------------

# Same settings as step 06
comparison <- scenarios |>
  export_comparison(
    travel_matrix,
    runcard,
    file = file.path(als_dir, "als_scenario_comparison.xlsx"),
    filters = filter,
    due = due_level,
    thresholds = thresholds,
    ci = TRUE,
    n_boot = 2000,
    resample = "call",
    predict_years = 1,
    seed = 343
  )

compare_files <- c(
  "Overall" = "compare_overall.parquet",
  "Calls" = "compare_calls.parquet",
  "By FDZ" = "compare_fdz.parquet",
  "By Station" = "compare_station.parquet",
  "By Territory" = "compare_territory.parquet"
)
for (sheet in names(compare_files)) {
  arrow::write_parquet(
    comparison[[sheet]],
    file.path(als_dir, compare_files[[sheet]]),
    compression = "zstd"
  )
}

# The scenarios, for step 13's baseline name, and the years the filter set
# covers, for its yearly averages
arrow::write_parquet(scenarios, file.path(als_dir, "scenarios.parquet"))
arrow::write_parquet(
  filter_years(filter, travel_matrix),
  file.path(als_dir, "filter_years.parquet")
)

message("ALS comparison written to ", als_dir)
