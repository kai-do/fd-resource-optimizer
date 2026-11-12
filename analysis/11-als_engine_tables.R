# 11 - Tables for the ALS engine placements
#
# Scores step 10's p-median solutions the same way step 06 scores the rescue
# scenarios (each call to its zone's first open station on the run card), with
# today's ALS engines as the baseline, and makes the tables:
#   tbl_als_solutions              the optimizer's solutions: stations, moves,
#                                  and its own totals (cheapest station)
#   tbl_als_summary                every solution against today, run card
#                                  scored
#   tbl_als_overall_solution_<k>   overall metrics, solution k vs today
#   tbl_als_station_solution_<k>   first-due calls by station
#   tbl_als_fdz_solution_<k>       zones whose first ALS station changes
#
# A scenario here is the fixed rescues plus the engine stations: the first
# ALS unit is whichever of them comes first on the run card.
#
# Not part of run_all.R (steps 03-09); run it after step 10.
#
# Inputs  (data_outputs_dir): p_median_solutions.parquet       (step 10)
#                             runcard.parquet                  (step 06)
#                             incident_travel_matrix_fdz.parquet  (step 04)
# Outputs (data_outputs_dir): als/als_scenario_comparison.xlsx,
#                             als/compare_{overall,calls,fdz,station}.parquet
#                             tables/als/*.docx
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Keep these in sync with step 10
fixed_stations <- c(1, 11, 14, 16, 21, 23, 24, 30)
current_als_engines <- c(1, 2, 4, 5, 9, 12, 19, 21, 27)

# Keep in sync with steps 06 and 10
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

# Solutions (step 10 ranks) that get overall, station and FDZ tables
detail_solutions <- 1

# Metrics for each table type
overall_metrics <- c(
  "n_calls",
  "mean_minutes",
  "median_minutes",
  "p90_minutes",
  "total_minutes",
  "mean_distance_m",
  "calls_within_8m58s", # 8:58, EMS High First Unit
  "pct_within_8m58s",
  "calls_within_10m21s", # 10:21, EMS High ERF
  "pct_within_10m21s",
  "fdz_p90_over_8min",
  "fdz_p90_over_8m58s",
  "fdz_p90_over_10m21s"
)

station_metrics <- c(
  "n_incidents",
  "p90_minutes"
)

fdz_metrics <- c(
  "n_calls",
  "mean_minutes",
  "p90_minutes"
)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

als_dir <- file.path(cfg$data_outputs_dir, "als")
tables_dir <- file.path(cfg$data_outputs_dir, "tables", "als")
dir.create(als_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

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

# Every solution must keep the fixed stations; if not, step 10 ran with other
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
  "By Station" = "compare_station.parquet"
)
for (sheet in names(compare_files)) {
  arrow::write_parquet(
    comparison[[sheet]],
    file.path(als_dir, compare_files[[sheet]]),
    compression = "zstd"
  )
}

# Call counts as yearly averages, as in step 07
years <- filter_years(filter, travel_matrix)
rm(travel_matrix)

cmp_overall <- annualize_counts(comparison[["Overall"]], years)
cmp_calls <- annualize_counts(comparison[["Calls"]], years)
cmp_fdz <- annualize_counts(comparison[["By FDZ"]], years)
cmp_station <- annualize_counts(comparison[["By Station"]], years)

# Tables ----------------------------------------------------------------------

# The optimizer's solutions, its own (cheapest station) totals per year
p_median_solutions_table(solutions, years = years$years[1]) |>
  save_table(file.path(tables_dir, "tbl_als_solutions.docx"))

# Every solution against today's engines, run card scored
scenario_summary_table(
  overall_df = cmp_overall,
  calls_df = cmp_calls,
  station_df = cmp_station,
  scenario = NULL, # NULL = every scenario
  filter_name = filter$name,
  due = due_level,
  metrics = c(
    "total_minutes",
    "p90_minutes",
    "pct_within_8m58s", # 8:58, EMS High First Unit
    "pct_within_10m21s" # 10:21, EMS High ERF
  ),
  calls_metrics = c(
    "share_faster",
    "share_slower"
  ),
  sort_by = "total_minutes",
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_als_summary.docx"))

# Zones whose first ALS station changes in a scenario
changed_fdz <- function(scenario) {
  cmp_fdz[
    cmp_fdz$filter_name == filter$name &
      cmp_fdz$due == due_level &
      cmp_fdz$scenario_name == scenario &
      cmp_fdz$responder_changed,
  ]
}

for (k in detail_solutions) {
  scenario <- paste0("P-median #", k)

  overall_table(
    df = cmp_overall,
    scenario = scenario,
    filter_name = filter$name,
    due = due_level,
    metrics = overall_metrics,
    baseline_name = baseline_name,
    title = NULL,
    subtitle = NULL,
    time_format = "mmss"
  ) |>
    save_table(file.path(
      tables_dir,
      paste0("tbl_als_overall_solution_", k, ".docx")
    ))

  station_table(
    df = cmp_station,
    scenario = scenario,
    filter_name = filter$name,
    due = due_level,
    metrics = station_metrics,
    baseline_name = baseline_name,
    title = NULL,
    subtitle = NULL,
    pct_total = TRUE,
    time_format = "mmss"
  ) |>
    save_table(file.path(
      tables_dir,
      paste0("tbl_als_station_solution_", k, ".docx")
    ))

  fdz_table(
    df = changed_fdz(scenario),
    scenario = scenario,
    filter_name = filter$name,
    due = due_level,
    metrics = fdz_metrics,
    baseline_name = baseline_name,
    title = NULL,
    subtitle = NULL,
    pct_total = FALSE
  ) |>
    save_table(file.path(
      tables_dir,
      paste0("tbl_als_fdz_solution_", k, ".docx")
    ))
}

message("Tables written to ", tables_dir)
