# 13 - Tables for the ALS engine placements (output step, quick: rerun freely)
#
# From step 09's run card scoring of step 08's p-median solutions, today's ALS
# engines as the baseline, with call counts as yearly averages, saved in
# tables/als/:
#   tbl_als_solutions              the optimizer's solutions: stations, moves,
#                                  and its own totals (cheapest station)
#   tbl_als_summary                every solution's values and change from
#                                  today, run card scored
#   tbl_als_overall_solution_<k>   overall metrics, solution k vs today
#   tbl_als_station_solution_<k>   first-due calls by station
#   tbl_als_fdz_solution_<k>       zones whose first ALS station changes
#   tbl_als_territory_solution_<k> coverage of each station's territory (its
#                                  zones' calls, whoever responds)
#
# Inputs  (data_outputs_dir): p_median_solutions.parquet       (step 08)
#                             als/compare_{overall,calls,fdz,station,
#                                          territory}.parquet,
#                             als/scenarios.parquet,
#                             als/filter_years.parquet         (step 09)
# Outputs (data_outputs_dir): tables/als/*.docx
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Due level to show (step 09's due_level)
due_level <- 1

# Solutions (step 08 ranks) that get overall, station, FDZ and territory
# tables
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

# By territory: the calls in each station's zones, whoever responds
territory_metrics <- c(
  "n_calls",
  "mean_minutes",
  "p90_minutes"
)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

als_dir <- file.path(cfg$data_outputs_dir, "als")
tables_dir <- file.path(cfg$data_outputs_dir, "tables", "als")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Read ------------------------------------------------------------------------

read_als <- function(file) {
  as.data.frame(arrow::read_parquet(file.path(als_dir, file)))
}

solutions <- as.data.frame(arrow::read_parquet(
  file.path(cfg$data_outputs_dir, "p_median_solutions.parquet")
))
scenarios <- read_als("scenarios.parquet")
years <- read_als("filter_years.parquet")
baseline_name <- scenarios$scenario_name[scenarios$baseline]

# Call counts as yearly averages, as in step 10
cmp_overall <- read_als("compare_overall.parquet") |> annualize_counts(years)
cmp_calls <- read_als("compare_calls.parquet") |> annualize_counts(years)
cmp_fdz <- read_als("compare_fdz.parquet") |> annualize_counts(years)
cmp_station <- read_als("compare_station.parquet") |> annualize_counts(years)
cmp_territory <- read_als("compare_territory.parquet") |>
  annualize_counts(years)

# The filter set step 09 used
filter_name <- unique(cmp_overall$filter_name)[1]

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
  filter_name = filter_name,
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
    cmp_fdz$filter_name == filter_name &
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
    filter_name = filter_name,
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
    filter_name = filter_name,
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
    filter_name = filter_name,
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

  territory_table(
    df = cmp_territory,
    scenario = scenario,
    filter_name = filter_name,
    due = due_level,
    metrics = territory_metrics,
    baseline_name = baseline_name,
    title = NULL,
    subtitle = NULL,
    pct_total = TRUE,
    time_format = "mmss"
  ) |>
    save_table(file.path(
      tables_dir,
      paste0("tbl_als_territory_solution_", k, ".docx")
    ))
}

message("Tables written to ", tables_dir)
