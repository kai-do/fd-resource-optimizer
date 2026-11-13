# 10 - Print-ready tables for the analysis (output step, quick: rerun freely)
#
# For the first filter set in step 06's comparisons, with call counts as yearly
# averages, saved in tables/:
#   tbl_scenario_summary         every scenario's values and change from the
#                                baseline
#   tbl_overall_scenario_<k>_... overall, for each of the top 3 scenarios
#   tbl_station_scenario_<k>_... station workload, for each of the top 3
#   tbl_fdz_scenario_<k>_...     zones whose responder changes, for each of the 3
#   tbl_territory_scenario_<k>_. coverage of each station's territory (its
#                                zones' calls, whoever responds), for each of
#                                the 3
#
# The scenario-vs-scenario comparison (slow) is step 07.
#
# Inputs  (data_outputs_dir): compare_overall.parquet, compare_calls.parquet,
#                             compare_fdz.parquet, compare_station.parquet,
#                             compare_territory.parquet,
#                             scenarios.parquet, filter_years.parquet (step 06)
# Outputs (data_outputs_dir): tables/*.docx
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Overall metric the scenarios are ranked on, best first (the summary's sort
# and the example tables' top 3), e.g. "total_minutes", "p90_minutes",
# "pct_within_8min"
rank_by <- "total_minutes"

# Due level to show (step 06's due_levels)
due_level <- 1

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

tables_dir <- file.path(cfg$data_outputs_dir, "tables")

# Read ------------------------------------------------------------------------

read_output <- function(file) {
  arrow::read_parquet(file.path(cfg$data_outputs_dir, file))
}

scenarios <- read_output("scenarios.parquet")
years <- read_output("filter_years.parquet")
baseline_name <- scenarios$scenario_name[scenarios$baseline]

# Call counts (calls, incidents, total minutes, calls slower by X min) as
# yearly averages; times and shares are unchanged
cmp_overall <- read_output("compare_overall.parquet") |> annualize_counts(years)
cmp_calls <- read_output("compare_calls.parquet") |> annualize_counts(years)
cmp_fdz <- read_output("compare_fdz.parquet") |> annualize_counts(years)
cmp_station <- read_output("compare_station.parquet") |> annualize_counts(years)
cmp_territory <- read_output("compare_territory.parquet") |>
  annualize_counts(years)

# Tables ----------------------------------------------------------------------

# Summary for the first filter set, saved at the top of tables/
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
first_filter <- unique(cmp_overall$filter_name)[1]

scenario_summary_table(
  overall_df = cmp_overall,
  calls_df = cmp_calls,
  station_df = cmp_station,
  scenario = NULL, # NULL = every scenario
  filter_name = first_filter,
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
  sort_by = rank_by, # one of the columns above, or "busiest_station"
  baseline_name = baseline_name,
  title = NULL, # NULL = automatic
  subtitle = NULL,
  time_format = 'mmss'
) |>
  save_table(file.path(tables_dir, "tbl_scenario_summary.docx"))

# Example tables, every argument and metric spelled out ------------------------
#
# Overall and station tables for the top 3 scenarios in the first filter set,
# and the FDZ tables for the same 3. Drop the metrics you don't need.
# Threshold names come from step 06's thresholds: 8min and 10min, 8m58s (8:58,
# EMS High First Unit) and 10m21s (10:21, EMS High ERF).

scenarios <- rank_scenarios(
  cmp_overall,
  by = rank_by,
  filter_name = first_filter,
  due = due_level
)

scenario_1_r1_r26 <- scenarios[1]
scenario_2_r1 <- scenarios[2]
scenario_3_r1_r11 <- scenarios[3]

# Metrics for each table type, shared by the 3 scenarios
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

territory_metrics <- c(
  "n_calls",
  "mean_minutes",
  "p90_minutes"
)

# Overall: one row per metric; Baseline / Scenario / Diff / % Diff (and 95% CI)
overall_table(
  df = cmp_overall,
  scenario = scenario_1_r1_r26,
  filter_name = first_filter,
  due = due_level,
  metrics = overall_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_overall_scenario_1_r1_r26.docx"))

overall_table(
  df = cmp_overall,
  scenario = scenario_2_r1,
  filter_name = first_filter,
  due = due_level,
  metrics = overall_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_overall_scenario_2_r1.docx"))

overall_table(
  df = cmp_overall,
  scenario = scenario_3_r1_r11,
  filter_name = first_filter,
  due = due_level,
  metrics = overall_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_overall_scenario_3_r1_r11.docx"))


# Station Tables ----------------------------------------------------------

station_table(
  df = cmp_station,
  scenario = scenario_1_r1_r26,
  filter_name = first_filter,
  due = due_level,
  metrics = station_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_station_scenario_1_r1_r26.docx"))


station_table(
  df = cmp_station,
  scenario = scenario_2_r1,
  filter_name = first_filter,
  due = due_level,
  metrics = station_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_station_scenario_2_r1.docx"))


station_table(
  df = cmp_station,
  scenario = scenario_3_r1_r11,
  filter_name = first_filter,
  due = due_level,
  metrics = station_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_station_scenario_3_r1_r11.docx"))


# FDZ Tables ------------------------------------------------------------------
#
# One row per zone, only the zones whose responder changes in that scenario
# (all zones would be hundreds of rows)

changed_fdz <- function(scenario) {
  cmp_fdz[
    cmp_fdz$filter_name == first_filter &
      cmp_fdz$due == due_level &
      cmp_fdz$scenario_name == scenario &
      cmp_fdz$responder_changed,
  ]
}

fdz_table(
  df = changed_fdz(scenario_1_r1_r26),
  scenario = scenario_1_r1_r26,
  filter_name = first_filter,
  due = due_level,
  metrics = fdz_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = FALSE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_fdz_scenario_1_r1_r26.docx"))

fdz_table(
  df = changed_fdz(scenario_2_r1),
  scenario = scenario_2_r1,
  filter_name = first_filter,
  due = due_level,
  metrics = fdz_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = FALSE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_fdz_scenario_2_r1.docx"))

fdz_table(
  df = changed_fdz(scenario_3_r1_r11),
  scenario = scenario_3_r1_r11,
  filter_name = first_filter,
  due = due_level,
  metrics = fdz_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = FALSE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_fdz_scenario_3_r1_r11.docx"))


# Territory Tables ------------------------------------------------------------
#
# One row per station territory: the calls in its zones (1A, 1B, ...),
# whoever responds, so these show how well each territory is covered. Green
# diffs are better, red worse. One metric with scenario = c(...) puts several
# scenarios side by side.

territory_table(
  df = cmp_territory,
  scenario = scenario_1_r1_r26,
  filter_name = first_filter,
  due = due_level,
  metrics = territory_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_territory_scenario_1_r1_r26.docx"))

territory_table(
  df = cmp_territory,
  scenario = scenario_2_r1,
  filter_name = first_filter,
  due = due_level,
  metrics = territory_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_territory_scenario_2_r1.docx"))

territory_table(
  df = cmp_territory,
  scenario = scenario_3_r1_r11,
  filter_name = first_filter,
  due = due_level,
  metrics = territory_metrics,
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_territory_scenario_3_r1_r11.docx"))


message("Tables written to ", tables_dir)
