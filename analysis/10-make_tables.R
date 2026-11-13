# 07 - Print-ready tables for the analysis
#
# For the first filter set in step 06's comparisons, with call counts as yearly
# averages, saved in tables/:
#   tbl_scenario_summary         every scenario's change from the baseline
#   tbl_overall_scenario_<k>_... overall, for each of the top 3 scenarios
#   tbl_station_scenario_<k>_... station workload, for each of the top 3
#   tbl_fdz_scenario_<k>_...     zones whose responder changes, for each of the 3
#
# Also computes the top scenarios against each other and every other group,
# with simultaneous bootstrap intervals, saved as compare_pairs.parquet for
# step 08's plot.
#
# Inputs  (data_outputs_dir): compare_overall.parquet, compare_calls.parquet,
#                             compare_fdz.parquet, compare_station.parquet,
#                             scenarios.parquet, filter_years.parquet,
#                             paired_calls.parquet  (step 06)
# Outputs (data_outputs_dir): tables/*.docx
#                             compare_pairs.parquet (for step 08's plot)
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# How many scenarios the pairwise comparison uses, ranked best first on an
# overall metric (e.g. "total_minutes", "p90_minutes", "pct_within_8min")
n_top <- 3
rank_by <- "total_minutes"

# Or name them yourself (overrides n_top and rank_by), e.g.
#   top_scenarios <- c("R26->16 +R1", "+R1", "R11->16 +R1")
top_scenarios <- NULL

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
paired <- read_output("paired_calls.parquet")
baseline_name <- scenarios$scenario_name[scenarios$baseline]

# Call counts (calls, incidents, total minutes, calls slower by X min) as
# yearly averages; times and shares are unchanged
cmp_overall <- read_output("compare_overall.parquet") |> annualize_counts(years)
cmp_calls <- read_output("compare_calls.parquet") |> annualize_counts(years)
cmp_fdz <- read_output("compare_fdz.parquet") |> annualize_counts(years)
cmp_station <- read_output("compare_station.parquet") |> annualize_counts(years)

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

# Scenario vs scenario --------------------------------------------------------
#
# The top scenarios against each other and against every other group, the
# baseline included (first minus second, a top scenario always first). A diff
# is colored only when its interval excludes 0; otherwise the two can't be
# told apart on these calls. Each interval is the range the difference would
# fall in over a single future year with the same pattern of calls (calls
# resampled one by one, as step 06's overall intervals). They are
# simultaneous: per metric, all of them hold together 95% of the time, so the
# many pairs don't turn up chance differences. Saved as
# compare_pairs.parquet, which step 08 plots.
# Metrics: mean_minutes (ranks like total_minutes), median_minutes,
# p90_minutes, mean_distance_m, pct_within_<threshold>.

pair_top <- top_scenarios %||%
  utils::head(
    rank_scenarios(
      cmp_overall,
      by = rank_by,
      filter_name = first_filter,
      due = due_level
    ),
    n_top
  )

set.seed(343)
cmp_pairs <- compare_scenarios(
  paired,
  scenario = pair_top, # names or ids
  against = c(baseline_name, unique(paired$scenario_name)), # NULL = only pairs within scenario
  metrics = c(
    "mean_minutes",
    "p90_minutes",
    "pct_within_8m58s", # 8:58, EMS High First Unit
    "pct_within_10m21s" # 10:21, EMS High ERF
  ),
  filter_name = first_filter,
  due = due_level,
  baseline_name = baseline_name,
  n_boot = 5000, # simultaneous intervals sit far in the tails: thousands of resamples
  level = 0.95,
  simultaneous = TRUE, # FALSE = each pair's own 95% interval
  resample = "call", # or "zone": whole FDZs
  predict_years = 1, # NULL = CIs of the long-run differences
  data_years = years # how many years each filter set's calls cover
)
arrow::write_parquet(
  cmp_pairs,
  file.path(cfg$data_outputs_dir, "compare_pairs.parquet")
)


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
  pct_total = FALSE
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
  pct_total = FALSE
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
  pct_total = FALSE
) |>
  save_table(file.path(tables_dir, "tbl_fdz_scenario_3_r1_r11.docx"))


message("Tables written to ", tables_dir)
