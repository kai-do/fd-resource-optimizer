# 07 - Print-ready tables for the analysis
#
# For each filter set in step 06's comparisons, with call counts as yearly
# averages:
#   00_summary            every scenario's change from the baseline, ranked
#   01_overall_top        the top scenarios side by side, overall
#   02_station_top        the top scenarios side by side, incidents by station
#   top<k>_<scenario>_... for each top scenario: overall, call-level changes,
#                         station workload, and the zones whose responder
#                         changes
#
# And, for the first filter set, tbl_scenario_pairs: the top scenarios against
# each other and every other group, with simultaneous bootstrap intervals, to
# show whether they differ by more than chance; and tbl_dispatch_codes: the calls
# for every dispatch code the filter set includes, zeros too, under their
# chief complaints.
#
# The top scenarios are the best n_top on rank_by, or the ones you name in
# top_scenarios. All tables are written as Word tables (.docx) or images
# (.png), set by table_format.
#
# Inputs  (data_outputs_dir): compare_overall.parquet, compare_calls.parquet,
#                             compare_fdz.parquet, compare_station.parquet,
#                             scenarios.parquet, filter_years.parquet,
#                             paired_calls.parquet  (step 06)
#                             incident_fdz.parquet  (step 03)
#         (data_inputs_dir):  <date>_ems_call_mapping.csv
# Outputs (data_outputs_dir): tables/<filter set>/*.docx or *.png
#                             compare_pairs.parquet (for step 08's plot)
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# "docx" (editable Word tables) or "png" (images)
table_format <- "docx"

# How many scenarios get the full set of tables, ranked best first on an
# overall metric (e.g. "total_minutes", "p90_minutes", "pct_within_8min")
n_top <- 3
rank_by <- "total_minutes"

# Or name them yourself (overrides n_top and rank_by), e.g.
#   top_scenarios <- c("R26->16 +R1", "+R1", "R11->16 +R1")
top_scenarios <- NULL

# Due level to show (step 06's due_levels)
due_level <- 1

# Fire unit tier whose dispatch codes the dispatch code table lists: the first
# filter set's ems_capability in step 06
dispatch_tier <- "PARAMEDIC"

stopifnot(table_format %in% c("docx", "png"))

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

tables_dir <- file.path(cfg$data_outputs_dir, "tables")
mapping_file <- file.path(cfg$data_inputs_dir, "2026-11-09_ems_call_mapping.csv")

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

# Text safe for file and folder names: "R26->16 +R1" -> "R26_16_R1"
file_slug <- function(x) {
  gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", x))
}

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
  utils::head(rank_scenarios(cmp_overall, by = rank_by, filter_name = first_filter, due = due_level), n_top)

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
arrow::write_parquet(cmp_pairs, file.path(cfg$data_outputs_dir, "compare_pairs.parquet"))

scenario_pairs_table(
  cmp_pairs,
  metrics = NULL, # NULL = every metric in cmp_pairs
  title = NULL,
  subtitle = NULL,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_scenario_pairs.docx"))

# Dispatch codes --------------------------------------------------------------
#
# Every code of dispatch_tier in the EMS call mapping, with the first filter
# set's calls (zeros included) under their chief complaints. Codes,
# descriptors and CAD codes come from the mapping, saved from Excel as
# Windows-1252 text.

call_mapping <- read.csv(
  mapping_file,
  colClasses = "character", # codes keep their leading zeros (01C05)
  fileEncoding = "cp1252",
  check.names = FALSE
) |>
  prep_call_mapping()

incident_fdz <- read_output("incident_fdz.parquet")[c("incident_number", "dispatch_code")]
filter_codes <- incident_fdz[incident_fdz$incident_number %in% paired$dest_id[paired$filter_name == first_filter], ]
filter_years <- years$years[years$filter_name == first_filter]

dispatch_code_counts(
  filter_codes,
  call_mapping,
  tier = dispatch_tier,
  years = filter_years
) |>
  dispatch_code_table(
    filter_name = first_filter,
    title = NULL, # NULL = automatic
    subtitle = NULL,
    years = filter_years
  ) |>
  save_table(file.path(tables_dir, "tbl_dispatch_codes.docx"))

# Example tables, every argument and metric spelled out ------------------------
#
# One of each table type for the best scenario in the first filter set. Drop
# the metrics you don't need. Threshold names come from step 06's thresholds:
# 8min and 10min, 8m58s (8:58, EMS High First Unit) and 10m21s (10:21, EMS
# High ERF); the slower-by ones (2, 5 min) from its slower_by.

scenarios <- rank_scenarios(
  cmp_overall,
  by = rank_by,
  filter_name = first_filter,
  due = due_level
)

scenario_1_r1_r26 <- scenarios[1]
scenario_2_r1 <- scenarios[2]
scenario_3_r1_r11 <- scenarios[3]

# The best scenario, used by the call, station and FDZ examples below
example_scenario <- scenario_1_r1_r26

# Overall: one row per metric; Baseline / Scenario / Diff / % Diff (and 95% CI)
overall_table(
  df = cmp_overall,
  scenario = scenario_1_r1_r26,
  filter_name = first_filter,
  due = due_level,
  metrics = c(
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
  ),
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
  metrics = c(
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
  ),
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
  metrics = c(
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
  ),
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
  metrics = c(
    "n_incidents",
    "p90_minutes"
  ),
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
  metrics = c(
    "n_incidents",
    "p90_minutes"
  ),
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
  metrics = c(
    "n_incidents",
    "p90_minutes"
  ),
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE,
  time_format = "mmss"
) |>
  save_table(file.path(tables_dir, "tbl_station_scenario_3_r1_r11.docx"))


# Call Tables -----------------------------------------------------------------

# Call-level changes: values are already scenario minus baseline
calls_table(
  df = cmp_calls,
  scenario = example_scenario, # several for one column each
  filter_name = first_filter,
  due = due_level,
  metrics = c(
    "n_calls",
    "share_responder_changed",
    "share_faster",
    "share_slower",
    "share_unchanged",
    "mean_change",
    "median_change",
    "p90_change",
    "max_change",
    "calls_slower_by_2min",
    "share_slower_by_2min",
    "calls_slower_by_5min",
    "share_slower_by_5min"
  ),
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL
) |>
  save_table(file.path(tables_dir, "tbl_example_calls.docx"))

# Station workload: one row per station. Several metrics only work with one
# scenario; % of Total is added for the count metrics only
station_table(
  df = cmp_station,
  scenario = example_scenario,
  filter_name = first_filter,
  due = due_level,
  metrics = c(
    "n_incidents",
    "n_fdz",
    "total_minutes",
    "median_minutes",
    "p90_minutes"
  ),
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = TRUE
) |>
  save_table(file.path(tables_dir, "tbl_example_station.docx"))

# FDZ: one row per zone, here only the zones whose responder changes (all
# zones would be hundreds of rows). Several metrics only with one scenario
example_fdz <- cmp_fdz[
  cmp_fdz$filter_name == first_filter &
    cmp_fdz$due == due_level &
    cmp_fdz$scenario_name == example_scenario &
    cmp_fdz$responder_changed,
]

fdz_table(
  df = example_fdz,
  scenario = example_scenario,
  filter_name = first_filter,
  due = due_level,
  metrics = c(
    "n_calls",
    "mean_minutes",
    "total_minutes",
    "median_minutes",
    "p90_minutes",
    "pct_within_8m58s", # 8:58, EMS High First Unit
    "pct_within_10m21s" # 10:21, EMS High ERF
  ),
  baseline_name = baseline_name,
  title = NULL,
  subtitle = NULL,
  pct_total = FALSE
) |>
  save_table(file.path(tables_dir, "tbl_example_fdz.docx"))


# Auto Generate all Tables ----------------------------------------------------

for (filter in unique(cmp_overall$filter_name)) {
  filter_dir <- file.path(tables_dir, file_slug(filter))
  dir.create(filter_dir, recursive = TRUE, showWarnings = FALSE)

  write_table <- function(ft, name) {
    save_table(ft, file.path(filter_dir, paste0(name, ".", table_format)))
  }

  top <- top_scenarios
  if (is.null(top)) {
    ranked <- rank_scenarios(
      cmp_overall,
      by = rank_by,
      filter_name = filter,
      due = due_level
    )
    top <- utils::head(ranked, n_top)
  }
  message(filter, ": top scenarios ", paste(top, collapse = ", "))

  # All scenarios ---------------------------------------------------------------

  # Every scenario's change from the baseline, best first
  scenario_summary_table(
    cmp_overall,
    calls_df = cmp_calls,
    station_df = cmp_station,
    filter_name = filter,
    due = due_level,
    sort_by = rank_by,
    baseline_name = baseline_name
  ) |>
    write_table("00_summary")

  # Top scenarios side by side ------------------------------------------------

  overall_table(
    cmp_overall,
    scenario = top,
    filter_name = filter,
    due = due_level,
    baseline_name = baseline_name
  ) |>
    write_table("01_overall_top")

  station_table(
    cmp_station,
    scenario = top,
    filter_name = filter,
    due = due_level,
    metrics = "n_incidents",
    baseline_name = baseline_name,
    pct_total = TRUE
  ) |>
    write_table("02_station_top")

  # Each top scenario ---------------------------------------------------------

  for (k in seq_along(top)) {
    scenario <- top[k]
    prefix <- paste0("top", k, "_", file_slug(scenario), "_")

    # Overall: Baseline / Scenario / Diff / % Diff (and 95% CI)
    overall_table(
      cmp_overall,
      scenario = scenario,
      filter_name = filter,
      due = due_level,
      baseline_name = baseline_name
    ) |>
      write_table(paste0(prefix, "1_overall"))

    # Call-level changes: share faster / slower, by how much
    calls_table(
      cmp_calls,
      scenario = scenario,
      filter_name = filter,
      due = due_level,
      baseline_name = baseline_name
    ) |>
      write_table(paste0(prefix, "2_calls"))

    # Station workload, with each station's % of the total
    station_table(
      cmp_station,
      scenario = scenario,
      filter_name = filter,
      due = due_level,
      metrics = c("n_incidents", "n_fdz", "total_minutes"),
      baseline_name = baseline_name,
      pct_total = TRUE
    ) |>
      write_table(paste0(prefix, "3_station"))

    # Zones whose responding station changes
    changed <- cmp_fdz[
      cmp_fdz$filter_name == filter &
        cmp_fdz$due == due_level &
        cmp_fdz$scenario_name == scenario &
        cmp_fdz$responder_changed,
    ]
    if (nrow(changed) > 0) {
      fdz_table(
        changed,
        scenario = scenario,
        filter_name = filter,
        due = due_level,
        metrics = c("median_minutes", "p90_minutes"),
        baseline_name = baseline_name,
        title = "FDZs With a New Responding Station"
      ) |>
        write_table(paste0(prefix, "4_fdz_changed"))
    } else {
      message(scenario, ": no zone changes responder, no FDZ table")
    }
  }
}

message("Tables written to ", tables_dir)
