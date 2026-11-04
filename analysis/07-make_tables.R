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
# The top scenarios are the best n_top on rank_by, or the ones you name in
# top_scenarios. All tables are written as Word tables (.docx) or images
# (.png), set by table_format.
#
# Inputs  (data_outputs_dir): compare_overall.parquet, compare_calls.parquet,
#                             compare_fdz.parquet, compare_station.parquet,
#                             scenarios.parquet, filter_years.parquet
#                             (step 06)
# Outputs (data_outputs_dir): tables/<filter set>/*.docx or *.png
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

stopifnot(table_format %in% c("docx", "png"))

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

# Tables ----------------------------------------------------------------------

# Text safe for file and folder names: "R26->16 +R1" -> "R26_16_R1"
file_slug <- function(x) {
  gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", x))
}

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
