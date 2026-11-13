# 08 - Travel-time box plots for the analysis
#
# Box plots of modeled travel time for the baseline and the top scenarios:
#   overall       one box per scenario
#   station       per territory station
#   fdz           per zone, for the zones you pick
# a cumulative curve (ECDF), and forest plots of step 07's scenario-vs-
# scenario comparison, with simultaneous bootstrap intervals: the top
# scenarios vs the baseline (pairs_baseline.png), and vs each other and the
# alternatives (pairs_forest.png)
#
# The top scenarios are the best n_top on rank_by (as in step 07), or the ones
# you name in top_scenarios.
#
# Inputs  (data_outputs_dir): paired_calls.parquet, compare_overall.parquet,
#                             scenarios.parquet  (step 06)
#                             compare_pairs.parquet  (step 07; run it first)
# Outputs (data_outputs_dir): plots/*.png
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

n_top <- 3
rank_by <- "total_minutes"
top_scenarios <- NULL # or e.g. c("R26->16 +R1", "+R1")

due_level <- 1
filter_set <- NULL # NULL = the first filter set

# Zones for the FDZ plot: every zone in these territories, or name them in
# plot_fdz (e.g. c("16A", "16C")) and set plot_fdz_stations to NULL
plot_fdz_stations <- 16
plot_fdz <- NULL

# "horizontal" (groups down the y axis) or "vertical"
orientation <- "horizontal"

# Group stations / zones (calls, overall) into Changed and No Change: does
# the responding station change in any of the top scenarios? And whether to
# keep the ones that don't change (their boxes equal the baseline's)
split_changed <- FALSE
include_unchanged <- TRUE

# Benchmark minutes marked on the cumulative plot; names are the labels.
# The same EMS High benchmarks as step 06's thresholds (m:ss as min + sec / 60)
benchmarks <- c(
  "8:58 First Unit" = 8 + 58 / 60,
  "10:21 ERF" = 10 + 21 / 60
)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

plots_dir <- file.path(cfg$data_outputs_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Read ------------------------------------------------------------------------

read_output <- function(file) {
  arrow::read_parquet(file.path(cfg$data_outputs_dir, file))
}

paired <- read_output("paired_calls.parquet")
scenarios <- read_output("scenarios.parquet")
baseline_name <- scenarios$scenario_name[scenarios$baseline]

if (is.null(filter_set)) {
  filter_set <- unique(paired$filter_name)[1]
}

top <- top_scenarios
if (is.null(top)) {
  ranked <- rank_scenarios(
    read_output("compare_overall.parquet"),
    by = rank_by,
    filter_name = filter_set,
    due = due_level
  )
  top <- utils::head(ranked, n_top)
}
groups <- c(baseline_name, top)

# Plots -----------------------------------------------------------------------

# Overall: one box per group. Every argument spelled out once here
travel_time_boxplot(
  paired,
  scenario = groups, # names or ids, baseline included; NULL = all
  by = "overall", # "overall", "station" or "fdz"
  stations = NULL, # territory station ids to keep; NULL = all
  fdz = NULL, # zone ids to keep; NULL = all
  orientation = orientation,
  filter_name = filter_set,
  due = due_level,
  baseline_name = baseline_name,
  split_changed = split_changed, # Changed / No Change panels
  include_unchanged = include_unchanged, # FALSE = changed calls only
  time_format = "decimal", # or "mmss"
  outliers = TRUE,
  title = NULL, # NULL = automatic
  subtitle = NULL
) |>
  save_plot(file.path(plots_dir, "boxplot_overall.png"), width = 8, height = 5)

# By territory station: a box per group within each station
travel_time_boxplot(
  paired,
  scenario = groups,
  by = "station",
  orientation = orientation,
  filter_name = filter_set,
  due = due_level,
  baseline_name = baseline_name,
  stations = c(1:28, 30),
  split_changed = TRUE,
  include_unchanged = FALSE
) |>
  save_plot(file.path(plots_dir, "boxplot_station.png"), width = 8, height = 11)

# By FDZ, for the chosen zones
travel_time_boxplot(
  paired,
  scenario = groups,
  by = "fdz",
  stations = plot_fdz_stations,
  fdz = plot_fdz,
  orientation = orientation,
  filter_name = filter_set,
  due = due_level,
  baseline_name = baseline_name,
  split_changed = split_changed,
  include_unchanged = include_unchanged
) |>
  save_plot(file.path(plots_dir, "boxplot_fdz.png"), width = 8, height = 7)

# Cumulative: % of calls reached within X minutes, one line per group
travel_time_ecdf(
  paired,
  scenario = groups,
  stations = NULL, # territory station ids to keep; NULL = all
  fdz = NULL, # zone ids to keep; NULL = all
  filter_name = filter_set,
  due = due_level,
  baseline_name = baseline_name,
  include_unchanged = include_unchanged, # FALSE = changed calls only
  benchmarks = benchmarks,
  time_format = "decimal", # or "mmss"
  max_minutes = NULL, # e.g. 20 to zoom; percentages still count every call
  curve = "line", # or "step": the exact staircase of the 0.1-min rounded times
  title = NULL,
  subtitle = NULL
) |>
  save_plot(file.path(plots_dir, "ecdf_overall.png"), width = 8, height = 5)

# Scenario vs scenario: step 07's comparison (the top scenarios against each
# other and every other group, simultaneous intervals), a dot per difference
# and a line across its interval. Set the pairs and metrics in step 07.
# Two charts: the top scenarios against the baseline (pairs_baseline.png), and
# against each other and the other scenarios (pairs_forest.png), grouped by
# top scenario
cmp_pairs <- read_output("compare_pairs.parquet")
pair_metrics <- c("mean_minutes", "p90_minutes") # NULL = every metric in cmp_pairs, one panel each
vs_baseline <- cmp_pairs$scenario_b == baseline_name
n_pairs <- function(df) nrow(unique(df[c("scenario_a", "scenario_b")]))

scenario_pairs_plot(
  cmp_pairs[vs_baseline, ],
  metrics = pair_metrics,
  title = "Top Scenarios vs Baseline",
  subtitle = NULL
) |>
  save_plot(file.path(plots_dir, "pairs_baseline.png"), width = 10, height = 2.2 + 0.45 * n_pairs(cmp_pairs[vs_baseline, ]))

scenario_pairs_plot(
  cmp_pairs[!vs_baseline, ],
  metrics = pair_metrics,
  title = "Top Scenarios vs Each Other and the Alternatives",
  subtitle = NULL,
  group_first = TRUE # a block per top scenario
) |>
  save_plot(file.path(plots_dir, "pairs_forest.png"), width = 10, height = 2.6 + 0.4 * n_pairs(cmp_pairs[!vs_baseline, ]))

message("Plots written to ", plots_dir)
