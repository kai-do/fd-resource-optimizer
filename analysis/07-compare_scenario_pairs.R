# 07 - Scenario vs scenario comparison (data step, slow: a few minutes)
#
# The top scenarios against each other and against every other group, the
# baseline included (first minus second, a top scenario always first), with
# simultaneous bootstrap intervals. A diff is colored only when its interval
# excludes 0; otherwise the two can't be told apart on these calls. Each
# interval is the range the difference would fall in over a single future
# year with the same pattern of calls (calls resampled one by one, as step
# 06's overall intervals). They are simultaneous: per metric, all of them hold
# together 95% of the time, so the many pairs don't turn up chance
# differences.
#
# Rerun only after step 06 changes; the plots (step 11) read the saved result.
#
# Inputs  (data_outputs_dir): paired_calls.parquet, compare_overall.parquet,
#                             scenarios.parquet, filter_years.parquet (step 06)
# Outputs (data_outputs_dir): compare_pairs.parquet  (for step 11's plots)
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

# Due level (step 06's due_levels)
due_level <- 1

# Metrics: mean_minutes (ranks like total_minutes), median_minutes,
# p90_minutes, mean_distance_m, pct_within_<threshold> (step 06's thresholds)
pair_metrics <- c(
  "mean_minutes",
  "p90_minutes",
  "pct_within_8m58s", # 8:58, EMS High First Unit
  "pct_within_10m21s" # 10:21, EMS High ERF
)

# Worker processes for the bootstrap (~10 s to start). The intervals are the
# same for any number; 1 runs it in this process. Up to the logical cores
# (parallel::detectCores()) helps a little more.
n_cores <- parallel::detectCores(logical = FALSE)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

# Read ------------------------------------------------------------------------

read_output <- function(file) {
  arrow::read_parquet(file.path(cfg$data_outputs_dir, file))
}

scenarios <- read_output("scenarios.parquet")
years <- read_output("filter_years.parquet")
paired <- read_output("paired_calls.parquet")
cmp_overall <- read_output("compare_overall.parquet")
baseline_name <- scenarios$scenario_name[scenarios$baseline]
first_filter <- unique(cmp_overall$filter_name)[1]

# Compare ---------------------------------------------------------------------

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
  metrics = pair_metrics,
  filter_name = first_filter,
  due = due_level,
  baseline_name = baseline_name,
  n_boot = 5000, # simultaneous intervals sit far in the tails: thousands of resamples
  level = 0.95,
  simultaneous = TRUE, # FALSE = each pair's own 95% interval
  resample = "call", # or "zone": whole FDZs
  predict_years = 1, # NULL = CIs of the long-run differences
  data_years = years, # how many years each filter set's calls cover
  n_cores = n_cores
)

# Write -----------------------------------------------------------------------

arrow::write_parquet(
  cmp_pairs,
  file.path(cfg$data_outputs_dir, "compare_pairs.parquet")
)

message("Scenario pairs written to compare_pairs.parquet")
