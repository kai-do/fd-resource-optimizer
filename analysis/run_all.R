# Run the analysis pipeline, steps 03-13, in order
#
# Data steps (03-09) are slow: they build the zone-matched matrix, score the
# scenarios on the run card, bootstrap the scenario pairs, run the p-median
# and score the ALS placements. Output steps (10-13) only read the data steps'
# saved results and write tables, plots and maps, in seconds: rerun them
# freely after changing a table, plot or map setting.
#
# Skips steps 00-02 (OSRM backend setup, server start, travel matrix build):
# step 03 onward only needs incident_travel_matrix.parquet (step 02's output)
# to already exist in data_outputs_dir.
#
# Each step is sourced in its own environment, so settings in one script don't
# leak into the next. The run stops at the first step that fails.
#
# Usage, from the repo root:
#   Rscript analysis/run_all.R          # every step
#   Rscript analysis/run_all.R outputs  # only the tables, plots and maps
# or in an R session:
#   stage <- "outputs" # "data", "outputs" or "all" (the default)
#   source("analysis/run_all.R")
#
# To run only some steps, set steps before sourcing, e.g. steps <- c(10, 11);
# steps overrides stage.

# Setup -----------------------------------------------------------------------

if (!file.exists("DESCRIPTION") || !dir.exists("analysis")) {
  stop("Run from the repo root (the folder with DESCRIPTION).", call. = FALSE)
}

cfg <- config::get(file = "inst/golem-config.yml")

stages <- list(data = 3:9, outputs = 10:13, all = 3:13)

if (!exists("stage", inherits = FALSE)) {
  args <- commandArgs(trailingOnly = TRUE)
  stage <- if (length(args) > 0) args[1] else "all"
}
if (!stage %in% names(stages)) {
  stop("stage must be one of: ", paste(names(stages), collapse = ", "), call. = FALSE)
}

all_steps <- sort(list.files("analysis", pattern = "^[0-9]{2}-.*\\.R$"))
step_numbers <- as.integer(substr(all_steps, 1, 2))

run_numbers <- if (exists("steps", inherits = FALSE)) steps else stages[[stage]]
keep <- step_numbers %in% run_numbers & step_numbers >= 3
all_steps <- all_steps[keep]
step_numbers <- step_numbers[keep]

matrix_file <- file.path(cfg$data_outputs_dir, "incident_travel_matrix.parquet")
if (any(step_numbers <= 9) && !file.exists(matrix_file)) {
  stop(
    "Travel matrix not found: ",
    matrix_file,
    "\nBuild it with analysis/02-generate_travel_matrix.R first.",
    call. = FALSE
  )
}

# Run -------------------------------------------------------------------------

timings <- list()

for (script in all_steps) {
  message("\n==== ", script, " ====")
  start <- Sys.time()
  source(file.path("analysis", script), local = new.env(), echo = FALSE)
  timings[[script]] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
}

message("\n==== Done ====")
for (script in names(timings)) {
  message(sprintf("%-32s %7.1f s", script, timings[[script]]))
}
