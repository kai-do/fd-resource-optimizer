# Run the analysis pipeline, steps 03-09, in order
#
# Skips steps 00-02 (OSRM backend setup, server start, travel matrix build):
# step 03 onward only needs incident_travel_matrix.parquet (step 02's output)
# to already exist in data_outputs_dir.
#
# Each step is sourced in its own environment, so settings in one script don't
# leak into the next. The run stops at the first step that fails.
#
# Usage, from the repo root:
#   Rscript analysis/run_all.R
# or in an R session:
#   source("analysis/run_all.R")
#
# To run only some steps, set steps before sourcing, e.g. steps <- c(7, 8)

# Setup -----------------------------------------------------------------------

if (!file.exists("DESCRIPTION") || !dir.exists("analysis")) {
  stop("Run from the repo root (the folder with DESCRIPTION).", call. = FALSE)
}

cfg <- config::get(file = "inst/golem-config.yml")

matrix_file <- file.path(cfg$data_outputs_dir, "incident_travel_matrix.parquet")
if (!file.exists(matrix_file)) {
  stop(
    "Travel matrix not found: ",
    matrix_file,
    "\nBuild it with analysis/02-generate_travel_matrix.R first.",
    call. = FALSE
  )
}

all_steps <- sort(list.files("analysis", pattern = "^0[3-9]-.*\\.R$"))
step_numbers <- as.integer(substr(all_steps, 1, 2))

if (exists("steps", inherits = FALSE)) {
  all_steps <- all_steps[step_numbers %in% steps]
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
