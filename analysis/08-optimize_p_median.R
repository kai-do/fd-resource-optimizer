# 08 - Weighted p-median: where to add units around a fixed set (data step)
#
# For each of the top rescue scenarios from step 06, keeps its rescue stations open and
# picks the n_add other stations that minimize total travel time for the
# filter set's calls, each zone served by its cheapest open station: where ALS
# engines would go with those rescues in place.
#
# This is the plain p-median: no run card order and every unit always
# available. It proposes station sets; step 09 scores them on the run card
# against today's ALS engines.
#
# Inputs  (data_outputs_dir): incident_travel_matrix_fdz.parquet  (step 04)
#                             scenarios.parquet,
#                             compare_overall.parquet             (step 06)
# Outputs (data_outputs_dir): als/<rescue scenario>/p_median_solutions.parquet
#                             One row per solution, best first: rank,
#                             objective (total travel minutes with
#                             cost = "mean_minutes"), open stations and the
#                             added ones, the change from today's ALS
#                             engines, and which engines would move where
#                             (list columns). <rescue scenario> is the
#                             scenario's name as a folder, e.g. R26_16_R1.
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Rescue scenarios to place engines around: the best n_top from step 06 on
# rank_by (as in steps 07 and 10-12), or name them in rescue_scenarios
# (overrides n_top), e.g. c("R26->16 +R1", "+R1")
n_top <- 3
rank_by <- "total_minutes"
rescue_scenarios <- NULL

# How many more stations to open (ALS engines)
n_add <- 8

# Where the ALS engines are today, to compare the solutions with
# Where the ALS engines are today (8; none at a rescue station)
current_als_engines <- c(1, 2, 4, 5, 9, 12, 19, 27)

# Station-to-zone cost: "mean_minutes" (objective = total minutes),
# "median_minutes" or "p90_minutes"
cost <- "mean_minutes"

# Stations that can't take a unit, e.g. c(5, 12); NULL = none
exclude <- NULL

# The best and next-best station sets to return
n_solutions <- 5

# Which calls count. Keep in sync with step 06's filter set.
filter <- incident_filter(
  "Paramedic Calls, Last 3 Years",
  date_range = c("2023-10-01", "2026-09-30"),
  ems_capability = "PARAMEDIC"
)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

matrix_file <- file.path(
  cfg$data_outputs_dir,
  "incident_travel_matrix_fdz.parquet"
)
als_dir <- file.path(cfg$data_outputs_dir, "als")

# Read ------------------------------------------------------------------------

rescues <- pick_scenarios(
  as.data.frame(arrow::read_parquet(file.path(cfg$data_outputs_dir, "scenarios.parquet"))),
  arrow::read_parquet(file.path(cfg$data_outputs_dir, "compare_overall.parquet")),
  n_top = n_top,
  rank_by = rank_by,
  names = rescue_scenarios,
  due = 1
)
message("Rescue scenarios: ", paste(rescues$scenario_name, collapse = ", "))

# The matrix has 14M+ rows: only the travel columns and the ones the filter
# set uses
travel_matrix <- arrow::read_parquet(
  matrix_file,
  col_select = c(
    "source_id",
    "dest_id",
    "fdz_id",
    "duration_min",
    "distance_m",
    "incident_alarm_dt",
    "ems_unit_capability"
  )
)

# Travel stats for the filter set -----------------------------------------------

travel_stats <- travel_matrix |>
  filter_incidents(filter) |>
  summarize_travel_stats(by = c("source_id", "fdz_id"))

rm(travel_matrix)

# Optimize, for each rescue scenario --------------------------------------------

for (i in seq_len(nrow(rescues))) {
  fixed_stations <- sort(unlist(rescues$stations[[i]]))
  message("\n-- ", rescues$scenario_name[i], " --")

  result <- solve_p_median(
    travel_stats,
    p = length(fixed_stations) + n_add,
    cost = cost,
    include = fixed_stations,
    exclude = exclude,
    n_solutions = n_solutions
  )

  # Current ALS engines: the same objective for today's engines. With every
  # station required and p equal to their count, the solver has nothing to
  # choose and just scores the set. Engines at a fixed (rescue) station add no
  # new location, since only the first ALS unit counts here.
  current_open <- sort(union(fixed_stations, current_als_engines))
  current <- solve_p_median(
    travel_stats,
    p = length(current_open),
    cost = cost,
    include = current_open
  )

  overlap <- intersect(current_als_engines, fixed_stations)
  if (length(overlap) > 0) {
    message(
      "Current engines at fixed stations, no new location: ",
      paste(overlap, collapse = ", ")
    )
  }

  solutions <- tibble::tibble(
    rank = vapply(result$alternatives, `[[`, integer(1), "rank"),
    objective = vapply(result$alternatives, `[[`, numeric(1), "objective"),
    open_stations = lapply(result$alternatives, `[[`, "open_stations"),
    added_stations = lapply(open_stations, setdiff, fixed_stations),
    # change from today's engines: negative is less total travel time
    diff_from_current = objective - current$objective,
    pct_diff_from_current = 100 * diff_from_current / current$objective,
    # engines that would move, and where to (engines at a fixed station always
    # move, since the solutions only add new locations)
    move_from = lapply(added_stations, \(s) setdiff(current_als_engines, s)),
    move_to = lapply(added_stations, \(s) setdiff(s, current_als_engines))
  )

  # Station sets only: the objective is the optimizer's (cheapest station),
  # not the run card; step 09 scores the sets on the run card
  message("Fixed: ", paste(fixed_stations, collapse = ", "))
  message("Current engines at: ", paste(current_als_engines, collapse = ", "))
  for (k in seq_len(nrow(solutions))) {
    message(sprintf(
      "#%d  added: %s  |  move from %s to %s",
      solutions$rank[k],
      paste(solutions$added_stations[[k]], collapse = ", "),
      paste(solutions$move_from[[k]], collapse = ", "),
      paste(solutions$move_to[[k]], collapse = ", ")
    ))
  }

  out_dir <- file.path(als_dir, scenario_stub(rescues$scenario_name[i]))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(solutions, file.path(out_dir, "p_median_solutions.parquet"))
}
