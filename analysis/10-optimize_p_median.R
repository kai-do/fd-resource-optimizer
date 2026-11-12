# 10 - Weighted p-median: where to add units around a fixed set
#
# Keeps fixed_stations open (e.g. the chosen rescue placement) and picks the
# n_add other stations that minimize total travel time for the filter set's
# calls, each zone served by its cheapest open station. Use it to propose ALS
# engine placements once the rescues are settled.
#
# This is the plain p-median: no run card order and every unit always
# available. It proposes station sets; score them like any other scenario by
# pasting a solution's open_stations into step 06's scenarios tribble and
# rerunning steps 06-09.
#
# Not part of run_all.R (steps 03-09); run it on its own.
#
# Inputs  (data_outputs_dir): incident_travel_matrix_fdz.parquet  (step 04)
# Outputs (data_outputs_dir): p_median_solutions.parquet
#                             One row per solution, best first: rank,
#                             objective (total travel minutes with
#                             cost = "mean_minutes"), open stations and the
#                             added ones, the change from today's ALS
#                             engines, and which engines would move where
#                             (list columns).
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Stations that stay open: the chosen rescue placement ("R26->16 +R1")
fixed_stations <- c(1, 11, 14, 16, 21, 23, 24, 30)

# How many more stations to open (ALS engines)
n_add <- 9

# Where the ALS engines are today, to compare the solutions with
current_als_engines <- c(1, 2, 4, 5, 9, 12, 19, 21, 27)

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
output_file <- file.path(cfg$data_outputs_dir, "p_median_solutions.parquet")

# Read ------------------------------------------------------------------------

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

# Optimize --------------------------------------------------------------------

result <- solve_p_median(
  travel_stats,
  p = length(fixed_stations) + n_add,
  cost = cost,
  include = fixed_stations,
  exclude = exclude,
  n_solutions = n_solutions
)

# Current ALS engines ---------------------------------------------------------
#
# The same objective for today's engines: with every station required and
# p equal to their count, the solver has nothing to choose and just scores
# the set. Engines at a fixed (rescue) station add no new location, since
# only the first ALS unit counts here.

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

# Solutions vs today ------------------------------------------------------------

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

message("Fixed: ", paste(fixed_stations, collapse = ", "))
message(sprintf(
  "Current engines  %s %.0f  at: %s",
  cost,
  current$objective,
  paste(current_als_engines, collapse = ", ")
))
for (k in seq_len(nrow(solutions))) {
  message(sprintf(
    "#%d  %s %.0f (%+.1f%%)  added: %s  |  move from %s to %s",
    solutions$rank[k],
    cost,
    solutions$objective[k],
    solutions$pct_diff_from_current[k],
    paste(solutions$added_stations[[k]], collapse = ", "),
    paste(solutions$move_from[[k]], collapse = ", "),
    paste(solutions$move_to[[k]], collapse = ", ")
  ))
}

# Write -----------------------------------------------------------------------

arrow::write_parquet(solutions, output_file)
