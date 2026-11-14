# 09 - Score the ALS engine placements on the run card (data step, slow)
#
# For each of the top rescue scenarios from step 06: scores step 08's p-median solutions
# the same way step 06 scores the rescue scenarios (each call to its zone's
# first open station on the run card), with today's ALS engines as the
# baseline. Step 13 makes the tables.
#
# Also a summary set: today's rescues and engines as the baseline, then
# today's rescues alone, that scenario's rescues alone, with today's engines, with the top
# p-median solutions and with other engine sets (summary_engines).
#
# Also the build-up order: from that scenario's rescues alone, add engines one
# at a time, each at the station that saves the most travel minutes on the run
# card with the engines already added in place.
#
# A scenario here is the rescues plus the engine stations: the first ALS unit
# is whichever of them comes first on the run card.
#
# About two minutes per rescue scenario (two bootstrapped comparisons).
#
# Inputs  (data_outputs_dir): als/<rescue scenario>/p_median_solutions.parquet
#                                                              (step 08)
#                             scenarios.parquet, runcard.parquet,
#                             compare_overall.parquet           (step 06)
#                             incident_travel_matrix_fdz.parquet  (step 04)
# Outputs (data_outputs_dir), in als/<rescue scenario>/:
#                             als_scenario_comparison.xlsx,
#                             compare_{overall,calls,fdz,station,
#                                      territory}.parquet,
#                             scenarios.parquet, filter_years.parquet
#                             (the baseline's name and the years behind the
#                             yearly averages, for step 13),
#                             build_up_steps.parquet,
#                             build_up_territory.parquet (the build-up
#                             order, per step and per territory)
#                             and in als/<rescue scenario>/summary/, for step
#                             13's summary table: als_summary_comparison.xlsx,
#                             compare_*.parquet and scenarios.parquet, scoring
#                             today's rescues and engines (the
#                             baseline), today's rescues alone, the
#                             COA's rescues alone, with today's engines, with the
#                             engine plans (Greedy Moves, Greedy Build-Up,
#                             the top p-median solutions and the
#                             summary_engines sets); plans.parquet (each
#                             plan's engines); move_steps.parquet and
#                             move_territory.parquet, the order to move
#                             today's engines to each plan
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Rescue scenarios, as step 08 (keep in sync): the best n_top from step 06 on
# rank_by (as in steps 07 and 10-12), or name them in rescue_scenarios
# (overrides n_top), e.g. c("R26->16 +R1", "+R1")
n_top <- 3
rank_by <- "total_minutes"
rescue_scenarios <- NULL

# Keep in sync with step 08
# Where the ALS engines are today (8; none at a rescue station)
current_als_engines <- c(1, 2, 4, 5, 9, 12, 19, 27)

# Keep in sync with steps 06 and 08
filter <- incident_filter(
  "Paramedic Calls, Last 3 Years",
  date_range = c("2023-10-01", "2026-09-30"),
  ems_capability = "PARAMEDIC"
)
thresholds <- c(
  8,
  8 + 58 / 60, # 8:58, EMS High First Unit
  10,
  10 + 21 / 60 # 10:21, EMS High ERF
)
due_level <- 1

# Bootstrap resamples, as step 06
ci_n_boot <- 1000
# Worker processes for the bootstrap (~10 s to start). The intervals are the
# same for any number; 1 runs it in this process. Up to the logical cores
# (parallel::detectCores()) helps a little more.
n_cores <- parallel::detectCores(logical = FALSE)

# Build-up order: how many engines to add, one at a time, to the rescues
# alone (as step 08's n_add), and stations that can't take one (as step 08's
# exclude)
build_up_n <- 8
exclude <- NULL

# Summary table (step 13), against today's rescues and engines: the top summary_n_p_median solutions, and other engine sets for
# each rescue scenario (by its name; leave one out to skip its extra rows)
summary_n_p_median <- 3
# Each must have as many engines as today's (8), none at a rescue station.
# The call volume lists, busiest first: the first 8 of the 9 given
summary_engines <- list(
  "R26->16 +R1" = list("Call Volume" = c(8, 19, 26, 9, 22, 7, 10, 28)),
  "+R1" = list("Call Volume" = c(8, 19, 9, 16, 22, 7, 10, 28)),
  "R11->16 +R1" = list("Call Volume" = c(8, 19, 9, 11, 22, 7, 10, 28))
)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

als_dir <- file.path(cfg$data_outputs_dir, "als")

# Read ------------------------------------------------------------------------

read_output <- function(file) {
  as.data.frame(arrow::read_parquet(file.path(cfg$data_outputs_dir, file)))
}

rescue_scenarios_all <- read_output("scenarios.parquet")
baseline_rescues <- unlist(rescue_scenarios_all$stations[rescue_scenarios_all$baseline])
rescues <- pick_scenarios(
  rescue_scenarios_all,
  arrow::read_parquet(file.path(cfg$data_outputs_dir, "compare_overall.parquet")),
  n_top = n_top,
  rank_by = rank_by,
  names = rescue_scenarios,
  due = 1
)
message("Rescue scenarios: ", paste(rescues$scenario_name, collapse = ", "))
runcard <- read_output("runcard.parquet")

# The matrix has 14M+ rows: only the travel columns and the ones the filter
# set uses
travel_matrix <- arrow::read_parquet(
  file.path(cfg$data_outputs_dir, "incident_travel_matrix_fdz.parquet"),
  col_select = c(
    "source_id",
    "dest_id",
    "fdz_id",
    "station_id",
    "duration_min",
    "distance_m",
    "incident_alarm_dt",
    "ems_unit_capability"
  )
)
years <- filter_years(filter, travel_matrix)

compare_files <- c(
  "Overall" = "compare_overall.parquet",
  "Calls" = "compare_calls.parquet",
  "By FDZ" = "compare_fdz.parquet",
  "By Station" = "compare_station.parquet",
  "By Territory" = "compare_territory.parquet"
)
baseline_name <- "Current ALS Engines"

# Score, for each rescue scenario ---------------------------------------------------

for (i in seq_len(nrow(rescues))) {
  rescue_name <- rescues$scenario_name[i]
  fixed_stations <- sort(unlist(rescues$stations[[i]]))
  out_dir <- file.path(als_dir, scenario_stub(rescue_name))
  message("\n-- ", rescue_name, " --")

  solutions <- as.data.frame(arrow::read_parquet(file.path(out_dir, "p_median_solutions.parquet")))

  # Every solution must keep the rescues; if not, step 08 ran with other
  # settings
  stopifnot(all(vapply(
    solutions$open_stations,
    \(s) all(fixed_stations %in% unlist(s)),
    logical(1)
  )))

  scenarios <- tibble::tibble(
    scenario_id = c(1L, solutions$rank + 1L),
    scenario_name = c(baseline_name, paste0("P-median #", solutions$rank)),
    stations = c(
      list(sort(union(fixed_stations, current_als_engines))),
      lapply(solutions$open_stations, \(s) sort(unlist(s)))
    ),
    baseline = c(TRUE, rep(FALSE, nrow(solutions)))
  ) |>
    validate_scenarios()

  # Same settings as step 06
  comparison <- scenarios |>
    export_comparison(
      travel_matrix,
      runcard,
      file = file.path(out_dir, "als_scenario_comparison.xlsx"),
      filters = filter,
      due = due_level,
      thresholds = thresholds,
      ci = TRUE,
      n_boot = ci_n_boot,
      resample = "call",
      predict_years = 1,
      seed = 343,
      n_cores = n_cores
    )
  for (sheet in names(compare_files)) {
    arrow::write_parquet(
      comparison[[sheet]],
      file.path(out_dir, compare_files[[sheet]]),
      compression = "zstd"
    )
  }

  # Build-up order: a few seconds per engine (each step scores every remaining
  # station, no intervals)
  build_up <- build_up_order(
    base_stations = fixed_stations,
    matrix_df = travel_matrix,
    runcard_df = runcard,
    filter = filter,
    n_add = build_up_n,
    exclude = exclude,
    due = due_level,
    thresholds = thresholds
  )
  message("Build-up order: ", paste(build_up$steps$station[-1], collapse = " > "))
  arrow::write_parquet(build_up$steps, file.path(out_dir, "build_up_steps.parquet"))
  arrow::write_parquet(build_up$territory, file.path(out_dir, "build_up_territory.parquet"))

  # The scenarios, for step 13's baseline name, and the years the filter set
  # covers, for its yearly averages
  arrow::write_parquet(scenarios, file.path(out_dir, "scenarios.parquet"))
  arrow::write_parquet(years, file.path(out_dir, "filter_years.parquet"))

  # Engine plans, each 8 engines away from the rescues:
  #   Greedy Moves     today's engines moved one at a time, each to the empty
  #                    station that saves the most minutes on the run card,
  #                    until no move helps (each engine moves at most once)
  #   Greedy Build-Up  the build-up order above: engines added one at a time
  #                    to the rescues alone
  #   P-median #k      step 08's top solutions
  #   summary_engines  e.g. Call Volume
  # Plans with the same stations are one row, named e.g. "Greedy Build-Up =
  # P-median #2". About a minute for the greedy moves.
  greedy_moves <- move_order(
    base_stations = fixed_stations,
    from_stations = current_als_engines,
    to_stations = NULL,
    matrix_df = travel_matrix,
    runcard_df = runcard,
    filter = filter,
    exclude = exclude,
    due = due_level,
    thresholds = thresholds
  )
  extra <- summary_engines[[rescue_name]]
  if (is.null(extra)) {
    message("No summary_engines for ", rescue_name, ": plans without them")
  }
  plan_sets <- c(
    list("Greedy Moves" = greedy_moves$units, "Greedy Build-Up" = build_up$steps$station[-1]),
    stats::setNames(
      lapply(
        utils::head(solutions$open_stations, summary_n_p_median),
        \(s) setdiff(unlist(s), fixed_stations)
      ),
      paste0("P-median #", utils::head(solutions$rank, summary_n_p_median))
    ),
    extra
  )
  for (nm in names(plan_sets)) {
    engines <- sort(unique(plan_sets[[nm]]))
    at_rescue <- intersect(engines, fixed_stations)
    if (length(engines) != length(current_als_engines) || length(at_rescue) > 0) {
      stop(
        rescue_name, " / ", nm, ": a plan needs ", length(current_als_engines),
        " engines, none at a rescue station; got ", paste(engines, collapse = ", "),
        if (length(at_rescue) > 0) paste0(" (at rescues: ", paste(at_rescue, collapse = ", "), ")"),
        call. = FALSE
      )
    }
    plan_sets[[nm]] <- engines
  }
  key <- vapply(plan_sets, paste, character(1), collapse = ",")
  plans <- tibble::tibble(
    plan = vapply(unique(key), \(k) paste(names(plan_sets)[key == k], collapse = " = "), character(1)),
    methods = lapply(unique(key), \(k) names(plan_sets)[key == k]),
    engines = lapply(unique(key), \(k) plan_sets[[match(k, key)]])
  )
  message("Plans:\n", paste0("  ", plans$plan, ": ", vapply(plans$engines, paste, character(1), collapse = ", "), collapse = "\n"))

  # Summary: today's rescues and engines (the baseline), today's rescues
  # alone, then this scenario's rescues alone, with today's engines and with
  # each plan
  summary_scenarios <- als_summary_scenarios(
    baseline_rescues = baseline_rescues,
    rescue_stations = fixed_stations,
    current_engines = current_als_engines,
    extra = stats::setNames(plans$engines, plans$plan),
    label = paste("COA", i)
  )
  summary_dir <- file.path(out_dir, "summary")
  dir.create(summary_dir, showWarnings = FALSE)

  summary_comparison <- summary_scenarios |>
    export_comparison(
      travel_matrix,
      runcard,
      file = file.path(summary_dir, "als_summary_comparison.xlsx"),
      filters = filter,
      due = due_level,
      thresholds = thresholds,
      ci = TRUE,
      n_boot = ci_n_boot,
      resample = "call",
      predict_years = 1,
      seed = 343,
      n_cores = n_cores
    )
  for (sheet in names(compare_files)) {
    arrow::write_parquet(
      summary_comparison[[sheet]],
      file.path(summary_dir, compare_files[[sheet]]),
      compression = "zstd"
    )
  }
  arrow::write_parquet(summary_scenarios, file.path(summary_dir, "scenarios.parquet"))
  arrow::write_parquet(plans, file.path(summary_dir, "plans.parquet"))

  # Move order to each plan, from this scenario's rescues with today's
  # engines, one engine at a time: Greedy Moves in its own order, the others
  # each move the one that saves the most minutes, ending at the plan
  moves <- lapply(seq_len(nrow(plans)), function(k) {
    out <- if ("Greedy Moves" %in% plans$methods[[k]]) {
      greedy_moves
    } else {
      move_order(
        base_stations = fixed_stations,
        from_stations = current_als_engines,
        to_stations = plans$engines[[k]],
        matrix_df = travel_matrix,
        runcard_df = runcard,
        filter = filter,
        due = due_level,
        thresholds = thresholds
      )
    }
    message("Moves to ", plans$plan[k], ": ", paste(out$steps$from[-1], "->", out$steps$station[-1], collapse = ", "))
    out$steps$plan <- plans$plan[k]
    out$steps$scenario_name <- paste0("COA ", i, " + ", plans$plan[k])
    out$territory$plan <- rep(plans$plan[k], nrow(out$territory))
    out
  })
  arrow::write_parquet(
    dplyr::bind_rows(lapply(moves, `[[`, "steps")),
    file.path(summary_dir, "move_steps.parquet")
  )
  arrow::write_parquet(
    dplyr::bind_rows(lapply(moves, `[[`, "territory")),
    file.path(summary_dir, "move_territory.parquet")
  )
}

message("ALS comparisons written to ", als_dir)
