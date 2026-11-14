# 13 - Tables for the ALS engine placements (output step, quick: rerun freely)
#
# For each of the top rescue scenarios from step 06: from step 09's run card scoring of
# step 08's p-median solutions, today's ALS engines as the baseline, with call
# counts as yearly averages, saved in tables/als/<rescue scenario>/:
#   tbl_als_solutions              the optimizer's solutions: stations, moves,
#                                  run card scored against today's engines
#   tbl_als_summary                as step 10's scenario summary, against
#                                  today's rescues and engines: today's
#                                  rescues alone, the COA's rescues alone,
#                                  with today's engines, with the top
#                                  p-median solutions and with step 09's
#                                  summary_engines (e.g. Call Volume), with
#                                  each row's stations in the footer
#   tbl_als_overall_solution_<k>   overall metrics, solution k vs today
#   tbl_als_station_solution_<k>   first-due calls by station
#   tbl_als_fdz_solution_<k>       zones whose first ALS station changes
#   tbl_als_territory_solution_<k> coverage of each station's territory (its
#                                  zones' calls, whoever responds)
#   tbl_als_summary_master (in tables/als/)  every COA's scenarios in one
#                                  summary, best total minutes first
#   tbl_als_scenarios              what each summary row is: description,
#                                  rescues, engines and open stations
#   tbl_als_moves_<plan>           the order to move today's engines to each
#                                  engine plan in the summary (Build-Up Plan,
#                                  P-median 1, ...), one engine at a time
#   tbl_als_build_up               the build-up order: engines added one at a
#                                  time to the rescues alone, best first, and
#                                  the county after each
#   tbl_als_build_up_territory     each of those engines in its own territory,
#                                  before and after, and who else gains
#   tbl_als_territory_calls        calls per year in each territory and its
#                                  share (the same in every solution)
# Every table's footer names the rescue scenario.
#
# Inputs  (data_outputs_dir): scenarios.parquet,
#                             compare_overall.parquet          (step 06)
#                             als/<rescue scenario>/p_median_solutions.parquet
#                                                              (step 08)
#                             als/<rescue scenario>/compare_{overall,calls,
#                               fdz,station,territory}.parquet,
#                               scenarios.parquet, filter_years.parquet,
#                               build_up_steps.parquet,
#                               build_up_territory.parquet,
#                               summary/compare_{overall,calls,
#                               station}.parquet,
#                               summary/scenarios.parquet      (step 09)
# Outputs (data_outputs_dir): tables/als/<rescue scenario>/*.docx
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Rescue scenarios, as steps 08 and 09: the best n_top from step 06 on
# rank_by (as in steps 07 and 10-12), or name them in rescue_scenarios
# (overrides n_top), e.g. c("R26->16 +R1", "+R1")
n_top <- 3
rank_by <- "total_minutes"
rescue_scenarios <- NULL

# Due level to show (step 09's due_level)
due_level <- 1

# Where the ALS engines are today (keep in sync with steps 08 and 09)
current_als_engines <- c(1, 2, 4, 5, 9, 12, 19, 27)

# Solutions (step 08 ranks) that get overall, station, FDZ and territory
# tables
detail_solutions <- 1

# Metrics for each table type
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

# By territory: the calls in each station's zones, whoever responds
territory_metrics <- c(
  "n_calls", # the same in baseline and scenario: one count and its % of total
  "mean_minutes",
  "p90_minutes"
)

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

als_dir <- file.path(cfg$data_outputs_dir, "als")

rescues <- pick_scenarios(
  as.data.frame(arrow::read_parquet(file.path(cfg$data_outputs_dir, "scenarios.parquet"))),
  arrow::read_parquet(file.path(cfg$data_outputs_dir, "compare_overall.parquet")),
  n_top = n_top,
  rank_by = rank_by,
  names = rescue_scenarios,
  due = 1
)
message("Rescue scenarios: ", paste(rescues$scenario_name, collapse = ", "))
# only the ones step 09 has scored
scored <- file.exists(file.path(als_dir, scenario_stub(rescues$scenario_name), "build_up_steps.parquet")) &
  file.exists(file.path(als_dir, scenario_stub(rescues$scenario_name), "summary", "scenarios.parquet"))
if (any(!scored)) {
  message("Not scored by step 09, skipped: ", paste(rescues$scenario_name[!scored], collapse = ", "))
}
rescues <- rescues[scored, ]

# Tables, for each rescue scenario ----------------------------------------------

make_als_tables <- function(rescue_name, rescue_stations, coa_label) {
  in_dir <- file.path(als_dir, scenario_stub(rescue_name))
  tables_dir <- file.path(cfg$data_outputs_dir, "tables", "als", scenario_stub(rescue_name))
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

  read_als <- function(file) {
    as.data.frame(arrow::read_parquet(file.path(in_dir, file)))
  }

  # Every table's footer says which rescues it assumes
  rescue_note <- paste0(
    "Rescue scenario: ", rescue_name, " (rescues at ",
    paste(sort(rescue_stations), collapse = ", "), ")."
  )
  save <- function(ft, file) {
    ft <- flextable::add_footer_lines(ft, rescue_note)
    ft <- flextable::fontsize(ft, size = 8, part = "footer")
    ft <- flextable::font(ft, fontname = output_font, part = "footer")
    save_table(ft, file.path(tables_dir, file))
  }

  solutions <- read_als("p_median_solutions.parquet")
  scenarios <- read_als("scenarios.parquet")
  years <- read_als("filter_years.parquet")
  baseline_name <- scenarios$scenario_name[scenarios$baseline]

  # Call counts as yearly averages, as in step 10
  cmp_overall <- read_als("compare_overall.parquet") |> annualize_counts(years)
  cmp_calls <- read_als("compare_calls.parquet") |> annualize_counts(years)
  cmp_fdz <- read_als("compare_fdz.parquet") |> annualize_counts(years)
  cmp_station <- read_als("compare_station.parquet") |> annualize_counts(years)
  cmp_territory <- read_als("compare_territory.parquet") |>
    annualize_counts(years)

  # The filter set step 09 used
  filter_name <- unique(cmp_overall$filter_name)[1]

  # The optimizer's solutions, run card scored against today's engines
  p_median_solutions_table(
    solutions,
    cmp_overall,
    metrics = c("p90_minutes", "pct_within_8m58s", "pct_within_10m21s"),
    time_format = "mmss"
  ) |>
    save("tbl_als_solutions.docx")

  # Build-up order: engines added one at a time to the rescues alone, each
  # where it saves the most travel minutes with the ones above in place
  build_up_steps <- read_als("build_up_steps.parquet")
  build_up_territory <- read_als("build_up_territory.parquet")

  # Summary, as step 10's but against today's rescues and engines, best total
  # minutes first: step 09's plans, named by the methods that found them
  summary_scenarios <- read_als("summary/scenarios.parquet")
  plans <- read_als("summary/plans.parquet")
  plan_row <- match(summary_scenarios$scenario_name, paste(coa_label, "+", plans$plan))

  # Names: the rescue scenario, then the engine method's code in parentheses
  # (GM=GB=PM1, ...). P-medians are numbered by run card rank, from each
  # plan's run card total (its last move, which reaches the plan)
  move_steps <- read_als("summary/move_steps.parquet")
  plan_totals <- vapply(plans$plan, function(pl) {
    rows <- move_steps[move_steps$plan == pl, ]
    rows$total_minutes[which.max(rows$step)]
  }, numeric(1))
  plan_codes <- als_plan_codes(plans$methods, plan_totals)
  new_names <- als_scenario_names(rescue_name, plan_codes)
  old_names <- summary_scenarios$scenario_name
  stopifnot(length(new_names) == length(old_names))
  rename_scenarios <- function(df) {
    df$scenario_name <- new_names[match(df$scenario_name, old_names)]
    df
  }
  summary_scenarios$scenario_name <- new_names
  summary_baseline <- new_names[summary_scenarios$baseline]
  # what each row is, for the master table: the baseline, rescues alone
  # (today's and this scenario's), today's engines, or an engine plan
  summary_scenarios$kind <- c("baseline", "rescues", "rescues", "engines", rep("engines", nrow(summary_scenarios) - 4))
  # each row's engines: today's in the first and fourth rows, none in the
  # rescues-only rows, the plan's otherwise
  engines_of <- lapply(seq_len(nrow(summary_scenarios)), function(r) {
    if (r %in% c(1, 4)) sort(current_als_engines) else if (r %in% 2:3) integer() else sort(unlist(plans$engines[[plan_row[r]]]))
  })
  # Not yet annualized: the master table combines them first
  raw <- list(
    overall = read_als("summary/compare_overall.parquet") |> rename_scenarios(),
    calls = read_als("summary/compare_calls.parquet") |> rename_scenarios(),
    station = read_als("summary/compare_station.parquet") |> rename_scenarios()
  )
  sum_overall <- annualize_counts(raw$overall, years)
  sum_calls <- annualize_counts(raw$calls, years)
  sum_station <- annualize_counts(raw$station, years)

  # Each row's stations, in the footer
  station_lines <- vapply(seq_len(nrow(summary_scenarios)), function(r) {
    open <- sort(unlist(summary_scenarios$stations[[r]]))
    text <- if (r %in% 2:3) {
      paste("rescues at", paste(open, collapse = ", "))
    } else {
      paste("engines at", paste(engines_of[[r]], collapse = ", "))
    }
    if (r == 4 && length(intersect(current_als_engines, rescue_stations)) > 0) {
      text <- paste0(
        text, " (", paste(intersect(current_als_engines, rescue_stations), collapse = ", "),
        " at a rescue station: no added location)"
      )
    }
    paste0(summary_scenarios$scenario_name[r], ": ", text)
  }, character(1))

  scenario_summary_table(
    overall_df = sum_overall,
    calls_df = sum_calls,
    station_df = sum_station,
    scenario = summary_scenarios$scenario_name[!summary_scenarios$baseline],
    filter_name = filter_name,
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
    sort_by = "total_minutes", # best first, after the baseline row
    baseline_name = summary_baseline,
    title = paste0("ALS Scenario Summary: ", coa_label, " (", rescue_name, ")"),
    subtitle = NULL,
    time_format = "mmss"
  ) |>
    flextable::add_footer_lines(station_lines) |>
    save("tbl_als_summary.docx")

  # What each scenario is, in the summary's own order
  baseline_rescue_stations <- sort(unlist(summary_scenarios$stations[[2]]))
  method_text <- c(
    "Greedy Moves" = "today's engines moved one at a time, each to the empty station that saves the most minutes, until no move helps",
    "Greedy Build-Up" = "engines added one at a time to the rescues alone, each where it saves the most minutes",
    "Call Volume" = "engines at the busiest stations by call volume"
  )
  describe_plan <- function(methods) {
    parts <- vapply(methods, function(m) {
      if (startsWith(m, "P-median #")) {
        paste0("weighted p-median solution ", sub("P-median ", "", m), " (optimizer's order)")
      } else {
        method_text[[m]] %||% m
      }
    }, character(1))
    paste0("This option's rescues with ", paste(parts, collapse = "; also "))
  }
  describe <- c(
    "Today's rescues and today's ALS engines",
    "Today's rescues alone",
    paste0("This option's rescues alone (", rescue_name, ")"),
    "This option's rescues with today's ALS engines",
    vapply(seq_len(nrow(summary_scenarios))[-(1:4)], function(r) describe_plan(unlist(plans$methods[[plan_row[r]]])), character(1))
  )
  collapse <- \(x) if (length(x) == 0) "None" else paste(sort(x), collapse = ", ")
  explain <- data.frame(
    Scenario = summary_scenarios$scenario_name,
    Description = describe,
    Rescues = vapply(seq_len(nrow(summary_scenarios)), function(r) {
      collapse(if (r <= 2) baseline_rescue_stations else rescue_stations)
    }, character(1)),
    Engines = vapply(engines_of, collapse, character(1)),
    `Open Stations` = lengths(summary_scenarios$stations),
    check.names = FALSE
  )
  flextable::flextable(explain) |>
    flextable::set_caption(paste0("ALS Scenarios: ", coa_label, " (", rescue_name, ")")) |>
    flextable::font(fontname = output_font, part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(j = "Open Stations", align = "center", part = "all") |>
    flextable::width(j = "Description", width = 2.6) |>
    flextable::width(j = c("Scenario", "Rescues", "Engines"), width = 1.3) |>
    flextable::width(j = "Open Stations", width = 0.7) |>
    flextable::add_footer_lines(
      "Scores are on the run card. An engine at a rescue station adds no location; no plan puts one there."
    ) |>
    save("tbl_als_scenarios.docx")

  # Move order to each engine plan: from today's engines, one engine moved at
  # a time, ending at the plan
  for (k in seq_len(nrow(plans))) {
    plan_name <- new_names[4 + k]
    build_up_table(
      move_steps[move_steps$plan == plans$plan[k], ],
      years = years$years[1],
      pct_metrics = c("pct_within_8m58s", "pct_within_10m21s"),
      base_label = "Today's engines",
      title = paste0("Engine Moves to ", plan_name),
      subtitle = paste(
        filter_name,
        paste("Start: today's engines at", paste(sort(current_als_engines), collapse = ", ")),
        sep = " · "
      ),
      time_format = "mmss"
    ) |>
      save(paste0("tbl_als_moves_", scenario_stub(rescue_name), "__", scenario_stub(plan_codes[k]), ".docx"))
  }

  build_up_table(
    build_up_steps,
    years = years$years[1],
    pct_metrics = c("pct_within_8m58s", "pct_within_10m21s"),
    base_label = "Rescues only",
    title = "ALS Engine Build-Up Order",
    time_format = "mmss"
  ) |>
    save("tbl_als_build_up.docx")

  build_up_territory_table(
    build_up_steps,
    build_up_territory,
    years = years$years[1],
    pct_metric = "pct_within_8m58s", # 8:58, EMS High First Unit
    base_label = "Rescues only",
    title = "Each ALS Engine in Its Own Territory",
    time_format = "mmss"
  ) |>
    save("tbl_als_build_up_territory.docx")

  # Calls per year in each territory, with its share of the total: the same
  # for today's engines and every solution
  territory_table(
    df = cmp_territory,
    scenario = NULL, # any solution: the counts are the same
    filter_name = filter_name,
    due = due_level,
    metrics = "n_calls",
    baseline_name = baseline_name,
    title = "Calls per Year by Station Territory",
    subtitle = paste(filter_name, "Same in every solution", sep = " · "),
    pct_total = TRUE
  ) |>
    save("tbl_als_territory_calls.docx")

  # Zones whose first ALS station changes in a scenario
  changed_fdz <- function(scenario) {
    cmp_fdz[
      cmp_fdz$filter_name == filter_name &
        cmp_fdz$due == due_level &
        cmp_fdz$scenario_name == scenario &
        cmp_fdz$responder_changed,
    ]
  }

  for (k in detail_solutions) {
    scenario <- paste0("P-median #", k)

    overall_table(
      df = cmp_overall,
      scenario = scenario,
      filter_name = filter_name,
      due = due_level,
      metrics = overall_metrics,
      baseline_name = baseline_name,
      title = NULL,
      subtitle = NULL,
      time_format = "mmss"
    ) |>
      save(paste0("tbl_als_overall_solution_", k, ".docx"))

    station_table(
      df = cmp_station,
      scenario = scenario,
      filter_name = filter_name,
      due = due_level,
      metrics = station_metrics,
      baseline_name = baseline_name,
      title = NULL,
      subtitle = NULL,
      pct_total = TRUE,
      time_format = "mmss"
    ) |>
      save(paste0("tbl_als_station_solution_", k, ".docx"))

    fdz_table(
      df = changed_fdz(scenario),
      scenario = scenario,
      filter_name = filter_name,
      due = due_level,
      metrics = fdz_metrics,
      baseline_name = baseline_name,
      title = NULL,
      subtitle = NULL,
      pct_total = FALSE
    ) |>
      save(paste0("tbl_als_fdz_solution_", k, ".docx"))

    territory_table(
      df = cmp_territory,
      scenario = scenario,
      filter_name = filter_name,
      due = due_level,
      metrics = territory_metrics,
      baseline_name = baseline_name,
      title = NULL,
      subtitle = NULL,
      pct_total = TRUE,
      time_format = "mmss"
    ) |>
      save(paste0("tbl_als_territory_solution_", k, ".docx"))
  }

  message("Tables written to ", tables_dir)

  # For the master table
  invisible(list(
    raw = raw,
    years = years,
    filter_name = filter_name,
    baseline_name = summary_baseline,
    scenarios = summary_scenarios,
    plan_codes = plan_codes,
    engines_of = engines_of,
    rescue_name = rescue_name,
    coa_label = coa_label,
    rescue_stations = rescue_stations
  ))
}

coas <- lapply(seq_len(nrow(rescues)), function(i) {
  make_als_tables(rescues$scenario_name[i], unlist(rescues$stations[[i]]), coa_label = paste("COA", i))
})

# Master table: every COA's scenarios in one summary, against today's rescues
# and engines (the same baseline and scoring in each COA's comparison). The
# rows every COA shares, today's rescues alone, appear once.
combine <- function(part) {
  out <- dplyr::bind_rows(lapply(seq_along(coas), function(k) {
    df <- coas[[k]]$raw[[part]]
    if (k > 1) df <- df[df$scenario_name != "Baseline Rescues", ]
    df
  }))
  annualize_counts(out, coas[[1]]$years)
}

master_scenarios <- dplyr::bind_rows(lapply(seq_along(coas), function(k) {
  sc <- coas[[k]]$scenarios
  sc$coa <- coas[[k]]$coa_label
  sc$engines <- coas[[k]]$engines_of
  sc[if (k > 1) sc$scenario_name != "Baseline Rescues" & !sc$baseline else TRUE, ]
}))
master_rows <- master_scenarios$scenario_name[!master_scenarios$baseline]

master_lines <- c(
  vapply(coas, \(x) paste0(x$coa_label, ": ", x$rescue_name, " (rescues at ", paste(sort(x$rescue_stations), collapse = ", "), ")"), character(1)),
  vapply(seq_len(nrow(master_scenarios))[!master_scenarios$baseline], function(r) {
    paste0(
      master_scenarios$scenario_name[r], ": ",
      if (master_scenarios$kind[r] == "rescues") {
        paste("rescues at", paste(sort(unlist(master_scenarios$stations[[r]])), collapse = ", "))
      } else {
        paste("engines at", paste(master_scenarios$engines[[r]], collapse = ", "))
      }
    )
  }, character(1))
)

master_table <- scenario_summary_table(
  overall_df = combine("overall"),
  calls_df = combine("calls"),
  station_df = combine("station"),
  scenario = master_rows,
  filter_name = coas[[1]]$filter_name,
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
  sort_by = "total_minutes", # best first, after the baseline row
  baseline_name = coas[[1]]$baseline_name,
  title = "ALS Scenario Summary: All Rescue Options",
  subtitle = NULL,
  time_format = "mmss"
)
master_table <- flextable::add_footer_lines(master_table, master_lines)
master_table <- flextable::fontsize(master_table, size = 8, part = "footer")
master_table <- flextable::font(master_table, fontname = output_font, part = "footer")
master_file <- file.path(cfg$data_outputs_dir, "tables", "als", "tbl_als_summary_master.docx")
save_table(master_table, master_file)
message("Master table written to ", master_file)
