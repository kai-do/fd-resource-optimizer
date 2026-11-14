# 14 - Engine transition charts for the ALS plans (output step, quick: rerun
# freely)
#
# For each of the top rescue scenarios from step 06: one panel per engine plan
# (Greedy Moves, Greedy Build-Up, the top p-median solutions, Call Volume),
# each showing today's engines, then every engine move in order, with the
# county's run card value after each move. They show which order to make the
# moves in when they can't all be made at once. Saved in
# plots/als/<rescue scenario>/:
#   moves_total_minutes.png    total travel minutes per year
#   moves_p90_minutes.png      90th percentile minutes
#   moves_pct_within_8m58s.png calls within 8:58 (EMS High First Unit)
#   moves_pct_within_10m21s.png calls within 10:21 (EMS High ERF)
#
# Inputs  (data_outputs_dir): scenarios.parquet,
#                             compare_overall.parquet          (step 06)
#                             als/<rescue scenario>/summary/move_steps.parquet,
#                               plans.parquet, ../filter_years.parquet (step 09)
# Outputs (data_outputs_dir): plots/als/<rescue scenario>/*.png
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

# Rescue scenarios, as steps 08, 09 and 13: the best n_top from step 06 on
# rank_by, or name them in rescue_scenarios (overrides n_top)
n_top <- 3
rank_by <- "total_minutes"
rescue_scenarios <- NULL

# Run card metrics to chart, one image each
metrics <- c(
  "total_minutes",
  "p90_minutes",
  "pct_within_8m58s", # 8:58, EMS High First Unit
  "pct_within_10m21s" # 10:21, EMS High ERF
)

# Panels per row, and the image size in inches
ncol <- 2
width <- 10
height <- 10

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

# Charts, for each rescue scenario --------------------------------------------

for (i in seq_len(nrow(rescues))) {
  rescue_name <- rescues$scenario_name[i]
  in_dir <- file.path(als_dir, scenario_stub(rescue_name))
  steps_file <- file.path(in_dir, "summary", "move_steps.parquet")
  if (!file.exists(steps_file)) {
    message("Not scored by step 09, skipped: ", rescue_name)
    next
  }
  steps <- as.data.frame(arrow::read_parquet(steps_file))
  plans <- as.data.frame(arrow::read_parquet(file.path(in_dir, "summary", "plans.parquet")))

  # Panel names as in step 13's tables: the rescue scenario, then the engine
  # method's code in parentheses; p-medians numbered by run card rank, from
  # each plan's run card total (its last move, which reaches the plan)
  plan_totals <- vapply(plans$plan, function(pl) {
    rows <- steps[steps$plan == pl, ]
    rows$total_minutes[which.max(rows$step)]
  }, numeric(1))
  plan_names <- als_scenario_names(rescue_name, als_plan_codes(plans$methods, plan_totals))[-(1:4)]
  steps$plan <- plan_names[match(steps$plan, plans$plan)]
  # best plan first: the panels read in order of run card total minutes
  steps$plan <- factor(steps$plan, levels = plan_names[order(plan_totals)])
  steps <- steps[order(steps$plan, steps$step), ]
  steps$plan <- as.character(steps$plan)

  years <- as.data.frame(arrow::read_parquet(file.path(in_dir, "filter_years.parquet")))
  plots_dir <- file.path(cfg$data_outputs_dir, "plots", "als", scenario_stub(rescue_name))
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  coa_label <- paste("COA", i)
  rescue_stations <- paste(sort(unlist(rescues$stations[[i]])), collapse = ", ")

  for (m in metrics) {
    move_plan_plot(
      steps,
      metric = m,
      years = years$years[1],
      title = paste0("ALS Engine Transition Plans: ", coa_label, " (", rescue_name, ")"),
      subtitle = paste0(
        steps$filter_name[1], " · rescues at ", rescue_stations,
        " · each panel: today's engines, then each move in order"
      ),
      ncol = ncol,
      time_format = "mmss"
    ) |>
      save_plot(file.path(plots_dir, paste0("moves_", m, ".png")), width = width, height = height)
  }
  message("Charts written to ", plots_dir)
}
