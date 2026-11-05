# 09 - FDZ maps for the analysis
#
# Maps of the baseline and the top scenarios, each as a static image (for
# reports) and an interactive web page (for exploring):
#   change        scenario minus baseline in a travel-time metric, per zone
#   territory     1st-due station per zone, baseline and scenario side by side
#   benchmark     % of calls reached within each benchmark, per zone
#   demand        calls per year per zone (once, not per scenario)
#
# The top scenarios are the best n_top on rank_by (as in steps 07 and 08), or
# the ones you name in top_scenarios.
#
# Inputs  (data_inputs_dir):  <date>_fdzs.csv, <date>_station_addresses.csv
#         (data_outputs_dir): compare_fdz.parquet, compare_overall.parquet,
#                             scenarios.parquet, filter_years.parquet (step 06)
# Outputs (data_outputs_dir): maps/*.png, maps/*.html (each .html with a
#                             *_files folder next to it; keep them together)
#
# Static maps download street tiles (Esri light gray) the first time; set
# tiles to FALSE to draw without them. Only tile requests leave the network,
# no call data.
#
# Run from the repo root.

pkgload::load_all()

# Settings --------------------------------------------------------------------

n_top <- 3
rank_by <- "total_minutes"
top_scenarios <- NULL # or e.g. c("R26->16 +R1", "+R1")

due_level <- 1
filter_set <- NULL # NULL = the first filter set

# Metric for the change map: "p90_minutes", "median_minutes", or a share
# such as "pct_within_8m58s"
change_metric <- "p90_minutes"

# Benchmarks for the benchmark maps: thresholds step 06 was run with
# (m:ss as min + sec / 60)
benchmarks <- c(
  "8m58s" = 8 + 58 / 60, # EMS High First Unit
  "10m21s" = 10 + 21 / 60 # EMS High ERF
)

time_format <- "decimal" # or "mmss"
tiles <- TRUE

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

fdz_file <- file.path(cfg$data_inputs_dir, "2026-10-14_fdzs.csv")
station_file <- file.path(cfg$data_inputs_dir, "2026-10-13_station_addresses.csv")

maps_dir <- file.path(cfg$data_outputs_dir, "maps")
dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)

# Read ------------------------------------------------------------------------

read_output <- function(file) {
  arrow::read_parquet(file.path(cfg$data_outputs_dir, file))
}

fdz_compare <- read_output("compare_fdz.parquet")
scenarios <- read_output("scenarios.parquet")
years <- read_output("filter_years.parquet")

fdz_sf <- data.table::fread(fdz_file, na.strings = "NULL", data.table = FALSE) |>
  prep_fdz_polygons()
stations <- data.table::fread(station_file, data.table = FALSE) |>
  prep_stations(keep_dept = "Cobb", exclude_abbrev = "FHQ")

if (is.null(filter_set)) {
  filter_set <- unique(fdz_compare$filter_name)[1]
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

# File names: the scenario name with anything but letters and digits as "_"
file_stub <- function(name) gsub("[^A-Za-z0-9]+", "_", name)

# Save each map as .png and .html
save_both <- function(make_map, name, width = 8, height = 8) {
  make_map(interactive = FALSE) |>
    save_map(file.path(maps_dir, paste0(name, ".png")), width = width, height = height)
  make_map(interactive = TRUE) |>
    save_map(file.path(maps_dir, paste0(name, ".html")))
}

# Maps ------------------------------------------------------------------------

for (scenario in top) {
  stub <- file_stub(scenario)

  # Change in one metric, scenario minus baseline. Every argument spelled out
  # once here
  save_both(
    function(interactive) {
      fdz_change_map(
        fdz_compare,
        fdz_sf,
        stations,
        scenarios,
        scenario = scenario, # name or id
        metric = change_metric,
        filter_name = filter_set,
        due = due_level,
        time_format = time_format,
        interactive = interactive, # TRUE = leaflet web page
        tiles = tiles, # street tiles under static maps
        title = NULL, # NULL = automatic
        subtitle = NULL
      )
    },
    paste0("change_", stub)
  )

  # 1st-due territories, baseline and scenario side by side
  save_both(
    function(interactive) {
      fdz_territory_map(
        fdz_compare, fdz_sf, stations, scenarios,
        scenario = scenario,
        filter_name = filter_set,
        due = due_level,
        interactive = interactive,
        tiles = tiles
      )
    },
    paste0("territory_", stub),
    width = 11,
    height = 7
  )

  # % within each benchmark, baseline and scenario side by side
  for (b in names(benchmarks)) {
    save_both(
      function(interactive) {
        fdz_benchmark_map(
          fdz_compare, fdz_sf, stations, scenarios,
          scenario = scenario,
          threshold = benchmarks[[b]],
          filter_name = filter_set,
          due = due_level,
          interactive = interactive,
          tiles = tiles
        )
      },
      paste0("benchmark_", b, "_", stub),
      width = 11,
      height = 7
    )
  }
}

# Calls per year, with the baseline's units marked
save_both(
  function(interactive) {
    fdz_demand_map(
      fdz_compare, fdz_sf, stations, scenarios,
      scenario = NULL, # or a scenario, to mark its unit moves
      years = years,
      filter_name = filter_set,
      due = due_level,
      interactive = interactive,
      tiles = tiles
    )
  },
  "demand"
)

message("Maps written to ", maps_dir)
