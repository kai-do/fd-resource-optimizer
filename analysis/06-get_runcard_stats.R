# 06 - Run card travel stats by scenario and filter set (data step, slow)
#
# Each scenario is a set of open stations; each filter set
# (incident_filter()) says which calls count. Every scenario runs under every
# filter set: each incident goes to its zone's run card responder at the chosen
# due levels, and stats are summarized by responding station and by FDZ. Results
# for all scenarios and filter sets sit side by side in one file each.
#
# Inputs  (data_outputs_dir): incident_travel_matrix_fdz.parquet  (step 04)
#         (data_inputs_dir):  <date>_cad_run_card.csv  (cad_run_card.sql)
# Outputs (data_outputs_dir): runcard.parquet
#                             Run card order: one row per zone and Cobb
#                             responding station, in card order.
#                             runcard_stats_station.parquet
#                             One row per filter set, scenario, due level and
#                             responding station, with n_fdz (zones served).
#                             runcard_stats_fdz.parquet
#                             One row per filter set, scenario, due level and
#                             zone.
#                             compare_overall.parquet, compare_calls.parquet,
#                             compare_fdz.parquet, compare_station.parquet,
#                             compare_territory.parquet
#                             Each scenario minus the baseline scenario, one
#                             row per metric (see R/fct_compare.R).
#                             compare_territory groups calls by the territory
#                             they are in, whoever responds.
#                             scenario_comparison.xlsx
#                             The same comparisons in one workbook, with the
#                             settings, scenarios and filter sets used.
#                             paired_calls.parquet
#                             One row per filter set, scenario (other than
#                             the baseline), call and due level: the call's
#                             zone, territory station, and its responder and
#                             minutes in the baseline and the scenario. For
#                             the scenario pairs (step 07) and the
#                             travel-time plots (step 11).
#                             scenarios.parquet
#                             The scenarios: id, name, stations, baseline.
#                             filter_years.parquet
#                             Dates and years each filter set covers, for
#                             yearly averages (steps 07 and 10-12).
#
# Run from the repo root.

pkgload::load_all()

# Config ----------------------------------------------------------------------

cfg <- config::get(file = "inst/golem-config.yml")

matrix_file <- file.path(
  cfg$data_outputs_dir,
  "incident_travel_matrix_fdz.parquet"
)
runcard_file <- file.path(cfg$data_inputs_dir, "2026-10-13_cad_run_card.csv")

runcard_output_file <- file.path(cfg$data_outputs_dir, "runcard.parquet")
station_output_file <- file.path(
  cfg$data_outputs_dir,
  "runcard_stats_station.parquet"
)
fdz_output_file <- file.path(cfg$data_outputs_dir, "runcard_stats_fdz.parquet")
comparison_output_file <- file.path(
  cfg$data_outputs_dir,
  "scenario_comparison.xlsx"
)
scenarios_output_file <- file.path(cfg$data_outputs_dir, "scenarios.parquet")
years_output_file <- file.path(cfg$data_outputs_dir, "filter_years.parquet")

# Read ------------------------------------------------------------------------

# The matrix has 14M+ rows: only the travel columns and the ones the filter
# sets use
travel_matrix <- arrow::read_parquet(
  matrix_file,
  col_select = c(
    "source_id",
    "dest_id",
    "fdz_id",
    "station_id",
    "duration_min",
    "distance_m",
    "incident_alarm_dt",
    "rms_source_system",
    "incident_category_code",
    "incident_subcategory_code",
    "rms_incident_type_code",
    "cad_incident_type_code",
    "dispatch_code",
    "dispatch_category_code",
    "dispatch_case_type_description",
    "ems_unit_capability"
  )
)

runcard <- data.table::fread(
  runcard_file,
  na.strings = "NULL",
  data.table = FALSE
) |>
  prep_runcard()

all_stations <- sort(unique(travel_matrix$source_id))

# Scenarios -------------------------------------------------------------------

# One row per scenario: a unique id, a name, the open stations, and whether
# it is the baseline the others are compared with (exactly one TRUE).
#
# Each option moves one rescue to 16 (R26 or R11) and adds one of R1, R9 or
# R27.
baseline_rescues <- c(11, 14, 21, 23, 24, 26, 30)

# The baseline with some stations removed and others added
change <- function(remove = NULL, add = NULL) {
  sort(union(setdiff(baseline_rescues, remove), add))
}

scenarios <- tibble::tribble(
  ~scenario_id , ~scenario_name     , ~stations                            , ~baseline ,
             1 , "Baseline Rescues" , baseline_rescues                     , TRUE      ,
             2 , "R26->16 +R1"      , change(remove = 26, add = c(16, 1))  , FALSE     ,
             3 , "R26->16 +R9"      , change(remove = 26, add = c(16, 9))  , FALSE     ,
             4 , "R26->16 +R27"     , change(remove = 26, add = c(16, 27)) , FALSE     ,
             5 , "R11->16 +R1"      , change(remove = 11, add = c(16, 1))  , FALSE     ,
             6 , "R11->16 +R9"      , change(remove = 11, add = c(16, 9))  , FALSE     ,
             7 , "R11->16 +R27"     , change(remove = 11, add = c(16, 27)) , FALSE     ,
             8 , "+R1"              , change(add = 1)                      , FALSE     ,
             9 , "+R9"              , change(add = 9)                      , FALSE     ,
            10 , "+R27"             , change(add = 27)                     , FALSE     ,
)

# Filter sets -----------------------------------------------------------------

# Which calls count. Add more with incident_filter(), e.g.
#   incident_filter(
#     "Night ALS, last 3 yrs",
#     date_range = c("2023-10-01", "2026-09-30"),
#     time_range = c("22:00", "06:00"),
#     ems_capability = "PARAMEDIC"
#   )
filters <- list(
  incident_filter(
    "Paramedic Calls, Last 3 Years",
    date_range = c("2023-10-01", "2026-09-30"),
    ems_capability = "PARAMEDIC"
  )
)

# Due levels: 1 for 1st due, 1:3 for 1st to 3rd due
due_levels <- 1

# Comparison settings: travel-time thresholds (minutes), and bootstrap
# confidence intervals on the overall diffs (slower; the seed in
# export_comparison() below makes them repeatable)
# m:ss benchmarks go in as minutes + seconds / 60; their metrics are named to
# the second (pct_within_8m58s, shown as "Within 8:58 Min")
thresholds <- c(
  8,
  8 + 58 / 60, # 8:58, EMS High First Unit
  10,
  10 + 21 / 60 # 10:21, EMS High ERF
)
with_ci <- TRUE
# Intervals on the overall diffs: calls resampled one by one, as the range
# each diff would fall in over a single future year with the same pattern of
# calls (predict_years = NULL for a CI of the long-run difference instead).
# Step 07's scenario-vs-scenario comparison uses the same method.
ci_resample <- "call"
ci_predict_years <- 1
# 1000 resamples: between seeds the interval ends move under half a second
# and under 0.1 percentage points, finer than the tables show
ci_n_boot <- 1000
# Worker processes for the bootstrap (~10 s to start). The intervals are the
# same for any number; 1 runs it in this process. Up to the logical cores
# (parallel::detectCores()) helps a little more.
n_cores <- parallel::detectCores(logical = FALSE)

# Stats -----------------------------------------------------------------------

# Prints, per filter set and scenario, how many zones have no open station at
# a due level
stats_station <- scenarios |>
  stats_by_station(travel_matrix, runcard, filters = filters, due = due_levels)

stats_fdz <- scenarios |>
  stats_by_fdz(travel_matrix, runcard, filters = filters, due = due_levels)

# Comparisons -----------------------------------------------------------------

# One run card assignment shared by all the comparisons, written to Excel with
# the settings, scenarios and filter sets; the returned sheets are also saved
# as parquet below
comparison <- scenarios |>
  export_comparison(
    travel_matrix,
    runcard,
    file = comparison_output_file,
    filters = filters,
    due = due_levels,
    thresholds = thresholds,
    ci = with_ci,
    n_boot = ci_n_boot,
    resample = ci_resample, # or "zone": whole FDZs
    predict_years = ci_predict_years,
    seed = 343,
    n_cores = n_cores
  )

# Checks ----------------------------------------------------------------------

# Within each filter set, scenario and due level: every zone once, and the
# station view covers the same incidents and zones as the zone view
keys <- c("filter_name", "scenario_id", "due")

fdz_totals <- stats_fdz |>
  dplyr::summarize(
    n_incidents = sum(n_incidents),
    n_fdz = dplyr::n(),
    .by = dplyr::all_of(keys)
  )
station_totals <- stats_station |>
  dplyr::summarize(
    n_incidents = sum(n_incidents),
    n_fdz = sum(n_fdz),
    .by = dplyr::all_of(keys)
  )

stopifnot(
  !anyDuplicated(stats_fdz[c(keys, "fdz_id")]),
  isTRUE(all.equal(
    as.data.frame(fdz_totals),
    as.data.frame(station_totals),
    check.attributes = FALSE
  ))
)

# Write -----------------------------------------------------------------------

arrow::write_parquet(runcard, runcard_output_file, compression = "zstd")
arrow::write_parquet(stats_station, station_output_file, compression = "zstd")
arrow::write_parquet(stats_fdz, fdz_output_file, compression = "zstd")

compare_files <- c(
  "Overall" = "compare_overall.parquet",
  "Calls" = "compare_calls.parquet",
  "By FDZ" = "compare_fdz.parquet",
  "By Station" = "compare_station.parquet",
  "By Territory" = "compare_territory.parquet"
)
for (sheet in names(compare_files)) {
  arrow::write_parquet(
    comparison[[sheet]],
    file.path(cfg$data_outputs_dir, compare_files[[sheet]]),
    compression = "zstd"
  )
}

# The call-level times behind the comparisons, for the scenario pairs (step
# 07) and the box plots (step 11)
arrow::write_parquet(
  attr(comparison, "paired"),
  file.path(cfg$data_outputs_dir, "paired_calls.parquet"),
  compression = "zstd"
)

# The scenarios (steps 07 and 10-12 take the baseline's name from them) and
# the years each filter set covers, for yearly averages in step 10: a filter set's
# date_range, or the data's first to last date if it has none
arrow::write_parquet(scenarios, scenarios_output_file, compression = "zstd")

years <- filter_years(filters, travel_matrix)
print(years)
arrow::write_parquet(years, years_output_file, compression = "zstd")
