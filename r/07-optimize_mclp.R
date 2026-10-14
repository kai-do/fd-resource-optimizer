library(tidyverse)
library(arrow)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# source data for the coverage matrix and demand weights
travel_stats_df <- read_parquet('datasets/outputs/travel_stats.parquet')

# Maximal covering location problem (MCLP).
# Choose the p stations that maximize the incident demand covered within threshold_minutes.
# A station covers an fdz when its cost_col value for that fdz is <= threshold_minutes.
# cost_col defaults to the 90th percentile, so "covered" means 90% of historical trips arrive within the threshold.
# Station/fdz pairs missing from travel_stats_df are treated as not covering.
# Returns a list with the solver status, covered demand, percent of demand covered, open stations, and coverage by fdz.
solve_mclp <- function(
  travel_stats_df,
  p,
  threshold_minutes,
  cost_col = 'p90_minutes',
  solver = 'glpk'
) {
  # incidents outside any fdz have no zone to cover
  travel_stats_df <- travel_stats_df |>
    filter(!is.na(fdz))

  # number of incidents per fdz for demand weighting
  incident_demand_df <- travel_stats_df |>
    distinct(fdz, n_incidents)

  stations <- sort(unique(travel_stats_df$source_id))
  zones <- sort(unique(travel_stats_df$fdz))

  # cost matrix
  # columns are fdzs
  # rows are stations
  # each cell is the chosen travel time statistic from station to incidents in that fdz
  cost_mat <- travel_stats_df |>
    select(source_id, fdz, all_of(cost_col)) |>
    pivot_wider(names_from = fdz, values_from = all_of(cost_col)) |>
    arrange(source_id) |>
    column_to_rownames('source_id') |>
    as.matrix()

  # pivot_wider orders columns by first appearance, so align them with zones
  cost_mat <- cost_mat[, as.character(zones), drop = FALSE]
  stopifnot(
    identical(rownames(cost_mat), as.character(stations)),
    identical(colnames(cost_mat), as.character(zones))
  )

  if (p > length(stations)) {
    stop('p is larger than the number of candidate stations')
  }

  # coverage matrix, 1 if the station reaches the fdz within the threshold
  cover_mat <- (!is.na(cost_mat) & cost_mat <= threshold_minutes) * 1L

  # get vector of all incident counts ordered by fdz
  demand_weights <- incident_demand_df$n_incidents[match(zones, incident_demand_df$fdz)]

  n_st <- length(stations)
  n_zn <- length(zones)

  model <- MIPModel() |>
    # variable for station, 1 is open, 0 is closed
    add_variable(y[i], i = 1:n_st, type = 'binary') |>
    # variable for zone, 1 if at least one open station covers it
    add_variable(z[j], j = 1:n_zn, type = 'binary') |>
    # maximize the demand in covered zones
    set_objective(
      sum_over(demand_weights[j] * z[j], j = 1:n_zn),
      sense = 'max'
    ) |>
    # a zone only counts as covered if an open station reaches it within the threshold
    add_constraint(
      z[j] <= sum_over(cover_mat[i, j] * y[i], i = 1:n_st),
      j = 1:n_zn
    ) |>
    # keep exactly p stations
    add_constraint(sum_over(y[i], i = 1:n_st) == p)

  result <- solve_model(model, with_ROI(solver = solver))

  open_idx <- get_solution(result, y[i]) |>
    filter(value > 0.5) |>
    pull(i)

  # derive coverage from the open stations rather than z, since z is free to be 0 for zones with no demand
  coverage <- tibble(
    fdz = zones,
    n_incidents = demand_weights,
    covered = colSums(cover_mat[open_idx, , drop = FALSE]) > 0
  )

  list(
    status = result$status,
    covered_demand = sum(coverage$n_incidents[coverage$covered]),
    pct_covered = sum(coverage$n_incidents[coverage$covered]) / sum(coverage$n_incidents),
    open_stations = stations[open_idx],
    coverage = coverage
  )
}

mclp_result <- solve_mclp(travel_stats_df, p = 10, threshold_minutes = 6)

mclp_result$status
mclp_result$covered_demand
mclp_result$pct_covered
mclp_result$open_stations
mclp_result$coverage
