library(tidyverse)
library(arrow)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# source data for the cost matrix and demand weights
travel_stats_df <- read_parquet('datasets/outputs/travel_stats.parquet')

# Choose the p stations that minimize total demand-weighted travel time.
# cost_col is the column of travel_stats_df used as the station-to-fdz cost, e.g. mean_minutes, median_minutes, p90_minutes.
# Returns a list with the solver status, objective value, open stations, and the station serving each fdz.
solve_p_median <- function(
  travel_stats_df,
  p,
  cost_col = 'mean_minutes',
  solver = 'glpk'
) {
  # incidents outside any fdz have no zone to serve
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

  if (anyNA(cost_mat)) {
    stop('cost matrix has missing station/fdz pairs')
  }
  if (p > length(stations)) {
    stop('p is larger than the number of candidate stations')
  }

  # get vector of all incident counts ordered by fdz
  demand_weights <- incident_demand_df$n_incidents[match(zones, incident_demand_df$fdz)]

  n_st <- length(stations)
  n_zn <- length(zones)

  model <- MIPModel() |>
    # variable for station, 1 is open, 0 is closed
    add_variable(y[i], i = 1:n_st, type = 'binary') |>
    # variable for station and zone assignments, 1 if station serves that zone
    add_variable(x[i, j], i = 1:n_st, j = 1:n_zn, type = 'binary') |>
    # minimize total demand-weighted travel time
    set_objective(
      sum_over(demand_weights[j] * cost_mat[i, j] * x[i, j], i = 1:n_st, j = 1:n_zn),
      sense = 'min'
    ) |>
    # each zone can be served by only one station
    add_constraint(sum_over(x[i, j], i = 1:n_st) == 1, j = 1:n_zn) |>
    # a zone can only use a station that is open
    add_constraint(x[i, j] <= y[i], i = 1:n_st, j = 1:n_zn) |>
    # keep exactly p stations
    add_constraint(sum_over(y[i], i = 1:n_st) == p)

  result <- solve_model(model, with_ROI(solver = solver))

  open_stations <- get_solution(result, y[i]) |>
    filter(value > 0.5) |>
    mutate(station = stations[i]) |>
    pull(station)

  assignments <- get_solution(result, x[i, j]) |>
    filter(value > 0.5) |>
    mutate(station = stations[i], fdz = zones[j]) |>
    select(fdz, station)

  list(
    status = result$status,
    objective = objective_value(result),
    open_stations = open_stations,
    assignments = assignments
  )
}

p_median_result <- solve_p_median(travel_stats_df, p = 10)

p_median_result$status
p_median_result$objective
p_median_result$open_stations
p_median_result$assignments
