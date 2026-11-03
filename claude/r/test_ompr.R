library(tidyverse)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# --- 1. Toy data: the 3 station x 4 zone example from the walkthrough ------

# long format, same shape as travel_stats_test_df (source_id, fdz, mean_minutes)
toy_df <- tribble(
  ~source_id, ~fdz, ~mean_minutes,
  'A', 1,  3, 'A', 2,  5, 'A', 3, 12, 'A', 4, 14,
  'B', 1, 10, 'B', 2,  4, 'B', 3,  4, 'B', 4,  9,
  'C', 1, 13, 'C', 2, 11, 'C', 3,  6, 'C', 4,  3
)

# one demand weight per zone
demand_df <- tribble(
  ~fdz, ~n_incidents,
  1, 10,
  2, 20,
  3,  5,
  4, 15
)

k <- 2

# --- 2. Reshape into what the model needs: a cost matrix and a weight vector

stations <- sort(unique(toy_df$source_id))
zones <- sort(unique(toy_df$fdz))

# cost_mat[i, j] = minutes from station i to zone j
cost_mat <- toy_df |>
  pivot_wider(names_from = fdz, values_from = mean_minutes) |>
  arrange(source_id) |>
  column_to_rownames('source_id') |>
  as.matrix()

# weights in the same order as the matrix columns
w <- demand_df$n_incidents[match(zones, demand_df$fdz)]

n_st <- length(stations)
n_zn <- length(zones)

# --- 3. The model: p-median ------------------------------------------------

model <- MIPModel() |>
  # y[i] = 1 if station i stays open
  add_variable(y[i], i = 1:n_st, type = 'binary') |>
  # x[i, j] = 1 if zone j is served by station i
  add_variable(x[i, j], i = 1:n_st, j = 1:n_zn, type = 'binary') |>
  # minimise total demand-weighted travel time
  set_objective(
    sum_over(w[j] * cost_mat[i, j] * x[i, j], i = 1:n_st, j = 1:n_zn),
    sense = 'min'
  ) |>
  # every zone is served by exactly one station
  add_constraint(sum_over(x[i, j], i = 1:n_st) == 1, j = 1:n_zn) |>
  # a zone can only use a station that is open
  add_constraint(x[i, j] <= y[i], i = 1:n_st, j = 1:n_zn) |>
  # keep exactly k stations
  add_constraint(sum_over(y[i], i = 1:n_st) == k)

# --- 4. Solve and read the answer back -------------------------------------

result <- solve_model(model, with_ROI(solver = 'glpk'))

result$status
objective_value(result)   # expect 205

# which stations are open
open_stations <- get_solution(result, y[i]) |>
  filter(value > 0.5) |>
  mutate(station = stations[i])
open_stations              # expect A and C

# which station serves each zone
get_solution(result, x[i, j]) |>
  filter(value > 0.5) |>
  mutate(station = stations[i], fdz = zones[j]) |>
  select(fdz, station)
