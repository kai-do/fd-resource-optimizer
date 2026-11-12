# Station placement optimization.
#
# solve_p_median() picks the p stations that minimize total demand-weighted
# travel time, each zone served by its cheapest open station. There is no run
# card ordering and no unit availability: it proposes station sets, and step 06
# scores them with the real run cards.
#
#   stats <- summarize_travel_stats(matrix_df, by = c("source_id", "fdz_id"))
#   solve_p_median(stats, p = 17, include = rescue_stations, n_solutions = 5)

#' Weighted p-median: the p stations with the least total travel time
#'
#' Chooses which `p` stations to open so that the sum over zones of
#' `n_incidents * cost` is smallest, with every zone served by its cheapest
#' open station. With `cost = "mean_minutes"` that sum is the total travel
#' minutes of all the zone's incidents.
#'
#' @param travel_stats One row per station and zone with `source_id`,
#'   `fdz_id`, `n_incidents` and the `cost` column, from
#'   [summarize_travel_stats()] with `by = c("source_id", "fdz_id")`. Rows
#'   with no zone (`NA` `fdz_id`) are dropped.
#' @param p Number of stations to open, `include` counted.
#' @param cost Column used as the station-to-zone cost, e.g. `"mean_minutes"`,
#'   `"median_minutes"` or `"p90_minutes"`.
#' @param include Station ids that must be open (count toward `p`).
#' @param exclude Station ids that must stay closed.
#' @param n_solutions How many station sets to return: the best, then the
#'   next best that differ from all earlier ones by at least one station.
#' @param solver ROI solver name.
#'
#' @return A list for the best solution: `rank`, `objective` (total weighted
#'   cost), `open_stations`, and `assignments` (one row per zone: `fdz_id`,
#'   `source_id` serving it, `n_incidents` and the cost). `alternatives` holds
#'   every solution found, best first, in the same shape; it can be shorter
#'   than `n_solutions` if fewer distinct sets exist.
#' @noRd
solve_p_median <- function(
  travel_stats,
  p,
  cost = "mean_minutes",
  include = NULL,
  exclude = NULL,
  n_solutions = 1,
  solver = "glpk"
) {
  missing_cols <- setdiff(c("source_id", "fdz_id", "n_incidents", cost), names(travel_stats))
  if (length(missing_cols) > 0) {
    stop("travel_stats is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  travel_stats <- travel_stats[!is.na(travel_stats$fdz_id), ]

  stations <- sort(unique(travel_stats$source_id))
  zones <- sort(unique(travel_stats$fdz_id))
  n_st <- length(stations)
  n_zn <- length(zones)

  if (p > n_st) {
    stop("p (", p, ") is larger than the number of stations (", n_st, ")", call. = FALSE)
  }

  # Cost matrix: rows are stations, columns are zones
  cost_mat <- matrix(NA_real_, n_st, n_zn)
  cost_mat[cbind(
    match(travel_stats$source_id, stations),
    match(travel_stats$fdz_id, zones)
  )] <- travel_stats[[cost]]
  if (anyNA(cost_mat)) {
    stop("travel_stats has missing or NA costs for some station/zone pairs", call. = FALSE)
  }

  # Incidents per zone. Every station row counts the same incidents, so take
  # the largest in case a station could not route to some of them
  demand <- tapply(travel_stats$n_incidents, factor(travel_stats$fdz_id, levels = zones), max)
  demand <- as.numeric(demand)
  weighted_cost <- sweep(cost_mat, 2, demand, `*`)

  idx <- station_constraint_idx(stations, p, include, exclude)

  model <- ompr::MIPModel() |>
    # 1 if the station is open
    ompr::add_variable(y[i], i = 1:n_st, type = "binary") |>
    # share of the zone served by the station; ends up 0 or 1 once y is fixed,
    # so continuous keeps the model smaller
    ompr::add_variable(x[i, j], i = 1:n_st, j = 1:n_zn, type = "continuous", lb = 0, ub = 1) |>
    ompr::set_objective(
      ompr::sum_over(weighted_cost[i, j] * x[i, j], i = 1:n_st, j = 1:n_zn),
      sense = "min"
    ) |>
    ompr::add_constraint(ompr::sum_over(y[i], i = 1:n_st) == p) |>
    # every zone fully served
    ompr::add_constraint(ompr::sum_over(x[i, j], i = 1:n_st) == 1, j = 1:n_zn) |>
    # only by open stations
    ompr::add_constraint(x[i, j] <= y[i], i = 1:n_st, j = 1:n_zn)

  if (length(idx$include) > 0) {
    inc <- idx$include
    model <- model |> ompr::add_constraint(y[i] == 1, i = inc)
  }
  if (length(idx$exclude) > 0) {
    exc <- idx$exclude
    model <- model |> ompr::add_constraint(y[i] == 0, i = exc)
  }

  alternatives <- list()
  for (k in seq_len(n_solutions)) {
    result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = solver))

    # no more distinct station sets once the model is infeasible
    if (result$status != "success") {
      if (k == 1) stop("no feasible solution, solver status: ", result$status, call. = FALSE)
      break
    }

    y_sol <- ompr::get_solution(result, y[i])
    open_idx <- sort(y_sol$i[y_sol$value > 0.5])

    # each zone to its cheapest open station
    served_by <- open_idx[apply(cost_mat[open_idx, , drop = FALSE], 2, which.min)]
    assignments <- tibble::tibble(
      fdz_id = zones,
      source_id = stations[served_by],
      n_incidents = demand
    )
    assignments[[cost]] <- cost_mat[cbind(served_by, seq_len(n_zn))]

    alternatives[[k]] <- list(
      rank = k,
      objective = sum(weighted_cost[cbind(served_by, seq_len(n_zn))]),
      open_stations = stations[open_idx],
      assignments = assignments
    )

    # forbid this exact set: any other set of p shares at most p - 1 of them
    model <- model |>
      ompr::add_constraint(ompr::sum_over(y[i], i = open_idx) <= p - 1)
  }

  c(alternatives[[1]], list(alternatives = alternatives))
}

#' Check required and excluded stations, as positions in `stations`
#'
#' @param stations All candidate station ids, sorted.
#' @param p Number of stations to open.
#' @param include,exclude Station ids that must be open or closed.
#'
#' @return A list with `include` and `exclude` positions in `stations`. Stops
#'   if an id is unknown, in both lists, or the lists can't be met with `p`.
#' @noRd
station_constraint_idx <- function(stations, p, include = NULL, exclude = NULL) {
  unknown <- setdiff(c(include, exclude), stations)
  if (length(unknown) > 0) {
    stop("unknown station(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  both <- intersect(include, exclude)
  if (length(both) > 0) {
    stop("station(s) both required and excluded: ", paste(both, collapse = ", "), call. = FALSE)
  }
  if (length(unique(include)) > p) {
    stop("more required stations than p", call. = FALSE)
  }
  if (length(stations) - length(unique(exclude)) < p) {
    stop("too many excluded stations to open p stations", call. = FALSE)
  }

  list(include = match(unique(include), stations), exclude = match(unique(exclude), stations))
}
