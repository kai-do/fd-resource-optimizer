# Function-only helpers shared by the pipeline scripts (05-07) and the Shiny app.
# No top-level execution, safe to source().
library(tidyverse)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Travel-time summaries per FDZ. If `filter_by` or `codes` is NA, no filter is applied and all incidents are used.
# Otherwise, restricted to incidents matching `codes` in the column chosen by `filter_by`.
# `filters` is a named list of codes that are all applied together (AND), one entry per column key:
# source_system, rms_category, rms_subcategory, rms_type, cad_type, dispatch_code, dispatch_category, proqa_case_type,
# ems_capability.
# NULL or empty entries are skipped.
# rms_category, rms_subcategory, dispatch_category and proqa_case_type need the incident_category_code, incident_subcategory_code,
# dispatch_category_code and dispatch_case_type_description columns on the matrix, and ems_capability needs ems_unit_capability.
get_travel_stats <- function(
  travel_matrix_df,
  filter_by = NA,
  codes = NA,
  filters = NULL
) {
  filter_cols <- c(
    source_system = 'rms_source_system',
    rms_category = 'incident_category_code',
    rms_subcategory = 'incident_subcategory_code',
    rms_type = 'rms_incident_type_code',
    cad_type = 'cad_incident_type_code',
    dispatch_code = 'dispatch_code',
    dispatch_category = 'dispatch_category_code',
    proqa_case_type = 'dispatch_case_type_description',
    ems_capability = 'ems_unit_capability'
  )

  if (!anyNA(filter_by) && !anyNA(codes)) {
    filter_by <- match.arg(filter_by, names(filter_cols))
    filters <- c(filters, setNames(list(codes), filter_by))
  }

  for (key in names(filters)) {
    key_codes <- filters[[key]]
    if (length(key_codes) == 0) next
    filter_col <- filter_cols[[match.arg(key, names(filter_cols))]]

    travel_matrix_df <- travel_matrix_df |>
      filter(.data[[filter_col]] %in% key_codes)
  }

  travel_matrix_df |>
    group_by(source_id, fdz) |>
    summarise(
      n_incidents = n_distinct(dest_id),
      mean_minutes = mean(duration_min, na.rm = TRUE),
      median_minutes = median(duration_min, na.rm = TRUE),
      p90_minutes = quantile(duration_min, 0.9, na.rm = TRUE),
      mean_distance_m = mean(distance_m, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Validate the required (`include`) and excluded (`exclude`) station ids and convert them to positions in `stations`.
# Returns a list with the `include` and `exclude` positions, stops with a message if the lists cannot be satisfied.
station_constraint_idx <- function(stations, p, include = NULL, exclude = NULL) {
  unknown <- setdiff(c(include, exclude), stations)
  if (length(unknown) > 0) {
    stop('unknown station(s): ', paste(unknown, collapse = ', '))
  }

  both <- intersect(include, exclude)
  if (length(both) > 0) {
    stop('station(s) both required and excluded: ', paste(both, collapse = ', '))
  }
  if (length(include) > p) {
    stop('more required stations than p')
  }
  if (length(stations) - length(exclude) < p) {
    stop('too many excluded stations to open p stations')
  }

  list(include = match(include, stations), exclude = match(exclude, stations))
}

# Run card response order for every zone, as station positions in `stations`.
# runcard_df has columns fdz, station (source_id) and rank. Stations that are not candidates are dropped, and candidates
# missing from a zone's card (or zones with no card) are appended in cost order so every zone can always be served.
# Returns a list with one integer vector per zone, ordered like `zones`.
runcard_order <- function(runcard_df, stations, zones, cost_mat) {
  cards <- split(runcard_df, runcard_df$fdz)

  map(seq_along(zones), \(j) {
    card <- cards[[zones[j]]]
    on_card <- if (is.null(card)) integer() else unique(na.omit(match(card$station[order(card$rank)], stations)))
    c(on_card, setdiff(order(cost_mat[, j]), on_card))
  })
}

# The first open station on each zone's run card, as a position in `stations`.
runcard_first_due <- function(zone_order, open_idx) {
  map_int(zone_order, \(o) o[o %in% open_idx][1])
}

# One row per (zone, card position) that can matter for first-due, in card order.
# Excluded stations are skipped, the card is cut after the first required station (always open), and after the position where
# an open station is guaranteed (at most n_available - p stations are closed).
# Returns a tibble with zone, station, k (row number), is_first and is_last.
runcard_steps <- function(zone_order, p, include_idx, exclude_idx, n_st) {
  max_depth <- n_st - length(exclude_idx) - p + 1

  map(seq_along(zone_order), \(j) {
    o <- setdiff(zone_order[[j]], exclude_idx)
    depth <- min(max_depth, length(o))
    first_required <- which(o %in% include_idx)
    if (length(first_required) > 0) depth <- min(depth, first_required[1])
    tibble(zone = j, station = o[seq_len(depth)])
  }) |>
    list_rbind() |>
    mutate(
      k = row_number(),
      is_first = !duplicated(zone),
      is_last = !duplicated(zone, fromLast = TRUE)
    )
}

# Adds the run card first-due rule to a model that already has the station variables y[i].
# u[k] is 1 while no station up to step k on the zone's card is open, so a zone is served by step k exactly when
# u[k - 1] - u[k] = 1. step_coef is the objective value of that zone being served by that step's station.
add_runcard_chain <- function(model, steps, step_coef, sense) {
  n_steps <- nrow(steps)
  station_at <- steps$station
  first_ids <- steps$k[steps$is_first]
  later_ids <- steps$k[!steps$is_first]
  last_ids <- steps$k[steps$is_last]

  # objective is sum(coef[k] * (u[k - 1] - u[k])) with u before the first step equal to 1, regrouped by u[k]
  next_coef <- c(step_coef[-1], 0)
  next_coef[steps$is_last] <- 0
  coef_on_u <- next_coef - step_coef
  objective_constant <- sum(step_coef[steps$is_first])

  model |>
    add_variable(u[k], k = 1:n_steps, type = 'continuous', lb = 0, ub = 1) |>
    set_objective(
      sum_over(coef_on_u[k] * u[k], k = 1:n_steps) + objective_constant,
      sense = sense
    ) |>
    # first step: still unserved exactly when its station is closed
    add_constraint(u[k] + y[station_at[k]] == 1, k = first_ids) |>
    # later steps: unserved only if unserved before and this station is closed, and unserved if both hold
    add_constraint(u[k] <= u[k - 1], k = later_ids) |>
    add_constraint(u[k] + y[station_at[k]] <= 1, k = later_ids) |>
    add_constraint(u[k] - u[k - 1] + y[station_at[k]] >= 0, k = later_ids) |>
    # the card is cut where an open station is guaranteed, so every zone is served by its last step
    add_constraint(u[k] == 0, k = last_ids)
}

# Choose the p stations that minimize total demand-weighted travel time.
# cost_col is the column of travel_stats_df used as the station-to-fdz cost, e.g. mean_minutes, median_minutes, p90_minutes.
# include and exclude are station ids (source_id) that must be open or must stay closed. Required stations count toward p.
# n_solutions > 1 also returns the next-best station sets, each a different set of open stations, best first.
# runcard_df (columns fdz, station, rank) makes each fdz served by the first open station on its run card instead of the cheapest.
# Returns a list with the solver status, objective value, open stations, and the station serving each fdz for the best solution.
# `alternatives` holds every solution (rank 1 is the best) in the same shape.
solve_p_median <- function(
  travel_stats_df,
  p,
  cost_col = 'mean_minutes',
  solver = 'glpk',
  include = NULL,
  exclude = NULL,
  n_solutions = 1,
  runcard_df = NULL
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

  idx <- station_constraint_idx(stations, p, include, exclude)

  model <- MIPModel() |>
    # variable for station, 1 is open, 0 is closed
    add_variable(y[i], i = 1:n_st, type = 'binary') |>
    # keep exactly p stations
    add_constraint(sum_over(y[i], i = 1:n_st) == p)

  if (is.null(runcard_df)) {
    model <- model |>
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
      add_constraint(x[i, j] <= y[i], i = 1:n_st, j = 1:n_zn)
  } else {
    # each zone is served by the first open station on its run card, see add_runcard_chain()
    zone_order <- runcard_order(runcard_df, stations, zones, cost_mat)
    steps <- runcard_steps(zone_order, p, idx$include, idx$exclude, n_st)
    model <- add_runcard_chain(
      model,
      steps,
      step_coef = demand_weights[steps$zone] * cost_mat[cbind(steps$station, steps$zone)],
      sense = 'min'
    )
  }

  # required stations are fixed open, excluded stations are fixed closed
  if (length(idx$include) > 0) {
    model <- model |> add_constraint(y[i] == 1, i = idx$include)
  }
  if (length(idx$exclude) > 0) {
    model <- model |> add_constraint(y[i] == 0, i = idx$exclude)
  }

  alternatives <- list()
  for (k in seq_len(n_solutions)) {
    result <- solve_model(model, with_ROI(solver = solver))

    # no more station sets once the model is infeasible
    if (result$status != 'success') {
      if (k == 1) stop('no feasible solution, solver status: ', result$status)
      break
    }

    open_idx <- get_solution(result, y[i]) |>
      filter(value > 0.5) |>
      pull(i)

    if (is.null(runcard_df)) {
      assignments <- get_solution(result, x[i, j]) |>
        filter(value > 0.5) |>
        mutate(station = stations[i], fdz = zones[j]) |>
        select(fdz, station)
      objective <- objective_value(result)
    } else {
      # score the open set by walking the cards, which is exactly what the model encodes
      served_by <- runcard_first_due(zone_order, open_idx)
      assignments <- tibble(fdz = zones, station = stations[served_by])
      objective <- sum(demand_weights * cost_mat[cbind(served_by, seq_len(n_zn))])
    }

    alternatives[[k]] <- list(
      rank = k,
      status = result$status,
      objective = objective,
      open_stations = stations[open_idx],
      assignments = assignments
    )

    # forbid this exact station set; any other set of p stations shares at most p - 1 of them
    model <- model |>
      add_constraint(sum_over(y[i], i = open_idx) <= p - 1)
  }

  c(alternatives[[1]], list(alternatives = alternatives))
}

# Maximal covering location problem (MCLP).
# Choose the p stations that maximize the incident demand covered within threshold_minutes.
# A station covers an fdz when its cost_col value for that fdz is <= threshold_minutes.
# cost_col defaults to the 90th percentile, so "covered" means 90% of historical trips arrive within the threshold.
# Station/fdz pairs missing from travel_stats_df are treated as not covering.
# include and exclude are station ids (source_id) that must be open or must stay closed. Required stations count toward p.
# n_solutions > 1 also returns the next-best station sets, each a different set of open stations, best first.
# Different station sets often cover the same demand, so alternatives can tie.
# runcard_df (columns fdz, station, rank) makes a zone covered only by the first open station on its run card.
# Returns a list with the solver status, covered demand, percent of demand covered, open stations, and coverage by fdz for the best solution.
# `alternatives` holds every solution (rank 1 is the best) in the same shape.
solve_mclp <- function(
  travel_stats_df,
  p,
  threshold_minutes,
  cost_col = 'p90_minutes',
  solver = 'glpk',
  include = NULL,
  exclude = NULL,
  n_solutions = 1,
  runcard_df = NULL
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

  idx <- station_constraint_idx(stations, p, include, exclude)

  model <- MIPModel() |>
    # variable for station, 1 is open, 0 is closed
    add_variable(y[i], i = 1:n_st, type = 'binary') |>
    # keep exactly p stations
    add_constraint(sum_over(y[i], i = 1:n_st) == p)

  if (is.null(runcard_df)) {
    model <- model |>
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
      )
  } else {
    # a zone is covered only if its first open station on the run card reaches it within the threshold
    zone_order <- runcard_order(runcard_df, stations, zones, cost_mat)
    steps <- runcard_steps(zone_order, p, idx$include, idx$exclude, n_st)
    model <- add_runcard_chain(
      model,
      steps,
      step_coef = demand_weights[steps$zone] * cover_mat[cbind(steps$station, steps$zone)],
      sense = 'max'
    )
  }

  # required stations are fixed open, excluded stations are fixed closed
  if (length(idx$include) > 0) {
    model <- model |> add_constraint(y[i] == 1, i = idx$include)
  }
  if (length(idx$exclude) > 0) {
    model <- model |> add_constraint(y[i] == 0, i = idx$exclude)
  }

  alternatives <- list()
  for (k in seq_len(n_solutions)) {
    result <- solve_model(model, with_ROI(solver = solver))

    # no more station sets once the model is infeasible
    if (result$status != 'success') {
      if (k == 1) stop('no feasible solution, solver status: ', result$status)
      break
    }

    open_idx <- get_solution(result, y[i]) |>
      filter(value > 0.5) |>
      pull(i)

    # derive coverage from the open stations rather than z, since z is free to be 0 for zones with no demand
    coverage <- tibble(
      fdz = zones,
      n_incidents = demand_weights,
      covered = colSums(cover_mat[open_idx, , drop = FALSE]) > 0
    )
    if (!is.null(runcard_df)) {
      # only the first open station on the card can cover the zone
      served_by <- runcard_first_due(zone_order, open_idx)
      coverage <- coverage |>
        mutate(
          station = stations[served_by],
          covered = cover_mat[cbind(served_by, seq_len(n_zn))] > 0
        )
    }

    alternatives[[k]] <- list(
      rank = k,
      status = result$status,
      covered_demand = sum(coverage$n_incidents[coverage$covered]),
      pct_covered = sum(coverage$n_incidents[coverage$covered]) / sum(coverage$n_incidents),
      open_stations = stations[open_idx],
      coverage = coverage
    )

    # forbid this exact station set; any other set of p stations shares at most p - 1 of them
    model <- model |>
      add_constraint(sum_over(y[i], i = open_idx) <= p - 1)
  }

  c(alternatives[[1]], list(alternatives = alternatives))
}

# Run card response order with only the selected stations, one row per fdz and one column per response position.
# Mirrors filter_runcard() in r/filter_runcard.R: stations that are not selected are dropped and the order is renumbered,
# so 1st Due is the first selected station on the card. Works on the app's runcard_beats data (fdz, station, rank, other_agency_first).
# Cells are station ids, blank where a zone's card has fewer selected stations than positions.
runcard_response_table <- function(runcard_df, selected_stations) {
  n_due <- length(selected_stations)
  due_labels <- paste0(
    seq_len(n_due),
    case_when(
      seq_len(n_due) %% 100 %in% 11:13 ~ 'th',
      seq_len(n_due) %% 10 == 1 ~ 'st',
      seq_len(n_due) %% 10 == 2 ~ 'nd',
      seq_len(n_due) %% 10 == 3 ~ 'rd',
      TRUE ~ 'th'
    ),
    ' Due'
  )

  zones_df <- runcard_df |>
    distinct(fdz, other_agency_first) |>
    arrange(fdz)

  responders_df <- runcard_df |>
    filter(station %in% selected_stations) |>
    group_by(fdz) |>
    arrange(rank, .by_group = TRUE) |>
    distinct(station, .keep_all = TRUE) |> # a few beats (e.g. 71A) have more than one card, keep the first appearance
    mutate(adjusted_response_sequence = row_number()) |>
    ungroup()

  wide_df <- zones_df |>
    left_join(
      responders_df |>
        mutate(position = factor(adjusted_response_sequence, levels = seq_len(n_due), labels = due_labels)) |>
        pivot_wider(id_cols = fdz, names_from = position, values_from = station, names_expand = TRUE),
      by = 'fdz'
    )

  wide_df |>
    transmute(
      FDZ = fdz,
      across(all_of(due_labels)),
      `Other Agency First Due` = if_else(other_agency_first, 'Yes', '')
    )
}
