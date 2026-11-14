# Build-up order: add units one at a time on top of a base set (e.g. ALS
# engines on top of the chosen rescues), each time the station that saves the
# most travel minutes on the run card with the units already added in place.
#
# Scoring is the run card's (each call to its zone's first open station), as
# everywhere else, without intervals. A step's saving can be small or even
# negative: a station first on a zone's card is not always the closest.
#
#   order <- build_up_order(rescues, travel_matrix, runcard, filter, n_add = 9)
#   build_up_table(order$steps, years = 3)
#   build_up_territory_table(order$steps, order$territory, years = 3)

#' Add units one at a time, each where it saves the most travel minutes
#'
#' @param base_stations Stations open before any unit is added.
#' @param matrix_df Travel matrix as for [compare_overall()], with
#'   `station_id` (each call's territory station).
#' @param runcard_df Run card order from [prep_runcard()].
#' @param filter One [incident_filter()]: the calls that count.
#' @param n_add How many units to add (fewer if the candidates run out).
#' @param candidates Stations that may get a unit. `NULL`: every station in
#'   the matrix not in `base_stations` or `exclude`.
#' @param exclude Stations that may not get a unit.
#' @param due One due level.
#' @param thresholds Minutes for the `pct_within_<t>` metrics.
#'
#' @return A list:
#'   - `steps`: one row per step, step 0 being `base_stations` alone:
#'     `step`, `station` (added; `NA` at step 0), `stations` (open, as text),
#'     `filter_name`, `n_calls`, `total_minutes`, `saved_minutes` (this
#'     step's unit, vs the step before), `cumulative_saved` (vs step 0),
#'     `p90_minutes`, `pct_within_<t>`, `unserved_calls`, and for the added
#'     station's own territory `own_n_calls`, `own_<metric>_before` and
#'     `own_<metric>_after` (`total_minutes`, `p90_minutes`,
#'     `pct_within_<t>`) and `own_share` (its share of the step's saving).
#'   - `territory`: one row per step, territory and metric (`n_calls`,
#'     `total_minutes`, `p90_minutes`, `pct_within_<t>`): `step`, `station`
#'     (added), `station_id` (territory), `metric`, `before`, `after`, `diff`.
#'   Counts are totals over the filter's calls, not per year.
#' @noRd
build_up_order <- function(
  base_stations,
  matrix_df,
  runcard_df,
  filter,
  n_add,
  candidates = NULL,
  exclude = NULL,
  due = 1,
  thresholds = c(4, 8)
) {
  if (!inherits(filter, "incident_filter")) {
    stop("filter must be one incident_filter()", call. = FALSE)
  }
  if (length(due) != 1) {
    stop("due must be one due level", call. = FALSE)
  }
  if (!"station_id" %in% names(matrix_df)) {
    stop("matrix_df needs a station_id column with each call's territory station", call. = FALSE)
  }
  base_stations <- sort(unique(base_stations))
  candidates <- candidates %||% sort(unique(matrix_df$source_id))
  candidates <- setdiff(candidates, c(base_stations, exclude))
  if (length(candidates) == 0) {
    stop("No candidate stations outside base_stations and exclude", call. = FALSE)
  }

  # the filter's calls once; each round re-applies it to these (no change)
  filtered <- filter_incidents(matrix_df, filter)

  added <- integer()
  for (k in seq_len(min(n_add, length(candidates)))) {
    left <- setdiff(candidates, added)
    scores <- score_station_sets(lapply(left, function(s) sort(c(base_stations, added, s))), filtered, runcard_df, filter, due)
    added <- c(added, left[order(scores$unserved, scores$minutes)[1]])
  }

  sets <- c(list(base_stations), lapply(seq_along(added), function(k) sort(c(base_stations, added[seq_len(k)]))))
  sequence_stats(sets, added, filtered, runcard_df, filter, due, thresholds)
}

#' Move units one at a time, each move the one that saves the most minutes
#'
#' From `base_stations` plus units at `from_stations`, each step moves one unit
#' that hasn't moved yet to an empty station: the move that saves the most
#' travel minutes on the run card, with the moves above it made. Each unit
#' moves at most once, and never to a base station or a station with a unit.
#' Units at a base station add no location, so moving one only adds its new
#' station.
#'
#' With `to_stations` the destination is fixed: the units not already there
#' move to its empty stations, and the last step reaches it. Without it, the
#' destination is wherever the moves lead: any station in `candidates` can
#' take a unit, and the moves stop when none saves minutes (or every unit has
#' moved).
#'
#' @param base_stations Stations open throughout (e.g. the rescues).
#' @param from_stations Where the units are now (e.g. today's ALS engines).
#' @param to_stations Where they should end up, as many stations as
#'   `from_stations`, or `NULL` to let the moves decide.
#' @param candidates Without `to_stations`: stations that may take a unit.
#'   `NULL`: every station in the matrix.
#' @param exclude Without `to_stations`: stations that may not take a unit.
#' @param chunk Station sets scored at once (fewer use less memory).
#' @inheritParams build_up_order
#'
#' @return As [build_up_order()], step 0 being `base_stations` with the units
#'   where they are now; `steps` also has `from` (the station a step's unit
#'   leaves), and `station` is where it goes.
#' @noRd
move_order <- function(
  base_stations,
  from_stations,
  to_stations = NULL,
  matrix_df,
  runcard_df,
  filter,
  candidates = NULL,
  exclude = NULL,
  due = 1,
  thresholds = c(4, 8),
  chunk = 40
) {
  if (!inherits(filter, "incident_filter")) {
    stop("filter must be one incident_filter()", call. = FALSE)
  }
  if (length(due) != 1) {
    stop("due must be one due level", call. = FALSE)
  }
  if (!"station_id" %in% names(matrix_df)) {
    stop("matrix_df needs a station_id column with each call's territory station", call. = FALSE)
  }
  base_stations <- sort(unique(base_stations))
  from_stations <- sort(unique(from_stations))
  free <- is.null(to_stations)
  if (free) {
    move_from <- from_stations
    move_to <- setdiff(candidates %||% sort(unique(matrix_df$source_id)), c(base_stations, from_stations, exclude))
  } else {
    to_stations <- sort(unique(to_stations))
    if (length(from_stations) != length(to_stations)) {
      stop(
        "from_stations and to_stations must have as many stations: ",
        length(from_stations), " vs ", length(to_stations),
        call. = FALSE
      )
    }
    at_base <- intersect(to_stations, base_stations)
    if (length(at_base) > 0) {
      stop("to_stations can't include base stations: ", paste(at_base, collapse = ", "), call. = FALSE)
    }
    move_from <- setdiff(from_stations, to_stations)
    move_to <- setdiff(to_stations, from_stations)
  }

  filtered <- filter_incidents(matrix_df, filter)
  open_with <- function(units) sort(union(base_stations, units))
  score <- function(sets) {
    groups <- split(seq_along(sets), ceiling(seq_along(sets) / chunk))
    do.call(rbind, lapply(groups, function(g) score_station_sets(sets[g], filtered, runcard_df, filter, due)))
  }

  units <- from_stations
  froms <- integer()
  tos <- integer()
  current <- score(list(open_with(units)))
  while (length(setdiff(move_from, froms)) > 0 && length(setdiff(move_to, tos)) > 0) {
    pairs <- expand.grid(from = setdiff(move_from, froms), to = setdiff(move_to, tos))
    sets <- lapply(seq_len(nrow(pairs)), function(r) open_with(c(setdiff(units, pairs$from[r]), pairs$to[r])))
    scores <- score(sets)
    best <- order(scores$unserved, scores$minutes)[1]
    # without a destination, stop when the best move doesn't help
    if (free && (scores$unserved[best] > current$unserved ||
      (scores$unserved[best] == current$unserved && scores$minutes[best] >= current$minutes))) {
      break
    }
    froms <- c(froms, pairs$from[best])
    tos <- c(tos, pairs$to[best])
    units <- c(setdiff(units, pairs$from[best]), pairs$to[best])
    current <- scores[best, ]
  }

  sets <- c(
    list(open_with(from_stations)),
    lapply(seq_along(tos), function(k) open_with(c(setdiff(from_stations, froms[seq_len(k)]), tos[seq_len(k)])))
  )
  out <- sequence_stats(sets, tos, filtered, runcard_df, filter, due, thresholds)
  out$steps$from <- c(NA_integer_, as.integer(froms))
  out$steps <- out$steps[c("step", "from", setdiff(names(out$steps), c("step", "from")))]
  out$territory$from <- as.integer(froms)[out$territory$step]
  out$units <- sort(units)
  out
}

# Total minutes and unserved calls of each station set, on the run card
score_station_sets <- function(sets, filtered, runcard_df, filter, due) {
  scenarios <- tibble::tibble(
    scenario_id = seq_along(sets),
    scenario_name = paste("Set", seq_along(sets)),
    stations = sets
  )
  r <- suppressMessages(scenario_responders(scenarios, filtered, runcard_df, filter, due))
  n_calls <- nrow(attr(r, "calls"))
  data.frame(
    minutes = vapply(seq_along(sets), function(i) sum(r$duration_min[r$scenario_id == i]), numeric(1)),
    unserved = n_calls - tabulate(r$scenario_id, length(sets))
  )
}

# A sequence of station sets (step 0, then one per step), each step's unit
# going to added[k]: the steps' county stats and every territory's change
sequence_stats <- function(sets, added, filtered, runcard_df, filter, due, thresholds) {
  # No steps (no move helped): step 0 is scored against a copy of itself, and
  # only its row is kept
  no_steps <- length(added) == 0
  if (no_steps) {
    sets <- c(sets, sets[1])
    added <- NA_integer_
  }
  # Score the chosen sequence once, every step against the base
  scenarios <- tibble::tibble(
    scenario_id = seq_along(sets) - 1L,
    scenario_name = paste("Step", seq_along(sets) - 1L),
    stations = sets,
    baseline = c(TRUE, rep(FALSE, length(added)))
  )
  paired <- suppressMessages(paired_calls(scenarios, filtered, runcard_df, filter, due))
  overall <- compare_overall_paired(paired, thresholds, ci = FALSE, n_boot = 0, level = 0.95)
  terr <- compare_by_territory_paired(paired, thresholds)

  pct <- paste0("pct_within_", threshold_key(thresholds))
  # a step's value: the baseline side for step 0, the scenario side otherwise
  value_at <- function(df, k, m, territory = NULL) {
    rows <- df[df$metric == m & (k == 0 | df$scenario_id == k), ]
    if (!is.null(territory)) {
      rows <- rows[rows$station_id == territory, ]
    }
    if (nrow(rows) == 0) {
      return(NA_real_)
    }
    if (k == 0) rows$baseline[1] else rows$scenario[1]
  }

  steps <- lapply(seq_along(sets) - 1L, function(k) {
    station <- if (k == 0) NA_integer_ else as.integer(added[k])
    row <- data.frame(
      step = k,
      station = station,
      stations = paste(sets[[k + 1]], collapse = ", "),
      filter_name = filter$name,
      n_calls = value_at(overall, k, "n_calls"),
      total_minutes = value_at(overall, k, "total_minutes"),
      p90_minutes = value_at(overall, k, "p90_minutes"),
      unserved_calls = value_at(overall, k, "unserved_calls")
    )
    for (m in pct) {
      row[[m]] <- value_at(overall, k, m)
    }
    row$own_n_calls <- if (k == 0) NA_real_ else value_at(terr, k, "n_calls", station)
    for (m in c("total_minutes", "p90_minutes", pct)) {
      row[[paste0("own_", m, "_before")]] <- if (k == 0) NA_real_ else value_at(terr, k - 1, m, station)
      row[[paste0("own_", m, "_after")]] <- if (k == 0) NA_real_ else value_at(terr, k, m, station)
    }
    row
  })
  steps <- dplyr::bind_rows(steps)
  steps$saved_minutes <- c(NA_real_, -diff(steps$total_minutes))
  steps$cumulative_saved <- steps$total_minutes[1] - steps$total_minutes
  steps$cumulative_saved[1] <- NA_real_
  own_saved <- steps$own_total_minutes_before - steps$own_total_minutes_after
  steps$own_share <- ifelse(is.na(steps$saved_minutes) | steps$saved_minutes == 0, NA_real_, own_saved / steps$saved_minutes)
  steps <- steps[c(
    "step", "station", "stations", "filter_name", "n_calls", "total_minutes", "saved_minutes",
    "cumulative_saved", "p90_minutes", pct, "unserved_calls",
    setdiff(names(steps), c(
      "step", "station", "stations", "filter_name", "n_calls", "total_minutes", "saved_minutes",
      "cumulative_saved", "p90_minutes", pct, "unserved_calls"
    ))
  )]

  # Every territory's change at each step, step k - 1 to step k
  metrics <- c("n_calls", "total_minutes", "p90_minutes", pct)
  territories <- sort(unique(terr$station_id))
  territory <- lapply(seq_along(added), function(k) {
    rows <- expand.grid(station_id = territories, metric = metrics, stringsAsFactors = FALSE)
    rows$before <- mapply(function(t, m) value_at(terr, k - 1, m, t), rows$station_id, rows$metric)
    rows$after <- mapply(function(t, m) value_at(terr, k, m, t), rows$station_id, rows$metric)
    rows$diff <- rows$after - rows$before
    cbind(step = k, station = as.integer(added[k]), rows)
  })
  territory <- dplyr::bind_rows(territory)
  if (no_steps) {
    steps <- steps[steps$step == 0, ]
    territory <- territory[0, ]
  }

  list(steps = steps, territory = territory)
}
