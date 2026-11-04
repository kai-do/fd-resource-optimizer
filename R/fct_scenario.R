# Station scenarios crossed with incident filter sets.
#
# A scenario is a set of open stations; a filter set (incident_filter()) says
# which calls count. Every scenario is run under every filter set, so within a
# filter set all scenarios are compared on the same calls.
#
#   scenarios <- tibble::tribble(
#     ~scenario_id, ~scenario_name, ~stations,
#     1,            "Current",      cobb_stations,
#     2,            "7 stations",   c(11, 14, 21, 23, 24, 26, 30)
#   )
#   scenarios |> stats_by_station(matrix_df, runcard, filters = list(all_calls, night_als))
#   scenarios |> stats_by_fdz(matrix_df, runcard, due = 1:2)

#' Run card travel stats by responding station, per scenario and filter set
#'
#' @param scenarios Data frame with `scenario_id` (unique), `scenario_name`
#'   and a list column `stations` (the open station ids of each scenario).
#' @param matrix_df Travel matrix with `source_id`, `dest_id`, `fdz_id`,
#'   `duration_min`, `distance_m`, `incident_alarm_dt` and the columns the
#'   filter sets use.
#' @param runcard_df Run card order from [prep_runcard()].
#' @param filters One [incident_filter()] or a list of them. The default keeps
#'   every incident.
#' @param due Due levels, the same for every scenario, e.g. `1` or `1:3`.
#'
#' @return One row per filter set, scenario, due level and responding station:
#'   `filter_name`, `scenario_id`, `scenario_name`, `due`, `source_id`,
#'   `n_incidents`, `n_fdz` (zones the station responds to) and the travel
#'   stats from [summarise_travel_stats()].
#' @noRd
stats_by_station <- function(
  scenarios,
  matrix_df,
  runcard_df,
  filters = incident_filter("All calls"),
  due = 1
) {
  responders <- scenario_responders(scenarios, matrix_df, runcard_df, filters, due)
  keys <- c("filter_name", "scenario_id", "scenario_name", "due", "source_id")

  stats <- summarise_travel_stats(responders, by = keys)

  n_fdz <- responders |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, "fdz_id")))) |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n_fdz")

  stats |>
    dplyr::left_join(n_fdz, by = keys) |>
    dplyr::relocate(n_fdz, .after = n_incidents) |>
    order_like_inputs(responders, then = c("due", "source_id"))
}

#' Run card travel stats by zone, per scenario and filter set
#'
#' @inheritParams stats_by_station
#'
#' @return One row per filter set, scenario, due level and zone:
#'   `filter_name`, `scenario_id`, `scenario_name`, `due`, `fdz_id`,
#'   `source_id` (the responding station), `station_id` (the zone's territory
#'   station, if the matrix has it) and the travel stats from
#'   [summarise_travel_stats()].
#' @noRd
stats_by_fdz <- function(
  scenarios,
  matrix_df,
  runcard_df,
  filters = incident_filter("All calls"),
  due = 1
) {
  responders <- scenario_responders(scenarios, matrix_df, runcard_df, filters, due)
  keys <- c(
    "filter_name",
    "scenario_id",
    "scenario_name",
    "due",
    "fdz_id",
    "source_id",
    intersect("station_id", names(responders))
  )

  summarise_travel_stats(responders, by = keys) |>
    order_like_inputs(responders, then = c("due", "fdz_id"))
}

# One row per filter set, scenario, incident and due level: the incident's run
# card responder in that scenario, with filter_name, scenario_id and
# scenario_name added.
scenario_responders <- function(scenarios, matrix_df, runcard_df, filters, due) {
  scenarios <- validate_scenarios(scenarios)
  if (inherits(filters, "incident_filter")) {
    filters <- list(filters)
  }
  if (!all(vapply(filters, inherits, logical(1), what = "incident_filter"))) {
    stop("filters must be incident_filter() objects", call. = FALSE)
  }
  filter_names <- vapply(filters, function(f) f$name, character(1))
  if (anyDuplicated(filter_names)) {
    stop("Filter set names must be unique", call. = FALSE)
  }

  # Only a zone's responders in some scenario matter, so cut the matrix to
  # those station x zone pairs once. Zones with no responder in any scenario
  # keep their rows, so they are still reported as unserved.
  pairs <- lapply(scenarios$stations, function(st) {
    runcard_responders(runcard_df, st, due = due)[c("fdz_id", "source_id")]
  })
  pairs <- unique(do.call(rbind, pairs))
  matrix_df <- rbind(
    dplyr::semi_join(matrix_df, pairs, by = c("fdz_id", "source_id")),
    matrix_df[!matrix_df$fdz_id %in% pairs$fdz_id, ]
  )

  out <- list()
  calls <- list()
  for (f in filters) {
    filtered <- filter_incidents(matrix_df, f)

    # Every call in the filter set that has a zone, served or not, for the
    # unserved counts in the compare_*() functions
    zoned <- filtered[!is.na(filtered$fdz_id), ]
    calls[[f$name]] <- data.frame(
      filter_name = f$name,
      dest_id = zoned$dest_id,
      fdz_id = zoned$fdz_id
    )[!duplicated(zoned$dest_id), ]

    for (i in seq_len(nrow(scenarios))) {
      label <- paste0(f$name, " / ", scenarios$scenario_name[i], ": ")
      responders <- withCallingHandlers(
        filter_by_runcard(filtered, runcard_df, scenarios$stations[[i]], due = due),
        message = function(m) {
          message(label, conditionMessage(m), appendLF = FALSE)
          invokeRestart("muffleMessage")
        }
      )
      responders$filter_name <- rep(f$name, nrow(responders))
      responders$scenario_id <- rep(scenarios$scenario_id[i], nrow(responders))
      responders$scenario_name <- rep(scenarios$scenario_name[i], nrow(responders))
      out[[length(out) + 1]] <- responders
    }
  }

  out <- dplyr::bind_rows(out)
  attr(out, "filter_order") <- filter_names
  attr(out, "scenario_order") <- scenarios$scenario_id
  attr(out, "calls") <- dplyr::bind_rows(calls)
  out
}

validate_scenarios <- function(scenarios) {
  missing <- setdiff(c("scenario_id", "scenario_name", "stations"), names(scenarios))
  if (length(missing) > 0) {
    stop("scenarios is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  # One station per scenario gives a plain column (e.g. from tribble()): treat
  # each value as that scenario's station set
  if (!is.list(scenarios$stations)) {
    scenarios$stations <- as.list(scenarios$stations)
  }
  if (anyDuplicated(scenarios$scenario_id) || anyNA(scenarios$scenario_id)) {
    stop("scenario_id must be unique and not missing", call. = FALSE)
  }
  empty <- lengths(scenarios$stations) == 0
  if (any(empty)) {
    stop(
      "Scenario(s) with no stations: ",
      paste(scenarios$scenario_name[empty], collapse = ", "),
      call. = FALSE
    )
  }
  scenarios
}

# Sort results by filter set and scenario in the order they were given (not
# alphabetically), then by the `then` columns
order_like_inputs <- function(stats, responders, then) {
  filter_rank <- match(stats$filter_name, attr(responders, "filter_order"))
  scenario_rank <- match(stats$scenario_id, attr(responders, "scenario_order"))
  ord <- do.call(order, c(list(filter_rank, scenario_rank), unname(as.list(stats[then]))))
  stats[ord, ]
}
