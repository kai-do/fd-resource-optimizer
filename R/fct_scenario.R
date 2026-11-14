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
#'   stats from [summarize_travel_stats()].
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

  stats <- summarize_travel_stats(responders, by = keys)

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
#'   [summarize_travel_stats()].
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

  summarize_travel_stats(responders, by = keys) |>
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
    # unserved counts in the compare_*() functions. The territory station
    # comes along when the matrix has it, for plots by station territory.
    zoned <- filtered[!is.na(filtered$fdz_id), ]
    zone_calls <- data.frame(
      filter_name = rep(f$name, nrow(zoned)),
      dest_id = zoned$dest_id,
      fdz_id = zoned$fdz_id
    )
    if ("station_id" %in% names(zoned)) {
      zone_calls$station_id <- zoned$station_id
    }
    calls[[f$name]] <- zone_calls[!duplicated(zoned$dest_id), ]

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

#' The top scenarios, or the ones named
#'
#' @param scenarios Scenarios with `scenario_name` (e.g. step 06's
#'   `scenarios.parquet`).
#' @param overall_df Output of [compare_overall()], to rank them.
#' @param n_top,rank_by The best `n_top` on `rank_by`, as [rank_scenarios()]
#'   (the baseline is never among them).
#' @param names Scenario names to take instead, in order (overrides `n_top`).
#' @param filter_name,due As [rank_scenarios()].
#'
#' @return The chosen rows of `scenarios`, best first (or in `names` order).
#' @noRd
pick_scenarios <- function(
  scenarios, overall_df, n_top = 3, rank_by = "total_minutes", names = NULL, filter_name = NULL, due = NULL
) {
  names <- names %||% utils::head(rank_scenarios(overall_df, by = rank_by, filter_name = filter_name, due = due), n_top)
  unknown <- setdiff(names, scenarios$scenario_name)
  if (length(unknown) > 0) {
    stop(
      "Unknown scenario(s): ", paste(unknown, collapse = ", "),
      ". Available: ", paste(scenarios$scenario_name, collapse = ", "),
      call. = FALSE
    )
  }
  scenarios[match(names, scenarios$scenario_name), ]
}

#' A scenario name as a file or folder name
#'
#' @param name Scenario names.
#' @return The names with every run of characters other than letters and
#'   digits as `_`, none at either end: `"R26->16 +R1"` is `"R26_16_R1"`,
#'   `"+R1"` is `"R1"`.
#' @noRd
scenario_stub <- function(name) {
  gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", name))
}

#' Scenarios for an ALS summary: today's setup, then one rescue scenario alone
#' and with each set of engines
#'
#' Rows, in order: today's rescues with today's engines (the baseline), today's
#' rescues alone, the rescue scenario alone, with today's engines, with each p-median solution, and with each
#' `extra` engine set. Each open set is the rescues plus the engines; an engine
#' at a rescue station adds no location.
#'
#' @param baseline_rescues Today's rescue stations.
#' @param rescue_stations The rescue scenario's stations.
#' @param current_engines Today's ALS engine stations.
#' @param p_median_open Open-station sets of the p-median solutions to show, in
#'   rank order (they already include the rescues), named `P-median #1`, ...
#' @param extra Named list of other engine sets, e.g. `list("Call Volume" =
#'   c(8, 19))`. `NULL` for none.
#' @param label Prefix for the rescue scenario's rows, e.g. `"COA 1"`.
#' @param baseline_name Name of the baseline row (today's rescues and engines).
#' @param rescues_only_name Name of the row for today's rescues alone.
#'
#' @return A scenarios tibble (`scenario_id`, `scenario_name`, `stations`,
#'   `baseline`).
#' @noRd
als_summary_scenarios <- function(
  baseline_rescues,
  rescue_stations,
  current_engines,
  p_median_open = list(),
  extra = NULL,
  label = "Rescues",
  baseline_name = "Current Setup",
  rescues_only_name = "Baseline Rescues"
) {
  if (length(extra) > 0 && (is.null(names(extra)) || any(names(extra) == ""))) {
    stop("extra must be a named list of engine sets", call. = FALSE)
  }
  rescue_stations <- unlist(rescue_stations)
  for (nm in names(extra)) {
    overlap <- intersect(extra[[nm]], rescue_stations)
    if (length(overlap) > 0) {
      warning(
        label, " + ", nm, ": engines at rescue stations add no location: ",
        paste(overlap, collapse = ", "),
        call. = FALSE
      )
    }
  }
  with_rescues <- function(engines) sort(union(rescue_stations, unlist(engines)))

  names_out <- c(
    baseline_name,
    rescues_only_name,
    paste(label, "Rescues"),
    paste(label, "+ Current ALS"),
    # paste() of nothing is "", not character(0): only when there are some
    if (length(p_median_open) > 0) paste0(label, " + P-median #", seq_along(p_median_open)),
    if (length(extra) > 0) paste(label, "+", names(extra))
  )
  stations <- c(
    list(
      sort(union(unlist(baseline_rescues), unlist(current_engines))),
      sort(unlist(baseline_rescues)),
      sort(rescue_stations),
      with_rescues(current_engines)
    ),
    lapply(p_median_open, function(s) sort(unlist(s))),
    lapply(unname(extra), with_rescues)
  )
  tibble::tibble(
    scenario_id = seq_along(names_out),
    scenario_name = names_out,
    stations = stations,
    baseline = c(TRUE, rep(FALSE, length(names_out) - 1))
  ) |>
    validate_scenarios()
}

#' Short codes for ALS plans
#'
#' A plan found by several methods (the same stations) has all their codes,
#' joined by `=`: `GM=GB=PM1`.
#'
#' | Code | Method |
#' |---|---|
#' | `GM` | Greedy Moves |
#' | `GB` | Greedy Build-Up |
#' | `PM1`, `PM2`, ... | weighted p-median solutions, numbered by run card rank |
#' | `CV` | Call Volume |
#'
#' Any other method keeps its own name as its code.
#'
#' @param methods List with one character vector per plan: the names of the
#'   methods that found it (`Greedy Moves`, `Greedy Build-Up`, `Call Volume`,
#'   `P-median #<optimizer rank>`, or others).
#' @param total_minutes Run card total minutes of each plan, in the same order.
#'   The plans with a p-median solution are numbered `PM1`, `PM2`, ... from the
#'   fewest minutes.
#'
#' @return One code per plan.
#' @noRd
als_plan_codes <- function(methods, total_minutes) {
  if (length(methods) != length(total_minutes)) {
    stop("methods and total_minutes must have one entry per plan", call. = FALSE)
  }
  known <- c("Greedy Moves" = "GM", "Greedy Build-Up" = "GB", "Call Volume" = "CV")
  has_pm <- vapply(methods, function(m) any(grepl("^P-median #", m)), logical(1))
  pm_rank <- rep(NA_integer_, length(methods))
  pm_rank[has_pm] <- rank(total_minutes[has_pm], ties.method = "first")
  vapply(seq_along(methods), function(k) {
    m <- methods[[k]]
    parts <- ifelse(
      grepl("^P-median #", m), paste0("PM", pm_rank[k]),
      ifelse(m %in% names(known), unname(known[m]), m)
    )
    parts <- unique(parts)
    order_key <- match(sub("[0-9]+$", "", parts), c("GM", "GB", "PM", "CV"))
    paste(parts[order(order_key, na.last = TRUE)], collapse = "=")
  }, character(1))
}

#' Names for an ALS summary's scenarios: the rescue scenario's name, with the
#' engine method's code in parentheses
#'
#' The rows are those of [als_summary_scenarios()], in order: today's rescues
#' with today's engines (`Baseline Rescues (CUR)`, the baseline), today's
#' rescues alone, the rescue scenario alone (its own name), with today's
#' engines (`<name> (CUR)`), then each plan (`<name> (<code>)`).
#'
#' @param rescue_name The rescue scenario's name, e.g. `"R26->16 +R1"`.
#' @param codes Plan codes from [als_plan_codes()], in plan order.
#' @param baseline_name Name of today's rescues alone.
#'
#' @return One name per scenario row.
#' @noRd
als_scenario_names <- function(rescue_name, codes, baseline_name = "Baseline Rescues") {
  c(
    paste0(baseline_name, " (CUR)"),
    baseline_name,
    rescue_name,
    paste0(rescue_name, " (CUR)"),
    if (length(codes) > 0) paste0(rescue_name, " (", codes, ")")
  )
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
