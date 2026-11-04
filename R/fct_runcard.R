# CAD run card order, and filtering travel data to run card assignments.
#
# prep_runcard() turns the run card extract into one row per zone and
# responding station in card order. runcard_responders() applies a set of open
# stations and renumbers the order into due levels (1st due, 2nd due, ...).
# filter_by_runcard() keeps the rows of the travel stats table, or the travel
# matrix, where the station is a zone's run card responder at the chosen level:
#
#   runcard <- prep_runcard(runcard_raw_df)
#   travel_stats |> filter_by_runcard(runcard, stations = 1:30, due = 1)

#' Prepare the run card order for each zone
#'
#' Keeps beat cards (beats are FDZs), sorts each beat by response sequence and
#' keeps only the responders of `responder_agency`, numbered in card order.
#' Responders of other agencies have no travel times, so they are dropped, but
#' a beat whose card starts with one is flagged.
#'
#' @param runcard_df Data frame from cad_run_card.sql with `location_type`,
#'   `location`, `agency`, `responding_value` (e.g. "CF/07") and
#'   `response_sequence`.
#' @param responder_agency Agency code of the stations to keep, the prefix of
#'   `responding_value`.
#'
#' @return Data frame with one row per beat card position: `fdz_id`,
#'   `source_id` (responding station number), `rank` (order on the card, ties
#'   in `response_sequence` in file order), `agency` (the beat's own agency) and
#'   `other_agency_first` (`TRUE` when the card's first responder is another
#'   agency's). Beats with two cards (71A, 72A) keep both, interleaved by
#'   sequence, so a station can appear twice; see [runcard_responders()].
#' @noRd
prep_runcard <- function(runcard_df, responder_agency = "CF") {
  prefix <- paste0(responder_agency, "/")

  runcard_df |>
    dplyr::filter(location_type == "Beat") |>
    dplyr::arrange(location, response_sequence) |>
    dplyr::mutate(
      agency = dplyr::first(agency),
      other_agency_first = !startsWith(dplyr::first(responding_value), prefix),
      .by = location
    ) |>
    dplyr::filter(startsWith(responding_value, prefix)) |>
    dplyr::mutate(rank = dplyr::row_number(), .by = location) |>
    dplyr::transmute(
      fdz_id = location,
      source_id = as.integer(stringr::str_extract(responding_value, "[0-9]+")),
      rank,
      agency,
      other_agency_first
    )
}

#' Each zone's run card responders among a set of open stations
#'
#' Drops the stations that are not open and renumbers each zone's card, so due
#' 1 is the first open station on the card. There is no fallback: a zone whose
#' card has fewer open stations than a due level has no row for that level.
#'
#' @param runcard_df Run card order from [prep_runcard()].
#' @param stations Open station ids.
#' @param due Due levels to keep, e.g. `1` or `1:2`; `NULL` keeps all.
#'
#' @return Data frame with `fdz_id`, `source_id` and `due`, one row per zone
#'   and responding station, in card order.
#' @noRd
runcard_responders <- function(runcard_df, stations, due = NULL) {
  out <- runcard_df[runcard_df$source_id %in% stations, c("fdz_id", "source_id", "rank")]
  out <- out[order(out$fdz_id, out$rank, method = "radix"), ]
  # A station on both cards of a two-card beat responds once, at its first place
  out <- out[!duplicated(out[c("fdz_id", "source_id")]), ]
  out$due <- as.integer(stats::ave(out$rank, out$fdz_id, FUN = seq_along))

  if (!is.null(due)) {
    out <- out[out$due %in% due, ]
  }
  rownames(out) <- NULL
  out[c("fdz_id", "source_id", "due")]
}

#' Keep only the run card responders at the chosen due levels
#'
#' @param df Data with `fdz_id` and `source_id`: the travel stats table (one
#'   row per station and zone) or the travel matrix (one row per station and
#'   incident).
#' @param runcard_df Run card order from [prep_runcard()].
#' @param stations Open station ids.
#' @param due Due levels to keep, e.g. `1` or `1:2`; `NULL` keeps all.
#'
#' @return The rows of `df` whose station is its zone's responder at a chosen
#'   level, with a `due` column added. On travel stats that is one row per zone
#'   and level; on the matrix, one row per incident and level. Zones with no
#'   open station at a level are dropped, with a message saying how many.
#' @noRd
filter_by_runcard <- function(df, runcard_df, stations, due = 1) {
  responders <- runcard_responders(runcard_df, stations, due = due)

  zones <- unique(df$fdz_id[!is.na(df$fdz_id)])
  # due = NULL keeps every level, so there is no level to report on
  for (d in sort(unique(due))) {
    n_missing <- length(setdiff(zones, responders$fdz_id[responders$due == d]))
    if (n_missing > 0) {
      message(n_missing, " zones have no due ", d, " station on the run card")
    }
  }

  dplyr::inner_join(
    df,
    responders,
    by = c("fdz_id", "source_id"),
    relationship = "many-to-one"
  )
}
