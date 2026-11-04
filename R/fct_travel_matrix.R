#' Build a drive-time and distance matrix with OSRM
#'
#' Routes every source (e.g. station) to every destination (e.g. incident) on
#' the OSRM server set in `options(osrm.server = )`, and returns one row per
#' source-destination pair. Destinations are sent in chunks of `chunk_size` so
#' each request stays under the server's table-size limit.
#'
#' The server must be started with `--max-table-size` at least
#' `chunk_size + nrow(src_df)`. Progress is printed after each chunk.
#'
#' @param src_df,dst_df Data frames of sources and destinations with WGS84
#'   (EPSG:4326) coordinates. Clean `dst_df` with [clean_incidents()] first:
#'   missing or (0, 0) coordinates and duplicate ids are an error here.
#' @param src_id,dst_id Names (strings) of the unique id columns.
#' @param src_lat,src_lon,dst_lat,dst_lon Names (strings) of the latitude and
#'   longitude columns.
#' @param chunk_size Number of destinations per OSRM request.
#'
#' @return A tibble with `nrow(src_df) * nrow(dst_df)` rows:
#'   * `source_id`, `dest_id`: the ids.
#'   * `source_lat`, `source_lon`, `dest_lat`, `dest_lon`: input coordinates.
#'   * `distance_m`: driving distance in metres.
#'   * `duration_min`: driving time in minutes.
#'   * `source_snap_m`, `dest_snap_m`: metres from each input point to the
#'     road point OSRM snapped it to. Large values mean the point is far from
#'     any road and its times are less reliable.
#' @noRd
build_travel_matrix <- function(
  src_df,
  dst_df,
  src_id = "station_id",
  dst_id = "incident_number",
  src_lat = "latitude",
  src_lon = "longitude",
  dst_lat = "latitude",
  dst_lon = "longitude",
  chunk_size = 900
) {
  to_sf <- function(df, id, lat, lon, label) {
    stopifnot(all(c(id, lat, lon) %in% names(df)))
    if (anyDuplicated(df[[id]])) {
      stop(label, " ids in '", id, "' must be unique")
    }
    # (0, 0) is how CAD exports missing coordinates; OSRM would snap it to the edge of the map
    bad <- is.na(df[[lat]]) | is.na(df[[lon]]) | df[[lat]] == 0 | df[[lon]] == 0
    if (any(bad)) {
      stop(sum(bad), " ", label, " rows have missing or (0, 0) coordinates")
    }
    sf::st_as_sf(
      df[c(id, lat, lon)],
      coords = c(lon, lat),
      crs = 4326,
      remove = FALSE
    )
  }

  src_sf <- to_sf(src_df, src_id, src_lat, src_lon, "source")
  dst_sf <- to_sf(dst_df, dst_id, dst_lat, dst_lon, "destination")

  n_src <- nrow(src_sf)
  n_dst <- nrow(dst_sf)
  starts <- seq(1, n_dst, by = chunk_size)
  tabs <- vector("list", length(starts))

  for (idx in seq_along(starts)) {
    i <- starts[idx]
    j <- min(i + chunk_size - 1, n_dst)

    tabs[[idx]] <- osrm::osrmTable(
      src = sf::st_geometry(src_sf),
      dst = sf::st_geometry(dst_sf)[i:j],
      measure = c("distance", "duration")
    )

    cat(
      "Chunk",
      idx,
      "of",
      length(starts),
      "| rows",
      i,
      "to",
      j,
      "|",
      round(100 * idx / length(starts), 1),
      "% complete\n"
    )
  }

  snap_dist <- function(orig_sf, snapped) {
    snapped_sf <- sf::st_as_sf(snapped, coords = c("lon", "lat"), crs = 4326)
    round(as.numeric(sf::st_distance(
      sf::st_geometry(orig_sf),
      snapped_sf,
      by_element = TRUE
    )))
  }

  dist_matrix <- do.call(cbind, purrr::map(tabs, "distances"))
  dur_matrix <- do.call(cbind, purrr::map(tabs, "durations"))

  # Matrices are sources x destinations; as.vector() reads column by column
  tibble::tibble(
    source_id = rep(src_sf[[src_id]], times = n_dst),
    dest_id = rep(dst_sf[[dst_id]], each = n_src),
    source_lat = rep(src_sf[[src_lat]], times = n_dst),
    source_lon = rep(src_sf[[src_lon]], times = n_dst),
    dest_lat = rep(dst_sf[[dst_lat]], each = n_src),
    dest_lon = rep(dst_sf[[dst_lon]], each = n_src),
    distance_m = as.vector(dist_matrix),
    duration_min = as.vector(dur_matrix),
    source_snap_m = rep(snap_dist(src_sf, tabs[[1]]$sources), times = n_dst),
    dest_snap_m = rep(
      snap_dist(dst_sf, dplyr::bind_rows(purrr::map(tabs, "destinations"))),
      each = n_src
    )
  )
}
