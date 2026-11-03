library(osrm)
library(tidyverse)
library(sf)

options(
  osrm.server  = "http://127.0.0.1:5000/",
  osrm.profile = "car"
)


# Long table with one row per source x destination pair: drive distance (m) and duration (min),
# plus how far OSRM had to move each endpoint to reach the road network (*_snap_m).
# Large snap distances mean the travel time is unreliable for that point.
#
# src_df / dst_df: data frames with an id column and WGS84 latitude / longitude columns.
# Ids must be unique; row order of the inputs does not matter for the result.
# osrm-routed must be started with --max-table-size >= chunk_size + nrow(src_df).
build_travel_matrix <- function(src_df, dst_df,
                                src_id = "station_id", dst_id = "incident_number",
                                src_lat = "latitude", src_lon = "longitude",
                                dst_lat = "latitude", dst_lon = "longitude",
                                chunk_size = 900) {
  to_sf <- function(df, id, lat, lon, label) {
    stopifnot(all(c(id, lat, lon) %in% names(df)))
    if (anyDuplicated(df[[id]])) stop(label, " ids in '", id, "' must be unique")
    # (0, 0) is how CAD exports missing coordinates; OSRM would snap it to the edge of the map
    bad <- is.na(df[[lat]]) | is.na(df[[lon]]) | df[[lat]] == 0 | df[[lon]] == 0
    if (any(bad)) stop(sum(bad), " ", label, " rows have missing or (0, 0) coordinates")
    st_as_sf(df[c(id, lat, lon)], coords = c(lon, lat), crs = 4326, remove = FALSE)
  }

  src_sf <- to_sf(src_df, src_id, src_lat, src_lon, "source")
  dst_sf <- to_sf(dst_df, dst_id, dst_lat, dst_lon, "destination")

  n_src  <- nrow(src_sf)
  n_dst  <- nrow(dst_sf)
  starts <- seq(1, n_dst, by = chunk_size)
  tabs   <- vector("list", length(starts))

  for (idx in seq_along(starts)) {
    i <- starts[idx]
    j <- min(i + chunk_size - 1, n_dst)

    tabs[[idx]] <- osrmTable(
      src = st_geometry(src_sf),
      dst = st_geometry(dst_sf)[i:j],
      measure = c("distance", "duration")
    )

    cat("Chunk", idx, "of", length(starts),
        "| rows", i, "to", j,
        "|", round(100 * idx / length(starts), 1), "% complete\n")
  }

  snap_dist <- function(orig_sf, snapped) {
    snapped_sf <- st_as_sf(snapped, coords = c("lon", "lat"), crs = 4326)
    round(as.numeric(st_distance(st_geometry(orig_sf), snapped_sf, by_element = TRUE)))
  }

  dist_matrix <- do.call(cbind, map(tabs, "distances"))
  dur_matrix  <- do.call(cbind, map(tabs, "durations"))

  # Matrices are sources x destinations; as.vector() reads column by column
  tibble(
    source_id     = rep(src_sf[[src_id]], times = n_dst),
    dest_id       = rep(dst_sf[[dst_id]], each = n_src),
    source_lat    = rep(src_sf[[src_lat]], times = n_dst),
    source_lon    = rep(src_sf[[src_lon]], times = n_dst),
    dest_lat      = rep(dst_sf[[dst_lat]], each = n_src),
    dest_lon      = rep(dst_sf[[dst_lon]], each = n_src),
    distance_m    = as.vector(dist_matrix),
    duration_min  = as.vector(dur_matrix),
    source_snap_m = rep(snap_dist(src_sf, tabs[[1]]$sources), times = n_dst),
    dest_snap_m   = rep(snap_dist(dst_sf, bind_rows(map(tabs, "destinations"))), each = n_src)
  )
}
