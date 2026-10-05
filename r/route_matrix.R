library(osrm)
library(tidyverse)
library(sf)

options(
  osrm.server  = "http://127.0.0.1:5000/",
  osrm.profile = "car"
)

# osrm-routed must be started with --max-table-size >= chunk_size + number of stations
chunk_size <- 900


station_df <- read.csv("datasets/stations.csv", stringsAsFactors = FALSE)
address_df <- read.csv("datasets/addresses.csv", stringsAsFactors = FALSE) %>%
  distinct(siteaddid, .keep_all = TRUE)
fdz_df <- read.csv("datasets/fdzs.csv", stringsAsFactors = FALSE)
cad_address_df <- read.csv("datasets/2026-03-16_cad_incident_locations.csv", stringsAsFactors = FALSE) %>%
  distinct(IncidentNumber, .keep_all = TRUE)

# CAD exports some incidents at (0, 0); OSRM would snap these to the edge of the map
bad_coords <- with(cad_address_df, is.na(Latitude) | is.na(Longitude) | Latitude == 0 | Longitude == 0)
cat("Dropping", sum(bad_coords), "CAD incidents with missing or (0, 0) coordinates\n")
cad_address_df <- cad_address_df[!bad_coords, ]

station_sf <- station_df %>%
  mutate(shape_wkt_2240 = shape_wkt) %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) %>%
  st_transform(crs = 4326)

address_sf <- address_df %>%
  mutate(shape_wkt_2240 = shape_wkt) %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) %>%
  st_transform(crs = 4326)

fdz_sf <- fdz_df %>%
  mutate(shape_wkt_2240 = shape_wkt) %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

cad_address_sf <- cad_address_df %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )


# Long table of drive distance (m) and duration (min) from every src point to every
# dst point, plus snap_dist_m: how far OSRM moved each dst point to reach the road
# network. Large snap distances mean the travel time is unreliable for that point.
build_od_matrix <- function(src_sf, dst_sf, src_ids, dst_ids) {
  n <- nrow(dst_sf)
  starts <- seq(1, n, by = chunk_size)
  tabs <- vector("list", length(starts))

  for (idx in seq_along(starts)) {
    i <- starts[idx]
    j <- min(i + chunk_size - 1, n)

    tabs[[idx]] <- osrmTable(
      src = st_geometry(src_sf),
      dst = st_geometry(dst_sf)[i:j],
      measure = c("distance", "duration")
    )

    pct <- round(100 * idx / length(starts), 1)
    cat("Chunk", idx, "of", length(starts),
        "| rows", i, "to", j,
        "|", pct, "% complete\n")
  }

  dist_matrix <- do.call(cbind, map(tabs, "distances"))
  dur_matrix  <- do.call(cbind, map(tabs, "durations"))

  snapped_sf <- map(tabs, "destinations") %>%
    bind_rows() %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  snap_dist_m <- round(as.numeric(st_distance(st_geometry(dst_sf), snapped_sf, by_element = TRUE)))

  # Matrices are stations x destinations; as.vector() reads column by column
  n_src <- length(src_ids)
  tibble(
    station_id   = rep(src_ids, times = n),
    dest_id      = rep(dst_ids, each = n_src),
    distance_m   = as.vector(dist_matrix),
    duration_min = as.vector(dur_matrix),
    snap_dist_m  = rep(snap_dist_m, each = n_src)
  )
}

station_info <- station_sf %>%
  transmute(station_id = station_int, station_address = address,
            station_shape = st_as_text(shape_wkt, digits = 10), station_shape_2240 = shape_wkt_2240) %>%
  st_drop_geometry()


### Cad Calls

cad_od <- build_od_matrix(station_sf, cad_address_sf, station_sf$station_int, cad_address_sf$IncidentNumber) %>%
  rename(incident_number = dest_id)

cad_info <- cad_address_sf %>%
  st_join(select(fdz_sf, fdz_station = station, firedemandzone, fdz), join = st_intersects, left = TRUE) %>%
  distinct(IncidentNumber, .keep_all = TRUE) %>%
  transmute(incident_number = IncidentNumber, cad_incident_key = IncidentKey,
            incident_lat = Latitude, incident_long = Longitude, incident_shape = st_as_text(geometry, digits = 10),
            fdz_station, firedemandzone, fdz) %>%
  st_drop_geometry()

od_long <- cad_od %>%
  left_join(station_info, by = "station_id") %>%
  left_join(cad_info, by = "incident_number") %>%
  select(station_id, station_address, station_shape, station_shape_2240,
         incident_number, cad_incident_key, incident_lat, incident_long, incident_shape,
         fdz_station, firedemandzone, fdz, distance_m, duration_min, snap_dist_m)

write_csv(od_long, "datasets/cad_incident_travel_matrix.csv", na = "")


od_long_minimal <- od_long %>%
  select(station_id, incident_number, fdz, distance_m, duration_min, snap_dist_m)

write_csv(od_long_minimal, "datasets/cad_incident_travel_matrix_minimal.csv", na = "")


### All Addresses

address_od <- build_od_matrix(station_sf, address_sf, station_sf$station_int, address_sf$siteaddid) %>%
  rename(dest_address_id = dest_id)

address_info <- address_sf %>%
  transmute(dest_address_id = siteaddid, dest_address = fulladdr,
            dest_shape = st_as_text(shape_wkt, digits = 10), dest_shape_2240 = shape_wkt_2240) %>%
  st_drop_geometry()

od_long <- address_od %>%
  left_join(station_info, by = "station_id") %>%
  left_join(address_info, by = "dest_address_id") %>%
  select(station_id, station_address, station_shape, station_shape_2240,
         dest_address_id, dest_address, dest_shape, dest_shape_2240,
         distance_m, duration_min, snap_dist_m)

write_csv(od_long, "datasets/travel_matrix.csv", na = "")


od_long_minimal <- od_long %>%
  select(station_id, dest_address_id, distance_m, duration_min, snap_dist_m)

write_csv(od_long_minimal, "datasets/travel_matrix_minimal.csv", na = "")
