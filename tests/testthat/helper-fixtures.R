# Synthetic fixtures shared by the tests. testthat sources helper-*.R files
# before running tests. Nothing here is real data: the polygons are made-up
# squares in EPSG:2240 (feet) and the incident numbers are invented.

# A square polygon as WKT, with its lower-left corner at (x, y) in EPSG:2240.
square_wkt <- function(x, y, size = 1000) {
  sprintf(
    "POLYGON ((%1$s %2$s, %3$s %2$s, %3$s %4$s, %1$s %4$s, %1$s %2$s))",
    x,
    y,
    x + size,
    y + size
  )
}

# FDZ rows in the shape fread() gives for fdzs.sql: `station` is integer and
# `shape_wkt` is text in EPSG:2240.
make_fdz_df <- function() {
  data.frame(
    station = c(1L, 1L),
    fire_demand_zone = c("A", "B"),
    shape_wkt = c(
      square_wkt(2200000, 1380000), # 1A
      square_wkt(2201000, 1380000) # 1B, east of 1A, sharing an edge
    )
  )
}

# WGS84 lon/lat for a point given in EPSG:2240, so incidents can be placed
# inside or outside the fixture polygons.
lonlat_from_2240 <- function(x, y) {
  sf::st_sfc(sf::st_point(c(x, y)), crs = 2240) |>
    sf::st_transform(4326) |>
    sf::st_coordinates()
}

# Incidents in the shape of the incident extract: one in 1A, one in 1B and
# one outside every polygon. The extract's own CAD `fdz` column is included
# (with deliberately wrong values) to check it is not used.
make_incident_df <- function() {
  pts <- rbind(
    lonlat_from_2240(2200500, 1380500), # inside 1A
    lonlat_from_2240(2201500, 1380500), # inside 1B
    lonlat_from_2240(2210000, 1390000) # outside both
  )
  data.frame(
    incident_number = c("CF-0000-00-00-00000001", "CF-0000-00-00-00000002", "CF-0000-00-00-00000003"),
    latitude = pts[, "Y"],
    longitude = pts[, "X"],
    fdz = c("9Z", "9Z", "9Z")
  )
}

# A synthetic run card in the shape of cad_run_card.sql (read with fread):
#
#   beat 1A, Cobb card:     CF/01, CF/07, MF/51, CF/22
#   beat 2B, Marietta card: MF/51, CF/07, CF/01
#   beat 3C, two cards:     card 3: CF/03, CF/01   card 4: CF/03, CF/22
#   a Location card for 1A (not a beat, so ignored): CF/09
make_runcard_raw_df <- function() {
  data.frame(
    location_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 4L, 5L),
    location_type = c(rep("Beat", 11), "Location"),
    location = c(rep("1A", 4), rep("2B", 3), rep("3C", 4), "1A"),
    agency = c(rep("CF", 4), rep("MF", 3), rep("CF", 4), "CF"),
    responding_value = c(
      "CF/01", "CF/07", "MF/51", "CF/22",
      "MF/51", "CF/07", "CF/01",
      "CF/03", "CF/01", "CF/03", "CF/22",
      "CF/09"
    ),
    response_sequence = c(1:4, 1:3, 1:2, 1:2, 1L)
  )
}

# A mini matrix on the run card in helper-fixtures.R. Run card responders:
#
#   1A: 1, 7, 22     2B: 7, 1     3C: 3, 1, 22
#
# Incidents I1 and I2 are in 1A, I3 in 2B, I4 in 3C. Every station has a row
# for every incident, with duration = station id + incident number / 10, so
# I2 from station 7 takes 7.2 minutes. I2 is the only night call.
make_scenario_matrix_df <- function() {
  incidents <- data.frame(
    dest_id = c("I1", "I2", "I3", "I4"),
    fdz_id = c("1A", "1A", "2B", "3C"),
    station_id = c(1L, 1L, 51L, 3L),
    incident_alarm_dt = as.POSIXct(
      c("2024-01-01 12:00", "2024-01-01 23:00", "2024-01-02 12:00", "2024-01-03 12:00"),
      tz = "America/New_York"
    ),
    ems_unit_capability = c("PARAMEDIC", "AEMT", "PARAMEDIC", NA),
    offset = c(0.1, 0.2, 0.3, 0.4)
  )
  df <- merge(data.frame(source_id = c(1L, 3L, 7L, 22L)), incidents)
  df$duration_min <- df$source_id + df$offset
  df$distance_m <- df$duration_min * 1000
  df$offset <- NULL
  df[order(df$dest_id, df$source_id), ]
}

# "All" is the baseline for the compare_*() tests; stats_by_*() ignore it
make_scenarios <- function() {
  tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "No 1", c(3, 7, 22), FALSE
  )
}
