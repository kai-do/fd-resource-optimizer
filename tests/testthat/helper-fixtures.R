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
