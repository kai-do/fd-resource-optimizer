# Synthetic stations in the shape fread() gives for station_addresses.sql.
# Names, addresses and points are made up.
make_station_df <- function() {
  data.frame(
    abbrev = c("F 3", "F11", "FHQ", "F 1"),
    name = c("Station 3", "Station 11", "Headquarters", "Station 1"),
    address = c("1 Test St", "2 Test St", "3 Test St", "4 Test St"),
    dept = c("Cobb", "Cobb", "Cobb", "Other"),
    shape_wkt = c(
      "POINT (2200000 1380000)",
      "POINT (2201000 1381000)",
      "POINT (2202000 1382000)",
      "POINT (2203000 1383000)"
    )
  )
}

test_that("prep_stations keeps the chosen department and drops headquarters", {
  out <- prep_stations(make_station_df())

  expect_equal(out$station_name, c("Station 3", "Station 11"))
  expect_equal(out$dept, c("Cobb", "Cobb"))
})

test_that("prep_stations takes the station number from abbrev, spaces and all", {
  expect_equal(prep_stations(make_station_df())$station_id, c(3L, 11L))
})

test_that("prep_stations converts the points to WGS84 latitude and longitude", {
  out <- prep_stations(make_station_df())
  expected <- sf::st_as_sfc("POINT (2200000 1380000)", crs = 2240) |>
    sf::st_transform(4326) |>
    sf::st_coordinates()

  expect_equal(out$latitude[1], expected[, "Y"][[1]])
  expect_equal(out$longitude[1], expected[, "X"][[1]])
  # A sanity check that latitude and longitude are not swapped
  expect_true(all(out$latitude > 30 & out$latitude < 36))
  expect_true(all(out$longitude < -80 & out$longitude > -90))
})

test_that("prep_stations returns the columns build_travel_matrix() expects", {
  out <- prep_stations(make_station_df())

  expect_named(
    out,
    c("station_id", "station_name", "dept", "station_address", "latitude", "longitude")
  )
  expect_false(inherits(out, "sf"))
})

test_that("prep_stations keeps several departments when asked", {
  out <- prep_stations(make_station_df(), keep_dept = c("Cobb", "Other"))

  expect_equal(out$station_id, c(3L, 11L, 1L))
})

test_that("prep_stations stops when an abbrev has no station number", {
  station_df <- make_station_df()
  station_df$abbrev[2] <- "FXX"

  expect_error(prep_stations(station_df), "No station number in abbrev: FXX")
})

test_that("prep_stations stops when two stations share a number", {
  station_df <- make_station_df()
  station_df$abbrev[2] <- "F03"

  expect_error(prep_stations(station_df), "Duplicate station numbers: 3")
})
