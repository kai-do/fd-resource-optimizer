# remove_bad_coords ----------------------------------------------------------

test_that("remove_bad_coords drops NA and (0, 0) coordinates in either column", {
  df <- data.frame(
    id = 1:6,
    latitude = c(33.9, NA, 33.9, 0, 33.9, 0),
    longitude = c(-84.5, -84.5, NA, -84.5, 0, 0)
  )

  expect_equal(remove_bad_coords(df)$id, 1L)
})

test_that("remove_bad_coords keeps every row when all coordinates are good", {
  df <- data.frame(latitude = c(33.9, 34.0), longitude = c(-84.5, -84.6))

  expect_equal(nrow(remove_bad_coords(df)), 2)
})

test_that("remove_bad_coords uses the column names it is given", {
  df <- data.frame(y = c(33.9, 0), x = c(-84.5, -84.5))

  expect_equal(nrow(remove_bad_coords(df, lat = "y", lon = "x")), 1)
})

# clean_incidents ------------------------------------------------------------

test_that("clean_incidents keeps the first row of a duplicated incident", {
  # The real extract has incidents listed twice, once per RMS source system.
  df <- data.frame(
    incident_number = c("CF-1", "CF-1", "CF-2"),
    source_system = c("NFIRS", "NERIS", "NFIRS"),
    latitude = 33.9,
    longitude = -84.5
  )

  out <- clean_incidents(df)

  expect_equal(out$incident_number, c("CF-1", "CF-2"))
  expect_equal(out$source_system, c("NFIRS", "NFIRS"))
})

test_that("clean_incidents drops bad coordinates before de-duplicating", {
  # If de-duplication ran first, the bad first row would win and the incident
  # would be lost entirely.
  df <- data.frame(
    incident_number = c("CF-1", "CF-1"),
    latitude = c(0, 33.9),
    longitude = c(0, -84.5)
  )

  out <- clean_incidents(df)

  expect_equal(nrow(out), 1)
  expect_equal(out$latitude, 33.9)
})

test_that("clean_incidents uses the id column it is given", {
  df <- data.frame(id = c(1, 1, 2), latitude = 33.9, longitude = -84.5)

  expect_equal(nrow(clean_incidents(df, incident_id = "id")), 2)
})

# assign_incident_fdz ---------------------------------------------------------

test_that("assign_incident_fdz puts each incident in the zone it falls in", {
  out <- suppressMessages(
    assign_incident_fdz(make_incident_df(), prep_fdz_polygons(make_fdz_df()))
  )

  expect_equal(out$fdz_id, c("1A", "1B", NA))
  expect_equal(out$station_id, c(1L, 1L, NA))
})

test_that("assign_incident_fdz returns only the id, zone and station columns", {
  out <- suppressMessages(
    assign_incident_fdz(make_incident_df(), prep_fdz_polygons(make_fdz_df()))
  )

  # In particular the extract's own CAD `fdz` column must not come through.
  expect_named(out, c("incident_number", "fdz_id", "station_id"))
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "sf"))
})

test_that("assign_incident_fdz reports incidents that fall in no zone", {
  expect_message(
    assign_incident_fdz(make_incident_df(), prep_fdz_polygons(make_fdz_df())),
    "^1 incidents fell in no FDZ"
  )
})

test_that("assign_incident_fdz stops when polygons overlap", {
  fdz_df <- rbind(
    make_fdz_df(),
    # 2A covers 1A, so the first incident lands in two zones
    data.frame(station = 2L, fire_demand_zone = "A", shape_wkt = square_wkt(2199900, 1379900, size = 1200))
  )

  expect_error(
    assign_incident_fdz(make_incident_df(), prep_fdz_polygons(fdz_df)),
    "more than one FDZ"
  )
})
