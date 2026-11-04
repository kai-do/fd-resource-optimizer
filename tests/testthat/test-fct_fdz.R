test_that("prep_fdz_polygons builds fdz_id from station and zone letter", {
  out <- prep_fdz_polygons(make_fdz_df())

  expect_equal(out$fdz_id, c("1A", "1B"))
  expect_equal(out$station_id, c(1L, 1L))
  expect_false("station" %in% names(out))
})

test_that("a zone with no station becomes LA, not NAA", {
  # The real extract has one zone with station = NULL (read as NA) and zone
  # letter A; paste0() turns that into "NAA".
  fdz_df <- data.frame(
    station = NA_integer_,
    fire_demand_zone = "A",
    shape_wkt = square_wkt(2200000, 1380000)
  )

  out <- prep_fdz_polygons(fdz_df)

  expect_equal(out$fdz_id, "LA")
  expect_true(is.na(out$station_id))
})

test_that("prep_fdz_polygons returns sf in the requested CRS", {
  expect_s3_class(prep_fdz_polygons(make_fdz_df()), "sf")
  expect_equal(sf::st_crs(prep_fdz_polygons(make_fdz_df()))$epsg, 2240)
  expect_equal(
    sf::st_crs(prep_fdz_polygons(make_fdz_df(), crs = 2239))$epsg,
    2239
  )
})

test_that("prep_fdz_polygons repairs self-intersecting polygons", {
  # The real extract has one zone (16I) whose ring crosses itself. A bowtie is
  # the smallest shape with the same problem: two 10 x 10 / 2 triangles that
  # meet at (5, 5).
  bowtie <- "POLYGON ((0 0, 10 10, 10 0, 0 10, 0 0))"
  fdz_df <- data.frame(station = 3L, fire_demand_zone = "D", shape_wkt = bowtie)

  # Check the fixture really is invalid, so the test can't pass by accident
  expect_false(sf::st_is_valid(sf::st_as_sfc(bowtie, crs = 2240)))

  out <- prep_fdz_polygons(fdz_df)

  expect_true(all(sf::st_is_valid(out)))
  # The repair splits the bowtie into its two triangles and keeps their area
  expect_equal(as.character(sf::st_geometry_type(out)), "MULTIPOLYGON")
  expect_equal(as.numeric(sf::st_area(out)), 50)
  expect_equal(out$fdz_id, "3D")
})

test_that("prep_fdz_polygons leaves valid polygons' shape unchanged", {
  out <- prep_fdz_polygons(make_fdz_df())
  before <- sf::st_as_sfc(make_fdz_df()$shape_wkt, crs = 2240)

  expect_true(all(lengths(sf::st_equals(out, before)) == 1))
})

test_that("prep_fdz_polygons accepts MULTIPOLYGON rows", {
  # The real extract mixes POLYGON and MULTIPOLYGON.
  fdz_df <- data.frame(
    station = 2L,
    fire_demand_zone = "C",
    shape_wkt = paste0(
      "MULTIPOLYGON (((0 0, 10 0, 10 10, 0 10, 0 0)), ",
      "((20 0, 30 0, 30 10, 20 10, 20 0)))"
    )
  )

  out <- prep_fdz_polygons(fdz_df)

  expect_equal(as.character(sf::st_geometry_type(out)), "MULTIPOLYGON")
  expect_equal(out$fdz_id, "2C")
})
