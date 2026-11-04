# These tests never contact an OSRM server. osrm::osrmTable() is replaced by
# a fake whose answers encode which source and destination they belong to,
# so the tests can check that build_travel_matrix() pairs every distance with
# the right station and incident, including across chunks.

# Synthetic points. The row number is hidden in the latitude: source s sits at
# 33 + s / 100 and destination d at 34 + d / 1000, so the fake can recover s
# and d from the coordinates it is sent.
make_sources <- function(n = 3) {
  data.frame(
    station_id = seq_len(n),
    latitude = 33 + seq_len(n) / 100,
    longitude = -84.5
  )
}

make_destinations <- function(n = 5) {
  data.frame(
    incident_number = paste0("D", seq_len(n)),
    latitude = 34 + seq_len(n) / 1000,
    longitude = -84.6
  )
}

# Stand-in for osrm::osrmTable(). Distance from source s to destination d is
# 1000 * s + d metres and the duration is a tenth of that in minutes. The
# snapped points are the input points, so snap distances are 0.
fake_osrm_table <- function(src, dst, measure, ...) {
  src_xy <- sf::st_coordinates(src)
  dst_xy <- sf::st_coordinates(dst)
  s <- round((src_xy[, "Y"] - 33) * 100)
  d <- round((dst_xy[, "Y"] - 34) * 1000)
  distances <- outer(s, d, function(s, d) 1000 * s + d)
  list(
    durations = distances / 10,
    distances = distances,
    sources = data.frame(lon = src_xy[, "X"], lat = src_xy[, "Y"]),
    destinations = data.frame(lon = dst_xy[, "X"], lat = dst_xy[, "Y"])
  )
}

# Run build_travel_matrix() against the fake, hiding its progress output.
run_matrix <- function(...) {
  local_mocked_bindings(osrmTable = fake_osrm_table, .package = "osrm")
  out <- NULL
  utils::capture.output(out <- build_travel_matrix(...))
  out
}

# Expected fake distance for each output row, from its ids
expected_distance <- function(out) {
  1000 * out$source_id + as.integer(sub("D", "", out$dest_id))
}

# Pairing ----------------------------------------------------------------------

test_that("every row pairs the distance with the right source and destination", {
  out <- run_matrix(make_sources(), make_destinations())

  expect_equal(nrow(out), 3 * 5)
  expect_equal(out$distance_m, expected_distance(out))
  expect_equal(out$duration_min, expected_distance(out) / 10)
})

test_that("pairing holds when destinations are split across chunks", {
  # 5 destinations in chunks of 2 means 3 requests, the last one short
  calls <- 0
  counting_fake <- function(...) {
    calls <<- calls + 1
    fake_osrm_table(...)
  }
  local_mocked_bindings(osrmTable = counting_fake, .package = "osrm")
  utils::capture.output(
    out <- build_travel_matrix(make_sources(), make_destinations(), chunk_size = 2)
  )

  expect_equal(calls, 3)
  expect_equal(nrow(out), 3 * 5)
  expect_equal(out$distance_m, expected_distance(out))
})

test_that("each id pair appears exactly once", {
  out <- run_matrix(make_sources(), make_destinations(), chunk_size = 2)

  expect_false(anyDuplicated(out[c("source_id", "dest_id")]) > 0)
  expect_setequal(unique(out$source_id), 1:3)
  expect_setequal(unique(out$dest_id), paste0("D", 1:5))
})

test_that("coordinates in the output belong to their own ids", {
  src <- make_sources()
  dst <- make_destinations()
  out <- run_matrix(src, dst, chunk_size = 2)

  expect_equal(out$source_lat, src$latitude[match(out$source_id, src$station_id)])
  expect_equal(out$dest_lat, dst$latitude[match(out$dest_id, dst$incident_number)])
})

test_that("snap distances are 0 when OSRM returns the input points", {
  out <- run_matrix(make_sources(), make_destinations(), chunk_size = 2)

  expect_true(all(out$source_snap_m == 0))
  expect_true(all(out$dest_snap_m == 0))
})

test_that("the output has the documented columns", {
  out <- run_matrix(make_sources(), make_destinations())

  expect_named(
    out,
    c(
      "source_id", "dest_id",
      "source_lat", "source_lon", "dest_lat", "dest_lon",
      "distance_m", "duration_min",
      "source_snap_m", "dest_snap_m"
    )
  )
})

test_that("custom id and coordinate column names work", {
  src <- make_sources()
  names(src) <- c("sid", "y", "x")

  out <- run_matrix(src, make_destinations(), src_id = "sid", src_lat = "y", src_lon = "x")

  expect_equal(out$distance_m, expected_distance(out))
})

# Input checks (these stop before any OSRM call) ------------------------------

test_that("duplicate ids are an error", {
  dst <- make_destinations()
  dst$incident_number[2] <- "D1"

  expect_error(run_matrix(make_sources(), dst), "ids in 'incident_number' must be unique")
})

test_that("missing or (0, 0) coordinates are an error", {
  dst <- make_destinations()
  dst$latitude[1] <- NA
  dst$longitude[2] <- 0

  expect_error(run_matrix(make_sources(), dst), "2 destination rows have missing or \\(0, 0\\)")
})

test_that("a missing column is an error", {
  src <- make_sources()
  src$longitude <- NULL

  expect_error(run_matrix(src, make_destinations()))
})
