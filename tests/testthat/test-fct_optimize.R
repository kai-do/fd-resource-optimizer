skip_if_not_installed("ompr.roi")
skip_if_not_installed("ROI.plugin.glpk")

# A hand-made travel stats table: 3 stations x 3 zones, with times and demand
# chosen so every answer can be worked out by hand.
#
#   mean_minutes   Z1  Z2  Z3
#   station 1       1   5   9
#   station 2       6   2   6
#   station 3       9   4   1
#   n_incidents    10   2   8
#
# Total minutes (demand x time from the cheapest open station):
#   p = 1: {1} 10 + 10 + 72 = 92   {2} 60 + 4 + 48 = 112   {3} 90 + 8 + 8 = 106
#   p = 2: {1,3} 10 + 8 + 8 = 26   {1,2} 10 + 4 + 48 = 62  {2,3} 60 + 4 + 8 = 72
make_stats_df <- function() {
  data.frame(
    source_id = rep(1:3, times = 3),
    fdz_id = rep(c("Z1", "Z2", "Z3"), each = 3),
    n_incidents = rep(c(10, 2, 8), each = 3),
    mean_minutes = c(1, 6, 9, 5, 2, 4, 9, 6, 1)
  )
}

test_that("solve_p_median picks the best single station", {
  out <- solve_p_median(make_stats_df(), p = 1)

  expect_equal(out$open_stations, 1L)
  expect_equal(out$objective, 92)
})

test_that("solve_p_median picks the best pair and serves each zone from its cheapest open station", {
  out <- solve_p_median(make_stats_df(), p = 2)

  expect_equal(out$open_stations, c(1L, 3L))
  expect_equal(out$objective, 26)
  expect_equal(out$assignments$fdz_id, c("Z1", "Z2", "Z3"))
  expect_equal(out$assignments$source_id, c(1L, 3L, 3L))
  expect_equal(out$assignments$n_incidents, c(10, 2, 8))
  expect_equal(out$assignments$mean_minutes, c(1, 4, 1))
})

test_that("solve_p_median keeps required stations open and excluded ones closed", {
  forced <- solve_p_median(make_stats_df(), p = 2, include = 2)
  expect_equal(forced$open_stations, c(1L, 2L))
  expect_equal(forced$objective, 62)

  closed <- solve_p_median(make_stats_df(), p = 2, exclude = 1)
  expect_equal(closed$open_stations, c(2L, 3L))
  expect_equal(closed$objective, 72)
})

test_that("solve_p_median returns distinct next-best sets, best first", {
  out <- solve_p_median(make_stats_df(), p = 2, n_solutions = 5)

  # only 3 pairs exist, so it stops there
  expect_length(out$alternatives, 3)
  expect_equal(
    lapply(out$alternatives, `[[`, "open_stations"),
    list(c(1L, 3L), c(1L, 2L), c(2L, 3L))
  )
  expect_equal(vapply(out$alternatives, `[[`, numeric(1), "objective"), c(26, 62, 72))
  expect_equal(out$open_stations, c(1L, 3L))
})

test_that("solve_p_median uses the cost column it is given", {
  df <- make_stats_df()
  df$p90_minutes <- df$mean_minutes
  df$p90_minutes[df$source_id == 2 & df$fdz_id == "Z3"] <- 0 # station 2 now best for Z3

  out <- solve_p_median(df, p = 2, cost = "p90_minutes")

  # {1,2} 10 + 4 + 0 = 14 beats {1,3} 26
  expect_equal(out$open_stations, c(1L, 2L))
  expect_equal(out$objective, 14)
  expect_named(out$assignments, c("fdz_id", "source_id", "n_incidents", "p90_minutes"))
})

test_that("solve_p_median drops rows with no zone", {
  df <- rbind(
    make_stats_df(),
    data.frame(source_id = 2L, fdz_id = NA, n_incidents = 1000, mean_minutes = 0)
  )

  out <- solve_p_median(df, p = 2)

  expect_equal(out$open_stations, c(1L, 3L))
  expect_equal(nrow(out$assignments), 3)
})

test_that("solve_p_median stops on bad inputs", {
  df <- make_stats_df()

  expect_error(solve_p_median(df, p = 4), "larger than the number of stations")
  expect_error(solve_p_median(df, p = 2, include = 7), "unknown station")
  expect_error(solve_p_median(df, p = 2, include = 1, exclude = 1), "both required and excluded")
  expect_error(solve_p_median(df, p = 1, include = 1:2), "more required stations than p")
  expect_error(solve_p_median(df, p = 2, exclude = 1:2), "too many excluded")
  expect_error(solve_p_median(df[-1, ], p = 2), "missing or NA costs")
  expect_error(solve_p_median(df[, -4], p = 2), "missing column")
})
