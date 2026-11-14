# Build-up order on the mini fixtures (helper-fixtures.R). Base: stations 7
# and 22. Run cards: 1A 1, 7, 22; 2B 7, 1 (51 is another agency); 3C 3, 1, 22.
# So the base serves 1A from 7 (7.1, 7.2), 2B from 7 (7.3) and 3C from 22
# (22.4): 44 minutes. Territories: I1, I2 in 1; I3 in 51; I4 in 3.
#
#   + station 1: 1A 1.1, 1.2 (-12) and 3C from 1, 1.4 (-21): saves 33
#   + station 3: saves 19 (3C 3.4)                -> step 1 is station 1
#   then + 3: 3C goes to 3 (first on its card), 1.4 -> 3.4: adds 2

runcard <- function() prep_runcard(make_runcard_raw_df())

header_text <- function(ft) {
  chunks <- flextable::information_data_chunk(ft)
  unique(chunks$txt[chunks$.part == "header"])
}

build_up <- function(...) {
  build_up_order(c(7, 22), make_scenario_matrix_df(), runcard(), incident_filter("All calls"), thresholds = 4, ...)
}

test_that("each step adds the station that saves the most minutes", {
  out <- build_up(n_add = 2)
  s <- out$steps

  expect_equal(s$step, 0:2)
  expect_equal(s$station, c(NA, 1L, 3L))
  expect_equal(s$stations, c("7, 22", "1, 7, 22", "1, 3, 7, 22"))
  expect_equal(s$total_minutes, c(44, 11, 13))
  expect_equal(s$saved_minutes, c(NA, 33, -2)) # station 3 is first on 3C's card but slower
  expect_equal(s$cumulative_saved, c(NA, 33, 31))
  expect_equal(s$pct_within_4min, c(0, 0.75, 0.75))
})

test_that("the added station's own territory, before and after", {
  s <- build_up(n_add = 2)$steps

  expect_equal(s$own_n_calls, c(NA, 2, 1))
  expect_equal(s$own_total_minutes_before, c(NA, 14.3, 1.4))
  expect_equal(s$own_total_minutes_after, c(NA, 2.3, 3.4))
  expect_equal(s$own_share[2], 12 / 33) # the rest (21) is in territory 3
  expect_equal(s$own_pct_within_4min_after[2], 1)
})

test_that("territory rows hold every territory's change at each step", {
  terr <- build_up(n_add = 2)$territory
  total <- terr[terr$metric == "total_minutes", ]

  expect_equal(total$diff[total$step == 1 & total$station_id == 3], -21)
  expect_equal(total$diff[total$step == 1 & total$station_id == 51], 0)
  expect_equal(total$before[total$step == 2 & total$station_id == 3], 1.4)
})

test_that("exclude, candidates and n_add past the candidates", {
  expect_equal(build_up(n_add = 1, exclude = 1)$steps$station, c(NA, 3L))
  expect_equal(build_up(n_add = 1, candidates = 3)$steps$station, c(NA, 3L))
  expect_equal(nrow(build_up(n_add = 5)$steps), 3) # only 1 and 3 can be added
  expect_error(build_up(n_add = 1, exclude = c(1, 3)), "No candidate")
  expect_error(build_up(n_add = 1, due = 1:2), "one due level")
})

test_that("the build-up table has the base row, then each unit", {
  ft <- build_up_table(build_up(n_add = 2)$steps, years = 2)
  b <- ft$body$dataset

  expect_equal(b$station, c("Rescues only", "1", "3"))
  expect_equal(b$saved, c("", "16", "-1")) # per year over 2 years, rounded
  colors <- ft$body$styles$text$color$data[, "saved"]
  expect_equal(unname(colors[2:3]), c(better_color, worse_color))
  expect_true("Minutes Saved per Year" %in% header_text(ft))
})

test_that("the own-territory table shows before -> after and who else gains", {
  out <- build_up(n_add = 2)
  b <- build_up_territory_table(out$steps, out$territory)$body$dataset

  expect_equal(b$station, c("1", "3"))
  expect_equal(b$total, c("14 → 2 (-12)", "1 → 3 (+2)"))
  expect_equal(b$own_share, c("36%", "100%"))
  expect_equal(b$also_helps, c("3 (21)", ""))
})

# Moves: units at 7 and 22 (both base stations, so neither adds a location)
# move to 1 and 3. Moving either to 1 saves 33, to 3 only 19, so 1 goes
# first; the totals then match the build-up's (44, 11, 13).
moves <- function(...) {
  move_order(c(7, 22), c(7, 22), c(1, 3), make_scenario_matrix_df(), runcard(), incident_filter("All calls"), thresholds = 4, ...)
}

test_that("each step moves the unit that saves the most minutes, ending at the target", {
  s <- moves()$steps

  expect_equal(s$step, 0:2)
  expect_equal(s$from, c(NA, 7L, 22L))
  expect_equal(s$station, c(NA, 1L, 3L))
  expect_equal(s$stations, c("7, 22", "1, 7, 22", "1, 3, 7, 22"))
  expect_equal(s$total_minutes, c(44, 11, 13))
  expect_equal(s$saved_minutes, c(NA, 33, -2))
})

test_that("units already at a target station stay; the sets must match in size", {
  matrix_df <- make_scenario_matrix_df()
  s <- move_order(22, c(1, 7), c(1, 3), matrix_df, runcard(), incident_filter("All calls"))$steps
  expect_equal(s$from, c(NA, 7L))
  expect_equal(s$station, c(NA, 3L))

  expect_error(
    move_order(22, c(1, 7), 3, matrix_df, runcard(), incident_filter("All calls")),
    "as many stations: 2 vs 1"
  )
})

test_that("the build-up table shows moves as from -> to", {
  ft <- build_up_table(moves()$steps, base_label = "Today's engines")
  b <- ft$body$dataset

  expect_equal(b$station, c("Today's engines", "7 → 1", "22 → 3"))
  expect_true("Move Unit (From → To)" %in% header_text(ft))
})

# Free moves (no destination). Base 22, one unit at 7: 44 minutes (as above).
#   7 -> 1: 1A, 2B and 3C all go to 1: 2.3 + 1.3 + 1.4 = 5
#   7 -> 3: 2B (card 7, 1) has no station: 1 unserved, worse
test_that("without a destination each step makes the best move", {
  out <- move_order(22, 7, NULL, make_scenario_matrix_df(), runcard(), incident_filter("All calls"))
  s <- out$steps

  expect_equal(s$from, c(NA, 7L))
  expect_equal(s$station, c(NA, 1L))
  expect_equal(s$total_minutes, c(44, 5))
  expect_equal(out$units, 1)
})

test_that("without a destination the moves stop when none saves minutes", {
  # a unit at 1 (5 min): to 3 leaves 2B unserved, to 7 is 44 min
  out <- move_order(22, 1, NULL, make_scenario_matrix_df(), runcard(), incident_filter("All calls"))
  expect_equal(nrow(out$steps), 1)
  expect_equal(out$units, 1)

  # units at 1 and 7 (11 min): 7 -> 3 saves 4 (1A 2.3, 2B to 1 1.3, 3C 3.4);
  # then no empty station is left
  s <- move_order(22, c(1, 7), NULL, make_scenario_matrix_df(), runcard(), incident_filter("All calls"))$steps
  expect_equal(s$from, c(NA, 7L))
  expect_equal(s$station, c(NA, 3L))
  expect_equal(s$total_minutes, c(11, 7))
})

test_that("a destination can't include a base station", {
  expect_error(
    move_order(22, 7, 22, make_scenario_matrix_df(), runcard(), incident_filter("All calls")),
    "can't include base stations: 22"
  )
})
