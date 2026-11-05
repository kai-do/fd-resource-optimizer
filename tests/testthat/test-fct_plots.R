# Travel-time box plots on the mini fixtures (helper-fixtures.R). Baseline
# "All" (stations 1, 3, 7, 22); "No 1" closes station 1 and "No 3" station 3.
# Calls and their territory stations: I1, I2 in 1A (station 1), I3 in 2B
# (station 51), I4 in 3C (station 3). Minutes:
#
#           I1   I2   I3   I4
#   All    1.1  1.2  7.3  3.4
#   No 1   7.1  7.2  7.3  3.4
#   No 3   1.1  1.2  7.3  1.4

runcard <- function() prep_runcard(make_runcard_raw_df())

three_scenarios <- function() {
  tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "No 1", c(3, 7, 22), FALSE,
    3, "No 3", c(1, 7, 22), FALSE
  )
}

paired <- function() {
  suppressMessages(paired_calls(
    three_scenarios(), make_scenario_matrix_df(), runcard(), list(incident_filter("All calls")), 1
  ))
}

boxplot_of <- function(...) travel_time_boxplot(paired(), baseline_name = "All", ...)

# The plotted rows (one per box) and the data behind them
boxes <- function(p) ggplot2::layer_data(p)

test_that("paired_calls carries each call's territory station", {
  p <- paired()
  station <- unique(p[c("dest_id", "station_id")])

  expect_equal(station$station_id[match(c("I1", "I2", "I3", "I4"), station$dest_id)], c(1L, 1L, 51L, 3L))
})

test_that("plot_times_long takes the baseline once per call and each scenario's own minutes", {
  long <- plot_times_long(paired(), c("All", "No 1"), "All")

  expect_equal(levels(long$group), c("All", "No 1"))
  expect_equal(long$minutes[long$group == "All"], c(1.1, 1.2, 7.3, 3.4))
  expect_equal(long$minutes[long$group == "No 1"], c(7.1, 7.2, 7.3, 3.4))
})

test_that("unserved calls are left out of their side only", {
  p <- paired()
  p$scenario_minutes[p$scenario_name == "No 1" & p$dest_id == "I3"] <- NA
  long <- plot_times_long(p, c("All", "No 1"), "All")

  expect_equal(sum(long$group == "All"), 4)
  expect_equal(sum(long$group == "No 1"), 3)
})

test_that("the overall plot has one box per group, baseline first, with no legend", {
  p <- boxplot_of()
  b <- boxes(p)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(b), 3)
  # horizontal: the first group sits at the top (highest y position)
  expect_equal(levels(p$data$category), c("No 3", "No 1", "All"))
  expect_equal(p$theme$legend.position, "none")
  expect_equal(unname(b$fill[b$y == 3]), plot_baseline_color)
})

test_that("scenario picks the groups and their order, baseline optional", {
  expect_equal(levels(boxplot_of(scenario = c("No 3", "All"))$data$group), c("No 3", "All"))
  expect_equal(levels(boxplot_of(scenario = "No 1")$data$group), "No 1")
  # ids work too
  expect_equal(levels(boxplot_of(scenario = c("All", 3))$data$group), c("All", "No 3"))
  expect_error(boxplot_of(scenario = "Nope"), "Unknown scenario: Nope")
})

test_that("orientation swaps the minutes and category axes", {
  horizontal <- boxplot_of()
  vertical <- boxplot_of(orientation = "vertical")

  expect_equal(rlang::as_label(horizontal$mapping$x), "minutes")
  expect_equal(rlang::as_label(horizontal$mapping$y), "category")
  expect_equal(rlang::as_label(vertical$mapping$x), "category")
  expect_equal(rlang::as_label(vertical$mapping$y), "minutes")
  expect_equal(levels(vertical$data$category), c("All", "No 1", "No 3"))
})

test_that("the station plot has a box per territory station and group", {
  p <- boxplot_of(by = "station", scenario = c("All", "No 1"))

  expect_equal(levels(p$data$category), c("51", "3", "1")) # 1 on top
  expect_equal(nrow(boxes(p)), 6)
  expect_equal(p$theme$legend.position, "bottom")
  expect_equal(p$labels$y, "Territory Station")
})

test_that("stations keeps those territories, in station and FDZ plots", {
  p <- boxplot_of(by = "station", stations = c(1, 3))
  expect_setequal(as.character(p$data$category), c("1", "3"))

  zones <- boxplot_of(by = "fdz", stations = 1)
  expect_setequal(as.character(zones$data$category), "1A")
  expect_error(boxplot_of(by = "station", stations = 99), "Unknown station: 99")
})

test_that("the FDZ plot has a box per zone, in natural order, and fdz filters", {
  p <- boxplot_of(by = "fdz", orientation = "vertical")
  expect_equal(levels(p$data$category), c("1A", "2B", "3C"))
  expect_equal(p$labels$x, "FDZ")

  one <- boxplot_of(by = "fdz", fdz = "1A")
  expect_equal(unique(as.character(one$data$category)), "1A")
  expect_equal(nrow(one$data), 6) # I1 and I2 in each of three groups
  expect_error(boxplot_of(by = "fdz", fdz = "9Z"), "Unknown FDZ: 9Z")
})

test_that("a station plot needs a station_id column", {
  p <- paired()
  p$station_id <- NULL

  expect_error(
    travel_time_boxplot(p, by = "station", baseline_name = "All"),
    "no station_id column"
  )
})

test_that("mmss labels the minutes axis as m:ss", {
  p <- boxplot_of(time_format = "mmss")

  expect_equal(p$labels$x, "Travel Time (m:ss)")
  expect_equal(p$scales$get_scales("x")$labels(c(7.5, 10)), c("7:30", "10:00"))
})

test_that("more than 8 scenarios share one colour overall but error by station", {
  groups <- c("Base", paste("S", 1:9))
  overall <- group_colors(groups, "Base", "overall")

  expect_equal(unname(overall[1]), plot_baseline_color)
  expect_true(all(overall[-1] == plot_colors[1]))
  expect_error(group_colors(groups, "Base", "station"), "At most 8 scenarios")
  expect_equal(unname(group_colors(groups[1:3], "Base", "station")), c(plot_baseline_color, plot_colors[1:2]))
})

# Changed / No Change ---------------------------------------------------------

test_that("responder_differs counts a new station or one-sided service as a change", {
  expect_equal(
    responder_differs(c(1, 1, NA, 1, NA), c(1, 7, 7, NA, NA)),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
})

# The change label of each category in a plot's data
change_of <- function(p) {
  d <- unique(p$data[c("category", "change")])
  stats::setNames(as.character(d$change), as.character(d$category))
}

test_that("split_changed puts zones into Changed and No Change panels", {
  p <- boxplot_of(by = "fdz", scenario = c("All", "No 1"), split_changed = TRUE)

  expect_equal(change_of(p)[c("1A", "2B", "3C")], c("1A" = "Changed", "2B" = "No Change", "3C" = "No Change"))
  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(levels(p$data$change), c("Changed", "No Change"))
})

test_that("a station or zone is Changed if any selected scenario changes it", {
  p <- boxplot_of(by = "fdz", scenario = c("All", "No 1", "No 3"), split_changed = TRUE)

  expect_equal(change_of(p)[c("1A", "2B", "3C")], c("1A" = "Changed", "2B" = "No Change", "3C" = "Changed"))
})

test_that("include_unchanged = FALSE keeps only what changes, at every level", {
  two <- c("All", "No 1")

  expect_equal(levels(boxplot_of(by = "fdz", scenario = two, include_unchanged = FALSE)$data$category), "1A")
  expect_equal(levels(boxplot_of(by = "station", scenario = two, include_unchanged = FALSE)$data$category), "1")

  overall <- boxplot_of(scenario = two, include_unchanged = FALSE)
  expect_setequal(overall$data$dest_id, c("I1", "I2"))
  expect_equal(nrow(overall$data), 4) # I1 and I2 in both groups
  expect_match(overall$labels$subtitle, "Changed only$")
})

test_that("the overall plot can split calls into Changed and No Change", {
  p <- boxplot_of(scenario = c("All", "No 1"), split_changed = TRUE)

  expect_equal(unique(as.character(p$data$change[p$data$dest_id %in% c("I1", "I2")])), "Changed")
  expect_equal(unique(as.character(p$data$change[p$data$dest_id %in% c("I3", "I4")])), "No Change")
  expect_s3_class(p$facet, "FacetGrid")
})

test_that("the change options need a scenario besides the baseline", {
  expect_error(boxplot_of(scenario = "All", include_unchanged = FALSE), "at least one scenario")
  expect_error(boxplot_of(scenario = "All", split_changed = TRUE), "at least one scenario")
  expect_error(
    boxplot_of(by = "fdz", fdz = "2B", scenario = c("All", "No 1"), include_unchanged = FALSE),
    "No calls change responder"
  )
})

test_that("save_plot writes a PNG", {
  file <- tempfile(fileext = ".png")
  save_plot(boxplot_of(), file, width = 4, height = 3)

  expect_true(file.exists(file))
  expect_error(save_plot(boxplot_of(), tempfile(fileext = ".docx")), ".png or .pdf")
})
