# FDZ maps on the mini fixtures (helper-fixtures.R). Baseline "All" (stations
# 1, 3, 7, 22); "No 1" closes station 1 and "No 3" station 3. Zones and their
# 1st-due responders, and call minutes:
#
#   zone  calls     All        No 1       No 3
#   1A    I1, I2    1 (1.1,    7 (7.1,    1 (1.1,
#                      1.2)       7.2)       1.2)
#   2B    I3        7 (7.3)    7 (7.3)    7 (7.3)
#   3C    I4        3 (3.4)    3 (3.4)    1 (1.4)
#
# Tiles are off throughout so the tests run offline.

map_scenarios <- function() {
  tibble::tribble(
    ~scenario_id, ~scenario_name, ~stations, ~baseline,
    1, "All", c(1, 3, 7, 22), TRUE,
    2, "No 1", c(3, 7, 22), FALSE,
    3, "No 3", c(1, 7, 22), FALSE
  )
}

fdz_compare <- function() {
  paired <- suppressMessages(paired_calls(
    map_scenarios(), make_scenario_matrix_df(), prep_runcard(make_runcard_raw_df()),
    list(incident_filter("All calls")), 1
  ))
  compare_by_fdz_paired(paired, thresholds = c(4, 8))
}

# Three side-by-side squares for 1A, 2B and 3C
map_fdz_sf <- function() {
  prep_fdz_polygons(data.frame(
    station = c(1L, 2L, 3L),
    fire_demand_zone = c("A", "B", "C"),
    shape_wkt = c(square_wkt(2200000, 1380000), square_wkt(2201000, 1380000), square_wkt(2202000, 1380000))
  ))
}

map_stations_df <- function() {
  pts <- rbind(
    lonlat_from_2240(2200500, 1380500),
    lonlat_from_2240(2202500, 1380500),
    lonlat_from_2240(2201500, 1380500),
    lonlat_from_2240(2203500, 1380500)
  )
  data.frame(station_id = c(1L, 3L, 7L, 22L), latitude = pts[, "Y"], longitude = pts[, "X"])
}

map_of <- function(f, ...) {
  f(fdz_compare(), map_fdz_sf(), map_stations_df(), map_scenarios(), ..., tiles = FALSE)
}

# The plotted zone layer's fill per zone (the second geom_sf: after the outlines)
zone_fills <- function(p, panel = 1) {
  d <- ggplot2::layer_data(p, 2)
  zones <- p$layers[[2]]$data
  stats::setNames(d$fill[d$PANEL == panel], zones$fdz_id[as.integer(zones$panel) == panel])
}

test_that("fdz_map_data joins one metric per zone to the polygons", {
  ctx <- map_context(fdz_compare(), map_scenarios(), "No 1", NULL, NULL)
  zones <- fdz_map_data(ctx, map_fdz_sf(), "p90_minutes")

  expect_s3_class(zones, "sf")
  expect_setequal(zones$fdz_id, c("1A", "2B", "3C"))
  expect_equal(zones$diff[zones$fdz_id == "2B"], 0)
  expect_gt(zones$diff[zones$fdz_id == "1A"], 5)
  expect_error(fdz_map_data(ctx, map_fdz_sf(), "nope"), "Unknown metric: nope")
})

test_that("zones without a polygon are reported and left off", {
  ctx <- map_context(fdz_compare(), map_scenarios(), "No 1", NULL, NULL)
  fdz <- map_fdz_sf()
  fdz <- fdz[fdz$fdz_id != "3C", ]

  expect_message(zones <- fdz_map_data(ctx, fdz, "n_calls"), "1 zone\\(s\\) with calls have no polygon")
  expect_setequal(zones$fdz_id, c("1A", "2B"))
})

test_that("map_context picks a scenario by name or id, never the baseline", {
  expect_equal(map_context(fdz_compare(), map_scenarios(), 3, NULL, NULL)$scenario_name, "No 3")
  expect_error(map_context(fdz_compare(), map_scenarios(), "Nope", NULL, NULL), "Unknown scenario: Nope")
  expect_error(map_context(fdz_compare(), map_scenarios(), "All", NULL, NULL), "not the baseline")
})

test_that("the change map colors slower zones red and faster ones green", {
  slower <- zone_fills(map_of(fdz_change_map, scenario = "No 1"))
  faster <- zone_fills(map_of(fdz_change_map, scenario = "No 3", metric = "median_minutes"))

  expect_equal(unname(slower["1A"]), worse_color) # 1A's p90 rises the most: the scale's red end
  expect_equal(unname(slower["2B"]), map_mid_color) # unchanged
  expect_equal(unname(faster["3C"]), better_color) # 3.4 -> 1.4 min
})

test_that("the change map flips colors for metrics where higher is better", {
  fills <- zone_fills(map_of(fdz_change_map, scenario = "No 1", metric = "pct_within_4min"))

  expect_equal(unname(fills["1A"]), worse_color) # 100% -> 0% within 4 min
})

test_that("the territory map shows each side's 1st-due station, changed zones outlined", {
  p <- map_of(fdz_territory_map, scenario = "No 1")
  zones <- p$layers[[2]]$data

  expect_equal(levels(zones$panel), c("All", "No 1"))
  base <- zones[zones$panel == "All", ]
  scen <- zones[zones$panel == "No 1", ]
  expect_equal(as.character(base$value[match(c("1A", "2B", "3C"), base$fdz_id)]), c("1", "7", "3"))
  expect_equal(as.character(scen$value[match(c("1A", "2B", "3C"), scen$fdz_id)]), c("7", "7", "3"))
  expect_setequal(p$layers[[3]]$data$fdz_id, "1A") # the outline layer
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("territory fills are the dark territory palette, lighter see-through", {
  p <- map_of(fdz_territory_map, scenario = "No 1")
  d <- ggplot2::layer_data(p, 2)

  expect_setequal(unique(d$fill), map_territory_colors[1:3]) # responders 1, 3, 7
  expect_true(all(d$alpha == map_territory_alpha))
  expect_true(all(ggplot2::layer_data(map_of(fdz_demand_map, years = 1), 2)$alpha == 0.8))
})

test_that("a station keeps its territory color when another station is off the map", {
  # without 3C under No 1, station 3 is off that map, but 7 keeps the third
  # color (the run's responders are 1, 3 and 7), as on the No 3 map
  fz <- fdz_compare()
  fz <- fz[!(fz$scenario_name == "No 1" & fz$fdz_id == "3C"), ]
  p <- fdz_territory_map(fz, map_fdz_sf(), map_stations_df(), map_scenarios(), "No 1", tiles = FALSE)
  fills <- zone_fills(p, panel = 2)

  expect_equal(unname(fills["1A"]), map_territory_colors[3])
  expect_equal(levels(p$layers[[2]]$data$value), c("1", "7"))
})

test_that("the benchmark map shows each side's share within the threshold", {
  p <- map_of(fdz_benchmark_map, scenario = "No 3", threshold = 4)
  zones <- p$layers[[2]]$data
  three_c <- zones[zones$fdz_id == "3C", ]

  expect_equal(three_c$value, c(1, 1)) # 3.4 and 1.4 are both within 4
  expect_equal(zones$value[zones$fdz_id == "2B"], c(0, 0)) # 7.3
  expect_error(map_of(fdz_benchmark_map, scenario = "No 3", threshold = 5), "Unknown metric: pct_within_5min")
})

test_that("the demand map shows calls per year", {
  p <- map_of(fdz_demand_map, years = 2)
  zones <- p$layers[[2]]$data

  expect_equal(zones$value[match(c("1A", "2B", "3C"), zones$fdz_id)], c(1, 0.5, 0.5))
  expect_null(p$facet$params$facets)
})

test_that("the density map divides calls per year by each zone's area", {
  # each zone is a 1,000 ft square: (1000 * 1200 / 3937 m)^2 in square miles
  # on the plane; the map measures on the sphere, a hair (~0.05%) larger
  sq_mi <- (1000 * 1200 / 3937)^2 / 2589988.110336
  p <- map_of(fdz_demand_map, years = 2, density = TRUE, cap = 1)
  zones <- p$layers[[2]]$data

  expect_equal(zones$value[match(c("1A", "2B", "3C"), zones$fdz_id)], c(1, 0.5, 0.5) / sq_mi, tolerance = 1e-3)
  expect_match(zones$hover[zones$fdz_id == "1A"], "28 calls per year per sq mi")
  expect_equal(p$labels$title, "Calls per Year per Square Mile by FDZ")
})

test_that("the density cap tops out the scale and marks its label with +", {
  # densities 27.9, 13.9, 13.9: the 95th percentile, 26.5, rounds up to 27
  p <- map_of(fdz_demand_map, years = 2, density = TRUE)
  zones <- p$layers[[2]]$data
  scale <- p$scales$get_scales("fill")

  expect_equal(max(zones$value), 27)
  expect_equal(scale$get_limits(), c(0, 27))
  expect_equal(scale$get_labels(), c("0", "7", "14", "20", "27+"))
  expect_match(zones$hover[zones$fdz_id == "1A"], "28 calls per year per sq mi") # the exact value
  expect_equal(vapply(c(158.6, 26.5, 160, 0.123), round_up_2, numeric(1)), c(160, 27, 160, 0.13))
})

test_that("legends go under the map by default, or to the right", {
  bottom <- map_of(fdz_change_map, scenario = "No 1")
  right <- map_of(fdz_change_map, scenario = "No 1", legend_position = "right")

  expect_equal(bottom$theme$legend.position, "bottom")
  expect_equal(bottom$theme$legend.box, "horizontal")
  expect_s3_class(bottom$guides$guides$fill, "GuideColourbar")
  expect_equal(right$theme$legend.position, "right")
  expect_null(right$guides$guides$fill)
  expect_error(map_of(fdz_change_map, scenario = "No 1", legend_position = "top"), "should be one of")
})

test_that("zone boundaries are gray, not white", {
  d <- ggplot2::layer_data(map_of(fdz_territory_map, scenario = "No 1"), 2)
  expect_true(all(d$colour == map_border_color))
})

test_that("stations are marked kept, added, removed or without a unit", {
  ctx <- map_context(fdz_compare(), map_scenarios(), "No 1", NULL, NULL)
  stations <- map_stations(rbind(map_stations_df(), data.frame(station_id = 9L, latitude = 34, longitude = -84.5)), ctx)

  expect_equal(
    as.character(stations$status),
    c("Unit removed", "Unit", "Unit", "Unit", "No unit")
  )
  ctx$scenario_stations <- c(ctx$scenario_stations, 9)
  expect_equal(as.character(map_stations(rbind(
    map_stations_df(), data.frame(station_id = 9L, latitude = 34, longitude = -84.5)
  ), ctx)$status[5]), "Unit added")
})

test_that("interactive maps are leaflet widgets with a layer per side", {
  m <- map_of(fdz_territory_map, scenario = "No 1", interactive = TRUE)
  calls <- vapply(m$x$calls, `[[`, character(1), "method")

  expect_s3_class(m, "leaflet")
  expect_true("addLayersControl" %in% calls)
  groups <- unlist(lapply(m$x$calls[calls == "addPolygons"], function(cl) cl$args[[3]]))
  expect_true(all(c("All", "No 1") %in% groups))

  single <- map_of(fdz_change_map, scenario = "No 1", interactive = TRUE)
  expect_false("addLayersControl" %in% vapply(single$x$calls, `[[`, character(1), "method"))
})

test_that("save_map writes a PNG for a static map and HTML for an interactive one", {
  dir <- withr::local_tempdir()
  png <- file.path(dir, "map.png")
  html <- file.path(dir, "map.html")

  save_map(map_of(fdz_demand_map, years = 1), png, width = 4, height = 4)
  save_map(map_of(fdz_demand_map, years = 1, interactive = TRUE), html)

  expect_true(file.exists(png))
  expect_true(file.exists(html))
  expect_error(save_map(map_of(fdz_demand_map, years = 1, interactive = TRUE), png), "saved as .html")
})
