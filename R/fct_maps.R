# FDZ maps (static ggplot2 or interactive leaflet), for reports and exploring.
#
# Each takes the zone-by-zone comparison from step 06 (compare_fdz.parquet,
# from compare_by_fdz()), the FDZ polygons (prep_fdz_polygons()), the
# stations (prep_stations()) and the scenarios table, and maps one scenario
# against the baseline:
#
#   fdz_change_map(fdz_compare, fdz_sf, stations_df, scenarios, "R26->16 +R1")
#   fdz_territory_map(fdz_compare, fdz_sf, stations_df, scenarios, "R26->16 +R1")
#   fdz_benchmark_map(fdz_compare, fdz_sf, stations_df, scenarios, "+R1", threshold = 8 + 58 / 60)
#   fdz_demand_map(fdz_compare, fdz_sf, stations_df, scenarios, years = 3)
#   fdz_demand_map(fdz_compare, fdz_sf, stations_df, scenarios, years = 3, density = TRUE)
#   m |> save_map("change.png")    # or .html with interactive = TRUE
#
# Static maps sit on CARTO light street tiles (downloaded and cached the first
# time; tiles = FALSE draws without them). Interactive maps add hover labels
# and, where there are baseline and scenario layers, a switch between them.

#' Map of the change in a travel-time metric by FDZ
#'
#' Each zone colored by scenario minus baseline: green where the scenario is
#' better, red where it is worse, near white where nothing changes.
#'
#' @param fdz_compare Output of [compare_by_fdz()], e.g.
#'   `compare_fdz.parquet`.
#' @param fdz_sf FDZ polygons from [prep_fdz_polygons()].
#' @param stations_df Stations from [prep_stations()]: `station_id`,
#'   `latitude`, `longitude`.
#' @param scenarios_df The scenarios table (`scenario_id`, `scenario_name`,
#'   `stations`, `baseline`), e.g. `scenarios.parquet`.
#' @param scenario Scenario to map, by name or id (not the baseline).
#' @param metric A zone metric in `fdz_compare`, e.g. `"p90_minutes"`,
#'   `"median_minutes"` or `"pct_within_8m58s"`.
#' @param filter_name,due Filter set and due level. `NULL` uses the first in
#'   `fdz_compare` (with a message if there are others).
#' @param time_format Time values as `"decimal"` minutes or `"mmss"`.
#' @param interactive `TRUE` returns a leaflet map instead of a ggplot.
#' @param tiles Draw street tiles under a static map (needs internet the
#'   first time).
#' @param legend_position Where a static map's legend goes: `"bottom"`
#'   (under the map, side by side) or `"right"`. Interactive maps keep theirs
#'   in the bottom-right corner.
#' @param title,subtitle Override the automatic title and subtitle.
#'
#' @return A ggplot, or a leaflet htmlwidget with `interactive = TRUE`.
#' @noRd
fdz_change_map <- function(
  fdz_compare,
  fdz_sf,
  stations_df,
  scenarios_df,
  scenario,
  metric = "p90_minutes",
  filter_name = NULL,
  due = NULL,
  time_format = c("decimal", "mmss"),
  interactive = FALSE,
  tiles = TRUE,
  legend_position = c("bottom", "right"),
  title = NULL,
  subtitle = NULL
) {
  time_format <- match.arg(time_format)
  ctx <- map_context(fdz_compare, scenarios_df, scenario, filter_name, due)
  zones <- fdz_map_data(ctx, fdz_sf, metric)
  zones$value <- zones$diff
  zones$panel <- factor(ctx$scenario_name)
  zones$outlined <- FALSE
  zones$hover <- zone_hover(zones, ctx, metric, time_format)

  # Green for better, red for worse, whichever way the metric runs
  limit <- max(abs(zones$value), na.rm = TRUE)
  if (!is.finite(limit) || limit == 0) {
    limit <- 1
  }
  ends <- if (higher_is_better(metric)) c(worse_color, better_color) else c(better_color, worse_color)
  if (!higher_is_better(metric) && !higher_is_worse(metric)) {
    ends <- c("#B3541E", "#1F5FA8") # neither better nor worse: orange down, blue up
  }
  fill <- list(
    kind = "diverging",
    colors = c(ends[1], map_mid_color, ends[2]),
    limits = c(-limit, limit),
    title = paste("Change in", map_metric_label(metric, time_format)),
    labels = function(x) format_values(x, metric, diff = TRUE, time_format = time_format, digits = 1)
  )

  if (is.null(title)) {
    title <- paste("Change in", map_metric_label(metric, time_format, units = FALSE), "by FDZ")
  }
  render_fdz_map(
    zones, fill, ctx, fdz_sf, stations_df, interactive, tiles, match.arg(legend_position), title,
    map_subtitle(ctx, subtitle, paste(ctx$scenario_name, "vs", ctx$baseline_name))
  )
}

#' Map of 1st-due territories, baseline and scenario
#'
#' Each zone colored by the station whose unit reaches it first (at the
#' mapped due level), a panel (or, interactive, a layer) each for the
#' baseline and the scenario. Zones whose responder changes are outlined.
#'
#' @inheritParams fdz_change_map
#' @noRd
fdz_territory_map <- function(
  fdz_compare,
  fdz_sf,
  stations_df,
  scenarios_df,
  scenario,
  filter_name = NULL,
  due = NULL,
  interactive = FALSE,
  tiles = TRUE,
  legend_position = c("bottom", "right"),
  title = NULL,
  subtitle = NULL
) {
  ctx <- map_context(fdz_compare, scenarios_df, scenario, filter_name, due)
  zones <- fdz_map_data(ctx, fdz_sf, "n_calls")
  zones <- both_sides(zones, ctx, "source_id")

  # Colors keyed on every station that responds in any scenario of the run,
  # so a station keeps its color from one territory map to the next
  run <- as.data.frame(fdz_compare)
  run <- run[run$filter_name == ctx$filter_name & run$due == ctx$due, ]
  all_responders <- sort(unique(stats::na.omit(c(run$baseline_source_id, run$scenario_source_id))))
  all_colors <- if (length(all_responders) <= length(map_territory_colors)) {
    map_territory_colors[seq_along(all_responders)]
  } else {
    grDevices::hcl.colors(length(all_responders), "Dynamic")
  }
  names(all_colors) <- all_responders
  responders <- sort(unique(stats::na.omit(zones$value)))
  colors <- all_colors[as.character(responders)]
  zones$value <- factor(zones$value, levels = responders)
  zones$hover <- sprintf(
    "<b>FDZ %s</b><br>%s: Station %s<br>%s: Station %s",
    zones$fdz_id,
    ctx$baseline_name, na_text(zones$baseline_source_id),
    ctx$scenario_name, na_text(zones$scenario_source_id)
  )
  fill <- list(
    kind = "factor",
    colors = colors,
    title = paste(due_ordinal(ctx$due), "Due Station"),
    alpha = map_territory_alpha
  )

  if (is.null(title)) {
    title <- paste(due_ordinal(ctx$due), "Due Territories")
  }
  render_fdz_map(
    zones, fill, ctx, fdz_sf, stations_df, interactive, tiles, match.arg(legend_position), title,
    map_subtitle(ctx, subtitle, "Outlined zones change responder")
  )
}

#' Map of the share of calls reached within a benchmark time, by FDZ
#'
#' @inheritParams fdz_change_map
#' @param threshold Benchmark minutes, one of the thresholds step 06 was run
#'   with (`8 + 58 / 60` for 8:58).
#' @noRd
fdz_benchmark_map <- function(
  fdz_compare,
  fdz_sf,
  stations_df,
  scenarios_df,
  scenario,
  threshold = 8 + 58 / 60,
  filter_name = NULL,
  due = NULL,
  interactive = FALSE,
  tiles = TRUE,
  legend_position = c("bottom", "right"),
  title = NULL,
  subtitle = NULL
) {
  metric <- paste0("pct_within_", threshold_key(threshold))
  ctx <- map_context(fdz_compare, scenarios_df, scenario, filter_name, due)
  zones <- fdz_map_data(ctx, fdz_sf, metric)
  hover <- zone_hover(zones, ctx, metric, "decimal")
  zones <- both_sides(zones, ctx, "value")
  zones$hover <- rep(hover, 2)

  fill <- list(
    kind = "sequential",
    colors = c("#F4F7FB", "#1F4F8A"),
    limits = c(0, 1),
    title = paste("Within", threshold_text(threshold), "Min"),
    labels = function(x) paste0(round(x * 100), "%")
  )

  if (is.null(title)) {
    title <- paste("Calls Reached Within", threshold_text(threshold), "Min by FDZ")
  }
  render_fdz_map(
    zones, fill, ctx, fdz_sf, stations_df, interactive, tiles, match.arg(legend_position), title,
    map_subtitle(ctx, subtitle)
  )
}

#' Map of calls per year by FDZ
#'
#' Calls are counted once per zone (the same in every scenario). With a
#' `scenario`, its unit moves are marked on the stations.
#'
#' Big zones collect more calls just by covering more ground; `density =
#' TRUE` divides each zone's calls by its area, so zones of different sizes
#' compare fairly.
#'
#' @inheritParams fdz_change_map
#' @param scenario Scenario whose stations to mark, or `NULL` for the
#'   baseline's.
#' @param years Years the calls cover: one number, or the output of
#'   [filter_years()] (`filter_years.parquet`).
#' @param density `TRUE` maps calls per year per square mile instead of
#'   calls per year.
#' @param cap With `density`, the quantile where the color scale tops out
#'   (rounded up to two significant digits), so a few small, busy zones
#'   don't wash out the rest. Zones above it share the darkest color and the
#'   legend's top label reads "160+"; hover labels keep the exact values.
#'   `1` for no cap.
#' @noRd
fdz_demand_map <- function(
  fdz_compare,
  fdz_sf,
  stations_df,
  scenarios_df,
  scenario = NULL,
  years,
  density = FALSE,
  cap = 0.95,
  filter_name = NULL,
  due = NULL,
  interactive = FALSE,
  tiles = TRUE,
  legend_position = c("bottom", "right"),
  title = NULL,
  subtitle = NULL
) {
  ctx <- map_context(fdz_compare, scenarios_df, scenario, filter_name, due)
  zones <- fdz_map_data(ctx, fdz_sf, "n_calls")
  per_year <- annualize_counts(
    data.frame(filter_name = ctx$filter_name, metric = "n_calls", baseline = zones$baseline),
    years
  )
  zones$value <- per_year$baseline
  zones$outlined <- FALSE
  count_text <- function(x, digits = 0) formatC(x, format = "f", digits = digits, big.mark = ",")

  if (density) {
    # Area on the sphere (s2), in square meters whatever the polygons' CRS
    sq_mi <- as.numeric(sf::st_area(sf::st_transform(zones, 4326))) / sq_m_per_sq_mi
    zones$value <- per_year$baseline / sq_mi
    zones$hover <- sprintf(
      "<b>FDZ %s</b><br>%s calls per year per sq mi<br>%s calls per year over %s sq mi",
      zones$fdz_id, count_text(zones$value), count_text(per_year$baseline), count_text(sq_mi, 2)
    )
    top <- max(zones$value, na.rm = TRUE)
    cap_at <- if (cap < 1) round_up_2(stats::quantile(zones$value, cap, na.rm = TRUE, names = FALSE)) else top
    capped <- cap_at < top
    zones$value <- pmin(zones$value, cap_at)
    zones$panel <- factor("Calls per Year per Square Mile")
    fill <- list(
      kind = "sequential",
      colors = c("#FDF3E7", "#B3541E"),
      limits = c(0, cap_at),
      breaks = if (capped) seq(0, cap_at, length.out = 5),
      title = "Calls per Year per Sq Mi",
      labels = function(x) ifelse(capped & !is.na(x) & x >= cap_at, paste0(count_text(x), "+"), count_text(x))
    )
    if (is.null(title)) {
      title <- "Calls per Year per Square Mile by FDZ"
    }
  } else {
    zones$panel <- factor("Calls per Year")
    zones$hover <- sprintf("<b>FDZ %s</b><br>%s calls per year", zones$fdz_id, count_text(zones$value))
    fill <- list(
      kind = "sequential",
      colors = c("#FDF3E7", "#B3541E"),
      limits = c(0, max(zones$value, na.rm = TRUE)),
      title = "Calls per Year",
      labels = function(x) count_text(x)
    )
    if (is.null(title)) {
      title <- "Calls per Year by FDZ"
    }
  }
  render_fdz_map(
    zones, fill, ctx, fdz_sf, stations_df, interactive, tiles, match.arg(legend_position), title,
    map_subtitle(ctx, subtitle)
  )
}

#' Save a map: a static one as .png or .pdf, an interactive one as .html
#'
#' Interactive maps are written as an .html file plus a `<name>_files`
#' folder of scripts next to it; keep the two together.
#'
#' @param m A map from one of the `fdz_*_map()` functions.
#' @param path File to write.
#' @param width,height Size in inches (static maps).
#'
#' @return `path`, invisibly.
#' @noRd
save_map <- function(m, path, width = 8, height = 8) {
  if (inherits(m, "leaflet")) {
    if (tolower(tools::file_ext(path)) != "html") {
      stop("Interactive maps are saved as .html", call. = FALSE)
    }
    # saveWidget() works in the file's folder, so give it an absolute path
    file <- file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))
    htmlwidgets::saveWidget(m, file, selfcontained = FALSE, title = map_title_text(m))
    return(invisible(path))
  }
  save_plot(m, path, width = width, height = height)
}

# Internals -------------------------------------------------------------------

# Literal values: this file loads before fct_plots.R and fct_tables.R, whose
# plot_ink, better_color and worse_color these match
map_mid_color <- "#F5F5F2"
map_na_color <- "#D9D8D3"
map_outline_color <- "#0b0b0b"
# Zone boundaries: a light gray, between plot_ink's muted and grid grays, that
# still shows on white but stands apart from the colored fills
map_border_color <- "#C3C2BC"

# Territory fills: dark, saturated hues, so that see-through over the light
# basemap they still sit well below map_border_color in lightness and the
# zone lines show. Colors go to the run's responders in station-number order;
# this order keeps touching Cobb rescue territories (stations 1, 9, 11, 14,
# 16, 21, 23, 24, 26, 27, 30, in any scenario) apart for normal and red-green
# color-blind vision. Station numbers on the map back up the colors.
map_territory_colors <- c(
  "#2e6b5e", "#d4500a", "#1d4fa6", "#0d7f45", "#00809e", "#a87400",
  "#5f7a00", "#c43c6a", "#8f3f00", "#b81d35", "#6b3db0", "#b0308a"
)
map_territory_alpha <- 0.6

sq_m_per_sq_mi <- 2589988.110336

# Round up to two significant digits: 158.6 -> 160, 26.5 -> 27
round_up_2 <- function(x) {
  if (!is.finite(x) || x <= 0) {
    return(x)
  }
  step <- 10^(floor(log10(x)) - 1)
  ceiling(x / step) * step
}

# Esri World Light Gray Canvas street tiles, keyless (CARTO's now stamp "API
# KEY REQUIRED" on every tile). Leaflet uses the same tiles
# (providers$Esri.WorldGrayCanvas). Its terms ask for the credit on every map.
map_tile_url <- paste0(
  "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
  "World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}"
)
map_tile_credit <- "Basemap: Esri, HERE, Garmin, © OpenStreetMap contributors"

# Station marker colors: which stations have a unit, and what the scenario
# does to them
map_station_colors <- c(
  "Unit" = "#0b0b0b",
  "Unit added" = "#1B7F3B",
  "Unit removed" = "#B00020",
  "No unit" = "#898781"
)

# The scenario, filter set and due level to map, and their rows of
# fdz_compare. `scenario = NULL` (demand map only) takes any scenario's rows:
# their baseline side is the same.
map_context <- function(fdz_compare, scenarios_df, scenario, filter_name, due) {
  df <- as.data.frame(fdz_compare)
  scenarios_df <- as.data.frame(scenarios_df)
  baseline <- scenarios_df[scenarios_df$baseline, ]
  if (nrow(baseline) != 1) {
    stop("scenarios_df needs exactly one baseline row", call. = FALSE)
  }

  filter_name <- pick_one(df$filter_name, filter_name, "filter set")
  due <- pick_one(df$due, due, "due level")
  df <- df[df$filter_name == filter_name & df$due == due, ]

  if (is.null(scenario)) {
    rows <- df[df$scenario_name == df$scenario_name[1], ]
    scen <- NULL
  } else {
    if (length(scenario) != 1) {
      stop("Map one scenario at a time", call. = FALSE)
    }
    hit <- scenarios_df$scenario_name == scenario | as.character(scenarios_df$scenario_id) == as.character(scenario)
    if (!any(hit)) {
      stop(
        "Unknown scenario: ", scenario,
        ". Available: ", paste(scenarios_df$scenario_name[!scenarios_df$baseline], collapse = ", "),
        call. = FALSE
      )
    }
    scen <- scenarios_df[hit, ][1, ]
    if (scen$baseline) {
      stop("Pick a scenario to compare with the baseline, not the baseline itself", call. = FALSE)
    }
    rows <- df[df$scenario_name == scen$scenario_name, ]
  }
  if (nrow(rows) == 0) {
    stop("fdz_compare has no rows for that scenario, filter set and due level", call. = FALSE)
  }

  list(
    rows = rows,
    filter_name = filter_name,
    due = due,
    baseline_name = baseline$scenario_name,
    baseline_stations = unlist(baseline$stations),
    scenario_name = if (is.null(scen)) NULL else scen$scenario_name,
    scenario_stations = if (is.null(scen)) NULL else unlist(scen$stations)
  )
}

# One metric's rows joined to the zone polygons: fdz_id, the responders,
# responder_changed, baseline, scenario, diff and the geometry. Zones with
# calls but no polygon are reported and left out.
fdz_map_data <- function(ctx, fdz_sf, metric) {
  rows <- ctx$rows
  if (!metric %in% rows$metric) {
    stop(
      "Unknown metric: ", metric, ". Available: ", paste(unique(rows$metric), collapse = ", "),
      call. = FALSE
    )
  }
  rows <- rows[rows$metric == metric, c(
    "fdz_id", "baseline_source_id", "scenario_source_id", "responder_changed",
    "baseline", "scenario", "diff"
  )]

  missing <- setdiff(rows$fdz_id, fdz_sf$fdz_id)
  if (length(missing) > 0) {
    message(
      length(missing), " zone(s) with calls have no polygon and are left off the map: ",
      paste(utils::head(missing, 10), collapse = ", "), if (length(missing) > 10) ", ..."
    )
  }
  zones <- merge(fdz_sf[, "fdz_id"], rows, by = "fdz_id")
  sf::st_as_sf(zones)
}

# Baseline and scenario stacked as two panels: `value` from the given side's
# column (`source_id` takes baseline_source_id / scenario_source_id, `value`
# takes baseline / scenario); zones whose responder changes outlined
both_sides <- function(zones, ctx, what) {
  col <- function(side) if (what == "source_id") paste0(side, "_source_id") else side
  base <- zones
  base$value <- zones[[col("baseline")]]
  scen <- zones
  scen$value <- zones[[col("scenario")]]
  out <- rbind(base, scen)
  out$panel <- factor(
    rep(c(ctx$baseline_name, ctx$scenario_name), each = nrow(zones)),
    levels = c(ctx$baseline_name, ctx$scenario_name)
  )
  out$outlined <- out$responder_changed %in% TRUE
  out
}

# Station points with their status under the scenario (or the baseline)
map_stations <- function(stations_df, ctx) {
  ids <- stations_df$station_id
  base <- ids %in% ctx$baseline_stations
  status <- ifelse(base, "Unit", "No unit")
  if (!is.null(ctx$scenario_stations)) {
    scen <- ids %in% ctx$scenario_stations
    status[scen & !base] <- "Unit added"
    status[base & !scen] <- "Unit removed"
  }
  sf::st_as_sf(
    data.frame(
      station_id = ids,
      status = factor(status, levels = names(map_station_colors)),
      longitude = stations_df$longitude,
      latitude = stations_df$latitude
    ),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
}

# Hover text per zone: baseline, scenario and change for one metric
zone_hover <- function(zones, ctx, metric, time_format) {
  fmt <- function(x, diff = FALSE) {
    out <- format_values(x, metric, diff = diff, time_format = time_format, digits = 1)
    ifelse(out == "", "no responder", out)
  }
  sprintf(
    "<b>FDZ %s</b> &middot; %s<br>%s: %s<br>%s: %s<br>Change: %s",
    zones$fdz_id,
    map_metric_label(metric, time_format, units = FALSE),
    ctx$baseline_name, fmt(zones$baseline),
    ctx$scenario_name, fmt(zones$scenario),
    fmt(zones$diff, diff = TRUE)
  )
}

na_text <- function(x) ifelse(is.na(x), "none", as.character(x))

# Metric names for titles and legends: "90th Percentile (min)"
map_metric_label <- function(metric, time_format = "decimal", units = TRUE) {
  label <- if (grepl("^pct_within_", metric)) {
    key <- sub("^pct_within_", "", metric)
    paste("% Within", sub("min$", "", sub("^(\\d+)m(\\d{2})s$", "\\1:\\2", key)), "Min")
  } else {
    switch(
      metric,
      p90_minutes = "90th Percentile",
      median_minutes = "Median",
      mean_minutes = "Mean",
      total_minutes = "Total Minutes",
      n_calls = "Calls",
      metric
    )
  }
  if (units && grepl("minutes$", metric) && metric != "total_minutes") {
    label <- paste0(label, if (time_format == "mmss") " (m:ss)" else " (min)")
  }
  label
}

map_subtitle <- function(ctx, subtitle, extra = NULL) {
  if (!is.null(subtitle)) {
    return(subtitle)
  }
  paste(c(ctx$filter_name, paste(due_ordinal(ctx$due), "Due"), extra), collapse = " · ")
}

render_fdz_map <- function(zones, fill, ctx, fdz_sf, stations_df, interactive, tiles, legend_position, title, subtitle) {
  stations <- map_stations(stations_df, ctx)
  if (interactive) {
    leaflet_fdz_map(zones, fill, fdz_sf, stations, title, subtitle)
  } else {
    static_fdz_map(zones, fill, fdz_sf, stations, tiles, legend_position, title, subtitle)
  }
}

# Static ------------------------------------------------------------------------

static_fdz_map <- function(zones, fill, fdz_sf, stations, tiles, legend_position, title, subtitle) {
  fill_scale <- switch(
    fill$kind,
    diverging = ggplot2::scale_fill_gradient2(
      low = fill$colors[1], mid = fill$colors[2], high = fill$colors[3], midpoint = 0,
      limits = fill$limits, labels = fill$labels, name = fill$title, na.value = map_na_color
    ),
    sequential = ggplot2::scale_fill_gradient(
      low = fill$colors[1], high = fill$colors[2], breaks = fill$breaks %||% ggplot2::waiver(),
      limits = fill$limits, labels = fill$labels, name = fill$title, na.value = map_na_color
    ),
    factor = ggplot2::scale_fill_manual(
      values = fill$colors, name = fill$title, na.value = map_na_color, drop = FALSE
    )
  )
  outlined <- zones[zones$outlined, ]
  units <- stations[stations$status != "No unit", ]

  # Optional pieces are NULL when off; ggplot skips NULL
  # annotation_map_tile() takes a source by name, so register the Esri one.
  # Its URL has no file extension to tell rosm the format: the tiles are JPEGs
  tile_layer <- if (tiles) {
    rosm::register_tile_source(
      esri_light_gray = rosm::source_from_url_format(map_tile_url, attribution = map_tile_credit, extension = "jpg")
    )
    ggspatial::annotation_map_tile(type = "esri_light_gray", zoomin = 0, progress = "none")
  }
  outline_layer <- if (nrow(outlined) > 0) {
    ggplot2::geom_sf(data = outlined, fill = NA, color = map_outline_color, linewidth = 0.5)
  }
  panels <- if (nlevels(zones$panel) > 1) ggplot2::facet_wrap(ggplot2::vars(panel))
  # under the map, a color bar runs across, long enough to read its labels
  fill_guide <- if (legend_position == "bottom" && fill$kind != "factor") {
    ggplot2::guides(fill = ggplot2::guide_colorbar(theme = ggplot2::theme(legend.key.width = ggplot2::unit(12, "lines"))))
  }

  ggplot2::ggplot() +
    tile_layer +
    ggplot2::geom_sf(data = fdz_sf, fill = NA, color = plot_ink[["muted"]], linewidth = 0.1) +
    ggplot2::geom_sf(data = zones, ggplot2::aes(fill = value), color = map_border_color, linewidth = 0.2, alpha = fill$alpha %||% 0.8) +
    outline_layer +
    ggplot2::geom_sf(data = stations, ggplot2::aes(color = status), size = 1.8) +
    ggplot2::geom_sf_text(
      data = units, ggplot2::aes(label = station_id),
      size = 2.6, vjust = -0.9, color = plot_ink[["primary"]], fontface = "bold"
    ) +
    fill_scale +
    ggplot2::scale_color_manual(values = map_station_colors, name = "Station", drop = TRUE) +
    fill_guide +
    panels +
    ggplot2::coord_sf(crs = 3857, datum = NA) +
    ggplot2::labs(
      title = title, subtitle = subtitle, x = NULL, y = NULL,
      caption = if (tiles) map_tile_credit
    ) +
    map_theme(legend_position)
}

# Legends at the bottom sit side by side, each titled above its keys
map_theme <- function(legend_position = "bottom") {
  bottom <- legend_position == "bottom"
  plot_theme(horizontal = FALSE, legend = TRUE) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.box = if (bottom) "horizontal",
      legend.title.position = if (bottom) "top",
      legend.box.spacing = if (bottom) ggplot2::unit(0.3, "lines"),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = plot_ink[["secondary"]], face = "bold", size = 11)
    )
}

# Interactive -------------------------------------------------------------------

leaflet_fdz_map <- function(zones, fill, fdz_sf, stations, title, subtitle) {
  zones <- sf::st_transform(zones, 4326)
  fdz_sf <- sf::st_transform(fdz_sf, 4326)

  pal <- switch(
    fill$kind,
    diverging = leaflet::colorNumeric(
      grDevices::colorRampPalette(fill$colors)(101), fill$limits, na.color = map_na_color
    ),
    sequential = leaflet::colorNumeric(fill$colors, fill$limits, na.color = map_na_color),
    factor = leaflet::colorFactor(unname(fill$colors), levels = names(fill$colors), na.color = map_na_color)
  )

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addPolygons(data = fdz_sf, fill = FALSE, color = plot_ink[["muted"]], weight = 0.5)

  panels <- levels(zones$panel)
  for (p in panels) {
    z <- zones[zones$panel == p, ]
    m <- m |>
      leaflet::addPolygons(
        data = z,
        group = p,
        fillColor = pal(z$value),
        fillOpacity = fill$alpha %||% 0.75,
        color = ifelse(z$outlined, map_outline_color, map_border_color),
        weight = ifelse(z$outlined, 2.5, 0.8),
        label = lapply(z$hover, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(weight = 3, color = map_outline_color, bringToFront = TRUE)
      )
  }

  m <- m |>
    leaflet::addCircleMarkers(
      data = stations,
      group = "Stations",
      radius = ifelse(stations$status == "No unit", 3, 6),
      color = "white",
      weight = 1,
      fillColor = unname(map_station_colors[as.character(stations$status)]),
      fillOpacity = 1,
      label = sprintf("Station %s: %s", stations$station_id, tolower(stations$status))
    )

  legend_values <- if (fill$kind == "factor") factor(names(fill$colors), levels = names(fill$colors)) else fill$limits
  label_format <- if (fill$kind == "factor") {
    leaflet::labelFormat()
  } else {
    function(type, ...) fill$labels(list(...)[[1]])
  }
  m <- m |>
    leaflet::addLegend(
      "bottomright",
      pal = pal, values = legend_values, title = fill$title,
      labFormat = label_format, opacity = fill$alpha %||% 0.9
    ) |>
    leaflet::addControl(
      htmltools::HTML(sprintf(
        "<div style='font:13px sans-serif'><b>%s</b><br><span style='color:%s'>%s</span></div>",
        htmltools::htmlEscape(title), plot_ink[["secondary"]], htmltools::htmlEscape(subtitle)
      )),
      position = "topleft"
    )

  if (length(panels) > 1) {
    m <- m |>
      leaflet::addLayersControl(
        baseGroups = panels,
        overlayGroups = "Stations",
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }
  attr(m, "map_title") <- title
  m
}

map_title_text <- function(m) {
  title <- attr(m, "map_title")
  if (is.null(title)) "Map" else title
}
