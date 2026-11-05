# Travel-time box plots (ggplot2), for reports.
#
# Each takes the call-level paired_calls() table (step 06 saves it as
# paired_calls.parquet) and shows the baseline and scenarios side by side:
# overall, by station territory, or by fire demand zone.
#
#   travel_time_boxplot(paired, baseline_name = "Baseline Rescues")
#   travel_time_boxplot(paired, scenario = c("Baseline Rescues", "+R1"), by = "station")
#   travel_time_boxplot(paired, by = "fdz", stations = 16, orientation = "vertical")
#   travel_time_boxplot(paired, by = "station", split_changed = TRUE)
#   travel_time_boxplot(paired, by = "fdz", include_unchanged = FALSE)
#   p |> save_plot("overall.png")

#' Box plots of modelled travel time, baseline and scenarios
#'
#' One box per group (the baseline or a scenario) over its calls' modelled
#' drive minutes. Calls a side can't serve (`NA` minutes) are left out of that
#' side's box only, so a box can hold a few calls fewer than another.
#'
#' Station and FDZ plots group calls by where they happened (`station_id`, the
#' territory station, and `fdz_id`), not by who responded, so every group in a
#' row holds the same calls.
#'
#' @param df Output of [paired_calls()], e.g. `paired_calls.parquet`. Needs a
#'   `station_id` column for `by = "station"` or `stations`.
#' @param scenario The groups to show, in order: scenario names or ids, and
#'   `baseline_name` for the baseline. `NULL` shows the baseline and then every
#'   scenario.
#' @param by `"overall"` (one box per group), `"station"` (per territory
#'   station) or `"fdz"` (per zone).
#' @param stations Territory station ids to keep. `NULL` keeps all.
#' @param fdz Zone ids to keep. `NULL` keeps all (within `stations`).
#' @param orientation `"horizontal"` (boxes run left to right, groups,
#'   stations or zones down the y axis) or `"vertical"`.
#' @param filter_name,due Filter set and due level to show. `NULL` uses the
#'   first in `df` (with a message if there are others).
#' @param baseline_name Name of the baseline, as shown and as given in
#'   `scenario`.
#' @param split_changed Group the category axis into "Changed" and "No
#'   Change" panels. A call changes when its responding station in any of
#'   the selected scenarios differs from the baseline's (or it is served on
#'   one side only); a station or zone changes when any of its calls do.
#'   Every box in a No Change station or zone equals the baseline's.
#' @param include_unchanged `FALSE` leaves out the calls (overall) or the
#'   stations and zones (by station or FDZ) that don't change, and adds
#'   "Changed only" to the automatic subtitle.
#' @param time_format Minutes axis labels: `"decimal"` (7.5) or `"mmss"`
#'   (7:30).
#' @param outliers Draw points beyond the whiskers. With `FALSE` they are
#'   also left out of the axis range.
#' @param title,subtitle Override the automatic title and subtitle.
#'
#' @return A ggplot. Up to 8 scenarios get their own colour, in the order
#'   given; the baseline is gray. Overall plots with more than 8 share one
#'   colour (the axis labels name them); station and FDZ plots need 8 or
#'   fewer.
#' @noRd
travel_time_boxplot <- function(
  df,
  scenario = NULL,
  by = c("overall", "station", "fdz"),
  stations = NULL,
  fdz = NULL,
  orientation = c("horizontal", "vertical"),
  filter_name = NULL,
  due = NULL,
  baseline_name = "Baseline",
  split_changed = FALSE,
  include_unchanged = TRUE,
  time_format = c("decimal", "mmss"),
  outliers = TRUE,
  title = NULL,
  subtitle = NULL
) {
  by <- match.arg(by)
  orientation <- match.arg(orientation)
  time_format <- match.arg(time_format)

  df <- as.data.frame(df)
  filter_name <- pick_one(df$filter_name, filter_name, "filter set")
  due <- pick_one(df$due, due, "due level")
  df <- df[df$filter_name == filter_name & df$due == due, ]

  if ((by == "station" || !is.null(stations)) && !"station_id" %in% names(df)) {
    stop(
      "df has no station_id column; re-run step 06 to save paired_calls.parquet with it",
      call. = FALSE
    )
  }
  check_ids(stations, df$station_id, "station")
  check_ids(fdz, df$fdz_id, "FDZ")

  groups <- plot_groups(df, scenario, baseline_name)
  if ((split_changed || !include_unchanged) && all(groups == baseline_name)) {
    stop(
      "split_changed and include_unchanged = FALSE need at least one scenario besides the baseline",
      call. = FALSE
    )
  }
  long <- plot_times_long(df, groups, baseline_name)
  if (!is.null(stations)) {
    long <- long[long$station_id %in% stations, ]
  }
  if (!is.null(fdz)) {
    long <- long[long$fdz_id %in% fdz, ]
  }

  if (by == "overall") {
    long$category <- long$group
  } else if (by == "station") {
    no_station <- is.na(long$station_id)
    if (any(no_station)) {
      message(sum(no_station), " call rows have no territory station and are left out")
      long <- long[!no_station, ]
    }
    long$category <- factor(long$station_id, levels = sort(unique(long$station_id)))
  } else {
    zones <- unique(long$fdz_id)
    long$category <- factor(long$fdz_id, levels = zones[natural_order(zones)])
  }
  if (nrow(long) == 0) {
    stop("No calls left to plot", call. = FALSE)
  }

  # Changed / No Change: each call overall, a whole station or zone otherwise
  changed <- if (by == "overall") long$changed else stats::ave(long$changed, long$category, FUN = any)
  long$change <- factor(ifelse(changed, "Changed", "No Change"), levels = c("Changed", "No Change"))
  if (!include_unchanged) {
    long <- long[long$change == "Changed", ]
    if (nrow(long) == 0) {
      stop("No calls change responder in the selected scenarios", call. = FALSE)
    }
  }
  long$category <- droplevels(long$category)
  if (by == "fdz" && is.null(stations) && is.null(fdz) && nlevels(long$category) > 30) {
    message("Showing ", nlevels(long$category), " zones; narrow it with `stations` or `fdz`")
  }

  horizontal <- orientation == "horizontal"
  # Read top to bottom: the first category (and the first group within it) on top
  if (horizontal) {
    long$category <- factor(long$category, levels = rev(levels(long$category)))
  }
  mapping <- if (horizontal) {
    ggplot2::aes(x = minutes, y = category, fill = group)
  } else {
    ggplot2::aes(x = category, y = minutes, fill = group)
  }

  boxes <- ggplot2::geom_boxplot(
    orientation = if (horizontal) "y" else "x",
    position = ggplot2::position_dodge2(preserve = "single", reverse = horizontal),
    width = 0.7,
    colour = plot_ink[["secondary"]],
    linewidth = 0.4,
    outliers = outliers,
    outlier.size = 0.8,
    outlier.alpha = 0.4
  )

  minutes_scale <- if (horizontal) ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
  minutes_labels <- if (time_format == "mmss") {
    function(x) vapply(x, function(v) if (is.na(v)) "" else format_mmss(v), character(1))
  } else {
    ggplot2::waiver()
  }
  minutes_title <- if (time_format == "mmss") "Travel Time (m:ss)" else "Travel Time (min)"
  category_title <- switch(by, overall = NULL, station = "Territory Station", fdz = "FDZ")

  if (is.null(title)) {
    title <- switch(
      by,
      overall = "Modelled Travel Time",
      station = "Modelled Travel Time by Station Territory",
      fdz = "Modelled Travel Time by FDZ"
    )
  }
  if (is.null(subtitle)) {
    subtitle <- paste(filter_name, paste(due_ordinal(due), "Due"), sep = " · ")
    if (!include_unchanged) {
      subtitle <- paste(subtitle, "Changed only", sep = " · ")
    }
  }

  # A second level on the category axis: a panel each for Changed and No
  # Change, sized to the categories in it
  panels <- if (!split_changed) {
    NULL
  } else if (horizontal) {
    ggplot2::facet_grid(rows = ggplot2::vars(change), scales = "free_y", space = "free_y", switch = "y")
  } else {
    ggplot2::facet_grid(cols = ggplot2::vars(change), scales = "free_x", space = "free_x", switch = "x")
  }

  ggplot2::ggplot(long, mapping) +
    boxes +
    panels +
    minutes_scale(name = minutes_title, labels = minutes_labels) +
    ggplot2::scale_fill_manual(values = group_colors(groups, baseline_name, by), name = NULL) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = if (horizontal) minutes_title else category_title,
      y = if (horizontal) category_title else minutes_title
    ) +
    plot_theme(horizontal, legend = by != "overall")
}

#' Save a plot to an image file
#'
#' @param p A ggplot, e.g. from [travel_time_boxplot()].
#' @param path File to write: `.png` or `.pdf`.
#' @param width,height Size in inches.
#'
#' @return `path`, invisibly.
#' @noRd
save_plot <- function(p, path, width = 8, height = 6) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("png", "pdf")) {
    stop("save_plot() writes .png or .pdf files", call. = FALSE)
  }
  ggplot2::ggsave(path, p, width = width, height = height, dpi = 200, bg = "white")
  invisible(path)
}

# Internals -------------------------------------------------------------------

# Categorical colours in fixed order (validated for colour-blind separation);
# the baseline is a neutral gray, so scenarios stand out against it
plot_colors <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
plot_baseline_color <- "#a3a29b"
plot_ink <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781", grid = "#e1e0d9")

# The groups to plot as names, in order: scenario names or ids, and the
# baseline's name. NULL = baseline, then every scenario in df.
plot_groups <- function(df, scenario, baseline_name) {
  available <- unique(df$scenario_name)
  if (is.null(scenario)) {
    return(c(baseline_name, available))
  }
  by_id <- df$scenario_name[match(as.character(scenario), as.character(df$scenario_id))]
  groups <- ifelse(scenario %in% c(baseline_name, available), as.character(scenario), by_id)
  if (anyNA(groups)) {
    stop(
      "Unknown scenario: ", paste(scenario[is.na(groups)], collapse = ", "),
      ". Available: ", paste(c(baseline_name, available), collapse = ", "),
      call. = FALSE
    )
  }
  unique(groups)
}

# The wide paired table (one filter set and due level) as one row per call and
# group: group, dest_id, fdz_id, station_id (if any), minutes, and changed
# (the call's responder differs from the baseline's in any selected
# scenario). The baseline repeats once per scenario in paired_calls(), so it
# is taken once per call.
plot_times_long <- function(df, groups, baseline_name) {
  scen_rows <- df[df$scenario_name %in% setdiff(groups, baseline_name), ]
  call_changed <- tapply(
    responder_differs(scen_rows$baseline_source_id, scen_rows$scenario_source_id),
    as.character(scen_rows$dest_id),
    any
  )

  cols <- intersect(c("dest_id", "fdz_id", "station_id"), names(df))
  parts <- lapply(groups, function(g) {
    if (g == baseline_name) {
      rows <- df[!duplicated(df$dest_id), ]
      minutes <- rows$baseline_minutes
    } else {
      rows <- df[df$scenario_name == g, ]
      minutes <- rows$scenario_minutes
    }
    out <- data.frame(group = rep(g, nrow(rows)), rows[cols], minutes = minutes)
    out[!is.na(out$minutes), ]
  })
  out <- do.call(rbind, parts)
  out$group <- factor(out$group, levels = groups)
  out$changed <- unname(call_changed[as.character(out$dest_id)])
  out$changed[is.na(out$changed)] <- FALSE # baseline only: nothing to compare
  rownames(out) <- NULL
  out
}

check_ids <- function(given, available, label) {
  if (is.null(given)) {
    return(invisible())
  }
  available <- unique(available[!is.na(available)])
  unknown <- setdiff(given, available)
  if (length(unknown) > 0) {
    stop(
      "Unknown ", label, ": ", paste(unknown, collapse = ", "),
      ". Available: ", paste(sort(available), collapse = ", "),
      call. = FALSE
    )
  }
}

group_colors <- function(groups, baseline_name, by) {
  scen <- setdiff(groups, baseline_name)
  if (length(scen) <= length(plot_colors)) {
    scen_colors <- plot_colors[seq_along(scen)]
  } else if (by == "overall") {
    scen_colors <- rep(plot_colors[1], length(scen))
  } else {
    stop(
      "At most ", length(plot_colors), " scenarios can be told apart by colour; ",
      "pick fewer with `scenario`",
      call. = FALSE
    )
  }
  stats::setNames(c(plot_baseline_color, scen_colors)[match(groups, c(baseline_name, scen))], groups)
}

# Minimal theme: quiet gridlines along the minutes axis only, legend below
plot_theme <- function(horizontal, legend) {
  category_grid <- if (horizontal) "panel.grid.major.y" else "panel.grid.major.x"
  theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(colour = plot_ink[["primary"]], face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = plot_ink[["secondary"]]),
      axis.title = ggplot2::element_text(colour = plot_ink[["secondary"]]),
      axis.text = ggplot2::element_text(colour = plot_ink[["secondary"]]),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = plot_ink[["grid"]], linewidth = 0.3),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(colour = plot_ink[["secondary"]], face = "bold"),
      legend.position = if (legend) "bottom" else "none",
      legend.text = ggplot2::element_text(colour = plot_ink[["secondary"]])
    )
  theme + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_blank()), category_grid))
}
