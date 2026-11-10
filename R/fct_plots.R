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
#
# And a cumulative curve per group (% of calls reached within X minutes):
#
#   travel_time_ecdf(paired, baseline_name = "Baseline Rescues", benchmarks = c("8 min" = 8))
#
# And a forest plot of scenario-vs-scenario differences with their intervals,
# from compare_scenarios():
#
#   scenario_pairs_plot(compare_scenarios(paired, baseline_name = "Baseline Rescues"))

#' Box plots of modeled travel time, baseline and scenarios
#'
#' One box per group (the baseline or a scenario) over its calls' modeled
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
#' @return A ggplot. Up to 8 scenarios get their own color, in the order
#'   given; the baseline is gray. Overall plots with more than 8 share one
#'   color (the axis labels name them); station and FDZ plots need 8 or
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
    color = plot_ink[["secondary"]],
    linewidth = 0.4,
    outliers = outliers,
    outlier.size = 0.8,
    outlier.alpha = 0.4
  )

  minutes_scale <- if (horizontal) ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
  minutes_labels <- minutes_axis_labels(time_format)
  minutes_title <- minutes_axis_title(time_format)
  category_title <- switch(by, overall = NULL, station = "Territory Station", fdz = "FDZ")

  if (is.null(title)) {
    title <- switch(
      by,
      overall = "Modeled Travel Time",
      station = "Modeled Travel Time by Station Territory",
      fdz = "Modeled Travel Time by FDZ"
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

#' Cumulative modeled travel time, baseline and scenarios overlaid
#'
#' One line per group (the baseline or a scenario): the share of its calls
#' reached within each number of minutes. Calls a side can't serve (`NA`
#' minutes) are left out of that side's line only.
#'
#' @inheritParams travel_time_boxplot
#' @param include_unchanged `FALSE` keeps only the calls whose responding
#'   station changes in any selected scenario, and adds "Changed only" to the
#'   automatic subtitle.
#' @param benchmarks Minutes to mark with a dashed vertical line, e.g.
#'   `c(4, 8)`. Names become the labels (`c("NFPA" = 8)`); unnamed ones are
#'   labeled with the time ("8 min", or "8:00" with `time_format = "mmss"`).
#' @param max_minutes Cut the x axis off here. A zoom only: the percentages
#'   still count every call.
#' @param curve `"line"` (straight lines across each 0.1-minute rounding
#'   step; smooth) or `"step"` (the exact staircase of the rounded times).
#'
#' @return A ggplot. Up to 8 scenarios get their own color, in the order
#'   given; the baseline is gray.
#' @noRd
travel_time_ecdf <- function(
  df,
  scenario = NULL,
  stations = NULL,
  fdz = NULL,
  filter_name = NULL,
  due = NULL,
  baseline_name = "Baseline",
  include_unchanged = TRUE,
  benchmarks = NULL,
  time_format = c("decimal", "mmss"),
  max_minutes = NULL,
  curve = c("line", "step"),
  title = NULL,
  subtitle = NULL
) {
  time_format <- match.arg(time_format)
  curve <- match.arg(curve)
  if (!is.null(benchmarks) && (!is.numeric(benchmarks) || anyNA(benchmarks))) {
    stop("benchmarks must be numeric minutes, e.g. c(4, 8)", call. = FALSE)
  }

  df <- as.data.frame(df)
  filter_name <- pick_one(df$filter_name, filter_name, "filter set")
  due <- pick_one(df$due, due, "due level")
  df <- df[df$filter_name == filter_name & df$due == due, ]

  if (!is.null(stations) && !"station_id" %in% names(df)) {
    stop(
      "df has no station_id column; re-run step 06 to save paired_calls.parquet with it",
      call. = FALSE
    )
  }
  check_ids(stations, df$station_id, "station")
  check_ids(fdz, df$fdz_id, "FDZ")

  groups <- plot_groups(df, scenario, baseline_name)
  if (!include_unchanged && all(groups == baseline_name)) {
    stop("include_unchanged = FALSE needs at least one scenario besides the baseline", call. = FALSE)
  }
  long <- plot_times_long(df, groups, baseline_name)
  if (!is.null(stations)) {
    long <- long[long$station_id %in% stations, ]
  }
  if (!is.null(fdz)) {
    long <- long[long$fdz_id %in% fdz, ]
  }
  if (!include_unchanged) {
    long <- long[long$changed, ]
    if (nrow(long) == 0) {
      stop("No calls change responder in the selected scenarios", call. = FALSE)
    }
  }
  if (nrow(long) == 0) {
    stop("No calls left to plot", call. = FALSE)
  }

  if (is.null(title)) {
    title <- "Cumulative Modeled Travel Time"
  }
  if (is.null(subtitle)) {
    subtitle <- paste(filter_name, paste(due_ordinal(due), "Due"), sep = " · ")
    if (!include_unchanged) {
      subtitle <- paste(subtitle, "Changed only", sep = " · ")
    }
  }

  # Dashed line per benchmark, its name along the top
  marks <- NULL
  if (length(benchmarks) > 0) {
    labels <- names(benchmarks)
    if (is.null(labels)) {
      labels <- rep("", length(benchmarks))
    }
    unnamed <- is.na(labels) | labels == ""
    labels[unnamed] <- if (time_format == "mmss") {
      vapply(benchmarks[unnamed], format_mmss, character(1))
    } else {
      paste(format(benchmarks[unnamed], trim = TRUE), "min")
    }
    marks <- list(
      ggplot2::geom_vline(
        xintercept = unname(benchmarks),
        linetype = "dashed",
        color = plot_ink[["muted"]],
        linewidth = 0.4
      ),
      ggplot2::annotate(
        "text",
        x = unname(benchmarks),
        y = 1,
        label = labels,
        angle = 90,
        hjust = 1,
        vjust = -0.4,
        family = output_font,
        size = 3,
        color = plot_ink[["secondary"]]
      )
    )
  }

  curve_layer <- if (curve == "step") {
    ggplot2::stat_ecdf(ggplot2::aes(x = minutes, color = group), geom = "step", pad = FALSE, linewidth = 0.7)
  } else {
    ggplot2::geom_line(
      data = ecdf_points(long),
      ggplot2::aes(x = minutes, y = reached, color = group),
      linewidth = 0.7
    )
  }

  ggplot2::ggplot(long) +
    curve_layer +
    marks +
    ggplot2::scale_x_continuous(labels = minutes_axis_labels(time_format)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::coord_cartesian(xlim = if (is.null(max_minutes)) NULL else c(0, max_minutes), ylim = c(0, 1)) +
    ggplot2::scale_color_manual(values = group_colors(groups, baseline_name, "station"), name = NULL) +
    ggplot2::labs(title = title, subtitle = subtitle, x = minutes_axis_title(time_format), y = "Calls Reached") +
    plot_theme(horizontal = FALSE, legend = TRUE) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = plot_ink[["grid"]], linewidth = 0.3)
    )
}

#' Forest plot of scenario-vs-scenario differences
#'
#' One row per pair ("A vs B", first pair on top): a dot at the difference,
#' first minus second, and a line across its bootstrap interval. Pairs whose
#' interval crosses 0 are drawn as gray open dots: on these calls they can't
#' be told apart. Several metrics sit side by side, each on its own axis.
#'
#' @param df Output of [compare_scenarios()].
#' @param metrics Metrics to show, in order. `NULL` shows every one in `df`.
#' @param filter_name,due Filter set and due level to show. `NULL` uses the
#'   first in `df` (with a message if there are others).
#' @param title,subtitle Override the automatic title and subtitle.
#' @param group_first `TRUE` groups the rows by the first scenario: a label
#'   per group ("A vs") down the left, and each row named only by the second
#'   scenario.
#'
#' @return A ggplot. Times are in minutes, `pct_within` shares in percentage
#'   points and distances in miles.
#' @noRd
scenario_pairs_plot <- function(
  df,
  metrics = NULL,
  filter_name = NULL,
  due = NULL,
  title = NULL,
  subtitle = NULL,
  group_first = FALSE
) {
  df <- as.data.frame(df)
  filter_name <- pick_one(df$filter_name, filter_name, "filter set")
  due <- pick_one(df$due, due, "due level")
  df <- df[df$filter_name == filter_name & df$due == due, ]
  metrics <- metrics %||% unique(df$metric)
  check_metrics(metrics, df$metric)
  df <- df[df$metric %in% metrics, ]

  # display units: shares as percentage points, distance in miles
  unit <- ifelse(is_share(df$metric), 100, ifelse(is_distance(df$metric), 1 / 1609.344, 1))
  df$x <- df$diff * unit
  df$lo <- df$ci_lower * unit
  df$hi <- df$ci_upper * unit

  if (group_first) {
    groups <- unique(paste(df$scenario_a, "vs"))
    df$group <- factor(paste(df$scenario_a, "vs"), levels = groups)
    df$pair <- factor(df$scenario_b, levels = rev(unique(df$scenario_b)))
  } else {
    pairs <- unique(paste(df$scenario_a, "vs", df$scenario_b))
    df$pair <- factor(paste(df$scenario_a, "vs", df$scenario_b), levels = rev(pairs))
  }
  df$facet <- factor(pair_strip_label(df$metric), levels = pair_strip_label(metrics))
  panels <- if (group_first) {
    # one row of panels per group, each only as tall as its pairs, the group
    # label left of the scenario names
    ggplot2::facet_grid(
      rows = ggplot2::vars(group), cols = ggplot2::vars(facet),
      scales = "free", space = "free_y", switch = "y"
    )
  } else {
    ggplot2::facet_wrap(ggplot2::vars(facet), nrow = 1, scales = "free_x")
  }

  ci <- pairs_ci_text(df)
  verdicts <- c("Clear difference", paste0("Within noise (", ci$label, " crosses 0)"))
  df$verdict <- factor(ifelse(df$ci_excludes_zero, verdicts[1], verdicts[2]), levels = verdicts)

  if (is.null(title)) {
    title <- "Scenario vs Scenario"
  }
  if (is.null(subtitle)) {
    # the interval description on its own line: it can be long
    subtitle <- paste0(paste(filter_name, paste(due_ordinal(due), "Due"), sep = " · "), "\n", ci$source)
  }

  ggplot2::ggplot(df, ggplot2::aes(y = pair)) +
    ggplot2::geom_vline(xintercept = 0, color = plot_ink[["secondary"]], linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi, color = verdict), linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = x, color = verdict, shape = verdict), size = 2.6, stroke = 0.9, fill = "white") +
    panels +
    ggplot2::expand_limits(x = 0) +
    ggplot2::scale_x_continuous(labels = signed_labels) +
    ggplot2::scale_color_manual(
      values = stats::setNames(c(plot_colors[1], plot_ink[["muted"]]), verdicts),
      drop = TRUE, # only the outcomes drawn: an absent one would have an empty key
      name = NULL
    ) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(19, 21), verdicts), drop = TRUE, name = NULL) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Difference, First Minus Second", y = NULL) +
    plot_theme(horizontal = TRUE, legend = TRUE) +
    ggplot2::theme(
      panel.spacing.x = ggplot2::unit(1.5, "lines"),
      panel.spacing.y = ggplot2::unit(0.8, "lines"),
      strip.text.y.left = ggplot2::element_text(
        angle = 0, hjust = 1, face = "bold", color = plot_ink[["primary"]]
      )
    )
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

# Categorical colors in fixed order (validated for color-blind separation);
# the baseline is a neutral gray, so scenarios stand out against it
plot_colors <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
plot_baseline_color <- "#a3a29b"
plot_ink <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781", grid = "#e1e0d9")
# One font for every output: plots, maps, tables and the workbook
output_font <- "Times New Roman"

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
      "At most ", length(plot_colors), " scenarios can be told apart by color; ",
      "pick fewer with `scenario`",
      call. = FALSE
    )
  }
  stats::setNames(c(plot_baseline_color, scen_colors)[match(groups, c(baseline_name, scen))], groups)
}

# Cumulative curve points per group, for straight lines between them. osrm
# rounds drive times to 0.1 min, so a time shown as 7.3 is really somewhere in
# 7.25-7.35. With calls spread evenly inside that window, the true curve rises
# in a straight line across it: from (min - 0.05, 0) through each
# (time + 0.05, share reached).
ecdf_points <- function(long, resolution = 0.1) {
  half <- resolution / 2
  parts <- lapply(split(long$minutes, long$group, drop = TRUE), function(m) {
    v <- sort(unique(m))
    data.frame(minutes = c(v[1] - half, v + half), reached = c(0, stats::ecdf(m)(v)))
  })
  out <- do.call(rbind, Map(function(g, d) cbind(group = g, d), names(parts), parts))
  out$group <- factor(out$group, levels = levels(long$group))
  rownames(out) <- NULL
  out
}

# Facet label for a pairs plot: the metric, its unit when the label doesn't
# carry one, and which side of 0 favors the first scenario
pair_strip_label <- function(metric) {
  label <- metric_label(metric)
  label <- ifelse(is_share(metric), paste(label, "(pct pts)"), label)
  better <- ifelse(higher_is_better(metric), "above 0", "below 0")
  paste0(label, "\n", better, " = first better")
}

# How a compare_scenarios() result's intervals are described: `label` ("95%
# CI", "95% range for a single year", "95% simultaneous CI"), `column` (a
# table header), `source` (where they come from) and `meaning` (for
# simultaneous ones, what holds together)
pairs_ci_text <- function(df) {
  pct <- function(x) paste0(signif(100 * x, 3), "%")
  joint <- pct(df$level[1])
  by_call <- "resample" %in% names(df) && df$resample[1] == "call"
  od <- if ("overdispersion" %in% names(df)) df$overdispersion[1] else 1
  pred <- if ("predict_years" %in% names(df)) df$predict_years[1] else NA
  resampled <- paste0(
    if (by_call) "calls resampled" else "FDZs resampled",
    if (od > 1) sprintf(", widened ×%.2f for year-to-year swings in zone demand", sqrt(od))
  )
  # a CI of the long-run difference, or a range for a future period
  kind <- interval_text(df$level[1], pred)
  noun <- if (is.na(pred)) "CI" else "range"
  span <- if (is.na(pred)) "" else sub("^.*range", "", kind$phrase) # " for a single year"
  if (!"ci_type" %in% names(df) || df$ci_type[1] != "simultaneous") {
    return(list(
      label = kind$phrase,
      column = kind$short,
      source = if (by_call || od > 1 || !is.na(pred)) paste0(kind$phrase, ", ", resampled) else paste(joint, "CI from resampling whole FDZs"),
      meaning = NULL
    ))
  }
  each <- df$per_interval_level[!is.na(df$per_interval_level)]
  each <- if (length(each) == 0) {
    ""
  } else if (pct(min(each)) == pct(max(each))) {
    pct(min(each))
  } else {
    paste0(pct(min(each)), "–", pct(max(each)))
  }
  list(
    label = paste(joint, "simultaneous", noun),
    column = sub(" ", " Sim. ", kind$short, fixed = TRUE), # "95% Sim. CI"
    source = paste0(joint, " simultaneous ", noun, "s", span, " per metric (each ", each, " alone), ", resampled),
    meaning = paste0(
      "Simultaneous: per metric, all ", max(df$n_comparisons), " ", noun, "s hold together ", joint,
      " of the time; on its own each is a ", each, " ", noun, "."
    )
  )
}

# Axis labels with a sign on gains: -0.5, 0, +0.5
signed_labels <- function(x) {
  out <- format(x, trim = TRUE, drop0trailing = TRUE)
  ifelse(is.na(x), "", ifelse(x > 0, paste0("+", out), out))
}

# Minutes axis labels and title, as decimal minutes or m:ss
minutes_axis_labels <- function(time_format) {
  if (time_format == "mmss") {
    function(x) vapply(x, function(v) if (is.na(v)) "" else format_mmss(v), character(1))
  } else {
    ggplot2::waiver()
  }
}

minutes_axis_title <- function(time_format) {
  if (time_format == "mmss") "Travel Time (m:ss)" else "Travel Time (min)"
}

# Minimal theme: quiet gridlines along the minutes axis only, legend below
plot_theme <- function(horizontal, legend) {
  category_grid <- if (horizontal) "panel.grid.major.y" else "panel.grid.major.x"
  theme <- ggplot2::theme_minimal(base_size = 11, base_family = output_font) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(color = plot_ink[["primary"]], face = "bold"),
      plot.subtitle = ggplot2::element_text(color = plot_ink[["secondary"]]),
      axis.title = ggplot2::element_text(color = plot_ink[["secondary"]]),
      axis.text = ggplot2::element_text(color = plot_ink[["secondary"]]),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = plot_ink[["grid"]], linewidth = 0.3),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(color = plot_ink[["secondary"]], face = "bold"),
      legend.position = if (legend) "bottom" else "none",
      legend.text = ggplot2::element_text(color = plot_ink[["secondary"]])
    )
  theme + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_blank()), category_grid))
}
