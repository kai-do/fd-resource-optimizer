# Print-ready comparison tables (flextable), for Word or images.
#
# Each function takes a compare_*() result and shows one scenario against the
# baseline by default; pass several scenarios to put them side by side.
#
#   overall_table(cmp_overall, baseline_name = "Baseline Rescues")
#   station_table(cmp_station, scenario = c("R26->16 +R1", "R11->16 +R1"))
#   fdz_table(cmp_fdz, metrics = c("median_minutes", "p90_minutes"))
#   calls_table(cmp_calls) |> save_table("calls.docx")

#' Overall comparison table
#'
#' @param df Output of [compare_overall()].
#' @param scenario Scenario name(s) or id(s). `NULL` shows the first scenario
#'   in `df`; several show one column group each, next to one Baseline column.
#' @param filter_name,due Filter set and due level to show. `NULL` uses the
#'   first in `df` (with a message if there are others).
#' @param metrics Metrics to show, in order. `NULL` for the default set.
#' @param baseline_name Name of the baseline scenario, for the subtitle.
#' @param title,subtitle Override the automatic title and subtitle.
#'
#' @return A flextable: one row per metric with Baseline, Scenario, Diff and %
#'   Diff columns (and 95% CI when `df` has intervals).
#' @noRd
overall_table <- function(
  df,
  scenario = NULL,
  filter_name = NULL,
  due = NULL,
  metrics = NULL,
  baseline_name = "Baseline",
  title = NULL,
  subtitle = NULL
) {
  sel <- select_compare(df, scenario, filter_name, due)
  if (is.null(metrics)) {
    metrics <- intersect(
      c(
        "n_calls", "mean_minutes", "median_minutes", "p90_minutes", "total_minutes",
        grep("^pct_within_", unique(sel$df$metric), value = TRUE),
        "unserved_calls"
      ),
      sel$df$metric
    )
  }
  check_metrics(metrics, sel$df$metric)
  has_ci <- "ci_lower" %in% names(sel$df)

  body <- data.frame(metric = metric_label(metrics))
  colors <- data.frame(metric = rep(NA_character_, length(metrics)))
  first <- sel$df[sel$df$scenario_name == sel$scenarios[1], ]
  body$baseline <- format_values(first$baseline[match(metrics, first$metric)], metrics)
  colors$baseline <- NA_character_
  groups <- c(NA, NA)
  labels <- c("Metric", "Baseline")

  for (k in seq_along(sel$scenarios)) {
    rows <- sel$df[sel$df$scenario_name == sel$scenarios[k], ]
    rows <- rows[match(metrics, rows$metric), ]
    id <- paste0("s", k)
    body[[paste0(id, "_scenario")]] <- format_values(rows$scenario, metrics)
    body[[paste0(id, "_diff")]] <- format_values(rows$diff, metrics, diff = TRUE)
    body[[paste0(id, "_pct")]] <- format_pct_diff(rows$pct_diff, metrics)
    colors[[paste0(id, "_scenario")]] <- NA_character_
    colors[[paste0(id, "_diff")]] <- diff_colors(rows$diff, metrics)
    colors[[paste0(id, "_pct")]] <- NA_character_
    new_labels <- c("Scenario", "Diff", "% Diff")
    if (has_ci) {
      body[[paste0(id, "_ci")]] <- format_ci(rows$ci_lower, rows$ci_upper, metrics)
      colors[[paste0(id, "_ci")]] <- NA_character_
      new_labels <- c(new_labels, "95% CI")
    }
    labels <- c(labels, new_labels)
    groups <- c(groups, rep(sel$scenarios[k], length(new_labels)))
  }

  build_table(
    body, labels,
    groups = if (length(sel$scenarios) > 1) groups,
    colors = colors,
    n_keys = 1,
    title = title %||% "Scenario Comparison: Overall",
    subtitle = subtitle %||% compare_subtitle(sel, baseline_name)
  )
}

#' Call-level change table
#'
#' @inheritParams overall_table
#' @param df Output of [compare_calls()].
#'
#' @return A flextable: one row per metric, one value column per scenario.
#' @noRd
calls_table <- function(
  df,
  scenario = NULL,
  filter_name = NULL,
  due = NULL,
  metrics = NULL,
  baseline_name = "Baseline",
  title = NULL,
  subtitle = NULL
) {
  sel <- select_compare(df, scenario, filter_name, due)
  metrics <- metrics %||% unique(sel$df$metric)
  check_metrics(metrics, sel$df$metric)

  body <- data.frame(metric = metric_label(metrics))
  colors <- data.frame(metric = rep(NA_character_, length(metrics)))
  for (k in seq_along(sel$scenarios)) {
    rows <- sel$df[sel$df$scenario_name == sel$scenarios[k], ]
    rows <- rows[match(metrics, rows$metric), ]
    # changes are already scenario minus baseline, so they read as diffs
    is_change <- grepl("_change$", metrics)
    body[[paste0("s", k)]] <- ifelse(
      is_change,
      format_values(rows$value, metrics, diff = TRUE),
      format_values(rows$value, metrics)
    )
    colors[[paste0("s", k)]] <- ifelse(is_change, diff_colors(rows$value, metrics), NA_character_)
  }
  labels <- c("Metric", if (length(sel$scenarios) == 1) "Value" else sel$scenarios)

  build_table(
    body, labels,
    groups = NULL,
    colors = colors,
    n_keys = 1,
    title = title %||% "Scenario Comparison: Call-Level Changes",
    subtitle = subtitle %||% compare_subtitle(sel, baseline_name)
  )
}

#' Zone comparison table
#'
#' @inheritParams overall_table
#' @param df Output of [compare_by_fdz()].
#' @param metrics Metric(s) to show; several only with one scenario. Default
#'   `"median_minutes"`.
#'
#' @return A flextable: one row per FDZ, with the baseline and scenario
#'   responding stations and Baseline / Scenario / Diff for each metric.
#' @noRd
fdz_table <- function(
  df,
  scenario = NULL,
  filter_name = NULL,
  due = NULL,
  metrics = "median_minutes",
  baseline_name = "Baseline",
  title = NULL,
  subtitle = NULL
) {
  rows_table(
    df, key = "fdz_id", key_label = "FDZ",
    scenario, filter_name, due, metrics, baseline_name,
    title_prefix = "FDZ Comparison",
    neutral = FALSE,
    responders = TRUE,
    title = title, subtitle = subtitle
  )
}

#' Station workload table
#'
#' @inheritParams overall_table
#' @param df Output of [compare_by_station()].
#' @param metrics Metric(s) to show; several only with one scenario. Default
#'   `"n_incidents"`.
#'
#' @return A flextable: one row per station with Baseline / Scenario / Diff for
#'   each metric. A station that does not respond on one side shows 0 calls.
#' @noRd
station_table <- function(
  df,
  scenario = NULL,
  filter_name = NULL,
  due = NULL,
  metrics = "n_incidents",
  baseline_name = "Baseline",
  title = NULL,
  subtitle = NULL
) {
  rows_table(
    df, key = "source_id", key_label = "Station",
    scenario, filter_name, due, metrics, baseline_name,
    title_prefix = "Station Workload",
    neutral = TRUE,
    responders = FALSE,
    title = title, subtitle = subtitle
  )
}

#' Save a table to Word or PNG
#'
#' @param ft A flextable.
#' @param path File path ending in .docx (an editable Word table) or .png.
#'
#' @return `path`, invisibly.
#' @noRd
save_table <- function(ft, path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "docx") {
    flextable::save_as_docx(ft, path = path)
  } else if (ext == "png") {
    flextable::save_as_image(ft, path = path, res = 200)
  } else {
    stop("save_table() writes .docx or .png files", call. = FALSE)
  }
  invisible(path)
}

# Station and FDZ tables ------------------------------------------------------

rows_table <- function(
  df, key, key_label, scenario, filter_name, due, metrics, baseline_name,
  title_prefix, neutral, responders, title, subtitle
) {
  sel <- select_compare(df, scenario, filter_name, due)
  check_metrics(metrics, sel$df$metric)
  if (length(sel$scenarios) > 1 && length(metrics) > 1) {
    stop("With several scenarios, pick one metric", call. = FALSE)
  }

  sub <- sel$df[sel$df$metric %in% metrics, ]
  keys <- unique(sub[[key]])
  keys <- keys[natural_order(keys)]
  get <- function(sc, m, col) {
    rows <- sub[sub$scenario_name == sc & sub$metric == m, ]
    rows[[col]][match(keys, rows[[key]])]
  }
  # a station missing from a scenario's rows responds to nothing there
  fill_zero <- function(x, m) if (neutral && is_count(m)) ifelse(is.na(x), 0, x) else x
  diff_color <- function(x, m) if (neutral) neutral_colors(x) else diff_colors(x, rep(m, length(x)))

  body <- stats::setNames(data.frame(as.character(keys)), key)
  colors <- data.frame(key = rep(NA_character_, length(keys)))
  labels <- key_label
  groups <- NA
  n_keys <- 1

  if (responders) {
    first <- sub[sub$scenario_name == sel$scenarios[1] & sub$metric == metrics[1], ]
    body$baseline_station <- as.character(first$baseline_source_id[match(keys, first[[key]])])
    colors$baseline_station <- NA_character_
    labels <- c(labels, "Baseline Station")
    groups <- c(groups, NA)
    n_keys <- 2
  }

  if (length(sel$scenarios) == 1) {
    # one scenario: a Baseline / Scenario / Diff group per metric
    sc <- sel$scenarios
    if (responders) {
      first <- sub[sub$scenario_name == sc & sub$metric == metrics[1], ]
      body$scenario_station <- as.character(first$scenario_source_id[match(keys, first[[key]])])
      colors$scenario_station <- NA_character_
      labels <- c(labels, "Scenario Station")
      groups <- c(groups, NA)
      n_keys <- 3
    }
    for (m in metrics) {
      b <- fill_zero(get(sc, m, "baseline"), m)
      s <- fill_zero(get(sc, m, "scenario"), m)
      d <- if (neutral && is_count(m)) s - b else get(sc, m, "diff")
      body[[paste0(m, "_b")]] <- format_values(b, m)
      body[[paste0(m, "_s")]] <- format_values(s, m)
      body[[paste0(m, "_d")]] <- format_values(d, m, diff = TRUE)
      colors[[paste0(m, "_b")]] <- NA_character_
      colors[[paste0(m, "_s")]] <- NA_character_
      colors[[paste0(m, "_d")]] <- diff_color(d, m)
      labels <- c(labels, "Baseline", "Scenario", "Diff")
      groups <- c(groups, rep(metric_label(m), 3))
    }
    groups <- if (length(metrics) > 1) groups
  } else {
    # several scenarios, one metric: Baseline once, then Scenario / Diff each
    m <- metrics
    b <- NULL
    for (sc in sel$scenarios) {
      x <- get(sc, m, "baseline")
      b <- if (is.null(b)) x else ifelse(is.na(b), x, b)
    }
    b <- fill_zero(b, m)
    body$baseline <- format_values(b, m)
    colors$baseline <- NA_character_
    labels <- c(labels, "Baseline")
    groups <- c(groups, NA)
    for (k in seq_along(sel$scenarios)) {
      sc <- sel$scenarios[k]
      s <- fill_zero(get(sc, m, "scenario"), m)
      d <- if (neutral && is_count(m)) s - b else get(sc, m, "diff")
      new <- c()
      if (responders) {
        first <- sub[sub$scenario_name == sc & sub$metric == m, ]
        body[[paste0("s", k, "_station")]] <- as.character(first$scenario_source_id[match(keys, first[[key]])])
        colors[[paste0("s", k, "_station")]] <- NA_character_
        new <- "Station"
      }
      body[[paste0("s", k, "_s")]] <- format_values(s, m)
      body[[paste0("s", k, "_d")]] <- format_values(d, m, diff = TRUE)
      colors[[paste0("s", k, "_s")]] <- NA_character_
      colors[[paste0("s", k, "_d")]] <- diff_color(d, m)
      new <- c(new, "Scenario", "Diff")
      labels <- c(labels, new)
      groups <- c(groups, rep(sc, length(new)))
    }
  }

  body[is.na(body)] <- ""
  build_table(
    body, labels,
    groups = groups,
    colors = colors,
    n_keys = n_keys,
    title = title %||% if (length(metrics) == 1) paste0(title_prefix, ": ", metric_label(metrics)) else title_prefix,
    subtitle = subtitle %||% compare_subtitle(sel, baseline_name)
  )
}

# Selection -------------------------------------------------------------------

# Narrow a compare_*() result to one filter set, one due level and the chosen
# scenarios (first one by default)
select_compare <- function(df, scenario, filter_name, due) {
  df <- as.data.frame(df)
  filter_name <- pick_one(df$filter_name, filter_name, "filter set")
  due <- pick_one(df$due, due, "due level")
  df <- df[df$filter_name == filter_name & df$due == due, ]

  names_in_order <- unique(df$scenario_name)
  if (is.null(scenario)) {
    scenarios <- names_in_order[1]
  } else {
    by_id <- df$scenario_name[match(scenario, df$scenario_id)]
    scenarios <- ifelse(scenario %in% names_in_order, scenario, by_id)
    if (anyNA(scenarios)) {
      stop(
        "Unknown scenario: ", paste(scenario[is.na(scenarios)], collapse = ", "),
        ". Available: ", paste(names_in_order, collapse = ", "),
        call. = FALSE
      )
    }
  }
  list(df = df[df$scenario_name %in% scenarios, ], scenarios = scenarios, filter_name = filter_name, due = due)
}

pick_one <- function(values, given, label) {
  available <- unique(values)
  if (is.null(given)) {
    if (length(available) > 1) {
      message(
        "Showing ", label, " '", available[1], "'; also available: ",
        paste(available[-1], collapse = ", ")
      )
    }
    return(available[1])
  }
  if (length(given) != 1 || !given %in% available) {
    stop("Unknown ", label, ": ", given, ". Available: ", paste(available, collapse = ", "), call. = FALSE)
  }
  given
}

# Order zone ids by their number, then letter (1A, 1B, 4A, 12A, not 12A, 1A);
# ids without a number (e.g. LA) go last
natural_order <- function(x) {
  number <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", x)))
  number[!grepl("^[0-9]", x)] <- NA
  order(number, as.character(x), na.last = TRUE)
}

check_metrics <- function(metrics, available) {
  unknown <- setdiff(metrics, available)
  if (length(unknown) > 0) {
    stop(
      "Unknown metric(s): ", paste(unknown, collapse = ", "),
      ". Available: ", paste(unique(available), collapse = ", "),
      call. = FALSE
    )
  }
}

compare_subtitle <- function(sel, baseline_name) {
  versus <- if (length(sel$scenarios) == 1) {
    paste(sel$scenarios, "vs", baseline_name)
  } else {
    paste("vs", baseline_name)
  }
  paste(sel$filter_name, paste(due_ordinal(sel$due), "Due"), versus, sep = " · ")
}

due_ordinal <- function(k) {
  suffix <- ifelse(k %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[k %% 10 + 1])
  paste0(k, suffix)
}

# Labels and formats ----------------------------------------------------------

metric_labels <- c(
  n_calls = "Calls",
  n_incidents = "Incidents",
  n_fdz = "FDZs",
  mean_minutes = "Mean Minutes",
  median_minutes = "Median Minutes",
  p90_minutes = "90th Percentile Minutes",
  total_minutes = "Total Minutes",
  mean_distance_m = "Mean Distance (Miles)",
  unserved_zones = "Unserved FDZs",
  unserved_calls = "Unserved Calls",
  share_responder_changed = "Share With Responder Changed",
  share_faster = "Share Faster",
  share_slower = "Share Slower",
  share_unchanged = "Share Unchanged",
  mean_change = "Mean Change (Minutes)",
  median_change = "Median Change (Minutes)",
  p90_change = "90th Percentile Change (Minutes)",
  max_change = "Largest Change (Minutes)"
)

# Title Case (AP style: words of four or more letters capitalised, so "With"
# and "Over" but "by") label for each metric; threshold metrics carry their
# minutes
metric_label <- function(metric) {
  vapply(metric, function(m) {
    if (m %in% names(metric_labels)) {
      return(metric_labels[[m]])
    }
    x <- sub(".*_(\\d+(\\.\\d+)?)min$", "\\1", m)
    if (grepl("^pct_within_", m)) return(paste0("Within ", x, " Min"))
    if (grepl("^calls_within_", m)) return(paste0("Calls Within ", x, " Min"))
    if (grepl("^fdz_p90_over_", m)) return(paste0("FDZs With 90th Percentile Over ", x, " Min"))
    if (grepl("^calls_slower_by_", m)) return(paste0("Calls Slower by More Than ", x, " Min"))
    if (grepl("^share_slower_by_", m)) return(paste0("Share Slower by More Than ", x, " Min"))
    tools::toTitleCase(gsub("_", " ", m))
  }, character(1), USE.NAMES = FALSE)
}

is_share <- function(m) grepl("^(pct_within_|share_)", m)
is_count <- function(m) grepl("^(n_|calls_|unserved_|fdz_p90_over_)", m) | m == "total_minutes"
is_distance <- function(m) m == "mean_distance_m"

# Values as display text: counts with separators, minutes 1 decimal, shares as
# percent (their diffs in percentage points), distance in miles
format_values <- function(x, metrics, diff = FALSE) {
  metrics <- rep_len(metrics, length(x))
  out <- character(length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    m <- metrics[i]
    if (is.na(v)) {
      out[i] <- ""
      next
    }
    sign <- if (diff && v > 0) "+" else ""
    out[i] <- if (is_share(m)) {
      if (diff) sprintf("%+.1f pts", v * 100) else sprintf("%.1f%%", v * 100)
    } else if (is_distance(m)) {
      paste0(sign, formatC(v / 1609.344, format = "f", digits = 2, big.mark = ","))
    } else if (is_count(m)) {
      paste0(sign, formatC(round(v), format = "d", big.mark = ","))
    } else {
      paste0(sign, formatC(v, format = "f", digits = 1, big.mark = ","))
    }
  }
  out
}

format_pct_diff <- function(x, metrics) {
  ifelse(is.na(x) | is_share(metrics), "", sprintf("%+.1f%%", x * 100))
}

format_ci <- function(lower, upper, metrics) {
  lo <- format_values(lower, metrics, diff = TRUE)
  hi <- format_values(upper, metrics, diff = TRUE)
  ifelse(lo == "" | hi == "", "", paste0("[", lo, ", ", hi, "]"))
}

# Diff colours: red when the change is worse, green when better
worse_colour <- "#B00020"
better_colour <- "#1B7F3B"

diff_colors <- function(x, metrics) {
  metrics <- rep_len(metrics, length(x))
  higher_is_better <- grepl("^(pct_within_|calls_within_|share_faster)", metrics)
  higher_is_worse <- !higher_is_better &
    grepl("minutes$|_change$|^mean_distance|^unserved_|^fdz_p90_over_|^calls_slower_by_|^share_slower", metrics)
  out <- rep(NA_character_, length(x))
  up <- !is.na(x) & x > 0
  down <- !is.na(x) & x < 0
  out[up & higher_is_worse] <- worse_colour
  out[down & higher_is_worse] <- better_colour
  out[up & higher_is_better] <- better_colour
  out[down & higher_is_better] <- worse_colour
  out
}

# Workload changes are neither better nor worse: blue up, orange down
neutral_colors <- function(x) {
  ifelse(is.na(x) | x == 0, NA_character_, ifelse(x > 0, "#1F5FA8", "#B3541E"))
}

# Building --------------------------------------------------------------------

# A styled flextable from display text. `labels` name every column; `groups`
# (or NULL) gives a spanner label per column, NA for none; `colors` holds a
# text colour per cell (NA for default); the first `n_keys` columns are row
# labels.
build_table <- function(body, labels, groups, colors, n_keys, title, subtitle) {
  names(colors) <- names(body)
  ft <- flextable::flextable(body)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(labels), names(body)))

  if (!is.null(groups)) {
    spans <- rle(ifelse(is.na(groups), "", groups))
    ft <- flextable::add_header_row(ft, values = spans$values, colwidths = spans$lengths)
  }

  for (j in names(body)) {
    for (col in setdiff(unique(colors[[j]]), NA)) {
      ft <- flextable::color(ft, i = which(colors[[j]] == col), j = j, color = col)
    }
  }

  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, j = seq_len(n_keys), align = "left", part = "all")
  ft <- flextable::align(ft, j = setdiff(seq_along(body), seq_len(n_keys)), align = "right", part = "body")

  # dividers before each spanned group
  if (!is.null(groups)) {
    starts <- which(!is.na(groups) & c(TRUE, groups[-1] != groups[-length(groups)] | is.na(groups[-length(groups)])))
    for (j in starts[starts > 1] - 1) {
      ft <- flextable::vline(ft, j = j, border = officer::fp_border(color = "#BBBBBB"), part = "all")
    }
  }

  ft <- flextable::add_header_lines(ft, values = subtitle)
  ft <- flextable::add_header_lines(ft, values = title)
  ft <- flextable::fontsize(ft, i = 1, size = 13, part = "header")
  ft <- flextable::italic(ft, i = 2, part = "header")
  ft <- flextable::bold(ft, i = 2, bold = FALSE, part = "header")
  ft <- flextable::align(ft, i = 1:2, align = "left", part = "header")
  note <- "Modelled OSRM drive time; CAD run card order. Diff = scenario minus baseline."
  ft <- flextable::add_footer_lines(ft, note)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::autofit(ft)

  # autofit sizes columns to the data only; widen a narrow table so the title,
  # subtitle and note fit on one line (rough inches per character at their
  # font sizes)
  widths <- dim(ft)$widths
  needed <- max(nchar(title) * 0.13, nchar(subtitle) * 0.085, nchar(note) * 0.06)
  if (sum(widths) < needed) {
    ft <- flextable::width(ft, width = widths * needed / sum(widths))
  }
  ft
}
