# Print-ready comparison tables (flextable), for Word or images.
#
# Each function takes a compare_*() result and shows one scenario against the
# baseline by default; pass several scenarios to put them side by side.
#
#   overall_table(cmp_overall, baseline_name = "Baseline Rescues")
#   station_table(cmp_station, scenario = c("R26->16 +R1", "R11->16 +R1"))
#   fdz_table(cmp_fdz, metrics = c("median_minutes", "p90_minutes"))
#   calls_table(cmp_calls) |> save_table("calls.docx")
#
# And scenarios against each other, from compare_scenarios():
#
#   scenario_pairs_table(cmp_pairs, time_format = "mmss")

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
#' @param time_format How times (mean, median, 90th percentile minutes and
#'   their changes) are shown: `"decimal"` (7.50) or `"mmss"` (7:30). Total
#'   minutes stay a whole-number count either way.
#' @param digits Decimal places for times when `time_format = "decimal"`.
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
  subtitle = NULL,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
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
  # a CI of the long-run difference, or a range for a future period
  kind <- if (has_ci) {
    interval_text(
      if ("level" %in% names(sel$df)) sel$df$level[1] else 0.95,
      if ("predict_years" %in% names(sel$df)) sel$df$predict_years[1] else NA
    )
  }
  interval_note <- if (has_ci && "resample" %in% names(sel$df)) {
    by_call <- sel$df$resample[1] == "call"
    paste0(
      if (grepl("range", kind$phrase)) {
        paste0(kind$short, ": where the difference would fall over ", sub("^.*range for ", "", kind$phrase), " with the same pattern of calls")
      } else {
        paste0(kind$short, ": how precisely these calls pin down the difference")
      },
      if (by_call) "; calls resampled." else "; whole FDZs resampled."
    )
  }

  body <- data.frame(metric = metric_label(metrics, sel$years))
  colors <- data.frame(metric = rep(NA_character_, length(metrics)))
  first <- sel$df[sel$df$scenario_name == sel$scenarios[1], ]
  body$baseline <- fmt(first$baseline[match(metrics, first$metric)], metrics)
  colors$baseline <- NA_character_
  groups <- c(NA, NA)
  labels <- c("Metric", "Baseline")

  for (k in seq_along(sel$scenarios)) {
    rows <- sel$df[sel$df$scenario_name == sel$scenarios[k], ]
    rows <- rows[match(metrics, rows$metric), ]
    id <- paste0("s", k)
    body[[paste0(id, "_scenario")]] <- fmt(rows$scenario, metrics)
    body[[paste0(id, "_diff")]] <- fmt(rows$diff, metrics, diff = TRUE)
    body[[paste0(id, "_pct")]] <- format_pct_diff(rows$pct_diff, metrics)
    colors[[paste0(id, "_scenario")]] <- NA_character_
    colors[[paste0(id, "_diff")]] <- diff_colors(rows$diff, metrics)
    colors[[paste0(id, "_pct")]] <- NA_character_
    new_labels <- c("Scenario", "Diff", "% Diff")
    if (has_ci) {
      body[[paste0(id, "_ci")]] <- format_ci(rows$ci_lower, rows$ci_upper, metrics, time_format, digits)
      colors[[paste0(id, "_ci")]] <- NA_character_
      new_labels <- c(new_labels, kind$short)
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
    subtitle = subtitle %||% compare_subtitle(sel, baseline_name),
    years = sel$years,
    notes = interval_note,
    time_format = time_format
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
  subtitle = NULL,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  sel <- select_compare(df, scenario, filter_name, due)
  metrics <- metrics %||% unique(sel$df$metric)
  check_metrics(metrics, sel$df$metric)

  body <- data.frame(metric = metric_label(metrics, sel$years))
  colors <- data.frame(metric = rep(NA_character_, length(metrics)))
  for (k in seq_along(sel$scenarios)) {
    rows <- sel$df[sel$df$scenario_name == sel$scenarios[k], ]
    rows <- rows[match(metrics, rows$metric), ]
    # changes are already scenario minus baseline, so they read as diffs
    is_change <- grepl("_change$", metrics)
    body[[paste0("s", k)]] <- ifelse(
      is_change,
      fmt(rows$value, metrics, diff = TRUE),
      fmt(rows$value, metrics)
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
    subtitle = subtitle %||% compare_subtitle(sel, baseline_name),
    years = sel$years,
    time_format = time_format
  )
}

#' Zone comparison table
#'
#' @inheritParams overall_table
#' @param df Output of [compare_by_fdz()].
#' @param metrics Metric(s) to show; several only with one scenario. Default
#'   `"median_minutes"`.
#' @param pct_total If `TRUE`, a "% of Total" column after Baseline and after
#'   each Scenario: the row's share of that column's total over the rows shown.
#'   Count metrics only (calls, incidents, FDZs, total minutes).
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
  subtitle = NULL,
  pct_total = FALSE,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  rows_table(
    df, key = "fdz_id", key_label = "FDZ",
    scenario, filter_name, due, metrics, baseline_name,
    title_prefix = "FDZ Comparison",
    neutral = FALSE,
    responders = TRUE,
    title = title, subtitle = subtitle,
    pct_total = pct_total,
    time_format = match.arg(time_format),
    digits = digits
  )
}

#' Station workload table
#'
#' @inheritParams overall_table
#' @inheritParams fdz_table
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
  subtitle = NULL,
  pct_total = FALSE,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  rows_table(
    df, key = "source_id", key_label = "Station",
    scenario, filter_name, due, metrics, baseline_name,
    title_prefix = "Station Workload",
    neutral = TRUE,
    responders = FALSE,
    title = title, subtitle = subtitle,
    pct_total = pct_total,
    time_format = match.arg(time_format),
    digits = digits
  )
}

#' Station territory comparison table
#'
#' Coverage of each station's territory (the calls in its zones, whoever
#' responds), not the station's workload: see [station_table()] for that.
#'
#' @inheritParams overall_table
#' @inheritParams fdz_table
#' @param df Output of [compare_by_territory()].
#' @param metrics Metric(s) to show; several only with one scenario. Default
#'   `"p90_minutes"`.
#'
#' @return A flextable: one row per territory with Baseline / Scenario / Diff
#'   for each metric, diffs colored green when better and red when worse.
#'   Calls and FDZs, the same in baseline and scenario, get one Count column
#'   (and % of Total with `pct_total`).
#' @noRd
territory_table <- function(
  df,
  scenario = NULL,
  filter_name = NULL,
  due = NULL,
  metrics = "p90_minutes",
  baseline_name = "Baseline",
  title = NULL,
  subtitle = NULL,
  pct_total = FALSE,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  rows_table(
    df, key = "station_id", key_label = "Territory",
    scenario, filter_name, due, metrics, baseline_name,
    title_prefix = "Station Territory Comparison",
    neutral = FALSE,
    responders = FALSE,
    title = title, subtitle = subtitle,
    pct_total = pct_total,
    time_format = match.arg(time_format),
    digits = digits,
    # the same calls on both sides: one Count (and % of Total) column
    fixed_metrics = c("n_calls", "n_fdz")
  )
}

#' Scenario summary table: every scenario's values and change from the baseline
#'
#' One row per scenario, one column per metric, so the options can be ranked
#' side by side. The first row holds the baseline's own values; the others,
#' each scenario's value with its change from the baseline in parentheses,
#' e.g. `7:30 (+0:12)`; only the change is colored. The best scenario in each
#' column is bold.
#'
#' @param overall_df Output of [compare_overall()] (or [annualize_counts()] of
#'   it).
#' @param calls_df Optional output of [compare_calls()]: adds call-level
#'   columns (`calls_metrics`), which are already relative to the baseline.
#' @param station_df Optional output of [compare_by_station()]: adds a Busiest
#'   Station column, the largest station's share of incidents (and its id).
#' @param scenario Scenario names or ids to show, in order. `NULL` shows all.
#' @param filter_name,due Filter set and due level. `NULL` uses the first (with
#'   a message if there are others).
#' @param metrics Overall metrics to show, in order. `NULL`: median and 90th
#'   percentile minutes, each within-threshold share, total minutes.
#' @param calls_metrics Call-level metrics to show when `calls_df` is given.
#'   `NULL`: share slower and calls slower by the largest threshold.
#' @param sort_by A metric shown in the table (or `"busiest_station"`): rows
#'   are ordered best first. `NULL` keeps the scenario order.
#' @param baseline_name Name of the baseline scenario, for its row and the
#'   subtitle.
#' @param title,subtitle Override the automatic title and subtitle.
#' @inheritParams overall_table
#'
#' @return A flextable.
#' @noRd
scenario_summary_table <- function(
  overall_df,
  calls_df = NULL,
  station_df = NULL,
  scenario = NULL,
  filter_name = NULL,
  due = NULL,
  metrics = NULL,
  calls_metrics = NULL,
  sort_by = NULL,
  baseline_name = "Baseline",
  title = NULL,
  subtitle = NULL,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  overall_df <- as.data.frame(overall_df)
  sel <- select_compare(overall_df, scenario %||% unique(overall_df$scenario_name), filter_name, due)
  scen <- sel$scenarios
  if (is.null(metrics)) {
    metrics <- intersect(
      c("median_minutes", "p90_minutes", grep("^pct_within_", unique(sel$df$metric), value = TRUE), "total_minutes"),
      sel$df$metric
    )
  }
  check_metrics(metrics, sel$df$metric)

  value_of <- function(df, sc, m, col) {
    x <- df[[col]][df$scenario_name == sc & df$metric == m]
    if (length(x) == 0) NA_real_ else x[1]
  }

  # One entry per column: label, the baseline row's text, and per scenario the
  # number (for sorting and best), its text and its color. Overall metrics
  # also keep their parts (value, diff, diff color), so only the diff in
  # parentheses is colored
  columns <- list()
  for (m in metrics) {
    v <- vapply(scen, function(sc) value_of(sel$df, sc, m, "scenario"), numeric(1))
    d <- vapply(scen, function(sc) value_of(sel$df, sc, m, "diff"), numeric(1))
    value_text <- fmt(v, m)
    diff_text <- fmt(d, m, diff = TRUE)
    columns[[m]] <- list(
      label = metric_label(m, sel$years),
      base = fmt(value_of(sel$df, scen[1], m, "baseline"), m),
      num = d,
      text = ifelse(value_text == "" | diff_text == "", value_text, paste0(value_text, " (", diff_text, ")")),
      color = rep(NA_character_, length(d)),
      parts = list(value = value_text, diff = diff_text, color = diff_colors(d, m))
    )
  }

  if (!is.null(calls_df)) {
    csel <- select_compare(calls_df, scen, sel$filter_name, sel$due)
    if (is.null(calls_metrics)) {
      slower <- grep("^calls_slower_by_", unique(csel$df$metric), value = TRUE)
      calls_metrics <- intersect(c("share_slower", utils::tail(slower, 1)), csel$df$metric)
    }
    check_metrics(calls_metrics, csel$df$metric)
    for (m in calls_metrics) {
      v <- vapply(scen, function(sc) value_of(csel$df, sc, m, "value"), numeric(1))
      columns[[m]] <- list(
        label = metric_label(m, csel$years),
        base = "",
        num = v,
        text = fmt(v, m),
        color = rep(NA_character_, length(v))
      )
    }
  }

  if (!is.null(station_df)) {
    ssel <- select_compare(station_df, scen, sel$filter_name, sel$due)
    busiest <- function(rows, col) {
      n <- ifelse(is.na(rows[[col]]), 0, rows[[col]])
      k <- which.max(n)
      list(share = n[k] / sum(n), station = rows$source_id[k])
    }
    station_rows <- function(sc) ssel$df[ssel$df$scenario_name == sc & ssel$df$metric == "n_incidents", ]
    base <- busiest(station_rows(scen[1]), "baseline")
    top <- lapply(scen, function(sc) busiest(station_rows(sc), "scenario"))
    share <- vapply(top, function(x) x$share, numeric(1))
    columns[["busiest_station"]] <- list(
      label = "Busiest Station Share",
      base = sprintf("%.1f%% (%s)", 100 * base$share, base$station),
      num = share,
      text = sprintf("%.1f%% (%s)", 100 * share, vapply(top, function(x) as.character(x$station), character(1))),
      color = rep(NA_character_, length(share))
    )
  }

  # lower is better for the busiest station's share: a more even workload
  better <- function(key) {
    if (key == "busiest_station" || higher_is_worse(key)) "low" else if (higher_is_better(key)) "high" else NA
  }

  order_rows <- seq_along(scen)
  if (!is.null(sort_by)) {
    if (!sort_by %in% names(columns)) {
      stop("sort_by must be a column in the table: ", paste(names(columns), collapse = ", "), call. = FALSE)
    }
    if (is.na(better(sort_by))) {
      stop("sort_by: '", sort_by, "' has no better direction", call. = FALSE)
    }
    num <- columns[[sort_by]]$num
    order_rows <- order(if (better(sort_by) == "high") -num else num, na.last = TRUE)
  }

  body <- data.frame(scenario = c(baseline_name, scen[order_rows]))
  colors <- data.frame(scenario = rep(NA_character_, nrow(body)))
  best_cells <- list()
  for (key in names(columns)) {
    col <- columns[[key]]
    body[[key]] <- c(col$base, col$text[order_rows])
    colors[[key]] <- c(NA_character_, col$color[order_rows])
    num <- col$num[order_rows]
    text <- col$text[order_rows]
    if (!is.na(better(key)) && length(unique(stats::na.omit(num))) > 1) {
      target <- if (better(key) == "high") which.max(num) else which.min(num)
      # ties as displayed (22.4% and 22.4%) are all best
      best_cells[[key]] <- which(text == text[target]) + 1 # + 1 for the baseline row
    }
  }
  body[is.na(body)] <- ""

  labels <- c("Scenario", vapply(columns, function(x) x$label, character(1)))
  ft <- build_table(
    body, labels,
    groups = NULL,
    colors = colors,
    n_keys = 1,
    title = title %||% "Scenario Summary",
    subtitle = subtitle %||% paste(
      sel$filter_name, paste(due_ordinal(sel$due), "Due"), paste("vs", baseline_name),
      sep = " · "
    ),
    years = sel$years,
    notes = paste0(
      "First row: baseline values; other rows: scenario values, change from baseline in parentheses",
      if (!is.null(calls_df) || !is.null(station_df)) {
        paste0(
          " (", paste(c(if (!is.null(calls_df)) "call-level", if (!is.null(station_df)) "busiest station"), collapse = " and "),
          " columns: scenario values only)"
        )
      },
      ". Bold: best scenario in each column."
    ),
    max_col_width = 1.3,
    time_format = time_format
  )

  ft <- flextable::italic(ft, i = 1, part = "body")
  ft <- flextable::hline(ft, i = 1, border = officer::fp_border(color = "#BBBBBB"), part = "body")
  for (key in names(best_cells)) {
    ft <- flextable::bold(ft, i = best_cells[[key]], j = key, part = "body")
  }

  # color only the change in parentheses, not the value
  for (key in names(columns)) {
    parts <- columns[[key]]$parts
    if (is.null(parts)) next
    for (r in seq_along(order_rows)) {
      k <- order_rows[r]
      if (is.na(parts$color[k]) || parts$value[k] == "" || parts$diff[k] == "") next
      ft <- flextable::compose(
        ft,
        i = r + 1, # + 1 for the baseline row
        j = key,
        part = "body",
        value = flextable::as_paragraph(
          parts$value[k], " (", flextable::colorize(parts$diff[k], color = parts$color[k]), ")"
        )
      )
    }
  }
  ft
}

#' Scenarios ranked best first on an overall metric
#'
#' @param df Output of [compare_overall()] (or [annualize_counts()] of it).
#' @param by Metric whose diff from the baseline ranks the scenarios: lower is
#'   better for minutes, distance and unserved calls, higher for
#'   within-threshold shares.
#' @param filter_name,due Filter set and due level. `NULL` uses the first (with
#'   a message if there are others).
#'
#' @return Scenario names, best first.
#' @noRd
rank_scenarios <- function(df, by = "total_minutes", filter_name = NULL, due = NULL) {
  df <- as.data.frame(df)
  sel <- select_compare(df, unique(df$scenario_name), filter_name, due)
  check_metrics(by, sel$df$metric)
  rows <- sel$df[sel$df$metric == by, ]
  if (higher_is_better(by)) {
    rows <- rows[order(-rows$diff), ]
  } else if (higher_is_worse(by)) {
    rows <- rows[order(rows$diff), ]
  } else {
    stop("'", by, "' has no better direction to rank by", call. = FALSE)
  }
  rows$scenario_name
}

#' Scenario-vs-scenario table
#'
#' @param df Output of [compare_scenarios()].
#' @param metrics Metrics to show, in order. `NULL` shows every one in `df`.
#' @inheritParams overall_table
#'
#' @return A flextable: one row per pair, with First, Second, Diff (first
#'   minus second) and CI columns per metric. A diff is colored only when its
#'   interval excludes 0: green when the first scenario is better, red when
#'   worse.
#' @noRd
scenario_pairs_table <- function(
  df,
  metrics = NULL,
  filter_name = NULL,
  due = NULL,
  title = NULL,
  subtitle = NULL,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  df <- as.data.frame(df)
  filter_name <- pick_one(df$filter_name, filter_name, "filter set")
  due <- pick_one(df$due, due, "due level")
  df <- df[df$filter_name == filter_name & df$due == due, ]
  metrics <- metrics %||% unique(df$metric)
  check_metrics(metrics, df$metric)

  pairs <- unique(df[c("scenario_a", "scenario_b")])
  pair_key <- paste(pairs$scenario_a, pairs$scenario_b, sep = "\r")
  ci <- pairs_ci_text(df)
  ci_label <- ci$column

  body <- data.frame(scenario_a = pairs$scenario_a, scenario_b = pairs$scenario_b)
  colors <- data.frame(scenario_a = rep(NA_character_, nrow(pairs)), scenario_b = NA_character_)
  labels <- c("Scenario", "vs")
  groups <- c(NA, NA)
  for (m in metrics) {
    rows <- df[df$metric == m, ]
    rows <- rows[match(pair_key, paste(rows$scenario_a, rows$scenario_b, sep = "\r")), ]
    body[[paste0(m, "_a")]] <- fmt(rows$value_a, m)
    body[[paste0(m, "_b")]] <- fmt(rows$value_b, m)
    body[[paste0(m, "_d")]] <- fmt(rows$diff, m, diff = TRUE)
    body[[paste0(m, "_ci")]] <- format_ci(rows$ci_lower, rows$ci_upper, m, time_format, digits)
    colors[[paste0(m, "_a")]] <- NA_character_
    colors[[paste0(m, "_b")]] <- NA_character_
    colors[[paste0(m, "_d")]] <- ifelse(rows$ci_excludes_zero, diff_colors(rows$diff, m), NA_character_)
    colors[[paste0(m, "_ci")]] <- NA_character_
    labels <- c(labels, "First", "Second", "Diff", ci_label)
    groups <- c(groups, rep(metric_label(m), 4))
  }

  build_table(
    body, labels,
    groups = groups,
    colors = colors,
    n_keys = 2,
    title = title %||% "Scenario vs Scenario",
    subtitle = subtitle %||% paste(filter_name, paste(due_ordinal(due), "Due"), sep = " · "),
    note = "Modeled OSRM drive time; CAD run card order. Diff = first minus second scenario, on the calls both serve.",
    notes = c(
      paste0(
        ci$source, ". A diff is colored only when its CI excludes 0; ",
        "a CI that crosses 0 means the two scenarios can't be told apart on these calls."
      ),
      ci$meaning
    ),
    time_format = time_format
  )
}

#' P-median solutions table: where the optimizer would put the units
#'
#' One row per solution from step 08: the stations it adds to the fixed set,
#' the current units that would move and where to, and its run card scoring
#' against today's placement, best total minutes first. The optimizer's own
#' totals (each zone served by its cheapest open station) are not shown.
#'
#' @param solutions Step 08's `p_median_solutions.parquet`: `rank`,
#'   `added_stations`, `move_from`, `move_to`.
#' @param overall_df Run card comparison of the solutions against today's
#'   units: [compare_overall()] (or [annualize_counts()] of it) with scenarios
#'   named `P-median #<rank>` and today's units as the baseline.
#' @param metrics Overall metrics after Total Minutes, each shown as the value
#'   and its change from today.
#' @param baseline_label Label of the first row, today's units.
#' @param title,subtitle Table title and subtitle.
#' @inheritParams overall_table
#'
#' @return A flextable.
#' @noRd
p_median_solutions_table <- function(
  solutions,
  overall_df,
  metrics = c("p90_minutes", "pct_within_8m58s", "pct_within_10m21s"),
  baseline_label = "Today's engines",
  title = "Optimized ALS Engine Placements",
  subtitle = "Best station sets around the fixed rescues, against today's engines",
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  stations <- function(x) vapply(x, \(s) paste(sort(unlist(s)), collapse = ", "), character(1))
  names <- paste0("P-median #", solutions$rank)
  sel <- select_compare(overall_df, names, NULL, NULL)
  check_metrics(c("total_minutes", metrics), sel$df$metric)
  col <- function(sc, m, what) {
    x <- sel$df[[what]][sel$df$scenario_name == sc & sel$df$metric == m]
    if (length(x) == 0) NA_real_ else x[1]
  }
  values <- function(m, what) vapply(names, col, numeric(1), m = m, what = what, USE.NAMES = FALSE)

  total <- values("total_minutes", "scenario")
  diff <- values("total_minutes", "diff")
  base_total <- col(names[1], "total_minutes", "baseline")
  ord <- order(total)

  body <- data.frame(
    rank = c(baseline_label, paste0("#", solutions$rank)[ord]),
    added = c("", stations(solutions$added_stations)[ord]),
    move_from = c("", stations(solutions$move_from)[ord]),
    move_to = c("", stations(solutions$move_to)[ord]),
    total = c(fmt(base_total, "total_minutes"), fmt(total[ord], "total_minutes")),
    diff = c("", fmt(diff[ord], "total_minutes", diff = TRUE)),
    pct = c("", sprintf("%+.1f%%", 100 * diff[ord] / base_total))
  )
  colors <- data.frame(
    rank = NA_character_, added = NA_character_, move_from = NA_character_,
    move_to = NA_character_, total = NA_character_,
    diff = c(NA, diff_colors(diff[ord], "total_minutes")),
    pct = c(NA, diff_colors(diff[ord], "total_minutes"))
  )
  for (m in metrics) {
    v <- values(m, "scenario")[ord]
    d <- values(m, "diff")[ord]
    body[[m]] <- c(fmt(col(names[1], m, "baseline"), m), paste0(fmt(v, m), " (", fmt(d, m, diff = TRUE), ")"))
    colors[[m]] <- c(NA, diff_colors(d, m))
  }

  minutes_label <- if (is.null(sel$years)) "Total Minutes" else "Total Minutes / Year"
  ft <- build_table(
    body,
    labels = c(
      "Solution", "Engine Stations", "Moved From", "Moved To",
      minutes_label, "Diff", "% Diff", metric_label(metrics)
    ),
    groups = c(
      NA, NA, "Change From Today", "Change From Today", NA, "vs Today", "vs Today",
      rep(NA, length(metrics))
    ),
    colors = colors,
    n_keys = 1,
    title = title,
    subtitle = subtitle,
    years = sel$years,
    note = "Modeled OSRM drive time; CAD run card order.",
    notes = paste(
      "Diff = solution minus today's engines, change in parentheses.",
      "Solutions are numbered in the optimizer's order and sorted by run card total minutes."
    ),
    max_col_width = 1.3,
    time_format = time_format
  )
  ft <- flextable::italic(ft, i = 1, part = "body")
  flextable::hline(ft, i = 1, border = officer::fp_border(color = "#BBBBBB"), part = "body")
}

#' Build-up order table: each added unit and the county after it
#'
#' @param steps `steps` from [build_up_order()].
#' @param years Years of calls behind the counts: minutes and calls are shown
#'   per year. `NULL` shows the totals.
#' @param pct_metrics Within-threshold metrics to show (`pct_within_<t>`).
#'   `NULL` shows every one in `steps`.
#' @param base_label Label of step 0, the base stations alone.
#' @param title,subtitle Override the automatic title and subtitle.
#' @inheritParams overall_table
#'
#' @return A flextable: a row for the base stations alone, then one per added
#'   unit: the minutes it saves, the cumulative saving, and the county's
#'   total minutes, 90th percentile and within-threshold shares with every
#'   unit so far in place.
#' @noRd
build_up_table <- function(
  steps,
  years = NULL,
  pct_metrics = NULL,
  base_label = "Rescues only",
  title = NULL,
  subtitle = NULL,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  steps <- as.data.frame(steps)
  pct_metrics <- pct_metrics %||% grep("^pct_within_", names(steps), value = TRUE)
  check_metrics(pct_metrics, grep("^pct_within_", names(steps), value = TRUE))
  per_year <- function(x) if (is.null(years)) x else x / years

  # from move_order(): each step moves a unit, from -> station
  moves <- "from" %in% names(steps)
  saved <- per_year(steps$saved_minutes)
  body <- data.frame(
    order = ifelse(steps$step == 0, "", as.character(steps$step)),
    station = ifelse(
      steps$step == 0, base_label,
      if (moves) paste(steps$from, "→", steps$station) else as.character(steps$station)
    ),
    saved = fmt(saved, "total_minutes"),
    cumulative = fmt(per_year(steps$cumulative_saved), "total_minutes"),
    total = fmt(per_year(steps$total_minutes), "total_minutes"),
    p90 = fmt(steps$p90_minutes, "p90_minutes")
  )
  for (m in pct_metrics) {
    body[[m]] <- fmt(steps[[m]], m)
  }
  body[is.na(body)] <- ""
  colors <- as.data.frame(lapply(body, function(x) rep(NA_character_, nrow(body))))
  colors$saved <- ifelse(is.na(saved) | saved == 0, NA_character_, ifelse(saved > 0, better_color, worse_color))

  suffix <- if (is.null(years)) "" else " per Year"
  labels <- c(
    "Order", if (moves) "Move Unit (From → To)" else "Add Unit At",
    paste0("Minutes Saved", suffix), paste0("Cumulative Saved", suffix),
    paste0("Total Minutes", suffix), metric_label("p90_minutes"), metric_label(pct_metrics)
  )
  ft <- build_table(
    body, labels,
    groups = NULL,
    colors = colors,
    n_keys = 2,
    title = title %||% if (moves) "Move Order" else "Build-Up Order",
    subtitle = subtitle %||% paste(steps$filter_name[1], paste(base_label, "at", steps$stations[1]), sep = " · "),
    years = years,
    note = "Modeled OSRM drive time; CAD run card order.",
    notes = c(
      if (moves) {
        paste(
          "Each row moves the unit that saves the most travel minutes, with the moves above it made;",
          "the last row reaches the target. County values are with every move so far."
        )
      } else {
        paste(
          "Each row adds the unit that saves the most travel minutes, with the units above it in place;",
          "county values are with every unit so far."
        )
      },
      paste(
        "No intervals: savings a few hundred minutes apart are effectively ties.",
        "A unit can save little or even add minutes when its station is first on a card but farther away."
      )
    ),
    max_col_width = 1.2,
    time_format = time_format
  )
  ft <- flextable::italic(ft, i = 1, part = "body")
  flextable::hline(ft, i = 1, border = officer::fp_border(color = "#BBBBBB"), part = "body")
}

#' Build-up order, each unit in its own territory
#'
#' @inheritParams build_up_table
#' @param territory `territory` from [build_up_order()].
#' @param pct_metric One within-threshold metric to show.
#' @param n_helps How many other territories to list under Also Helps.
#'
#' @return A flextable: one row per added unit, with its own territory's calls
#'   and its total minutes, 90th percentile and within-threshold share before
#'   (with the units above in place) and after it is added, the share of the
#'   unit's saving inside its own territory, and the other territories that
#'   gain most.
#' @noRd
build_up_territory_table <- function(
  steps,
  territory,
  years = NULL,
  pct_metric = NULL,
  n_helps = 3,
  base_label = "Rescues only",
  title = NULL,
  subtitle = NULL,
  time_format = c("decimal", "mmss"),
  digits = 2
) {
  time_format <- match.arg(time_format)
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  steps <- as.data.frame(steps)
  territory <- as.data.frame(territory)
  pct_metric <- pct_metric %||% grep("^pct_within_", names(steps), value = TRUE)[1]
  check_metrics(pct_metric, grep("^pct_within_", names(steps), value = TRUE))
  per_year <- function(x) if (is.null(years)) x else x / years
  arrow <- " → "

  s <- steps[steps$step > 0, ]
  before_after <- function(m, with_diff = FALSE) {
    b <- s[[paste0("own_", m, "_before")]]
    a <- s[[paste0("own_", m, "_after")]]
    if (m == "total_minutes") {
      b <- per_year(b)
      a <- per_year(a)
    }
    text <- paste0(fmt(b, m), arrow, fmt(a, m))
    if (with_diff) {
      text <- paste0(text, " (", fmt(a - b, m, diff = TRUE), ")")
    }
    ifelse(is.na(b) | is.na(a), "", text)
  }

  # other territories with the most minutes saved at each step
  also_helps <- vapply(seq_len(nrow(s)), function(i) {
    rows <- territory[
      territory$step == s$step[i] & territory$metric == "total_minutes" & territory$station_id != s$station[i],
    ]
    rows$saved <- per_year(-rows$diff)
    rows <- rows[!is.na(rows$saved) & round(rows$saved) > 0, ]
    rows <- utils::head(rows[order(-rows$saved), ], n_helps)
    paste(sprintf("%s (%s)", rows$station_id, fmt(rows$saved, "total_minutes")), collapse = ", ")
  }, character(1))

  body <- data.frame(
    order = as.character(s$step),
    station = as.character(s$station),
    calls = fmt(per_year(s$own_n_calls), "n_calls"),
    total = before_after("total_minutes", with_diff = TRUE),
    p90 = before_after("p90_minutes"),
    pct = before_after(pct_metric),
    own_share = ifelse(is.na(s$own_share), "", sprintf("%.0f%%", 100 * s$own_share)),
    also_helps = also_helps
  )
  body[is.na(body)] <- ""
  colors <- as.data.frame(lapply(body, function(x) rep(NA_character_, nrow(body))))

  suffix <- if (is.null(years)) "" else " per Year"
  labels <- c(
    "Order", "Unit At", paste0("Calls", suffix), paste0("Total Minutes", suffix),
    metric_label("p90_minutes"), metric_label(pct_metric),
    "Saving in Own Territory", paste0("Also Helps (Minutes Saved", suffix, ")")
  )
  build_table(
    body, labels,
    groups = NULL,
    colors = colors,
    n_keys = 2,
    title = title %||% "Each Unit in Its Own Territory",
    subtitle = subtitle %||% paste(steps$filter_name[1], paste(base_label, "at", steps$stations[1]), sep = " · "),
    years = years,
    note = "Modeled OSRM drive time; CAD run card order.",
    notes = c(
      paste(
        "Before: with the units above in place; after: with this one added. Own territory: the calls in",
        "the station's zones. Also Helps: other territories whose minutes drop most with this unit."
      ),
      "Order as in the build-up table; no intervals."
    ),
    max_col_width = 1.8,
    time_format = time_format
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
  title_prefix, neutral, responders, title, subtitle, pct_total = FALSE,
  time_format = "decimal", digits = 2, fixed_metrics = character()
) {
  fmt <- function(x, m, diff = FALSE) format_values(x, m, diff, time_format, digits)
  sel <- select_compare(df, scenario, filter_name, due)
  check_metrics(metrics, sel$df$metric)
  if (length(sel$scenarios) > 1 && length(metrics) > 1) {
    stop("With several scenarios, pick one metric", call. = FALSE)
  }
  if (pct_total && !any(is_count(metrics))) {
    message("% of Total is only shown for counts (e.g. n_incidents, total_minutes), not ", paste(metrics, collapse = ", "))
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

  # a metric that is the same in the baseline and every scenario (e.g. a
  # territory's calls): one Count column (and % of Total), not Baseline /
  # Scenario / Diff
  add_fixed <- function(m) {
    x <- get(sel$scenarios[1], m, "baseline")
    with_pct <- pct_total && is_count(m)
    body[[paste0(m, "_v")]] <<- fmt(x, m)
    colors[[paste0(m, "_v")]] <<- NA_character_
    if (with_pct) {
      body[[paste0(m, "_vp")]] <<- format_pct_of_total(x)
      colors[[paste0(m, "_vp")]] <<- NA_character_
    }
    new <- if (with_pct) c("Count", "% of Total") else "Count"
    labels <<- c(labels, new)
    groups <<- c(groups, rep(metric_label(m, sel$years), length(new)))
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
      if (m %in% fixed_metrics) {
        add_fixed(m)
        next
      }
      b <- fill_zero(get(sc, m, "baseline"), m)
      s <- fill_zero(get(sc, m, "scenario"), m)
      d <- if (neutral && is_count(m)) s - b else get(sc, m, "diff")
      with_pct <- pct_total && is_count(m)
      body[[paste0(m, "_b")]] <- fmt(b, m)
      if (with_pct) body[[paste0(m, "_bp")]] <- format_pct_of_total(b)
      body[[paste0(m, "_s")]] <- fmt(s, m)
      if (with_pct) body[[paste0(m, "_sp")]] <- format_pct_of_total(s)
      body[[paste0(m, "_d")]] <- fmt(d, m, diff = TRUE)
      colors[[paste0(m, "_b")]] <- NA_character_
      if (with_pct) colors[[paste0(m, "_bp")]] <- NA_character_
      colors[[paste0(m, "_s")]] <- NA_character_
      if (with_pct) colors[[paste0(m, "_sp")]] <- NA_character_
      colors[[paste0(m, "_d")]] <- diff_color(d, m)
      new <- if (with_pct) {
        c("Baseline", "% of Total", "Scenario", "% of Total", "Diff")
      } else {
        c("Baseline", "Scenario", "Diff")
      }
      labels <- c(labels, new)
      groups <- c(groups, rep(metric_label(m, sel$years), length(new)))
    }
    groups <- if (length(metrics) > 1) groups
  } else {
    # several scenarios, one metric: Baseline once, then Scenario / Diff each
    m <- metrics
    if (m %in% fixed_metrics) {
      # the same in every scenario: one Count column, no spanner needed
      add_fixed(m)
      groups <- NULL
    } else {
      b <- NULL
      for (sc in sel$scenarios) {
        x <- get(sc, m, "baseline")
        b <- if (is.null(b)) x else ifelse(is.na(b), x, b)
      }
      b <- fill_zero(b, m)
      with_pct <- pct_total && is_count(m)
      body$baseline <- fmt(b, m)
      colors$baseline <- NA_character_
      labels <- c(labels, "Baseline")
      groups <- c(groups, NA)
      if (with_pct) {
        body$baseline_pct <- format_pct_of_total(b)
        colors$baseline_pct <- NA_character_
        labels <- c(labels, "% of Total")
        groups <- c(groups, NA)
      }
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
        body[[paste0("s", k, "_s")]] <- fmt(s, m)
        colors[[paste0("s", k, "_s")]] <- NA_character_
        new <- c(new, "Scenario")
        if (with_pct) {
          body[[paste0("s", k, "_sp")]] <- format_pct_of_total(s)
          colors[[paste0("s", k, "_sp")]] <- NA_character_
          new <- c(new, "% of Total")
        }
        body[[paste0("s", k, "_d")]] <- fmt(d, m, diff = TRUE)
        colors[[paste0("s", k, "_d")]] <- diff_color(d, m)
        new <- c(new, "Diff")
        labels <- c(labels, new)
        groups <- c(groups, rep(sc, length(new)))
      }
    }
  }

  body[is.na(body)] <- ""
  build_table(
    body, labels,
    groups = groups,
    colors = colors,
    n_keys = n_keys,
    title = title %||% if (length(metrics) == 1) paste0(title_prefix, ": ", metric_label(metrics, sel$years)) else title_prefix,
    subtitle = subtitle %||% compare_subtitle(sel, baseline_name),
    years = sel$years,
    time_format = time_format
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
  list(
    df = df[df$scenario_name %in% scenarios, ],
    scenarios = scenarios,
    filter_name = filter_name,
    due = due,
    # from annualize_counts(): counts are yearly averages over this many years
    years = if ("years" %in% names(df)) df$years[1]
  )
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

# Title Case (AP style: words of four or more letters capitalized, so "With"
# and "Over" but "by") label for each metric; threshold metrics carry their
# minutes. With `years` (counts from annualize_counts()), call counts end in
# "per Year".
metric_label <- function(metric, years = NULL) {
  labels <- vapply(metric, function(m) {
    if (m %in% names(metric_labels)) {
      return(metric_labels[[m]])
    }
    x <- sub(".*_(\\d+(\\.\\d+)?)min$", "\\1", m)
    # thresholds in minutes and seconds (threshold_key()): 8m58s -> 8:58
    if (grepl("_\\d+m\\d{2}s$", m)) {
      x <- sub(".*_(\\d+)m(\\d{2})s$", "\\1:\\2", m)
    }
    if (grepl("^pct_within_", m)) return(paste0("Within ", x, " Min"))
    if (grepl("^calls_within_", m)) return(paste0("Calls Within ", x, " Min"))
    if (grepl("^fdz_p90_over_", m)) return(paste0("FDZs With 90th Percentile Over ", x, " Min"))
    if (grepl("^calls_slower_by_", m)) return(paste0("Calls Slower by More Than ", x, " Min"))
    if (grepl("^share_slower_by_", m)) return(paste0("Share Slower by More Than ", x, " Min"))
    tools::toTitleCase(gsub("_", " ", m))
  }, character(1), USE.NAMES = FALSE)
  if (!is.null(years)) {
    yearly <- is_call_count(metric)
    labels[yearly] <- paste(labels[yearly], "per Year")
  }
  labels
}

is_share <- function(m) grepl("^(pct_within_|share_)", m)
is_count <- function(m) grepl("^(n_|calls_|unserved_|fdz_p90_over_)", m) | m == "total_minutes"
is_distance <- function(m) m == "mean_distance_m"

# Values as display text: counts with separators, times with `digits` decimals
# (or as m:ss with time_format = "mmss"), shares as percent (their diffs in
# percentage points), distance in miles
format_values <- function(x, metrics, diff = FALSE, time_format = "decimal", digits = 2) {
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
    } else if (time_format == "mmss") {
      format_mmss(v, diff)
    } else {
      paste0(sign, formatC(v, format = "f", digits = digits, big.mark = ","))
    }
  }
  out
}

# Minutes as m:ss, rounded to the second: 7.5 -> "7:30"; a diff carries its
# sign ("+1:05", "-0:30"), and no change is "0:00"
format_mmss <- function(v, diff = FALSE) {
  secs <- round(abs(v) * 60)
  sign <- if (secs == 0) "" else if (v < 0) "-" else if (diff) "+" else ""
  sprintf("%s%d:%02d", sign, secs %/% 60, secs %% 60)
}

# Each value as a percent of the column's total (the rows shown)
format_pct_of_total <- function(x) {
  total <- sum(x, na.rm = TRUE)
  ifelse(is.na(x) | total == 0, "", sprintf("%.1f%%", 100 * x / total))
}

format_pct_diff <- function(x, metrics) {
  ifelse(is.na(x) | is_share(metrics), "", sprintf("%+.1f%%", x * 100))
}

format_ci <- function(lower, upper, metrics, time_format = "decimal", digits = 2) {
  lo <- format_values(lower, metrics, diff = TRUE, time_format, digits)
  hi <- format_values(upper, metrics, diff = TRUE, time_format, digits)
  ifelse(lo == "" | hi == "", "", paste0("[", lo, ", ", hi, "]"))
}

# Diff colors: red when the change is worse, green when better
worse_color <- "#B00020"
better_color <- "#1B7F3B"

# Which way is better for a metric; neither for workload counts like n_calls
higher_is_better <- function(m) grepl("^(pct_within_|calls_within_|share_faster)", m)
higher_is_worse <- function(m) {
  !higher_is_better(m) &
    grepl("minutes$|_change$|^mean_distance|^unserved_|^fdz_p90_over_|^calls_slower_by_|^share_slower", m)
}

diff_colors <- function(x, metrics) {
  metrics <- rep_len(metrics, length(x))
  out <- rep(NA_character_, length(x))
  up <- !is.na(x) & x > 0
  down <- !is.na(x) & x < 0
  out[up & higher_is_worse(metrics)] <- worse_color
  out[down & higher_is_worse(metrics)] <- better_color
  out[up & higher_is_better(metrics)] <- better_color
  out[down & higher_is_better(metrics)] <- worse_color
  out
}

# Workload changes are neither better nor worse: blue up, orange down
neutral_colors <- function(x) {
  ifelse(is.na(x) | x == 0, NA_character_, ifelse(x > 0, "#1F5FA8", "#B3541E"))
}

# Building --------------------------------------------------------------------

# A styled flextable from display text. `labels` name every column; `groups`
# (or NULL) gives a spanner label per column, NA for none; `colors` holds a
# text color per cell (NA for default); the first `n_keys` columns are row
# labels. `years` (or NULL) adds a footer line saying the counts are yearly;
# `note` (or NULL for the scenario-minus-baseline one) is the first footer
# line; `notes` are added to the footer; `max_col_width` (inches) caps the value
# columns so long headers wrap; `time_format = "mmss"` notes that times are
# minutes:seconds.
build_table <- function(
  body, labels, groups, colors, n_keys, title, subtitle,
  years = NULL, notes = NULL, max_col_width = NULL, time_format = "decimal", note = NULL
) {
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
  note <- note %||% "Modeled OSRM drive time; CAD run card order. Diff = scenario minus baseline."
  if (!is.null(years)) {
    note <- paste0(note, " Call counts are yearly averages over ", sprintf("%.1f", years), " years.")
  }
  if (time_format == "mmss") {
    note <- paste0(note, " Times are minutes:seconds.")
  }
  # extra notes go on their own lines, which wrap rather than widen the table
  ft <- flextable::add_footer_lines(ft, c(note, notes))
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::font(ft, fontname = output_font, part = "all")
  ft <- flextable::autofit(ft)

  widths <- dim(ft)$widths
  if (!is.null(max_col_width)) {
    value_cols <- setdiff(seq_along(widths), seq_len(n_keys))
    ft <- flextable::width(ft, j = value_cols, width = pmin(widths[value_cols], max_col_width))
    widths <- dim(ft)$widths
  }

  # autofit sizes columns to the data only; widen a narrow table so the title,
  # subtitle and note fit on one line (rough inches per character at their
  # font sizes)
  needed <- max(nchar(title) * 0.13, nchar(subtitle) * 0.085, nchar(note) * 0.06)
  if (sum(widths) < needed) {
    ft <- flextable::width(ft, width = widths * needed / sum(widths))
  }
  ft
}
