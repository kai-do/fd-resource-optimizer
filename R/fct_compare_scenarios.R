# Compare scenarios with each other, not only with the baseline.
#
# compare_overall() puts every scenario against the baseline. When the top
# scenarios are close, the question is whether they differ from each other by
# more than which calls happened to occur. compare_scenarios() takes every pair
# of the scenarios you pick (the baseline can be one of them) from the
# call-level paired_calls() table, so the slow run card assignment is not
# repeated, and adds the same zone-resampled bootstrap intervals.
#
#   set.seed(343)
#   paired |> compare_scenarios(c("Baseline Rescues", "R26->16 +R1", "+R1"))
#   paired |> compare_scenarios(metrics = c("mean_minutes", "pct_within_8m58s"))

#' Pairwise comparison of the chosen scenarios
#'
#' Each pair is compared on the calls both scenarios serve: first minus
#' second, in the order the scenarios are given.
#'
#' @param paired Output of [paired_calls()], e.g. `paired_calls.parquet`.
#' @param scenario The scenarios to compare, in order: names or ids, and
#'   `baseline_name` for the baseline. `NULL` takes the baseline and then
#'   every scenario.
#' @param metrics Metrics to compare, in order: `mean_minutes`,
#'   `median_minutes`, `p90_minutes`, `mean_distance_m`, or `pct_within_<t>`
#'   (`<t>` as in [compare_overall()], e.g. `8min` or `8m58s`; the threshold
#'   is read from the name). `mean_minutes` ranks scenarios the same as
#'   `total_minutes`.
#' @param filter_name,due Filter sets and due levels to compare. `NULL` takes
#'   every one in `paired`.
#' @param baseline_name Name of the baseline, as given in `scenario`.
#' @param n_boot,level Number of bootstrap resamples and interval level. Whole
#'   zones are resampled, as in [compare_overall()]; the interval reflects
#'   which calls happened to occur, not error in the modeled travel times. Use
#'   `set.seed()` first for repeatable intervals.
#'
#' @return One row per filter set, due level, pair and metric: `filter_name`,
#'   `due`, `scenario_a`, `scenario_b`, `metric`, `n_calls` (served by both),
#'   `value_a`, `value_b`, `diff` (a minus b), `ci_lower`, `ci_upper`,
#'   `level`, and `ci_excludes_zero` (the pair differs by more than chance).
#' @noRd
compare_scenarios <- function(
  paired,
  scenario = NULL,
  metrics = "mean_minutes",
  filter_name = NULL,
  due = NULL,
  baseline_name = "Baseline",
  n_boot = 1000,
  level = 0.95
) {
  check_pair_metrics(metrics)
  paired <- as.data.frame(paired)
  groups <- plot_groups(paired, scenario, baseline_name)
  if (length(groups) < 2) {
    stop("Pick at least two scenarios to compare", call. = FALSE)
  }
  check_ids(filter_name, paired$filter_name, "filter set")
  check_ids(due, paired$due, "due level")
  filter_names <- filter_name %||% unique(paired$filter_name)
  dues <- due %||% sort(unique(paired$due))

  thresholds <- unique(threshold_minutes(sub("^pct_within_", "", metrics[grepl("^pct_within_", metrics)])))
  # bootstrap_diffs() needs at least one threshold; an unrequested one is
  # computed and dropped
  if (length(thresholds) == 0) {
    thresholds <- 8
  }

  pairs <- utils::combn(groups, 2, simplify = FALSE)
  out <- list()
  for (f in filter_names) {
    for (d in dues) {
      df <- paired[paired$filter_name == f & paired$due == d, ]
      calls <- stats::setNames(lapply(groups, function(g) group_calls(df, g, baseline_name)), groups)
      for (p in pairs) {
        out[[length(out) + 1]] <- compare_pair(calls[[p[1]]], calls[[p[2]]], metrics, thresholds, n_boot, level) |>
          cbind(filter_name = f, due = d, scenario_a = p[1], scenario_b = p[2], row.names = NULL)
      }
    }
  }

  cols <- c(
    "filter_name", "due", "scenario_a", "scenario_b", "metric", "n_calls",
    "value_a", "value_b", "diff", "ci_lower", "ci_upper", "level", "ci_excludes_zero"
  )
  dplyr::bind_rows(out)[cols]
}

#' Minutes for a threshold key, the inverse of threshold_key()
#'
#' @param key Keys such as `"8min"` or `"8m58s"`.
#' @return Minutes: 8, 8 + 58 / 60.
#' @noRd
threshold_minutes <- function(key) {
  if (!all(grepl("^\\d+(min|m\\d{2}s)$", key))) {
    stop("Thresholds look like 8min or 8m58s, not: ", paste(key, collapse = ", "), call. = FALSE)
  }
  mins <- as.numeric(sub("^(\\d+).*$", "\\1", key))
  secs <- rep(0, length(key))
  with_secs <- grepl("s$", key)
  secs[with_secs] <- as.numeric(sub("^\\d+m(\\d{2})s$", "\\1", key[with_secs]))
  mins + secs / 60
}

# Internals -------------------------------------------------------------------

# The metrics bootstrap_diffs() gives intervals for
check_pair_metrics <- function(metrics) {
  if (length(metrics) == 0) {
    stop("Pick at least one metric", call. = FALSE)
  }
  if ("total_minutes" %in% metrics) {
    stop(
      "Use mean_minutes instead of total_minutes: on the calls both serve it ranks the same, ",
      "and its diff is the total's divided by the number of calls",
      call. = FALSE
    )
  }
  supported <- metrics %in% c("mean_minutes", "median_minutes", "p90_minutes", "mean_distance_m") |
    grepl("^pct_within_\\d+(min|m\\d{2}s)$", metrics)
  if (!all(supported)) {
    stop(
      "Unknown metric(s): ", paste(metrics[!supported], collapse = ", "),
      ". Available: mean_minutes, median_minutes, p90_minutes, mean_distance_m, pct_within_<t> ",
      "(e.g. pct_within_8min, pct_within_8m58s)",
      call. = FALSE
    )
  }
}

# One group's calls from the wide paired table (one filter set and due
# level): dest_id, fdz_id, minutes and distance_m, NA where it has no
# responder. The baseline repeats once per scenario in paired_calls(), so it
# is taken once per call.
group_calls <- function(df, group, baseline_name) {
  if (group == baseline_name) {
    rows <- df[!duplicated(df$dest_id), ]
    side <- "baseline"
  } else {
    rows <- df[df$scenario_name == group, ]
    side <- "scenario"
  }
  data.frame(
    dest_id = rows$dest_id,
    fdz_id = rows$fdz_id,
    minutes = rows[[paste0(side, "_minutes")]],
    distance_m = rows[[paste0(side, "_distance_m")]]
  )
}

# The metrics for one pair, a minus b, on the calls both serve. The pair goes
# into the shape compare_overall() uses (a as the scenario, b as the
# baseline), so the values and intervals come from the same code.
compare_pair <- function(a, b, metrics, thresholds, n_boot, level) {
  joined <- merge(a, b[c("dest_id", "minutes", "distance_m")], by = "dest_id", suffixes = c("_a", "_b"))
  joined <- joined[!is.na(joined$minutes_a) & !is.na(joined$minutes_b), ]
  both <- data.frame(
    fdz_id = joined$fdz_id,
    baseline_minutes = joined$minutes_b,
    scenario_minutes = joined$minutes_a,
    baseline_distance_m = joined$distance_m_b,
    scenario_distance_m = joined$distance_m_a
  )

  value_a <- overall_metrics(both, "scenario", thresholds)[metrics]
  value_b <- overall_metrics(both, "baseline", thresholds)[metrics]
  intervals <- bootstrap_diffs(both, thresholds, n_boot, level)
  ci <- intervals[match(metrics, intervals$metric), ]

  data.frame(
    metric = metrics,
    n_calls = nrow(both),
    value_a = unname(value_a),
    value_b = unname(value_b),
    diff = unname(value_a - value_b),
    ci_lower = ci$ci_lower,
    ci_upper = ci$ci_upper,
    level = level,
    ci_excludes_zero = !is.na(ci$ci_lower) & (ci$ci_lower > 0 | ci$ci_upper < 0)
  )
}
