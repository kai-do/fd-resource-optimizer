# Compare scenarios with a baseline scenario.
#
# The scenarios table needs a logical `baseline` column with exactly one TRUE.
# Every other scenario is compared with it, scenario minus baseline, within each
# filter set and due level. Comparisons are paired on the same calls: each
# call's baseline responder time against its responder time in the scenario.
#
#   scenarios |> compare_overall(matrix_df, runcard, thresholds = c(4, 8), ci = TRUE)
#   scenarios |> compare_calls(matrix_df, runcard)
#   scenarios |> compare_by_fdz(matrix_df, runcard)
#   scenarios |> compare_by_station(matrix_df, runcard)
#
# Results are long: one row per metric, with baseline, scenario, diff and
# pct_diff (diff / baseline) columns.

#' Overall comparison of each scenario with the baseline
#'
#' Time, distance and threshold metrics are over the paired calls (served in
#' both); calls served in only one are counted as unserved instead.
#' `total_minutes` sums every paired call's minutes, a demand-weighted total
#' (the quantity the p-median optimizer minimizes); with the same calls on both
#' sides its diff is `n_calls` times the mean diff.
#'
#' @inheritParams stats_by_station
#' @param scenarios Scenarios as for [stats_by_station()], plus a logical
#'   `baseline` column with exactly one `TRUE`.
#' @param thresholds Minutes for the `pct_within_<t>`, `calls_within_<t>`
#'   and `fdz_p90_over_<t>` metrics. `<t>` is `8min` for whole minutes, or
#'   minutes and seconds such as `8m58s` for 8:58 (give it as `8 + 58 / 60`).
#' @param ci Add bootstrap intervals (`ci_lower`, `ci_upper`) for the time,
#'   distance and `pct_within` diffs. They reflect which calls happened to
#'   occur, not error in the modeled travel times. Use `set.seed()` first for
#'   repeatable intervals.
#' @param n_boot,level Number of bootstrap resamples and interval level.
#' @param resample What is resampled: `"zone"`, whole zones (the default), or
#'   `"call"`, each call on its own.
#' @param predict_years `NULL` gives a confidence interval for the long-run
#'   difference. A number of years (with `resample = "call"`) gives instead
#'   the range the difference would fall in over a future period that long,
#'   with the same pattern of calls; the data's years are each filter set's
#'   [filter_years()].
#'
#' @return One row per filter set, scenario, due level and metric:
#'   `filter_name`, `scenario_id`, `scenario_name`, `due`, `metric`,
#'   `baseline`, `scenario`, `diff`, `pct_diff` (and, with `ci`, `ci_lower`,
#'   `ci_upper`, `level`, `resample` and `predict_years`).
#' @noRd
compare_overall <- function(
  scenarios,
  matrix_df,
  runcard_df,
  filters = incident_filter("All calls"),
  due = 1,
  thresholds = c(4, 8),
  ci = FALSE,
  n_boot = 1000,
  level = 0.95,
  resample = c("zone", "call"),
  predict_years = NULL
) {
  resample <- match.arg(resample)
  check_predict(predict_years, resample)
  paired <- paired_calls(scenarios, matrix_df, runcard_df, filters, due)
  data_years <- if (!is.null(predict_years)) filter_years(filters, matrix_df)
  compare_overall_paired(paired, thresholds, ci, n_boot, level, resample, predict_years, data_years)
}

# compare_overall() from precomputed paired_calls(), so several
# comparisons can share one (slow) run card assignment. `data_years`:
# filter_years(), needed with predict_years
compare_overall_paired <- function(
  paired, thresholds, ci, n_boot, level,
  resample = "zone", predict_years = NULL, data_years = NULL
) {
  groups <- split_groups(paired)

  out <- lapply(groups, function(g) {
    both <- g[!is.na(g$baseline_minutes) & !is.na(g$scenario_minutes), ]
    b <- overall_metrics(both, "baseline", thresholds)
    s <- overall_metrics(both, "scenario", thresholds)

    unserved <- data.frame(
      metric = c("unserved_zones", "unserved_calls"),
      baseline = c(
        length(unique(g$fdz_id[is.na(g$baseline_minutes)])),
        sum(is.na(g$baseline_minutes))
      ),
      scenario = c(
        length(unique(g$fdz_id[is.na(g$scenario_minutes)])),
        sum(is.na(g$scenario_minutes))
      )
    )

    rows <- rbind(
      data.frame(metric = names(b), baseline = unname(b), scenario = unname(s)),
      unserved
    )
    rows <- add_diffs(rows)

    if (ci) {
      share <- if (!is.null(predict_years)) predict_years / data_years$years[match(g$filter_name[1], data_years$filter_name)]
      cluster <- if (resample == "zone") both$fdz_id else both$dest_id
      intervals <- bootstrap_diffs(both, thresholds, n_boot, level, cluster = cluster, predict_share = share)
      rows <- merge(rows, intervals, by = "metric", all.x = TRUE, sort = FALSE)
      rows$level <- level
      rows$resample <- resample
      rows$predict_years <- predict_years %||% NA_real_
    }

    cbind(g[1, c("filter_name", "scenario_id", "scenario_name", "due")], rows, row.names = NULL)
  })

  out <- dplyr::bind_rows(out)
  metric_order <- c(names(overall_metrics(paired[0, ], "baseline", thresholds)), "unserved_zones", "unserved_calls")
  sort_compare(out, paired, then = "due", metric_order = metric_order)
}

#' How individual calls change between the baseline and each scenario
#'
#' @inheritParams compare_overall
#' @param slower_by Minutes for the `calls_slower_by_<x>min` and
#'   `share_slower_by_<x>min` metrics (calls more than x minutes slower).
#'
#' @return One row per filter set, scenario, due level and metric:
#'   `filter_name`, `scenario_id`, `scenario_name`, `due`, `metric`, `value`.
#'   Over the paired calls; change is scenario minus baseline minutes.
#' @noRd
compare_calls <- function(
  scenarios,
  matrix_df,
  runcard_df,
  filters = incident_filter("All calls"),
  due = 1,
  slower_by = c(2, 5)
) {
  paired <- paired_calls(scenarios, matrix_df, runcard_df, filters, due)
  compare_calls_paired(paired, slower_by)
}

# compare_calls() from precomputed paired_calls(), so several
# comparisons can share one (slow) run card assignment
compare_calls_paired <- function(paired, slower_by) {
  groups <- split_groups(paired)

  out <- lapply(groups, function(g) {
    both <- g[!is.na(g$baseline_minutes) & !is.na(g$scenario_minutes), ]
    change <- both$scenario_minutes - both$baseline_minutes

    values <- c(
      n_calls = length(change),
      share_responder_changed = mean(both$scenario_source_id != both$baseline_source_id),
      share_faster = mean(change < 0),
      share_slower = mean(change > 0),
      share_unchanged = mean(change == 0),
      mean_change = mean(change),
      median_change = quantile_or_na(change, 0.5),
      p90_change = quantile_or_na(change, 0.9),
      max_change = if (length(change) > 0) max(change) else NA_real_
    )
    for (x in slower_by) {
      values[paste0("calls_slower_by_", x, "min")] <- sum(change > x)
      values[paste0("share_slower_by_", x, "min")] <- mean(change > x)
    }

    cbind(
      g[1, c("filter_name", "scenario_id", "scenario_name", "due")],
      data.frame(metric = names(values), value = unname(values)),
      row.names = NULL
    )
  })

  out <- dplyr::bind_rows(out)
  sort_compare(out, paired, then = "due", metric_order = unique(out$metric))
}

#' Zone-by-zone comparison of each scenario with the baseline
#'
#' @inheritParams compare_overall
#'
#' @return One row per filter set, scenario, due level, zone and metric:
#'   `filter_name`, `scenario_id`, `scenario_name`, `due`, `fdz_id`,
#'   `baseline_source_id`, `scenario_source_id` (the responders),
#'   `responder_changed`, `metric`, `baseline`, `scenario`, `diff`,
#'   `pct_diff`. Metrics: `n_calls`, `mean_minutes`, `total_minutes` (the
#'   zone's summed call minutes, so busy zones weigh more), `median_minutes`,
#'   `p90_minutes`, `pct_within_<t>min`. A side with no responder in the zone
#'   has `NA` metrics.
#' @noRd
compare_by_fdz <- function(
  scenarios,
  matrix_df,
  runcard_df,
  filters = incident_filter("All calls"),
  due = 1,
  thresholds = c(4, 8)
) {
  paired <- paired_calls(scenarios, matrix_df, runcard_df, filters, due)
  compare_by_fdz_paired(paired, thresholds)
}

# compare_by_fdz() from precomputed paired_calls(), so several
# comparisons can share one (slow) run card assignment
compare_by_fdz_paired <- function(paired, thresholds) {
  keys <- c("filter_name", "scenario_id", "scenario_name", "due", "fdz_id")

  zone_side <- function(side) {
    minutes <- paired[[paste0(side, "_minutes")]]
    df <- paired[!is.na(minutes), ]
    df$minutes <- minutes[!is.na(minutes)]
    df$responder <- df[[paste0(side, "_source_id")]]
    summarized <- df |>
      dplyr::summarize(
        source_id = dplyr::first(responder),
        n_calls = dplyr::n(),
        mean_minutes = mean(minutes),
        total_minutes = sum(minutes),
        median_minutes = quantile_or_na(minutes, 0.5),
        p90_minutes = quantile_or_na(minutes, 0.9),
        .by = dplyr::all_of(keys)
      )
    for (t in thresholds) {
      within <- df |>
        dplyr::summarize(value = mean(minutes <= t), .by = dplyr::all_of(keys))
      names(within)[names(within) == "value"] <- paste0("pct_within_", threshold_key(t))
      summarized <- dplyr::left_join(summarized, within, by = keys)
    }
    summarized
  }

  zones <- unique(paired[keys])
  base <- zone_side("baseline")
  scen <- zone_side("scenario")
  wide <- zones |>
    dplyr::left_join(base, by = keys) |>
    dplyr::left_join(scen, by = keys, suffix = c(".baseline", ".scenario"))

  metrics <- c(
    "n_calls",
    "mean_minutes",
    "total_minutes",
    "median_minutes",
    "p90_minutes",
    paste0("pct_within_", threshold_key(thresholds))
  )
  b_src <- wide$source_id.baseline
  s_src <- wide$source_id.scenario
  zone_info <- data.frame(
    wide[keys],
    baseline_source_id = b_src,
    scenario_source_id = s_src,
    responder_changed = responder_differs(b_src, s_src)
  )

  out <- long_metrics(zone_info, wide, metrics)
  sort_compare(out, paired, then = c("due", "fdz_id"), metric_order = metrics)
}

#' Station workload under each scenario compared with the baseline
#'
#' Calls each station responds to, and the zones they are in, in the baseline
#' and in the scenario. The time metrics are over each station's own calls,
#' which differ between the two, so they describe the station's load, not a
#' paired change in speed.
#'
#' @inheritParams compare_overall
#'
#' @return One row per filter set, scenario, due level, station and metric:
#'   `filter_name`, `scenario_id`, `scenario_name`, `due`, `source_id`,
#'   `metric` (`n_incidents`, `n_fdz`, `total_minutes` = drive minutes of
#'   all its calls, `median_minutes`, `p90_minutes`), `baseline`, `scenario`,
#'   `diff`, `pct_diff`. A station that does not respond on one side has 0
#'   incidents, zones and total minutes and `NA` median and p90 there.
#' @noRd
compare_by_station <- function(
  scenarios,
  matrix_df,
  runcard_df,
  filters = incident_filter("All calls"),
  due = 1
) {
  paired <- paired_calls(scenarios, matrix_df, runcard_df, filters, due)
  compare_by_station_paired(paired)
}

# compare_by_station() from precomputed paired_calls(), so several
# comparisons can share one (slow) run card assignment
compare_by_station_paired <- function(paired) {
  keys <- c("filter_name", "scenario_id", "scenario_name", "due", "source_id")

  station_side <- function(side) {
    minutes <- paired[[paste0(side, "_minutes")]]
    df <- paired[!is.na(minutes), ]
    df$source_id <- df[[paste0(side, "_source_id")]]
    df$minutes <- minutes[!is.na(minutes)]
    df |>
      dplyr::summarize(
        n_incidents = dplyr::n(),
        n_fdz = dplyr::n_distinct(fdz_id),
        total_minutes = sum(minutes),
        median_minutes = quantile_or_na(minutes, 0.5),
        p90_minutes = quantile_or_na(minutes, 0.9),
        .by = dplyr::all_of(keys)
      )
  }

  wide <- dplyr::full_join(
    station_side("baseline"),
    station_side("scenario"),
    by = keys,
    suffix = c(".baseline", ".scenario")
  )
  # a station that does not respond on one side serves no calls there
  for (col in c("n_incidents", "n_fdz", "total_minutes")) {
    for (side in c(".baseline", ".scenario")) {
      wide[[paste0(col, side)]][is.na(wide[[paste0(col, side)]])] <- 0L
    }
  }

  metrics <- c("n_incidents", "n_fdz", "total_minutes", "median_minutes", "p90_minutes")
  out <- long_metrics(wide[keys], wide, metrics)
  sort_compare(out, paired, then = c("due", "source_id"), metric_order = metrics)
}

# Internals -------------------------------------------------------------------

# One row per filter set, scenario (other than the baseline), call and due
# level: the call's zone (and territory station_id, if the matrix has it) and
# its responder, minutes and meters in the baseline and in the scenario. NA on
# a side where the zone has no responder.
paired_calls <- function(scenarios, matrix_df, runcard_df, filters, due) {
  base_row <- require_baseline(scenarios)
  if (is.null(due) || length(due) == 0) {
    stop("due must be one or more due levels, e.g. 1 or 1:3", call. = FALSE)
  }

  responders <- scenario_responders(scenarios, matrix_df, runcard_df, filters, due)
  calls <- attr(responders, "calls")
  frame <- dplyr::cross_join(calls, data.frame(due = as.integer(due)))

  side_cols <- function(rows, side) {
    out <- rows[c("filter_name", "dest_id", "due", "source_id", "duration_min", "distance_m")]
    names(out)[4:6] <- paste0(side, c("_source_id", "_minutes", "_distance_m"))
    out
  }
  join_by <- c("filter_name", "dest_id", "due")

  base_id <- scenarios$scenario_id[base_row]
  base <- side_cols(responders[responders$scenario_id == base_id, ], "baseline")
  framed <- dplyr::left_join(frame, base, by = join_by)

  others <- which(!seq_len(nrow(scenarios)) %in% base_row)
  out <- lapply(others, function(i) {
    scen <- side_cols(responders[responders$scenario_id == scenarios$scenario_id[i], ], "scenario")
    joined <- dplyr::left_join(framed, scen, by = join_by)
    joined$scenario_id <- rep(scenarios$scenario_id[i], nrow(joined))
    joined$scenario_name <- rep(scenarios$scenario_name[i], nrow(joined))
    joined
  })

  out <- dplyr::bind_rows(out)
  attr(out, "filter_order") <- attr(responders, "filter_order")
  attr(out, "scenario_order") <- scenarios$scenario_id[others]
  out
}

# The position of the baseline scenario; errors unless there is exactly one
require_baseline <- function(scenarios) {
  if (!"baseline" %in% names(scenarios)) {
    stop("scenarios needs a logical `baseline` column with one TRUE", call. = FALSE)
  }
  b <- scenarios$baseline
  if (!is.logical(b) || anyNA(b) || sum(b) != 1) {
    stop("scenarios$baseline must be TRUE for exactly one scenario", call. = FALSE)
  }
  if (nrow(scenarios) < 2) {
    stop("scenarios needs at least one scenario besides the baseline", call. = FALSE)
  }
  which(b)
}

# Threshold minutes as they appear in metric names: whole minutes as "8min",
# others to the nearest second as "8m58s" (8:58), so 8 + 58 / 60 is not
# named "8.96666666666667min"
threshold_key <- function(t) {
  secs <- round(t * 60)
  ifelse(
    secs %% 60 == 0,
    paste0(secs %/% 60, "min"),
    sprintf("%dm%02ds", as.integer(secs %/% 60), as.integer(secs %% 60))
  )
}

# Threshold minutes for display: "8" or "8:58"
threshold_text <- function(t) {
  key <- threshold_key(t)
  ifelse(grepl("min$", key), sub("min$", "", key), sub("^(\\d+)m(\\d{2})s$", "\\1:\\2", key))
}

# TRUE where the baseline and scenario responders differ: a different station,
# or served on one side only (unserved on both is not a change)
responder_differs <- function(base, scen) {
  ifelse(is.na(base) & is.na(scen), FALSE, is.na(base) | is.na(scen) | base != scen)
}

split_groups <- function(paired) {
  key <- interaction(paired$filter_name, paired$scenario_id, paired$due, drop = TRUE, lex.order = TRUE)
  split(paired, key)
}

quantile_or_na <- function(x, p) {
  if (length(x) == 0) NA_real_ else stats::quantile(x, p, names = FALSE)
}

# Overall metrics for one side of the paired calls
overall_metrics <- function(both, side, thresholds) {
  minutes <- both[[paste0(side, "_minutes")]]
  values <- c(
    n_calls = length(minutes),
    mean_minutes = if (length(minutes) > 0) mean(minutes) else NA_real_,
    # demand-weighted: every call's minutes count, so busy areas weigh more
    total_minutes = sum(minutes),
    median_minutes = quantile_or_na(minutes, 0.5),
    p90_minutes = quantile_or_na(minutes, 0.9),
    mean_distance_m = if (length(minutes) > 0) mean(both[[paste0(side, "_distance_m")]]) else NA_real_
  )
  zone_p90 <- if (length(minutes) > 0) {
    tapply(minutes, both$fdz_id, stats::quantile, 0.9, names = FALSE)
  } else {
    numeric()
  }
  for (t in thresholds) {
    values[paste0("pct_within_", threshold_key(t))] <- if (length(minutes) > 0) mean(minutes <= t) else NA_real_
    values[paste0("calls_within_", threshold_key(t))] <- sum(minutes <= t)
    values[paste0("fdz_p90_over_", threshold_key(t))] <- sum(zone_p90 > t)
  }
  values
}

add_diffs <- function(rows) {
  rows$diff <- rows$scenario - rows$baseline
  rows$pct_diff <- ifelse(is.na(rows$baseline) | rows$baseline == 0, NA_real_, rows$diff / rows$baseline)
  rows
}

# Long rows from `wide`, which has <metric>.baseline and <metric>.scenario
# columns; `info` holds the columns repeated on every metric row
long_metrics <- function(info, wide, metrics) {
  out <- lapply(metrics, function(m) {
    rows <- info
    rows$metric <- m
    rows$baseline <- as.numeric(wide[[paste0(m, ".baseline")]])
    rows$scenario <- as.numeric(wide[[paste0(m, ".scenario")]])
    add_diffs(rows)
  })
  dplyr::bind_rows(out)
}

# Sort by filter set and scenario in input order, then the `then` columns,
# then metric in `metric_order`
sort_compare <- function(out, paired, then, metric_order) {
  # no calls in any filter set: nothing to sort
  if (nrow(out) == 0) {
    return(out)
  }
  ord <- do.call(
    order,
    c(
      list(
        match(out$filter_name, attr(paired, "filter_order")),
        match(out$scenario_id, attr(paired, "scenario_order"))
      ),
      unname(as.list(out[then])),
      list(match(out$metric, metric_order))
    )
  )
  out <- out[ord, ]
  rownames(out) <- NULL
  out
}

# Bootstrap intervals for the scenario minus baseline diffs on the paired
# calls (port of legacy bootstrap_response_diff()). `cluster` says what is
# resampled: whole zones by default, or each call on its own.
# `predict_share` (the future period's calls as a share of the data's, e.g.
# 1/3 for one year of three) turns the interval into a prediction range:
# where the diff would fall over such a period, not the long-run average.
bootstrap_diffs <- function(both, thresholds, n_boot, level, cluster = both$fdz_id, predict_share = NULL) {
  metrics <- bootstrap_metrics(thresholds)
  if (nrow(both) == 0) {
    return(data.frame(metric = metrics, ci_lower = NA_real_, ci_upper = NA_real_))
  }

  id <- as.integer(factor(cluster))
  n_cluster <- max(id)
  diff_of <- bootstrap_diff_fn(both, thresholds, id, n_cluster)
  draws <- vapply(seq_len(n_boot), function(b) {
    diff_of(resample_weights(n_cluster, predict_share))
  }, numeric(length(metrics)))
  draws <- matrix(draws, nrow = length(metrics))

  alpha <- (1 - level) / 2
  data.frame(
    metric = metrics,
    ci_lower = apply(draws, 1, stats::quantile, alpha, names = FALSE),
    ci_upper = apply(draws, 1, stats::quantile, 1 - alpha, names = FALSE)
  )
}

# One resample: how many times each of n clusters is drawn. With
# `predict_share`, two stages: the data resampled (how sure the estimate is),
# then a future period's share of calls drawn from that (that period's own
# luck of where calls fall)
resample_weights <- function(n, predict_share = NULL) {
  drawn <- sample.int(n, n, replace = TRUE)
  if (!is.null(predict_share)) {
    drawn <- drawn[sample.int(n, max(1, round(n * predict_share)), replace = TRUE)]
  }
  tabulate(drawn, n)
}

# The interval's name and its share of calls to predict: a CI of the long-run
# difference, or a range for a future period of `predict_years`
interval_text <- function(level, predict_years = NA) {
  pct <- paste0(signif(100 * level, 3), "%")
  if (is.null(predict_years) || is.na(predict_years)) {
    return(list(short = paste(pct, "CI"), phrase = paste(pct, "CI")))
  }
  span <- if (predict_years == 1) "a single year" else paste0("a ", predict_years, "-year period")
  list(
    short = paste0(pct, " ", predict_years, "-Yr Range"),
    phrase = paste0(pct, " range for ", span)
  )
}

check_predict <- function(predict_years, resample) {
  if (is.null(predict_years)) {
    return(invisible())
  }
  if (!is.numeric(predict_years) || length(predict_years) != 1 || is.na(predict_years) || predict_years <= 0) {
    stop("predict_years must be one positive number of years", call. = FALSE)
  }
  if (resample != "call") {
    stop("predict_years needs resample = \"call\": a future period is a draw of calls", call. = FALSE)
  }
}

# The metrics bootstrap_diffs() gives intervals for, in its row order
bootstrap_metrics <- function(thresholds) {
  c(
    "mean_minutes",
    "median_minutes",
    "p90_minutes",
    "mean_distance_m",
    paste0("pct_within_", threshold_key(thresholds))
  )
}

# A function of one resample's weights (how many times each of n_cluster
# clusters is drawn; `id` gives each call's cluster, 1 to n_cluster) that
# returns the scenario minus baseline diffs, one per bootstrap_metrics().
# A resample is fully described by those counts, so each side is sorted once
# and the weighted order statistics are read from running counts, matching
# quantile(type = 7) on the resampled calls. Clusters with no calls here
# (n_cluster can cover calls from elsewhere) just add nothing.
bootstrap_diff_fn <- function(both, thresholds, id, n_cluster) {
  cluster_n <- tabulate(id, n_cluster)
  present <- sort(unique(id))
  cluster_sum <- function(x) {
    out <- numeric(n_cluster)
    out[present] <- as.vector(rowsum(as.numeric(x), id, reorder = TRUE))
    out
  }

  side <- function(prefix) {
    minutes <- both[[paste0(prefix, "_minutes")]]
    o <- order(minutes)
    list(
      value = minutes[o],
      id = id[o],
      sums = c(
        list(minutes = cluster_sum(minutes), distance = cluster_sum(both[[paste0(prefix, "_distance_m")]])),
        stats::setNames(lapply(thresholds, function(t) cluster_sum(minutes <= t)), paste0("within_", thresholds))
      )
    )
  }
  sides <- list(baseline = side("baseline"), scenario = side("scenario"))

  draw_stats <- function(s, w) {
    count <- cumsum(w[s$id])
    n <- count[length(count)]
    order_stat <- function(j) s$value[findInterval(j - 1, count) + 1]
    q <- vapply(c(0.5, 0.9), function(p) {
      h <- (n - 1) * p + 1
      lo <- floor(h)
      x_lo <- order_stat(lo)
      x_lo + (h - lo) * (order_stat(min(lo + 1, n)) - x_lo)
    }, numeric(1))
    total <- sum(w * cluster_n)
    weighted <- vapply(s$sums, function(z) sum(w * z) / total, numeric(1))
    c(weighted[["minutes"]], q, weighted[["distance"]], weighted[paste0("within_", thresholds)])
  }

  function(w) draw_stats(sides$scenario, w) - draw_stats(sides$baseline, w)
}
