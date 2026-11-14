# Compare scenarios with each other, not only with the baseline.
#
# compare_overall() puts every scenario against the baseline. When the top
# scenarios are close, the question is whether they differ from each other by
# more than which calls happened to occur. compare_scenarios() takes every pair
# of the scenarios you pick (the baseline can be one of them) from the
# call-level paired_calls() table, so the slow run card assignment is not
# repeated, and adds the same zone-resampled bootstrap intervals.
#
# With many pairs, some ordinary 95% intervals exclude 0 by chance alone.
# simultaneous = TRUE widens each metric's intervals just enough that all of
# them hold together 95% of the time, using one shared set of resamples so
# the correlation between pairs is taken into account (narrower than
# Bonferroni).
#
# What is resampled: whole zones (the default, as compare_overall()), or each
# call (resample = "call"). Calls are independent, but zone call rates also
# drift from year to year more than chance alone would make them;
# overdispersion_factor() measures that, and `overdispersion` widens the
# call-level intervals by its square root.
#
#   set.seed(343)
#   paired |> compare_scenarios(c("Baseline Rescues", "R26->16 +R1", "+R1"))
#   paired |> compare_scenarios(metrics = c("mean_minutes", "pct_within_8m58s"))
#   paired |> compare_scenarios(top3, against = everything, simultaneous = TRUE,
#     resample = "call", overdispersion = overdispersion_factor(zone, year))

#' Pairwise comparison of the chosen scenarios
#'
#' Each pair is compared on the calls both scenarios serve: first minus
#' second, in the order the scenarios are given.
#'
#' @param paired Output of [paired_calls()], e.g. `paired_calls.parquet`.
#' @param scenario The scenarios to compare, in order: names or ids, and
#'   `baseline_name` for the baseline. `NULL` takes the baseline and then
#'   every scenario.
#' @param against `NULL` compares every pair within `scenario`. Or names or
#'   ids to compare each of `scenario` against: the pairs within `scenario`
#'   come first, then each of `scenario` against each of `against` not in
#'   `scenario`, with the `scenario` one always first.
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
#' @param simultaneous `FALSE` gives each pair its own `level` interval.
#'   `TRUE` widens them so that, per metric, all pairs' intervals hold
#'   together with probability `level`. Use several thousand `n_boot`: the
#'   widened intervals sit far in the tails.
#' @param resample `"zone"` resamples whole zones; `"call"` resamples calls,
#'   each on its own.
#' @param overdispersion How much more zone call counts vary than chance
#'   (1 = not at all), e.g. from [overdispersion_factor()]. Every interval is
#'   widened about its difference by the square root.
#' @param n_cores Worker processes for the bootstrap, as in
#'   [compare_overall()]: `NULL` draws in one stream, a number in fixed chunks
#'   (the same intervals for any number).
#' @param predict_years,data_years `NULL` gives confidence intervals for the
#'   long-run differences. A number of years (with `resample = "call"`) gives
#'   instead the range each difference would fall in over a future period
#'   that long, with the same pattern of calls; `data_years` is how many years
#'   the calls cover: one number, or [filter_years()] for each filter set.
#'
#' @return One row per filter set, due level, pair and metric: `filter_name`,
#'   `due`, `scenario_a`, `scenario_b`, `metric`, `n_calls` (served by both),
#'   `value_a`, `value_b`, `diff` (a minus b), `ci_lower`, `ci_upper`,
#'   `level`, `ci_excludes_zero` (the pair differs by more than chance),
#'   `ci_type` ("marginal" or "simultaneous"), `per_interval_level` (each
#'   interval's own level: `level`, or higher when simultaneous),
#'   `n_comparisons` (the pairs sharing the metric's guarantee), `resample`,
#'   `overdispersion` and `predict_years` (`NA` for confidence intervals).
#' @noRd
compare_scenarios <- function(
  paired,
  scenario = NULL,
  against = NULL,
  metrics = "mean_minutes",
  filter_name = NULL,
  due = NULL,
  baseline_name = "Baseline",
  n_boot = 1000,
  level = 0.95,
  simultaneous = FALSE,
  resample = c("zone", "call"),
  overdispersion = 1,
  predict_years = NULL,
  data_years = NULL,
  n_cores = NULL
) {
  resample <- match.arg(resample)
  check_n_cores(n_cores)
  if (!is.numeric(overdispersion) || length(overdispersion) != 1 || is.na(overdispersion) || overdispersion < 1) {
    stop("overdispersion must be one number, 1 or more", call. = FALSE)
  }
  check_predict(predict_years, resample)
  if (!is.null(predict_years) && is.null(data_years)) {
    stop("predict_years needs data_years: how many years the calls cover", call. = FALSE)
  }
  # the future period's share of the data's calls, per filter set
  predict_share <- function(f) {
    if (is.null(predict_years)) {
      return(NULL)
    }
    years <- if (is.numeric(data_years)) data_years else data_years$years[match(f, data_years$filter_name)]
    if (length(years) != 1 || is.na(years)) {
      stop("data_years has no years for filter set ", f, call. = FALSE)
    }
    predict_years / years
  }
  check_pair_metrics(metrics)
  paired <- as.data.frame(paired)
  groups <- plot_groups(paired, scenario, baseline_name)
  others <- if (is.null(against)) NULL else plot_groups(paired, against, baseline_name)
  pairs <- scenario_pairs(groups, others)
  if (length(pairs) == 0) {
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

  involved <- unique(unlist(pairs))
  chunked <- !is.null(n_cores)
  cl <- boot_cluster(n_cores)
  on.exit(boot_stop(cl))
  out <- list()
  for (f in filter_names) {
    for (d in dues) {
      df <- paired[paired$filter_name == f & paired$due == d, ]
      calls <- stats::setNames(lapply(involved, function(g) group_calls(df, g, baseline_name)), involved)
      boths <- lapply(pairs, function(p) pair_calls(calls[[p[1]]], calls[[p[2]]]))
      share <- predict_share(f)
      rows <- if (simultaneous) {
        universe <- if (resample == "zone") sort(unique(df$fdz_id)) else sort(unique(df$dest_id))
        simultaneous_pairs(
          boths, universe, resample, metrics, thresholds, n_boot, level, share,
          groups = pairs, chunked = chunked, cl = cl
        )
      } else {
        lapply(
          boths, marginal_pair, resample,
          metrics = metrics, thresholds = thresholds, n_boot = n_boot, level = level, predict_share = share,
          chunked = chunked, cl = cl
        )
      }
      for (k in seq_along(pairs)) {
        out[[length(out) + 1]] <- cbind(
          widen_intervals(rows[[k]], overdispersion),
          filter_name = f, due = d, scenario_a = pairs[[k]][1], scenario_b = pairs[[k]][2],
          resample = resample, overdispersion = overdispersion, predict_years = predict_years %||% NA_real_,
          row.names = NULL
        )
      }
    }
  }

  cols <- c(
    "filter_name", "due", "scenario_a", "scenario_b", "metric", "n_calls",
    "value_a", "value_b", "diff", "ci_lower", "ci_upper", "level", "ci_excludes_zero",
    "ci_type", "per_interval_level", "n_comparisons", "resample", "overdispersion", "predict_years"
  )
  dplyr::bind_rows(out)[cols]
}

#' How much more zone call counts vary between periods than chance
#'
#' If each zone kept a steady share of the calls, its count in each period
#' (e.g. year) would vary only by chance, like a Poisson count. The Pearson
#' chi-square of the zone-by-period table over its degrees of freedom says
#' how much more they vary: 1 is chance alone, 1.5 is half again as much.
#'
#' @param zone,period One value per call: its zone and its period.
#'
#' @return The ratio, at least 1 (1 when there are too few zones or periods
#'   to tell).
#' @noRd
overdispersion_factor <- function(zone, period) {
  tab <- table(zone, period)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(1)
  }
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  ratio <- sum((tab - expected)^2 / expected) / ((nrow(tab) - 1) * (ncol(tab) - 1))
  max(1, ratio)
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

#' Widen percentile intervals until they hold together
#'
#' @param draws Bootstrap draws, one row per comparison and one column per
#'   resample (the same resamples in every row).
#' @param level Joint level: the share of resamples in which every row lies
#'   inside its interval.
#'
#' @return A list: `alpha`, the tail probability each interval uses (at most
#'   `(1 - level) / 2`, the ordinary interval's), and `lower` / `upper`, each
#'   row's `alpha` and `1 - alpha` percentiles. A row whose draws never vary is
#'   always inside its interval and does not widen the others; with one
#'   varying row the ordinary interval comes back.
#' @noRd
calibrate_simultaneous <- function(draws, level) {
  if (is.null(dim(draws))) {
    draws <- matrix(draws, nrow = 1)
  }
  bounds <- function(a) {
    list(
      lower = apply(draws, 1, stats::quantile, a, names = FALSE),
      upper = apply(draws, 1, stats::quantile, 1 - a, names = FALSE)
    )
  }
  coverage <- function(a) {
    b <- bounds(a)
    mean(colSums(draws < b$lower | draws > b$upper) == 0)
  }

  marginal <- (1 - level) / 2
  varying <- sum(apply(draws, 1, function(x) diff(range(x)) > 0))
  alpha <- if (varying <= 1 || coverage(marginal) >= level) {
    marginal
  } else {
    # coverage only falls as alpha grows: find the largest alpha that still
    # covers `level` of the resamples (alpha = 0, the full range, covers all)
    lo <- 0
    hi <- marginal
    for (i in 1:40) {
      mid <- (lo + hi) / 2
      if (coverage(mid) >= level) lo <- mid else hi <- mid
    }
    lo
  }
  c(list(alpha = alpha), bounds(alpha))
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

# The pairs to compare, each c(first, second): every pair within `groups` in
# order, then each of `groups` against each of `against` not in `groups`
scenario_pairs <- function(groups, against = NULL) {
  pairs <- if (length(groups) >= 2) utils::combn(groups, 2, simplify = FALSE) else list()
  for (a in groups) {
    for (b in setdiff(unique(against), groups)) {
      pairs[[length(pairs) + 1]] <- c(a, b)
    }
  }
  pairs
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

# The calls both of a pair serve, in the shape compare_overall() uses (a as
# the scenario, b as the baseline), so values and intervals come from the
# same code
pair_calls <- function(a, b) {
  joined <- merge(a, b[c("dest_id", "minutes", "distance_m")], by = "dest_id", suffixes = c("_a", "_b"))
  joined <- joined[!is.na(joined$minutes_a) & !is.na(joined$minutes_b), ]
  data.frame(
    dest_id = joined$dest_id,
    fdz_id = joined$fdz_id,
    baseline_minutes = joined$minutes_b,
    scenario_minutes = joined$minutes_a,
    baseline_distance_m = joined$distance_m_b,
    scenario_distance_m = joined$distance_m_a
  )
}

# A pair's point values, a minus b
pair_values <- function(both, metrics, thresholds) {
  value_a <- overall_metrics(both, "scenario", thresholds)[metrics]
  value_b <- overall_metrics(both, "baseline", thresholds)[metrics]
  data.frame(
    metric = metrics,
    n_calls = nrow(both),
    value_a = unname(value_a),
    value_b = unname(value_b),
    diff = unname(value_a - value_b)
  )
}

with_interval <- function(rows, lower, upper, level, ci_type, per_interval_level, n_comparisons) {
  rows$ci_lower <- lower
  rows$ci_upper <- upper
  rows$level <- level
  rows$ci_excludes_zero <- !is.na(lower) & (lower > 0 | upper < 0)
  rows$ci_type <- ci_type
  rows$per_interval_level <- per_interval_level
  rows$n_comparisons <- n_comparisons
  rows
}

# What a pair's calls are resampled by: their zone, or each call itself
resample_cluster <- function(both, resample) {
  if (resample == "zone") both$fdz_id else both$dest_id
}

# Every interval widened about its difference by sqrt(overdispersion)
widen_intervals <- function(rows, overdispersion) {
  if (overdispersion == 1) {
    return(rows)
  }
  s <- sqrt(overdispersion)
  rows$ci_lower <- rows$diff + s * (rows$ci_lower - rows$diff)
  rows$ci_upper <- rows$diff + s * (rows$ci_upper - rows$diff)
  rows$ci_excludes_zero <- !is.na(rows$ci_lower) & (rows$ci_lower > 0 | rows$ci_upper < 0)
  rows
}

# One pair with its own resamples and ordinary intervals
marginal_pair <- function(
  both, resample, metrics, thresholds, n_boot, level, predict_share = NULL, chunked = FALSE, cl = NULL
) {
  intervals <- bootstrap_diffs(
    both, thresholds, n_boot, level,
    cluster = resample_cluster(both, resample), predict_share = predict_share,
    chunked = chunked, cl = cl
  )
  ci <- intervals[match(metrics, intervals$metric), ]
  pair_values(both, metrics, thresholds) |>
    with_interval(ci$ci_lower, ci$ci_upper, level, "marginal", level, 1L)
}

# All pairs on one shared set of resamples (of `universe`: every zone, or
# every call, of the filter set and due level), then each metric's intervals
# widened to hold together. A pair's resample is the shared one restricted to
# its own calls.
#
# `groups` names each pair's two scenarios, c(first, second). A scenario on
# the same calls in several pairs (the usual case: every pair served on every
# call) is computed once per resample and shared, which gives the same draws
# as computing it for each pair. `cache = FALSE` computes every pair's own,
# for checking that. `chunked` and `cl` as in bootstrap_diffs().
simultaneous_pairs <- function(
  boths, universe, resample, metrics, thresholds, n_boot, level, predict_share = NULL,
  groups = NULL, cache = TRUE, chunked = FALSE, cl = NULL
) {
  n_u <- length(universe)
  rows_of <- match(metrics, bootstrap_metrics(thresholds))
  served <- which(vapply(boths, nrow, integer(1)) > 0)
  if (is.null(groups) || !cache) {
    # a name of its own for every side: nothing shared
    groups <- lapply(seq_along(boths), function(k) paste0(c("first", "second"), "_", k))
  }

  # the distinct sides: a scenario on a set of calls
  sides <- list()
  keys <- list()
  side_of <- function(group, both, prefix) {
    for (i in seq_along(keys)) {
      if (identical(keys[[i]]$group, group) && identical(keys[[i]]$dest_id, both$dest_id)) {
        return(i)
      }
    }
    sides[[length(sides) + 1]] <<- bootstrap_side_fn(
      both[[paste0(prefix, "_minutes")]], both[[paste0(prefix, "_distance_m")]], thresholds,
      match(resample_cluster(both, resample), universe), n_u
    )
    keys[[length(keys) + 1]] <<- list(group = group, dest_id = both$dest_id)
    length(sides)
  }
  # pair_calls() puts the first scenario on the scenario side
  pair_sides <- lapply(served, function(k) {
    c(side_of(groups[[k]][1], boths[[k]], "scenario"), side_of(groups[[k]][2], boths[[k]], "baseline"))
  })

  draws_of <- pair_draws_fn(sides, pair_sides, n_u, predict_share, rows_of)
  served_draws <- if (chunked) {
    chunks <- run_boot_chunks(draws_of, n_boot, cl)
    lapply(seq_along(served), function(k) do.call(cbind, lapply(chunks, `[[`, k)))
  } else {
    draws_of(n_boot)
  }
  draws <- lapply(boths, function(both) matrix(NA_real_, length(metrics), n_boot))
  draws[served] <- served_draws
  # a resample that draws none of a pair's zones has no value for it; leave
  # those resamples out of every pair, so all are judged on the same ones
  usable <- Reduce(`&`, lapply(draws, function(x) colSums(!is.finite(x)) == 0))
  if (!all(usable)) {
    message(sum(!usable), " of ", n_boot, " resamples drew none of some pair's zones and are left out")
  }

  values <- lapply(boths, pair_values, metrics = metrics, thresholds = thresholds)
  served <- vapply(boths, nrow, integer(1)) > 0
  lower <- upper <- matrix(NA_real_, length(boths), length(metrics))
  per_level <- rep(NA_real_, length(metrics))
  for (j in seq_along(metrics)[any(served)]) {
    family <- do.call(rbind, lapply(draws[served], function(x) x[j, usable]))
    cal <- calibrate_simultaneous(family, level)
    lower[served, j] <- cal$lower
    upper[served, j] <- cal$upper
    per_level[j] <- 1 - 2 * cal$alpha
  }

  lapply(seq_along(boths), function(k) {
    with_interval(
      values[[k]], lower[k, ], upper[k, ], level, "simultaneous", per_level, sum(served)
    )
  })
}

# A function of n making n shared resamples: for each served pair, a
# metrics x n matrix of first minus second. Each distinct side is computed
# once per resample. Its environment holds only what a draw needs (it is
# copied to worker processes).
pair_draws_fn <- function(sides, pair_sides, n_u, predict_share, rows_of) {
  force(sides)
  force(pair_sides)
  force(n_u)
  force(predict_share)
  force(rows_of)
  function(n) {
    out <- lapply(pair_sides, function(p) matrix(NA_real_, length(rows_of), n))
    for (b in seq_len(n)) {
      w <- resample_weights(n_u, predict_share)
      values <- lapply(sides, function(side) side(w))
      for (k in seq_along(pair_sides)) {
        out[[k]][, b] <- (values[[pair_sides[[k]][1]]] - values[[pair_sides[[k]][2]]])[rows_of]
      }
    }
    out
  }
}
