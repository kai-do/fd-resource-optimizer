# Excel export of a scenario comparison: the run's settings, scenarios and
# filter sets, then the compare_*() tables, in one workbook.
#
#   export_comparison(
#     scenarios, matrix_df, runcard, "scenario_comparison.xlsx",
#     filters = list(all_calls, night_als), due = 1:2, ci = TRUE, seed = 2026
#   )

#' Write a scenario comparison workbook
#'
#' Runs [compare_overall()], [compare_calls()], [compare_by_fdz()],
#' [compare_by_station()] and, when the matrix has each call's territory
#' `station_id`, [compare_by_territory()] on one shared run card assignment
#' (computed once) and writes them, with the settings, scenarios and filter
#' sets used, to an Excel file.
#'
#' @inheritParams compare_overall
#' @inheritParams compare_calls
#' @param file Path of the .xlsx file to write (overwritten if it exists).
#' @param seed If not `NULL`, passed to `set.seed()` before the bootstrap, so
#'   the confidence intervals can be repeated; recorded in the Settings sheet.
#' @param n_cores Worker processes for the bootstrap, as in
#'   [compare_overall()].
#'
#' @return Invisibly, a named list of the sheets' data frames: Settings,
#'   Scenarios, Filter Sets, Overall, Calls, By FDZ, By Station (and By
#'   Territory when the matrix has `station_id`). Its
#'   `"paired"` attribute holds the call-level [paired_calls()] table the
#'   comparisons were built from (not written to the workbook), for
#'   [travel_time_boxplot()].
#' @noRd
export_comparison <- function(
  scenarios,
  matrix_df,
  runcard_df,
  file,
  filters = incident_filter("All calls"),
  due = 1,
  thresholds = c(4, 8),
  slower_by = c(2, 5),
  ci = FALSE,
  n_boot = 1000,
  level = 0.95,
  resample = c("zone", "call"),
  predict_years = NULL,
  seed = NULL,
  n_cores = NULL
) {
  resample <- match.arg(resample)
  check_predict(predict_years, resample)
  check_n_cores(n_cores)
  if (inherits(filters, "incident_filter")) {
    filters <- list(filters)
  }

  paired <- paired_calls(scenarios, matrix_df, runcard_df, filters, due)
  data_years <- if (!is.null(predict_years)) filter_years(filters, matrix_df)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  sheets <- list(
    "Settings" = settings_sheet(matrix_df, due, thresholds, slower_by, ci, n_boot, level, resample, predict_years, seed, n_cores),
    "Scenarios" = scenarios_sheet(scenarios),
    "Filter Sets" = filters_sheet(filters),
    "Overall" = compare_overall_paired(paired, thresholds, ci, n_boot, level, resample, predict_years, data_years, n_cores),
    "Calls" = compare_calls_paired(paired, slower_by),
    "By FDZ" = compare_by_fdz_paired(paired, thresholds),
    "By Station" = compare_by_station_paired(paired)
  )
  if ("station_id" %in% names(paired)) {
    sheets[["By Territory"]] <- compare_by_territory_paired(paired, thresholds)
  }

  write_workbook(sheets, file)
  attr(sheets, "paired") <- paired
  invisible(sheets)
}

settings_sheet <- function(
  matrix_df, due, thresholds, slower_by, ci, n_boot, level,
  resample = "zone", predict_years = NULL, seed = NULL, n_cores = NULL
) {
  listed <- function(x) paste(x, collapse = ", ")
  data.frame(
    setting = c(
      "created",
      "calls in travel matrix",
      "stations in travel matrix",
      "due levels",
      "thresholds (minutes)",
      "slower by (minutes)",
      "intervals",
      "bootstrap resamples",
      "resampled",
      "interval level",
      "seed",
      "random streams"
    ),
    value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
      format(length(unique(matrix_df$dest_id)), big.mark = ","),
      length(unique(matrix_df$source_id)),
      listed(due),
      listed(threshold_text(thresholds)),
      listed(slower_by),
      if (ci) interval_text(level, predict_years %||% NA)$phrase else "No",
      if (ci) n_boot else "",
      if (ci) (if (resample == "zone") "whole zones" else "calls") else "",
      if (ci) level else "",
      if (is.null(seed)) "" else seed,
      if (!ci) "" else if (is.null(n_cores)) "one stream" else "chunked (same intervals on any number of cores)"
    )
  )
}

scenarios_sheet <- function(scenarios) {
  scenarios <- validate_scenarios(scenarios)
  data.frame(
    scenario_id = scenarios$scenario_id,
    scenario_name = scenarios$scenario_name,
    baseline = ifelse(scenarios$baseline, "Yes", ""),
    n_stations = lengths(scenarios$stations),
    stations = vapply(
      scenarios$stations,
      function(st) paste(sort(st), collapse = ", "),
      character(1)
    )
  )
}

filters_sheet <- function(filters) {
  rows <- lapply(filters, function(f) {
    values <- c(
      if (!is.null(f$date_range)) c(date_range = paste(f$date_range, collapse = " to ")),
      if (!is.null(f$time_range)) c(time_range = paste(f$time_range, collapse = " to ")),
      vapply(
        f$codes,
        function(codes) paste(ifelse(is.na(codes), "(blank)", codes), collapse = ", "),
        character(1)
      )
    )
    if (length(values) == 0) {
      values <- c("(none)" = "All incidents")
    }
    data.frame(filter_name = f$name, filter = names(values), value = unname(values))
  })
  dplyr::bind_rows(rows)
}

# Write the sheets with a bold, frozen, filterable header and number formats:
# shares as percentages, counts as whole numbers, everything else 2 decimals
write_workbook <- function(sheets, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontName = output_font)
  header <- openxlsx::createStyle(textDecoration = "bold")
  styles <- list(
    pct = openxlsx::createStyle(numFmt = "0.0%"),
    count = openxlsx::createStyle(numFmt = "#,##0"),
    number = openxlsx::createStyle(numFmt = "#,##0.00")
  )

  for (name in names(sheets)) {
    df <- as.data.frame(sheets[[name]])
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df, headerStyle = header, keepNA = FALSE, withFilter = TRUE)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_along(df), widths = "auto")
    if (nrow(df) > 0 && "metric" %in% names(df)) {
      format_metric_cells(wb, name, df, styles)
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

format_metric_cells <- function(wb, name, df, styles) {
  value_cols <- which(names(df) %in% c("baseline", "scenario", "diff", "ci_lower", "ci_upper", "value"))
  is_share <- grepl("^(pct_within|share_)", df$metric)
  is_count <- grepl("^(n_|calls_|unserved_|fdz_p90_over_)", df$metric)
  kind <- ifelse(is_share, "pct", ifelse(is_count, "count", "number"))

  for (k in names(styles)) {
    rows <- which(kind == k) + 1 # + 1 for the header row
    if (length(rows) > 0 && length(value_cols) > 0) {
      openxlsx::addStyle(wb, name, styles[[k]], rows = rows, cols = value_cols, gridExpand = TRUE)
    }
  }
  if ("pct_diff" %in% names(df)) {
    openxlsx::addStyle(
      wb, name, styles$pct,
      rows = seq_len(nrow(df)) + 1,
      cols = which(names(df) == "pct_diff"),
      gridExpand = TRUE
    )
  }
}
