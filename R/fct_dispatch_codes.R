# Calls by dispatch code, for every code a filter set includes.
#
# The EMS call mapping lists each MPDS determinant code with its chief
# complaint, descriptor, default CAD code and the fire unit tier it needs.
# dispatch_code_counts() counts the analysis's calls against every code of a
# tier, so codes with no calls show as zeros, and dispatch_code_table() lays
# them out under their chief complaints. Text comes from the mapping only.
#
#   mapping <- read.csv(mapping_file, colClasses = "character",
#     fileEncoding = "cp1252", check.names = FALSE) |> prep_call_mapping()
#   calls |> dispatch_code_counts(mapping, tier = "PARAMEDIC", years = 3) |>
#     dispatch_code_table()

# Mapping column -> name used here
call_mapping_columns <- c(
  "CC #" = "cc_number",
  "Chief Complaint Name" = "chief_complaint",
  "Determinant Code" = "dispatch_code",
  "Determinant Descriptor" = "descriptor",
  "Current CAD Code" = "cad_code",
  "Fire Unit Tier" = "tier"
)

#' Prepare the EMS call mapping
#'
#' Read the file with every column as text (codes keep their leading zeros)
#' and, for the Windows-saved CSV, `fileEncoding = "cp1252"`.
#'
#' @param df The mapping as read, with its original column names.
#'
#' @return One row per code: `cc_number` (numeric), `chief_complaint`,
#'   `dispatch_code`, `descriptor`, `cad_code` and `tier`. Blank cells are
#'   `NA`; text goes through [fix_dispatch_text()] and is trimmed.
#' @noRd
prep_call_mapping <- function(df) {
  missing <- setdiff(names(call_mapping_columns), names(df))
  if (length(missing) > 0) {
    stop("Mapping is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- stats::setNames(as.data.frame(df)[names(call_mapping_columns)], call_mapping_columns)
  # symbols first, so a leading non-breaking space is trimmed too
  out[] <- lapply(out, function(x) {
    x <- trimws(fix_dispatch_text(as.character(x)))
    ifelse(x == "", NA_character_, x)
  })
  if (anyDuplicated(out$dispatch_code)) {
    stop(
      "Codes listed more than once in the mapping: ",
      paste(unique(out$dispatch_code[duplicated(out$dispatch_code)]), collapse = ", "),
      call. = FALSE
    )
  }
  out$cc_number <- as.numeric(out$cc_number)
  out
}

#' Repair symbols lost when the mapping was saved
#'
#' A "?" stands for a lost "≥" or "≤"; which one can't be told, so it becomes
#' "≥/≤". Non-breaking spaces become plain spaces. Nothing else changes.
#'
#' @param x Text.
#' @return `x` with the symbols repaired; `NA` stays `NA`.
#' @noRd
fix_dispatch_text <- function(x) {
  x <- gsub("?", "≥/≤", x, fixed = TRUE)
  gsub(intToUtf8(160), " ", x, fixed = TRUE) # non-breaking space
}

#' Count calls for every dispatch code of a tier
#'
#' @param calls One row per counted call, with `dispatch_code`.
#' @param mapping Output of [prep_call_mapping()].
#' @param tier Fire unit tiers whose codes to list (the filter set's
#'   `ems_capability`, e.g. `"PARAMEDIC"`).
#' @param years Years the calls cover, for the yearly averages.
#'
#' @return One row per code of `tier`, codes with no calls included, ordered
#'   by chief complaint number then code: `cc_number`, `chief_complaint`,
#'   `dispatch_code`, `descriptor`, `cad_code`, `n_calls`, `per_year` and
#'   `pct` (share of all counted calls). Calls whose code is not among them
#'   are reported in a message.
#' @noRd
dispatch_code_counts <- function(calls, mapping, tier, years) {
  if (!is.numeric(years) || length(years) != 1 || is.na(years) || years <= 0) {
    stop("years must be one positive number", call. = FALSE)
  }
  codes <- mapping[mapping$tier %in% tier, ]
  if (nrow(codes) == 0) {
    stop("No codes in the mapping with tier ", paste(tier, collapse = ", "), call. = FALSE)
  }
  codes <- codes[order(codes$cc_number, codes$dispatch_code), ]

  call_codes <- as.character(calls$dispatch_code)
  outside <- call_codes[!call_codes %in% codes$dispatch_code]
  if (length(outside) > 0) {
    n <- sort(table(outside, useNA = "ifany"), decreasing = TRUE)
    message(
      length(outside), " counted call(s) have a code outside the ", paste(tier, collapse = ", "),
      " codes and are left out: ",
      paste0(names(n), " (", n, ")", collapse = ", ")
    )
  }

  counts <- table(factor(call_codes, levels = codes$dispatch_code))
  out <- codes[c("cc_number", "chief_complaint", "dispatch_code", "descriptor", "cad_code")]
  out$n_calls <- as.integer(counts[out$dispatch_code])
  out$per_year <- out$n_calls / years
  out$pct <- if (length(call_codes) > 0) out$n_calls / length(call_codes) else 0
  rownames(out) <- NULL
  out
}

#' Table of calls by dispatch code, grouped by chief complaint
#'
#' @param df Output of [dispatch_code_counts()].
#' @param filter_name Filter set the calls come from, for the subtitle.
#' @param title,subtitle Text above the table; `NULL` for the defaults.
#' @param years Years the calls cover, for the footer.
#'
#' @return A flextable: a shaded row per chief complaint with its subtotals,
#'   then a row per code (Code, Determinant Descriptor, CAD Code, Calls, Per
#'   Year, % of Total). Codes with no calls are in gray.
#' @noRd
dispatch_code_table <- function(df, filter_name = NULL, title = NULL, subtitle = NULL, years = NULL) {
  df <- as.data.frame(df)
  fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")
  fmt_yr <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
  # a share that rounds to 0.0% but isn't zero shows as <0.1%
  fmt_pct <- function(x) ifelse(x > 0 & x < 0.0005, "<0.1%", sprintf("%.1f%%", 100 * x))

  groups <- unique(df[c("cc_number", "chief_complaint")])
  rows <- lapply(seq_len(nrow(groups)), function(g) {
    sub <- df[df$cc_number %in% groups$cc_number[g] & df$chief_complaint %in% groups$chief_complaint[g], ]
    heading <- if (is.na(groups$chief_complaint[g])) as.character(groups$cc_number[g]) else groups$chief_complaint[g]
    rbind(
      data.frame(
        code = heading, descriptor = "", cad_code = "",
        calls = fmt_n(sum(sub$n_calls)), per_year = fmt_yr(sum(sub$per_year)), pct = fmt_pct(sum(sub$pct)),
        is_group = TRUE, is_zero = FALSE
      ),
      data.frame(
        code = sub$dispatch_code,
        descriptor = ifelse(is.na(sub$descriptor), "", sub$descriptor),
        cad_code = ifelse(is.na(sub$cad_code), "", sub$cad_code),
        calls = fmt_n(sub$n_calls), per_year = fmt_yr(sub$per_year), pct = fmt_pct(sub$pct),
        is_group = FALSE, is_zero = sub$n_calls == 0
      )
    )
  })
  rows <- do.call(rbind, rows)
  body <- rows[c("code", "descriptor", "cad_code", "calls", "per_year", "pct")]
  colors <- as.data.frame(lapply(body, function(x) ifelse(rows$is_zero, plot_ink[["muted"]], NA_character_)))

  n_codes <- nrow(df)
  n_with <- sum(df$n_calls > 0)
  ft <- build_table(
    body,
    labels = c("Code", "Determinant Descriptor", "CAD Code", "Calls", "Per Year", "% of Total"),
    groups = NULL,
    colors = colors,
    n_keys = 3,
    title = title %||% "Calls by Dispatch Code",
    subtitle = subtitle %||% paste(
      c(filter_name, sprintf("%s codes, %s with calls", fmt_n(n_codes), fmt_n(n_with))),
      collapse = " · "
    ),
    note = "Every dispatch code the filter set includes; codes with no calls in gray.",
    notes = c(
      "Codes, descriptors and CAD codes from the EMS call mapping; CAD Code is the default each code maps to.",
      "≥/≤ marks a comparison symbol lost in the mapping file.",
      if (!is.null(years)) sprintf("Calls over %.1f years; Per Year is the yearly average.", years)
    )
  )

  groups_i <- which(rows$is_group)
  for (i in groups_i) {
    ft <- flextable::merge_at(ft, i = i, j = 1:3, part = "body")
  }
  # autofit sizes Code to the chief complaint names, which span three columns
  ft <- flextable::width(ft, j = 1, width = 0.75)
  ft <- flextable::bg(ft, i = groups_i, bg = "#E3E9F2", part = "body")
  ft <- flextable::bold(ft, i = groups_i, part = "body")
  ft
}
