#' Add the EMS unit capability each incident's dispatch code needs
#'
#' Looks up each incident's dispatch code in the EMS capability file. Coded
#' incidents the file does not list get `"OTHER"`; incidents with no dispatch
#' code stay `NA`.
#'
#' Read both files with dispatch codes as text, so leading zeros (01C05) are
#' kept and the codes match.
#'
#' @param incident_df Data frame of incidents.
#' @param ems_df Data frame with `code` and `ems_unit_capability` columns.
#' @param code Name (string) of the dispatch code column in both data frames.
#'
#' @return `incident_df` with an `ems_unit_capability` column added.
#' @noRd
add_ems_capability <- function(incident_df, ems_df, code = "dispatch_code") {
  if (!is.character(incident_df[[code]]) || !is.character(ems_df[[code]])) {
    stop("'", code, "' must be text in both data frames", call. = FALSE)
  }

  # Exact repeats are harmless; one code with two capabilities is ambiguous
  ems_df <- unique(ems_df[c(code, "ems_unit_capability")])
  if (anyDuplicated(ems_df[[code]])) {
    stop(
      "Dispatch codes with more than one capability: ",
      paste(
        unique(ems_df[[code]][duplicated(ems_df[[code]])]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  capability <- ems_df$ems_unit_capability[match(
    incident_df[[code]],
    ems_df[[code]]
  )]
  capability[is.na(capability) & !is.na(incident_df[[code]])] <- "OTHER"

  incident_df$ems_unit_capability <- capability
  incident_df
}
