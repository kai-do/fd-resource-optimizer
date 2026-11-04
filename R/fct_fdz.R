#' Prepare FDZ polygons
#'
#' @param fdz_df Data frame of FDZ rows with `station`, `fire_demand_zone`
#'   and a `shape_wkt` column from fdzs.sql (FIRE.gisadmin.FireDemandZones_evw)
#' @param crs EPSG code of `shape_wkt`.
#' @return An sf object with `station_id` and `fdz_id` columns. Invalid
#'   geometries (e.g. self-intersecting rings) are repaired with
#'   [sf::st_make_valid()], which can turn one polygon into a MULTIPOLYGON.
prep_fdz_polygons <- function(fdz_df, crs = 2240) {
  fdz_df |>
    dplyr::rename(station_id = station) |>
    dplyr::mutate(
      fdz_id = paste0(station_id, fire_demand_zone),
      fdz_id = dplyr::case_when(fdz_id == "NAA" ~ "LA", .default = fdz_id)
    ) |>
    sf::st_as_sf(wkt = "shape_wkt", crs = crs) |>
    sf::st_make_valid()
}
