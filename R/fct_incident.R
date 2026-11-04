#' Remove incidents with missing or (0, 0) coordinates
#'
#' CAD exports some incidents with no coordinates or at (0, 0). Those rows
#' cannot be routed, so they are dropped before building the travel matrix.
#'
#' @param df Data frame of incidents.
#' @param lat,lon Names (strings) of the latitude and longitude columns in `df`.
#'
#' @return `df` without the rows where either coordinate is `NA` or `0`.
#' @noRd
remove_bad_coords <- function(df, lat = "latitude", lon = "longitude") {
  bad <- is.na(df[[lat]]) | is.na(df[[lon]]) | df[[lat]] == 0 | df[[lon]] == 0
  df[!bad, ]
}

#' Clean the incident extract
#'
#' Drops incidents with missing or (0, 0) coordinates, then keeps one row per
#' incident. Both the travel matrix and the FDZ assignment start from this set.
#'
#' @param df Data frame of incidents.
#' @param lat,lon Names (strings) of the latitude and longitude columns in `df`.
#' @param incident_id Name (string) of the incident number column in `df`.
#'
#' @return `df` with bad-coordinate rows removed and duplicate incidents dropped.
#' @noRd
clean_incidents <- function(
  df,
  lat = "latitude",
  lon = "longitude",
  incident_id = "incident_number"
) {
  df <- remove_bad_coords(df, lat = lat, lon = lon)
  df[!duplicated(df[[incident_id]]), ]
}


#' Assign each incident to the FDZ polygon it falls in
#'
#' @param incident_df Data frame of incidents (see [clean_incidents()]) with
#'   WGS84 (EPSG:4326) longitude and latitude columns.
#' @param fdz_df sf polygons from [prep_fdz_polygons()].
#' @param lat,lon Names (strings) of the latitude and longitude columns in
#'   `incident_df`.
#' @param incident_id Name (string) of the incident number column in
#'   `incident_df`.
#' @param fdz_id,station_id Names (strings) of the zone and station columns in
#'   `fdz_df`.
#'
#' @return Data frame with the `incident_id`, `fdz_id` and `station_id` columns
#'   (under the names passed in). Incidents outside every polygon have `NA` in
#'   the zone and station columns.
#' @noRd
assign_incident_fdz <- function(
  incident_df,
  fdz_df,
  lat = "latitude",
  lon = "longitude",
  incident_id = "incident_number",
  fdz_id = "fdz_id",
  station_id = "station_id"
) {
  incident_sf <- incident_df[c(incident_id, lat, lon)] |>
    sf::st_as_sf(coords = c(lon, lat), crs = 4326) |>
    sf::st_transform(sf::st_crs(fdz_df))

  joined <- incident_sf |>
    sf::st_join(
      fdz_df[c(fdz_id, station_id)],
      join = sf::st_within,
      left = TRUE
    ) |>
    sf::st_drop_geometry()

  if (anyDuplicated(joined[[incident_id]])) {
    stop(
      "Some incidents fall in more than one FDZ (overlapping polygons)",
      call. = FALSE
    )
  }
  message(sum(is.na(joined[[fdz_id]])), " incidents fell in no FDZ")

  joined
}
