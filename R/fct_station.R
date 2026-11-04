#' Prepare station locations for routing
#'
#' Keeps the stations of the chosen departments, drops non-station sites such
#' as headquarters, takes the station number from `abbrev` and converts the
#' point geometry to WGS84 latitude and longitude, the form
#' [build_travel_matrix()] expects.
#'
#' @param station_df Data frame from station_addresses.sql with `dept`,
#'   `abbrev`, `name`, `address` and a `shape_wkt` POINT column.
#' @param keep_dept Departments to keep (values of `dept`).
#' @param exclude_abbrev Sites to drop (values of `abbrev`), e.g. headquarters.
#' @param crs EPSG code of `shape_wkt`.
#'
#' @return Data frame with one row per station: `station_id` (the digits in
#'   `abbrev`, so "F 3" is 3), `station_name`, `dept`, `station_address`, and
#'   WGS84 (EPSG:4326) `latitude` and `longitude`.
#' @noRd
prep_stations <- function(
  station_df,
  keep_dept = "Cobb",
  exclude_abbrev = "FHQ",
  crs = 2240
) {
  keep <- station_df$dept %in% keep_dept & !station_df$abbrev %in% exclude_abbrev
  station_df <- station_df[keep, ]

  station_id <- as.integer(stringr::str_extract(station_df$abbrev, "[0-9]+"))
  if (anyNA(station_id)) {
    stop(
      "No station number in abbrev: ",
      paste(station_df$abbrev[is.na(station_id)], collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(station_id)) {
    stop(
      "Duplicate station numbers: ",
      paste(unique(station_id[duplicated(station_id)]), collapse = ", "),
      call. = FALSE
    )
  }

  # shape_wkt is projected (EPSG:2240 by default); OSRM needs WGS84
  xy <- sf::st_as_sfc(station_df$shape_wkt, crs = crs) |>
    sf::st_transform(4326) |>
    sf::st_coordinates()

  data.frame(
    station_id = station_id,
    station_name = station_df$name,
    dept = station_df$dept,
    station_address = station_df$address,
    latitude = xy[, "Y"],
    longitude = xy[, "X"]
  )
}
