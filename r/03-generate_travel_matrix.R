source("r/02-build_travel_matrix.R")
library(data.table)
library(arrow)

incident_address_df <- fread('datasets/inputs/2026-10-13_incident_address_data.csv', na.strings = 'NULL', data.table = FALSE)

station_address_df <- fread('datasets/inputs/2026-10-13_station_addresses.csv', data.table = FALSE) |>
  filter(dept == "Cobb", abbrev != 'FHQ') |>
  mutate(station_id = as.integer(str_extract(abbrev, '[0-9]+')))

# shape_wkt is in EPSG:2240 (NAD83 / Georgia West, ft); OSRM needs WGS84 (EPSG:4326)
station_sf <- station_address_df |>
  st_as_sf(wkt = "shape_wkt", crs = 2240) |>
  st_transform(crs = 4326)

# Plain data frame with lat/lon columns, in the form build_travel_matrix() expects.
# abbrev is the unique station key (name repeats across departments).
station_df <- station_sf |>
  mutate(longitude = st_coordinates(geometry)[, "X"],
         latitude  = st_coordinates(geometry)[, "Y"]) |>
  st_drop_geometry() |>
  select(station_id, station_name = name, dept, station_address = address, latitude, longitude)


# CAD exports some incidents at (0, 0) or with no coordinates; build_travel_matrix() rejects them
bad_coords <- with(incident_address_df, is.na(latitude) | is.na(longitude) | latitude == 0 | longitude == 0)
cat("Dropping", sum(bad_coords), "incidents with missing or (0, 0) coordinates\n")

incident_df <- incident_address_df[!bad_coords, ] |>
  distinct(incident_number, .keep_all = TRUE)

cat("Building matrix:", nrow(station_df), "stations x", nrow(incident_df), "incidents =",
    format(nrow(station_df) * nrow(incident_df), big.mark = ","), "rows\n")

# osrm-routed must be started with --max-table-size >= 900 + nrow(station_df)
travel_matrix <- build_travel_matrix(station_df, incident_df)

write_parquet(travel_matrix, "datasets/outputs/incident_travel_matrix.parquet", compression = "zstd")
