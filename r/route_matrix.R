library(osrm)
library(tidyverse)
library(sf)

options(osrm.server = "http://localhost:5000/")
options(osrm.profile = "driving")


station_df <- read.csv("datasets/stations.csv", stringsAsFactors = FALSE)
address_df <- read.csv("datasets/addresses.csv", stringsAsFactors = FALSE) %>%
  distinct(siteaddid, .keep_all = TRUE)
fdz_df <- read.csv("datasets/fdzs.csv", stringsAsFactors = FALSE)

station_sf <- station_df %>%
  mutate(shape_wkt_2240 = shape_wkt) %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) %>%
  st_transform(crs = 4326)

address_sf <- address_df %>%
  mutate(shape_wkt_2240 = shape_wkt) %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) %>%
  st_transform(crs = 4326)

fdz_sf <- fdz_df %>%
  mutate(shape_wkt_2240 = shape_wkt) %>%
  st_as_sf(wkt = "shape_wkt", crs = 2240) %>%
  st_transform(crs = 4326)


chunk_size <- 900
n <- nrow(address_sf)
total_chunks <- ceiling(n / chunk_size)

dist_list <- vector("list", ceiling(n / chunk_size))
dur_list <- vector("list", ceiling(n / chunk_size))

for (i in seq(1, n, by = chunk_size)) {
  j <- min(i + chunk_size - 1, n)
  dst_chunk <- address_sf[i:j, ]
  
  tab <- osrmTable(
    src = station_sf,
    dst = dst_chunk,
    measure = c("distance", "duration")
  )
  
  idx <- ceiling(i / chunk_size)
  dist_list[[idx]] <- tab$distances
  dur_list[[idx]]  <- tab$durations
  
  pct <- round(100 * idx / total_chunks, 1)
  cat("Chunk", idx, "of", total_chunks,
      "| rows", i, "to", j,
      "|", pct, "% complete\n")
}

dist_matrix <- do.call(cbind, dist_list)
dur_matrix <- do.call(cbind, dur_list)


rownames(dist_matrix) <- station_sf$station_int
rownames(dur_matrix)  <- station_sf$station_int

colnames(dist_matrix) <- address_sf$siteaddid
colnames(dur_matrix)  <- address_sf$siteaddid

#colnames(dist_matrix) <- make.unique(as.character(colnames(dist_matrix)))
#colnames(dur_matrix)  <- make.unique(as.character(colnames(dur_matrix)))


dist_long <- dist_matrix %>%
  as.data.frame() %>%
  rownames_to_column("station_int") %>%
  pivot_longer(
    cols = -station_int,
    names_to = "siteaddid",
    values_to = "distance_m"
  )

dur_long <- dur_matrix %>%
  as.data.frame() %>%
  rownames_to_column("station_int") %>%
  pivot_longer(
    cols = -station_int,
    names_to = "siteaddid",
    values_to = "duration_min"
  )

od_long <- dist_long %>%
  left_join(dur_long, by = c("station_int", "siteaddid")) %>%
  mutate(station_int = as.integer(station_int)) %>%
  left_join(station_sf, by = "station_int") %>%
  left_join(address_sf, by = "siteaddid") %>%
  transmute(station_id = station_int, station_address = address, station_shape = shape_wkt.x, station_shape_2240 = shape_wkt_2240.x, 
            dest_address_id = siteaddid, dest_address = fulladdr, dest_shape = shape_wkt.y, dest_shape_2240 = shape_wkt_2240.y, 
            distance_m, duration_min)

write.csv(od_long, "datasets/travel_matrix.csv", row.names = FALSE)


od_long_minimial <- od_long %>%
  select(station_id, dest_address_id, distance_m, duration_min)

write.csv(od_long_minimial, "datasets/travel_matrix_minimal.csv", row.names = FALSE)
