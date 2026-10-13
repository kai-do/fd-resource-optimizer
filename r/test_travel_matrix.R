# test build_travel_matrix(): 1000 incident addresses x 2 stations
# requires osrm-routed running at osrm.server (see generate_travel_matrix.R)

source("r/generate_travel_matrix.R")

set.seed(343)

test_src <- station_df %>%
  filter(station_id %in% c(11, 18))

test_dst <- incident_address_df %>%
  filter(!is.na(latitude), !is.na(longitude), latitude != 0, longitude != 0) %>%
  distinct(incident_number, .keep_all = TRUE) %>%
  slice_sample(n = 1000)

# chunk_size = 400 so the 1000 destinations go through 3 chunks (400, 400, 200)
od <- build_travel_matrix(test_src, test_dst, chunk_size = 400)

# shape: every station x every address
stopifnot(
  nrow(od) == nrow(test_src) * nrow(test_dst),
  !anyDuplicated(od[c("source_id", "dest_id")]),
  setequal(od$source_id, test_src$station_id),
  setequal(od$dest_id, test_dst$incident_number)
)

# ids still line up with their coordinates after the chunked reshape
chk <- od %>%
  left_join(test_dst %>% select(incident_number, latitude, longitude),
            by = c("dest_id" = "incident_number"))
stopifnot(all(abs(chk$dest_lat - chk$latitude) < 1e-9), all(abs(chk$dest_lon - chk$longitude) < 1e-9))

# the first chunk-boundary destination must match a standalone single-chunk call
one_chunk <- build_travel_matrix(test_src, test_dst[395:405, ], chunk_size = 1000)
cmp <- inner_join(od, one_chunk, by = c("source_id", "dest_id"), suffix = c("", "_one"))
stopifnot(nrow(cmp) == nrow(one_chunk), all(cmp$duration_min == cmp$duration_min_one))

print(summary(od[c("distance_m", "duration_min", "source_snap_m", "dest_snap_m")]))
cat("NA durations:", sum(is.na(od$duration_min)), "\n")
cat("dest snap > 100 m:", sum(od$dest_snap_m > 100) / nrow(test_src), "addresses\n")
