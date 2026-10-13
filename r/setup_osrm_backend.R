if (!requireNamespace("osrm.backend", quietly = TRUE)) install.packages("osrm.backend")
library(osrm.backend)

# Downloads prebuilt OSRM binaries if osrm-routed isn't already available
osrm_installed <- tryCatch(
  !is.null(osrm_which(quiet = TRUE)$executable),
  error = function(e) FALSE
)

if (!osrm_installed) {
  osrm_install()
}

# point to a new pbf and rebuild if needed
pdf_file_path <- 'C:/OSMData/2026-10-13/georgia-261012.osm.pbf'

osrm_srv <- osrm_start(
  pdf_file_path,
  ip = "127.0.0.1",
  algorithm = "mld",
  port = 5000,
  max_table_size = 1500   # must be >= chunk_size (900) + number of stations
)

osrm_servers()   # lists running servers


# test osrm
library(sf)
options(osrm.server = "http://127.0.0.1:5000/", osrm.profile = "car")

pts <- st_sfc(st_point(c(-84.5499, 33.9526)), st_point(c(-84.5155, 33.9304)), crs = 4326)
osrmRoute(src = pts[1], dst = pts[2])
osrmTable(src = pts, dst = pts, measure = c("distance", "duration"))
