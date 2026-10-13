# test osrm
library(osrm.backend)
library(osrm)
library(sf)
options(osrm.server = "http://127.0.0.1:5000/", osrm.profile = "car")

pts <- st_sfc(st_point(c(-84.5499, 33.9526)), st_point(c(-84.5155, 33.9304)), crs = 4326)
osrmRoute(src = pts[1], dst = pts[2])
osrmTable(src = pts, dst = pts, measure = c("distance", "duration"))
