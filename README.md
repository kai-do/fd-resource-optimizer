# fd-resource-optimizer

Drive-time matrices from every fire station to every address point and every historical CAD incident location, for station coverage and resource-placement analysis.

## Inputs (`datasets/`)

| File | Contents |
| --- | --- |
| `stations.csv` | Fire stations, geometry as WKT in EPSG:2240 (NAD83 / Georgia West, ftUS) |
| `addresses.csv` | Address points (`siteaddid`), WKT in EPSG:2240 |
| `fdzs.csv` | Fire demand zone polygons, WKT in EPSG:2240 |
| `<date>_cad_incident_locations.csv` | CAD incidents with WGS84 lat/long. Not committed: it holds incident-level locations. |

## Outputs (generated, not committed)

`r/route_matrix.R` writes one row per station × destination:

| File | Columns |
| --- | --- |
| `travel_matrix_minimal.csv` | `station_id`, `dest_address_id`, `distance_m`, `duration_min`, `snap_dist_m` |
| `travel_matrix.csv` | The minimal columns plus station and address names and WKT geometry (EPSG:4326 and 2240) |
| `cad_incident_travel_matrix_minimal.csv` | `station_id`, `incident_number`, `fdz`, `distance_m`, `duration_min`, `snap_dist_m` |
| `cad_incident_travel_matrix.csv` | The minimal columns plus station info, incident key, lat/long, WKT and fire demand zone |

The full files are 1.5–2 GB, so prefer the minimal files and join station or address details only when you need them.

`snap_dist_m` is how far OSRM moved the destination to reach the nearest routable road. Large values (long driveways, parks, points outside the map extract) mean the travel time for that point is unreliable.

The script drops CAD incidents with missing or `(0, 0)` coordinates before routing.

## Running

1. Start a local [OSRM](https://github.com/Project-OSRM/osrm-backend) server on port 5000 using the `car` profile and a Georgia extract (for example from Geofabrik):

   ```sh
   docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-extract -p /opt/car.lua /data/georgia-latest.osm.pbf
   docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-partition /data/georgia-latest.osrm
   docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-customize /data/georgia-latest.osrm
   docker run -t -p 5000:5000 -v "${PWD}:/data" osrm/osrm-backend osrm-routed --algorithm mld --max-table-size 1000 /data/georgia-latest.osrm
   ```

   `--max-table-size` must be at least `chunk_size` (900) plus the number of stations. The OSRM default of 100 is too small.

2. Open `fd-resource-optimizer.Rproj` so the working directory is the repo root, then run `r/route_matrix.R`. It needs the `osrm`, `tidyverse` and `sf` packages.

## Caveats

- OSRM's `car` profile models civilian driving speeds, not emergency response. Calibrate against observed CAD en-route-to-on-scene times before using these as response-time estimates.
- `duration_min` is travel time only. Add turnout time before comparing against total response-time standards such as NFPA 1710.
