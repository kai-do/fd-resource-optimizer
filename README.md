# fd-resource-optimizer

Fire station deployment and resource-placement analysis for Cobb County Fire (GA). Coordinates are EPSG:2240 (NAD83 / Georgia West, ftUS) unless noted.

The project has two halves that share the same functions:

- **Analysis:** drive-time matrices from every station to historical CAD incidents (local OSRM), travel-time summaries by Fire Demand Zone (FDZ), and station-placement optimization (P-median and MCLP, with `ompr` + GLPK).
- **App:** a Shiny app for comparing deployment scenarios.

## Legacy vs. new

The repo is being rebuilt as a golem app-package, `fdresopt`.

| | `legacy/` | Everything else (`R/`, `analysis/`, `reports/`, `tests/`) |
|---|---|---|
| What | All code written before the refactor (mixed human and AI authorship) | New code, written by hand |
| Status | Quarantined. It runs, but it isn't vetted as a whole. | Vetted |
| Rules | Moved verbatim. Only path references and the quarantine header were changed. Don't build on it. | Never `source()` or read from `legacy/`. |

Vetted pieces move across by hand, one at a time: rewrite them into `R/fct_*.R`, test them in `tests/testthat/`, and compare them against `_baseline/`. `archive/pre-refactor/` is a read-only, byte-identical copy of the code before the move.

## Layout

```
DESCRIPTION, NAMESPACE   golem app-package fdresopt
R/                       package code: fct_* (logic, no Shiny), utils_*, mod_* (Shiny modules), app_* (golem shells)
inst/golem-config.yml    configuration (paths, OSRM), reads FDRO_* environment variables
inst/app/www/            static assets for the app
dev/                     golem dev scripts (command logs, not package code)
tests/testthat/          unit tests (testthat 3e)
analysis/                new pipeline scripts; call package functions, read paths from config
reports/                 new Quarto reports
legacy/                  quarantined pre-refactor code (working)
  R/                       helpers_optimize.R, transition_helpers.R (sourced by legacy scripts)
  scripts/                 00-08 pipeline, OSRM start script, one-off scripts, archive/route_matrix.R
  app/                     legacy Shiny app (app.R) and its precomputed data/
  sql/                     T-SQL extracts (agency network only)
  tutorials/               Quarto tutorials and their rendered output
archive/pre-refactor/    read-only copy of all code at commit 3529e1d
datasets/                data, outside the package build; gitignored except an allow-list
  inputs/                  dated CSV extracts (YYYY-MM-DD_<name>.csv)
  outputs/                 pipeline intermediates
  archive/                 old extracts and matrices
_baseline/               local, gitignored: legacy outputs for comparing new code against
renv.lock, renv/         package versions (package, tooling and legacy code)
```

`legacy/`, `archive/`, `datasets/`, `analysis/`, `reports/`, `dev/` and `_baseline/` are excluded from the package build (`.Rbuildignore`). The Air formatter and lintr both skip `legacy/` and `archive/`.

## Setup

1. Open `fd-resource-optimizer.Rproj` (or the folder in Positron), so the working directory is the repo root.
2. Run `renv::restore()` to install the locked package versions. The project uses R 4.5.2.
3. Copy `.Renviron.example` to `.Renviron`, fill in the paths for this machine, and restart R. `.Renviron` is gitignored.

## Data and sensitivity

Incident-level data (incident numbers, locations, alarm times) and travel matrices are **never committed**:

- `.gitignore` ignores everything under `datasets/` and `legacy/app/data/` by default. It allows only the non-incident reference files: run card, station addresses, EMS capabilities, FDZ polygons, and the app's lookup tables.
- `datasets/` is excluded from the package build, and no data lives in the package (`data/`, `inst/extdata/`).
- Some incident-level files were committed before the refactor: `app/data/travel_matrix_app.parquet`, `app/data/incident_categories.parquet` and `datasets/inputs/2026-10-05_deployment_analysis_phase_1.csv`. They are no longer tracked, but they are still in git history. History has not been rewritten.

## OSRM (local Windows binary, not Docker)

1. **Install the binaries once** with `legacy/scripts/00-setup_osrm_backend.R`. This uses the `osrm.backend` package and puts `osrm-routed.exe`, `osrm-extract.exe`, `osrm-partition.exe`, `osrm-customize.exe` and `profiles/car.lua` in its R cache folder.
2. **Prepare the graph** (MLD) from a Georgia OSM extract (`.osm.pbf`, for example from Geofabrik), using the binaries from step 1:
   ```powershell
   osrm-extract.exe -p <osrm.backend cache>\profiles\car.lua georgia-latest.osm.pbf
   osrm-partition.exe georgia-latest.osrm
   osrm-customize.exe georgia-latest.osrm
   ```
3. **Start the server.**
   - Run `powershell -File legacy\scripts\01-start_osrm_server.ps1`, which serves on `127.0.0.1:5000`. Its `-OsrmBin`, `-Graph`, `-Port` and `-MaxTableSize` parameters override the defaults.
   - `--max-table-size` must be at least the chunk size (900) plus the number of stations.
   - New code reads `FDRO_OSRM_BIN`, `FDRO_OSRM_GRAPH`, `FDRO_OSRM_HOST` and `FDRO_OSRM_PORT` from `.Renviron`, via `inst/golem-config.yml`.

## Running the legacy pipeline

Run each script from the repo root, for example `source("legacy/scripts/05-get_travel_stats.R")`. Steps 00–03 need the local OSRM server. The SQL extracts need the agency network.

| Step | Inputs | Outputs |
|---|---|---|
| `legacy/sql/*.sql` (run by hand) | CAD/RMS databases | `datasets/inputs/<date>_cad_run_card.csv`, `_fdzs.csv`, `_incident_address_data.csv`, `_station_addresses.csv` |
| `00-setup_osrm_backend.R` | none | OSRM binaries in the `osrm.backend` cache |
| `01-start_osrm_server.ps1` | `osrm-routed.exe`, prepared `.osrm` graph | OSRM server on `127.0.0.1:5000` |
| `02-build_travel_matrix.R` | none (sourced by 03) | `build_travel_matrix()` defined |
| `03-generate_travel_matrix.R` | `inputs/2026-10-13_incident_address_data.csv`, `inputs/2026-10-13_station_addresses.csv`, OSRM server | `outputs/incident_travel_matrix.parquet` |
| `04-geocode_fdzs.R` | `inputs/2026-10-14_fdzs.csv`, `inputs/2026-10-13_incident_address_data.csv`, `inputs/2026-10-14_ems_unit_capabilities.csv`, `outputs/incident_travel_matrix.parquet` | `outputs/incident_travel_matrix_fdz.parquet` |
| `05-get_travel_stats.R` | `outputs/incident_travel_matrix_fdz.parquet` | `outputs/travel_stats.parquet` |
| `06-optimize_p_median.R` | `outputs/travel_stats.parquet` | P-median result (p = 10), printed |
| `07-optimize_mclp.R` | `outputs/travel_stats.parquet` | MCLP result (p = 10, 6 min), printed |
| `08-prep_app_data.R` | `inputs/2026-10-14_fdzs.csv`, `inputs/2026-10-13_incident_address_data.csv`, `inputs/2026-10-13_cad_run_card.csv`, `inputs/2026-10-14_ems_unit_capabilities.csv`, `outputs/incident_travel_matrix_fdz.parquet`, `outputs/travel_stats.parquet` | `legacy/app/data/*` (8 files) |
| `filter_travel_matrix.R` | `outputs/incident_travel_matrix_fdz.parquet`, `inputs/2026-10-14_fdzs.csv` | `outputs/station_8_fdz_travel_stats.csv` |

All `inputs/` and `outputs/` paths are under `datasets/`. The other scripts in `legacy/scripts/` (`filter_runcard.R`, `enrich_calls.R`, `test_ompr.R`, `archive/route_matrix.R`) are one-off scripts or older ones, not pipeline steps.

**Tutorials:** `legacy/tutorials/simmer_des_tutorial.qmd` renders with `quarto render`. `legacy/tutorials/deployment_analysis_phase_1.qmd` currently does not render (see the open issues in the refactor report).

## Running the apps

- **Legacy app:** run `08-prep_app_data.R` once. Then, from the repo root, run `shiny::runApp("legacy/app")`. The app's incident-level data files are local-only, so a fresh clone needs step 08 first.
- **New app** (an empty golem shell for now): run `golem::run_dev()` for development (from `dev/run_dev.R`), or `devtools::install()` and then `fdresopt::run_app()`.

## Development

- **Formatting:** Air (`air.toml`), with format-on-save configured in `.vscode/settings.json`.
- **Linting:** `lintr::lint_package()` (`.lintr`: tidyverse defaults, with the layout linters off because Air owns layout).
- **Tests and checks:** `devtools::test()` and `devtools::check()`.
- **Dependencies:** run `renv::snapshot()` after adding packages, and add package dependencies to `DESCRIPTION` with `usethis::use_package()`.

## Caveats

- OSRM's `car` profile models civilian driving speeds, not emergency response. Calibrate against observed CAD en-route-to-on-scene times before treating these as response-time estimates.
- `duration_min` is travel time only. It excludes turnout time, so add turnout before comparing against total response-time standards such as NFPA 1710.
