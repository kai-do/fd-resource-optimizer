# analysis/ — pipeline scripts (new, hand-written)

The batch side of the project: scripts that turn the raw extracts into travel matrices, zone summaries, optimization results and the app's precomputed data. Empty until vetted steps arrive from `legacy/scripts/`.

Excluded from the package build (`.Rbuildignore`). Not package code: no functions are defined here.

## Conventions

- **Numbered steps**, one output per step where possible: `01-…R`, `02-…R`. Each script lists its inputs and outputs in a header comment.
- **Logic lives in the package.** A script loads the package and calls `fct_` functions:
  - while developing: `pkgload::load_all()` (run from the repo root);
  - otherwise: `library(fdresopt)` after `devtools::install()`.
  Scripts only read inputs, call functions, and write outputs.
- **Paths come from config**, never hard-coded: `config::get("data_inputs_dir", file = "inst/golem-config.yml")`, likewise `data_outputs_dir`, `app_data_dir`, `osrm_host`, `osrm_port`, `osrm_graph`, `osrm_bin`. Machine values are set in `.Renviron` (see `.Renviron.example`).
- **Outputs** go to `data_outputs_dir` (intermediates) or `app_data_dir` (what the app reads). Both are under `datasets/`, which is gitignored except for an explicit allow-list. Never write outputs into `inst/`, `data/`, `R/` or `tests/`.
- **OSRM**: the server is the local Windows `osrm-routed` binary, not Docker. Server start scripts (`.ps1`) also go here, reading `FDRO_OSRM_BIN` / `FDRO_OSRM_GRAPH` from the environment.
- **SQL extracts** that feed the pipeline go in `analysis/sql/`. They run by hand on the agency network, and their CSV output is saved to `data_inputs_dir` with a date prefix (`YYYY-MM-DD_<name>.csv`).

If the pipeline grows, `targets` is a candidate for orchestrating these steps; see the refactor notes in the top-level README.

## Steps

Data steps are slow and write the results the output steps read. Output steps only read those results and write tables, plots and maps in seconds, so rerun them freely after changing a setting.

| Step | Script | Kind | Main outputs |
|---|---|---|---|
| 00–02 | OSRM setup, server start, `02-generate_travel_matrix.R` | data (by hand) | `incident_travel_matrix.parquet` |
| 03 | `03-assign_incident_fdz.R` | data | `incident_fdz.parquet` |
| 04 | `04-attach_fdz_to_matrix.R` | data | `incident_travel_matrix_fdz.parquet` |
| 05 | `05-get_travel_stats.R` | data | `travel_stats.parquet` |
| 06 | `06-get_runcard_stats.R` | data | `compare_*.parquet`, `paired_calls.parquet`, `scenarios.parquet`, `filter_years.parquet`, `scenario_comparison.xlsx` |
| 07 | `07-compare_scenario_pairs.R` | data (~20 min) | `compare_pairs.parquet` |
| 08 | `08-optimize_p_median.R` | data | `p_median_solutions.parquet` |
| 09 | `09-score_als_placements.R` | data | `als/compare_*.parquet`, `als/scenarios.parquet`, `als/filter_years.parquet`, `als/als_scenario_comparison.xlsx` |
| 10 | `10-make_tables.R` | output | `tables/*.docx` |
| 11 | `11-make_plots.R` | output | `plots/*.png` |
| 12 | `12-make_maps.R` | output | `maps/*.png`, `maps/*.html` |
| 13 | `13-make_als_tables.R` | output | `tables/als/*.docx` |

Run them with `analysis/run_all.R`, from the repo root:

```r
stage <- "outputs" # or "data", or "all" (the default)
source("analysis/run_all.R")
```

or `Rscript analysis/run_all.R outputs`. Set `steps <- c(10, 13)` instead to run particular steps.
