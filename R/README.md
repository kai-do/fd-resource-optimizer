# R/ — package code for `fdresopt`

Hand-written, owner-vetted code only. Code arrives here by hand from `legacy/` once it has been reviewed and rewritten; nothing in this folder may `source()` or read from `legacy/`.

This file is excluded from the package build (`.Rbuildignore`), because `R CMD check` only accepts `.R` files in `R/`.

## File conventions (golem)

| Prefix | Contains | Shiny code? |
|---|---|---|
| `fct_<topic>.R` | Business logic: travel-time summaries, cost/coverage matrices, p-median and MCLP solvers, run-card ordering. Pure functions with data in and data out. | **No.** No `shiny`, no reactives, no `input`/`output`. Must run from a script or a test. |
| `utils_<topic>.R` | Small, generic helpers used by several `fct_` or `mod_` files. | No (except `utils_ui_*`/`utils_server_*` helpers, which may). |
| `mod_<name>.R` | One Shiny module (`mod_<name>_ui()` + `mod_<name>_server()`). Calls `fct_` functions; holds no analysis itself. | Yes. |
| `app_ui.R`, `app_server.R`, `run_app.R`, `app_config.R` | golem entry points. Wire modules together. | Yes. |

The analysis pipeline (`analysis/`), the reports (`reports/`) and the app all call the same `fct_` functions, so a calculation exists in exactly one place.

Create files with golem's helpers so names stay consistent, e.g. `golem::add_fct("travel_stats")`, `golem::add_utils("time")`, `golem::add_module(name = "scenario")`.

## Not here

- Data files of any kind (they live outside the package; see `datasets/` and `app_data_dir` in `inst/golem-config.yml`).
- Machine paths or credentials (use `inst/golem-config.yml` + `.Renviron`).
