# tests/testthat/ — unit tests for `fdresopt`

testthat 3rd edition (`Config/testthat/edition: 3` in `DESCRIPTION`). Empty on purpose: tests are written by hand, alongside the vetted code they cover.

- One file per `R/` file: `R/fct_travel_stats.R` → `tests/testthat/test-fct_travel_stats.R` (`usethis::use_test()` creates it).
- Test the `fct_` functions directly, with small inline fixtures. Never read `datasets/`, `legacy/app/data/` or any incident-level file from a test, because tests are part of the package build.
- Fixtures that need a file go in `tests/testthat/fixtures/`, and must be synthetic: no real incident numbers, addresses or locations.
- Module tests use `shiny::testServer()`.
- `_baseline/` (local, gitignored) holds the legacy outputs. Compare new code against it from `analysis/` scripts, not from package tests.

Run with `devtools::test()`; `devtools::check()` runs them too.
