# reports/ — Quarto documents (new, hand-written)

Analysis write-ups and tutorials. Empty until vetted reports arrive from `legacy/tutorials/`.

Excluded from the package build (`.Rbuildignore`).

## Conventions

- Load the package's functions (`pkgload::load_all()` or `library(fdresopt)`) instead of `source()`-ing files; never source anything from `legacy/`.
- Read data paths from config (`config::get(..., file = "inst/golem-config.yml")`) rather than relative paths. Quarto runs code from the document's own folder by default, so a path like `"datasets/..."` breaks. Render from the repo root with `quarto render reports/<file>.qmd --execute-dir .`, or resolve paths from config.
- Rendered output that contains incident-level detail (tables of incidents, maps of incident points) must not be committed. Commit only the `.qmd`, or render to a gitignored location.
