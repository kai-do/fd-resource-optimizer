# archive/pre-refactor (read-only)

A snapshot of every code file in the repo, taken on 2026-11-03 from commit
`3529e1d8e96b0cfd837e0b9954e17237b952c857`, before the refactor into `legacy/`.

**Do not edit, run or source anything in this folder.** It exists so the original code
stays recoverable after the quarantined copies in `legacy/` get path edits. The contents
are byte-identical to the git blobs at the source commit.

Paths are the repo paths at that commit. They show the half-done move of `r/` and
`scripts/` into `claude/`, so the files here reference `claude/r/...` paths. The code at
its earlier `r/` and `scripts/` paths is at commit `cb1a4ba`.

## Files

- `app/app.R`
- `claude/r/00-setup_osrm_backend.R`
- `claude/r/02-build_travel_matrix.R`
- `claude/r/03-generate_travel_matrix.R`
- `claude/r/04-geocode_fdzs.R`
- `claude/r/05-get_travel_stats.R`
- `claude/r/06-optimize_p_median.R`
- `claude/r/07-optimize_mclp.R`
- `claude/r/08-prep_app_data.R`
- `claude/r/archive/route_matrix.R`
- `claude/r/enrich_calls.R`
- `claude/r/filter_runcard.R`
- `claude/r/filter_travel_matrix.R`
- `claude/r/helpers_optimize.R`
- `claude/r/test_ompr.R`
- `claude/r/transition_helpers.R`
- `claude/scripts/01-start_osrm_server.ps1`
- `r/enrich_calls.R`
- `sql/cad_run_card.sql`
- `sql/fdzs.sql`
- `sql/incident_address_data.sql`
- `sql/station_addresses.sql`
- `tutorials/deployment_analysis_phase_1.qmd`
- `tutorials/simmer_des_tutorial.qmd`

Data files (`datasets/`, `app/data/`) and rendered outputs are not archived here.
