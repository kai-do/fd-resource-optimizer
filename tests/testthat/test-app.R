# Smoke tests for the golem app shell (R/app_*.R, R/run_app.R).

test_that("app_ui() builds a Shiny UI", {
  ui <- app_ui(request = NULL)
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("app_server() runs without error", {
  expect_type(app_server, "closure")
  expect_no_error(shiny::testServer(app_server, {}))
})

test_that("run_app() returns a Shiny app object", {
  expect_s3_class(run_app(), "shiny.appobj")
})

test_that("golem-config.yml has the expected keys", {
  expect_true(file.exists(app_sys("golem-config.yml")))
  expect_equal(get_golem_config("golem_name"), "fdresopt")
  for (key in c(
    "data_inputs_dir",
    "data_outputs_dir",
    "app_data_dir",
    "osrm_host",
    "osrm_port",
    "osrm_profile",
    "osrm_graph",
    "osrm_bin"
  )) {
    expect_type(get_golem_config(key), "character")
  }
})
