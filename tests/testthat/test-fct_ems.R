# Synthetic capability table in the shape of the EMS capability file, read
# with every column as text
make_ems_df <- function() {
  data.frame(
    dispatch_code = c("01C05", "09E01"),
    ems_unit_capability = c("AEMT", "PARAMEDIC")
  )
}

test_that("add_ems_capability handles listed, unlisted and missing codes", {
  incident_df <- data.frame(
    incident_number = c("CF-1", "CF-2", "CF-3", "CF-4"),
    dispatch_code = c("01C05", "09E01", "99Z99", NA)
  )

  out <- add_ems_capability(incident_df, make_ems_df())

  expect_equal(out$ems_unit_capability, c("AEMT", "PARAMEDIC", "OTHER", NA))
})

test_that("add_ems_capability keeps every incident row and column", {
  incident_df <- data.frame(
    incident_number = c("CF-1", "CF-2"),
    dispatch_code = c("01C05", "01C05"),
    other = c(1, 2)
  )

  out <- add_ems_capability(incident_df, make_ems_df())

  expect_equal(nrow(out), 2)
  expect_named(out, c("incident_number", "dispatch_code", "other", "ems_unit_capability"))
})

test_that("add_ems_capability ignores exact repeats in the capability file", {
  ems_df <- rbind(make_ems_df(), make_ems_df()[1, ])
  incident_df <- data.frame(dispatch_code = "01C05")

  out <- add_ems_capability(incident_df, ems_df)

  expect_equal(nrow(out), 1)
  expect_equal(out$ems_unit_capability, "AEMT")
})

test_that("add_ems_capability stops when a code has two capabilities", {
  ems_df <- rbind(
    make_ems_df(),
    data.frame(dispatch_code = "01C05", ems_unit_capability = "PARAMEDIC")
  )

  expect_error(
    add_ems_capability(data.frame(dispatch_code = "01C05"), ems_df),
    "more than one capability: 01C05"
  )
})

test_that("add_ems_capability stops when codes were read as numbers", {
  # A code like 01 read as a number loses its leading zero and never matches
  ems_df <- data.frame(dispatch_code = 1, ems_unit_capability = "AEMT")

  expect_error(
    add_ems_capability(data.frame(dispatch_code = "01"), ems_df),
    "must be text"
  )
})
