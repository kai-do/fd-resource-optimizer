# Dispatch code counts on a small mapping: chief complaints 1 and 9, and a
# BLS code that a PARAMEDIC table leaves out

call_mapping_raw <- function() {
  data.frame(
    `Response Level` = c("Charlie", "Delta", "Delta", "Echo", "Alpha"),
    `CC #` = c("9", "1", "1", "9", "1"),
    `Chief Complaint Name` = c(
      "Cardiac or Respiratory Arrest / Death", "Abdominal Pain / Problems", "Abdominal Pain / Problems",
      "Cardiac or Respiratory Arrest / Death", "Abdominal Pain / Problems"
    ),
    `Determinant Code` = c("09E01", "01D01", "01C01", "09E00", "01A01"),
    `Determinant Descriptor` = c(
      "Not breathing at all", "\u00a0Not\u00a0alert", "SUSPECTED aortic aneurysm ? 50", "Override", "Abdominal pain"
    ),
    `Current CAD Code` = c("ALS1F", "ALS", "ALS", "", "BLS"),
    `Fire Unit Tier` = c("PARAMEDIC", "PARAMEDIC", "PARAMEDIC", "PARAMEDIC", "EMT-I / BLS"),
    Column1 = "",
    check.names = FALSE
  )
}

test_calls <- function() {
  data.frame(dispatch_code = c("01C01", "01C01", "09E01", "01D01", "01C01", "01A01"))
}

test_that("prep_call_mapping renames, blanks to NA, and refuses repeated codes", {
  m <- prep_call_mapping(call_mapping_raw())

  expect_named(m, c("cc_number", "chief_complaint", "dispatch_code", "descriptor", "cad_code", "tier"))
  expect_equal(m$dispatch_code[1], "09E01")
  expect_true(is.na(m$cad_code[m$dispatch_code == "09E00"]))
  expect_equal(m$cc_number, c(9, 1, 1, 9, 1))
  expect_equal(m$descriptor[2], "Not alert") # non-breaking spaces made plain, then trimmed

  dup <- call_mapping_raw()
  dup$`Determinant Code`[2] <- "09E01"
  expect_error(prep_call_mapping(dup), "more than once in the mapping: 09E01")
  expect_error(prep_call_mapping(dup[-4]), "missing column")
})

test_that("fix_dispatch_text repairs lost symbols and nothing else", {
  expect_equal(
    fix_dispatch_text(c("pain ? 50", "Not alert", "12–50", "LifeVest®", NA)),
    c("pain ≥/≤ 50", "Not alert", "12–50", "LifeVest®", NA)
  )
})

test_that("counts list every code of the tier, zeros included, in chief complaint order", {
  m <- prep_call_mapping(call_mapping_raw())
  expect_message(
    out <- dispatch_code_counts(test_calls(), m, tier = "PARAMEDIC", years = 2),
    "1 counted call\\(s\\) have a code outside the PARAMEDIC codes and are left out: 01A01 \\(1\\)"
  )

  expect_equal(out$dispatch_code, c("01C01", "01D01", "09E00", "09E01"))
  expect_equal(out$n_calls, c(3L, 1L, 0L, 1L))
  expect_equal(out$per_year, c(1.5, 0.5, 0, 0.5))
  expect_equal(out$pct, c(3, 1, 0, 1) / 6) # of every counted call
  expect_equal(out$descriptor[1], "SUSPECTED aortic aneurysm ≥/≤ 50")
  expect_equal(out$cad_code[out$dispatch_code == "09E01"], "ALS1F")
  expect_error(dispatch_code_counts(test_calls(), m, tier = "NOPE", years = 2), "No codes")
})

test_that("the table has a subtotal row per chief complaint, zeros in gray", {
  m <- prep_call_mapping(call_mapping_raw())
  out <- suppressMessages(dispatch_code_counts(test_calls(), m, tier = "PARAMEDIC", years = 2))
  ft <- dispatch_code_table(out, years = 2)
  body <- ft$body$dataset

  expect_equal(
    body$code,
    c("Abdominal Pain / Problems", "01C01", "01D01", "Cardiac or Respiratory Arrest / Death", "09E00", "09E01")
  )
  expect_equal(body$calls, c("4", "3", "1", "1", "0", "1"))
  expect_equal(body$per_year[1], "2.0")
  expect_equal(body$pct[4], "16.7%")
  expect_equal(body$pct[5], "0.0%")
  tiny <- out
  tiny$pct[tiny$dispatch_code == "01D01"] <- 0.0004
  expect_equal(dispatch_code_table(tiny)$body$dataset$pct[3], "<0.1%")
  expect_equal(body$cad_code, c("", "ALS", "ALS", "", "", "ALS1F"))
  expect_equal(body$descriptor[3], "Not alert")
  expect_named(body, c("code", "descriptor", "cad_code", "calls", "per_year", "pct"))

  colors <- ft$body$styles$text$color$data
  expect_true(all(colors[5, ] == plot_ink[["muted"]])) # 09E00, no calls
  expect_false(any(colors[2, ] == plot_ink[["muted"]]))
})
