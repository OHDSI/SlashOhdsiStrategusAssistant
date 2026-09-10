test_that("testing report renders conceptual coverage to Markdown", {
  path <- tempfile(fileext = ".md")
  project_path <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  report <- strategusTestingReport(projectPath = project_path, outputFile = path, runTests = FALSE)
  expect_true(file.exists(path))
  expect_match(paste(report, collapse = "\\n"), "Harness coverage", fixed = TRUE)
  expect_match(paste(report, collapse = "\\n"), "Manual validation", fixed = TRUE)
  expect_identical(attr(report, "output_file"), normalizePath(path, winslash = "/", mustWork = TRUE))
  expect_identical(attr(report, "project_path"), project_path)
})

test_that("testing report requires a package source directory", {
  expect_error(
    strategusTestingReport(projectPath = tempfile(), runTests = FALSE),
    "DESCRIPTION file"
  )
})

test_that("testing report escapes Markdown without changing ordinary text", {
  expect_identical(.studyAgentSlashTestingReportEscape("ordinary text"), "ordinary text")
  expect_identical(.studyAgentSlashTestingReportEscape("left|right"), "left\\|right")
})
