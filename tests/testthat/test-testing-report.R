test_that("testing report describes coverage and writes Markdown", {
  path <- tempfile(fileext = ".md")
  report <- strategusTestingReport(path)
  expect_true(file.exists(path))
  expect_match(paste(report, collapse = "\n"), "Harness coverage", fixed = TRUE)
  expect_match(paste(report, collapse = "\n"), "Manual validation", fixed = TRUE)
  expect_identical(attr(report, "output_file"), normalizePath(path, winslash = "/", mustWork = TRUE))
})
