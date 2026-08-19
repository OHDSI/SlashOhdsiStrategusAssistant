test_that("Cohort Methods initializes its generated-script header before use", {
  source_file <- testthat::test_path("..", "..", "R", "strategus_cohort_methods_shell.R")
  source_lines <- readLines(source_file, warn = FALSE)
  header_definition <- grep("^  script_header <- c\\(", source_lines)
  first_header_use <- grep("^    script_header,$", source_lines)

  expect_length(header_definition, 1L)
  expect_true(length(first_header_use) > 0L)
  expect_lt(header_definition[[1]], first_header_use[[1]])
})
