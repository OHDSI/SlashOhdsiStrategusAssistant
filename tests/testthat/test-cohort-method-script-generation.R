test_that("Cohort Methods initializes its generated-script header before use", {
    source_file <- testthat::test_path("..", "..", "R", "strategus_cohort_methods_shell.R")
    if (!file.exists(source_file)) {
        skip("Source-tree static check is not available from an installed package.")
    }
    source_lines <- readLines(source_file, warn = FALSE)
    header_definition <- grep("^  script_header <- c\\(", source_lines)
    first_header_use <- grep("^    script_header,$", source_lines)

    expect_length(header_definition, 1L)
    expect_true(length(first_header_use) > 0L)
    expect_lt(header_definition[[1]], first_header_use[[1]])
})

test_that("CohortMethod execution scripts expose propensity-score progress and safe-stop guidance", {
    source_file <- testthat::test_path("..", "..", "R", "strategus_cohort_methods_shell.R")
    if (!file.exists(source_file)) {
        skip("Source-tree static check is not available from an installed package.")
    }
    source_text <- paste(readLines(source_file, warn = FALSE), collapse = "\n")

    expect_match(source_text, "noiseLevel = 'quiet'", fixed = TRUE)
    expect_match(source_text, "strategus_execution_progress.json", fixed = TRUE)
    expect_match(source_text, "Tidying covariates", fixed = TRUE)
    expect_match(source_text, "propensity_score_fitting", fixed = TRUE)
    expect_match(source_text, "RStudio Escape or terminal Ctrl+C", fixed = TRUE)
    expect_match(source_text, "interrupt = function(e)", fixed = TRUE)
})
