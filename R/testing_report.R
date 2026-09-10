#' Generate a Strategus shell testing report
#'
#' Runs the package test harness from a source project directory and writes a
#' concise Markdown report. The report pairs current structured test outcomes
#' with stable conceptual coverage and manual-validation guidance.
#'
#' @param projectPath Source directory containing the package `DESCRIPTION`.
#' @param outputFile Optional Markdown output path.
#' @param runTests Whether to run `testthat::test_local(projectPath)`. Set to
#'   `FALSE` to render only the conceptual coverage sections.
#' @param verbose Whether to emit one concise result line per test after the
#'   test run.
#' @return A character vector of Markdown lines, invisibly. Attributes provide
#'   `output_file`, `project_path`, `test_results`, and `test_summary`.
#' @export
strategusTestingReport <- function(projectPath, outputFile = NULL,
                                   runTests = TRUE, verbose = interactive()) {
  if (missing(projectPath) || !is.character(projectPath) || length(projectPath) != 1L ||
      !nzchar(projectPath)) {
    stop("projectPath must be the source package directory containing DESCRIPTION.", call. = FALSE)
  }
  projectPath <- normalizePath(projectPath, winslash = "/", mustWork = FALSE)
  description_path <- file.path(projectPath, "DESCRIPTION")
  if (!file.exists(description_path)) {
    stop(
      sprintf("projectPath does not contain a package DESCRIPTION file: %s", projectPath),
      call. = FALSE
    )
  }

  test_results <- data.frame(
    file = character(), test = character(), status = character(),
    assertions = integer(), duration_seconds = numeric(), details = character(),
    stringsAsFactors = FALSE
  )
  run_error <- NULL
  if (isTRUE(runTests)) {
    if (!requireNamespace("testthat", quietly = TRUE)) {
      stop("Package 'testthat' is required to run the testing report.", call. = FALSE)
    }
    reporter <- testthat::ListReporter$new()
    tryCatch(
      suppressMessages(capture.output(testthat::test_local(projectPath, reporter = reporter))),
      error = function(error) run_error <<- conditionMessage(error)
    )
    test_results <- .studyAgentSlashTestingReportRows(reporter$get_results())
    if (isTRUE(verbose) && nrow(test_results)) {
      for (i in seq_len(nrow(test_results))) {
        message(sprintf(
          "%s — %s [%s; %d assertion(s)]",
          test_results$status[[i]], test_results$test[[i]],
          test_results$file[[i]], test_results$assertions[[i]]
        ))
      }
    }
  }
  test_summary <- .studyAgentSlashTestingReportSummary(test_results, run_error)
  lines <- .studyAgentSlashTestingReportLines(
    projectPath = projectPath,
    test_results = test_results,
    test_summary = test_summary,
    runTests = isTRUE(runTests),
    run_error = run_error
  )
  if (!is.null(outputFile)) {
    outputFile <- normalizePath(outputFile, winslash = "/", mustWork = FALSE)
    parent <- dirname(outputFile)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, outputFile, useBytes = TRUE)
    attr(lines, "output_file") <- normalizePath(outputFile, winslash = "/", mustWork = TRUE)
  }
  attr(lines, "project_path") <- projectPath
  attr(lines, "test_results") <- test_results
  attr(lines, "test_summary") <- test_summary
  invisible(lines)
}

.studyAgentSlashTestingReportRows <- function(results) {
  if (!length(results)) {
    return(data.frame(
      file = character(), test = character(), status = character(),
      assertions = integer(), duration_seconds = numeric(), details = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(results, function(result) {
    expectations <- result$results %||% list()
    classes <- vapply(expectations, function(expectation) class(expectation)[[1]], character(1))
    messages <- vapply(expectations, function(expectation) {
      message <- expectation$message %||% ""
      gsub("[\\r\\n]+", " ", message)
    }, character(1))
    failure <- grepl("failure|error", classes)
    warning <- grepl("warning", classes)
    skipped <- grepl("skip", classes)
    status <- if (any(failure)) "FAIL" else if (any(warning)) "WARN" else if (length(classes) && all(skipped)) "SKIP" else "PASS"
    data.frame(
      file = basename(result$file %||% "<unknown>"),
      test = result$test %||% "<unnamed test>",
      status = status,
      assertions = length(expectations),
      duration_seconds = as.numeric(result$real %||% 0),
      details = paste(messages[failure | warning], collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.studyAgentSlashTestingReportSummary <- function(test_results, run_error = NULL) {
  statuses <- test_results$status %||% character()
  list(
    passed = sum(statuses == "PASS"),
    warned = sum(statuses == "WARN"),
    skipped = sum(statuses == "SKIP"),
    failed = sum(statuses == "FAIL") + as.integer(!is.null(run_error)),
    tests = nrow(test_results),
    assertions = sum(test_results$assertions %||% 0L),
    duration_seconds = sum(test_results$duration_seconds %||% 0),
    run_error = run_error
  )
}

.studyAgentSlashTestingReportLines <- function(projectPath, test_results, test_summary,
                                                runTests, run_error = NULL) {
  lines <- c(
    "# Strategus Shell Testing Coverage",
    "",
    sprintf("Source project: `%s`", projectPath),
    ""
  )
  if (isTRUE(runTests)) {
    lines <- c(
      lines,
      "## Current harness results",
      "",
      sprintf(
        "Tests: %d; passed: %d; warnings: %d; skipped: %d; failed: %d; assertions: %d; duration: %.2f seconds.",
        test_summary$tests, test_summary$passed, test_summary$warned,
        test_summary$skipped, test_summary$failed, test_summary$assertions,
        test_summary$duration_seconds
      ),
      ""
    )
    if (!is.null(run_error)) {
      lines <- c(lines, sprintf("Harness error: %s", .studyAgentSlashTestingReportEscape(run_error)), "")
    }
    if (nrow(test_results)) {
      lines <- c(lines, "| Result | Test file | Test | Assertions | Seconds |", "|:--|:--|:--|--:|--:|")
      for (i in seq_len(nrow(test_results))) {
        lines <- c(lines, sprintf(
          "| %s | %s | %s | %d | %.3f |",
          test_results$status[[i]],
          .studyAgentSlashTestingReportEscape(test_results$file[[i]]),
          .studyAgentSlashTestingReportEscape(test_results$test[[i]]),
          test_results$assertions[[i]], test_results$duration_seconds[[i]]
        ))
      }
      lines <- c(lines, "")
      notable <- test_results[nzchar(test_results$details), , drop = FALSE]
      if (nrow(notable)) {
        lines <- c(lines, "### Warnings and failures", "")
        for (i in seq_len(nrow(notable))) {
          lines <- c(lines, sprintf("- %s: %s", notable$test[[i]], .studyAgentSlashTestingReportEscape(notable$details[[i]])))
        }
        lines <- c(lines, "")
      }
    } else if (is.null(run_error)) {
      lines <- c(lines, "No test results were returned by the local harness.", "")
    }
  } else {
    lines <- c(lines, "Current test execution was not requested (`runTests = FALSE`).", "")
  }
  c(
    lines,
    "## Harness coverage",
    "- Specification: cohort acquisition, phenotype recommendation, review-gated make-computable flows, phenotype improvements, multiple outcomes, method configuration, navigation, `/ohdsi` input handling, and end-to-end no-AI local JSON acquisition/configuration.",
    "- Specification: cohort acquisition, phenotype recommendation, review-gated make-computable flows, phenotype improvements, multiple outcomes, method configuration, navigation, and `/ohdsi` input handling.",
    "- Execution state: status and artifact discovery, snapshots, restore, step resolution, skip/reset, exploration-command validation, and resume-menu command recovery.",
    "- Interrupted sessions: orphaned running steps, explicit retry/reset behavior, successful final-execution summaries, and SQLite/DuckDB diagnostics result-store detection.",
    "",
    "## Manual validation still required",
    "",
    "- Clinical appropriateness of recommendations, concept sets, mappings, temporal logic, and phenotype definitions.",
    "- ACP/MCP service integration, model behavior, and air-gapped deployment configuration.",
    "- Atlas/WebAPI import and review, source-database cohort acquisition, and real OMOP vocabulary behavior.",
    "- Long-running database execution, generated cohort counts, diagnostics contents, and Strategus scientific results.",
    "- Interruption recovery against a real VM logout, database transaction behavior, and installed CohortDiagnostics storage format.",
    "",
    "## Suggested command",
    "",
    "```r",
    "slashOhdsiStrategusAssistant::strategusTestingReport(",
    "  projectPath = '/absolute/path/to/slashOhdsiStrategusAssistant',",
    "  outputFile = 'testing-coverage.md',",
    "  verbose = TRUE",
    ")",
    "```"
  )
}

.studyAgentSlashTestingReportEscape <- function(value) {
  value <- gsub("|", "\\|", as.character(value), fixed = TRUE)
  gsub("[\r\n]+", " ", value)
}
