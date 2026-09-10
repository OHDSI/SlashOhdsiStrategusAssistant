#' Summarize Strategus shell test coverage
#'
#' Creates a concise Markdown report describing the conceptual coverage of the
#' package test harness and the validation that still requires a human-operated
#' environment.
#'
#' @param outputFile Optional Markdown output path.
#' @return A character vector of Markdown lines, invisibly. When `outputFile` is
#'   supplied, the normalized output path is attached as `output_file`.
#' @export
strategusTestingReport <- function(outputFile = NULL) {
  lines <- c(
    "# Strategus Shell Testing Coverage",
    "",
    "This report describes conceptual coverage. Run the package test suite for current pass/fail results.",
    "",
    "## Harness coverage",
    "",
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
    "## Suggested commands",
    "",
    "```r",
    "testthat::test_local('.')",
    "slashOhdsiStrategusAssistant::strategusTestingReport('testing-coverage.md')",
    "```"
  )
  if (!is.null(outputFile)) {
    outputFile <- normalizePath(outputFile, winslash = "/", mustWork = FALSE)
    parent <- dirname(outputFile)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, outputFile, useBytes = TRUE)
    attr(lines, "output_file") <- outputFile
  }
  invisible(lines)
}
#' Summarize Strategus shell test coverage
#'
#' Creates a concise Markdown report describing harness coverage and validation
#' that still requires a human-operated environment.
#'
#' @param outputFile Optional Markdown output path.
#' @return Markdown lines, invisibly; the written path is attached when supplied.
#' @export
strategusTestingReport <- function(outputFile = NULL) {
  lines <- c(
    "# Strategus Shell Testing Coverage", "",
    "This report describes conceptual coverage. Run the package test suite for current pass/fail results.", "",
    "## Harness coverage", "",
    "- Specification: cohort acquisition, phenotype recommendation, review-gated make-computable flows, improvements, multiple outcomes, method configuration, navigation, and `/ohdsi` input handling.",
    "- Execution state: status and artifact discovery, snapshots, restore, step resolution, skip/reset, exploration-command validation, and resume-menu command recovery.",
    "- Interrupted sessions: orphaned running steps, explicit retry/reset behavior, successful final-execution summaries, and SQLite/DuckDB diagnostics result-store detection.", "",
    "## Manual validation still required", "",
    "- Clinical appropriateness of recommendations, concept sets, mappings, temporal logic, and phenotype definitions.",
    "- ACP/MCP service integration, model behavior, air-gapped deployment configuration, Atlas/WebAPI import, and source-database acquisition.",
    "- Long-running database execution, generated cohort counts, diagnostics contents, and Strategus scientific results.",
    "- Real VM logout recovery, database transaction behavior, and the installed CohortDiagnostics storage format.", "",
    "## Suggested commands", "", "```r", "testthat::test_local('.')", "slashOhdsiStrategusAssistant::strategusTestingReport('testing-coverage.md')", "```"
  )
  if (!is.null(outputFile)) {
    outputFile <- normalizePath(outputFile, winslash = "/", mustWork = FALSE)
    parent <- dirname(outputFile)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, outputFile, useBytes = TRUE)
    attr(lines, "output_file") <- outputFile
  }
  invisible(lines)
}
