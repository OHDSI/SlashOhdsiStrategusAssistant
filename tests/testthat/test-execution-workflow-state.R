make_execution_fixture <- function(workflow_type = "strategus_incidence") {
  base_dir <- tempfile("execution-workflow-")
  dir.create(base_dir, recursive = TRUE)
  artifact_path <- file.path(base_dir, "outputs", "review.txt")
  dir.create(dirname(artifact_path), recursive = TRUE)
  writeLines("review evidence", artifact_path)
  plan <- list(
    list(step_id = "prepare", label = "Prepare cohorts", status = "not_started", optional = FALSE),
    list(step_id = "diagnostics", label = "Diagnostics", status = "not_started", optional = TRUE, depends_on = list("prepare"))
  )
  artifact <- .studyAgentSlashNewProjectArtifact("review", artifact_path, base_dir, type = "review", step_id = "prepare")
  state <- .studyAgentSlashNewProjectState(workflow_type, base_dir, execution_plan = plan, artifacts = list(review = artifact))
  state$resume$current_step_id <- "prepare"
  .studyAgentSlashWriteProjectState(state, base_dir)
  .studyAgentSlashWriteRuntimeState(.studyAgentSlashNewRuntimeState(state), base_dir)
  list(base_dir = base_dir, artifact_path = artifact_path)
}

test_that("execution state exposes status, artifacts, and safe snapshot restoration", {
  fixture <- make_execution_fixture()
  expect_match(.studyAgentSlashSummarizeWorkflowStatus(fixture$base_dir)[[1]], "Prepare cohorts \\[not_started\\]")
  registry <- .studyAgentSlashBuildArtifactRegistry(fixture$base_dir)
  expect_true(any(vapply(registry, function(item) identical(item$id, "review"), logical(1))))
  snapshot <- .studyAgentSlashBackupWorkflowState(fixture$base_dir, label = "before-change")
  changed_state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  changed_state$execution_plan[[1]]$status <- "completed"
  .studyAgentSlashWriteProjectState(changed_state, fixture$base_dir)
  .studyAgentSlashRestoreWorkflowState(fixture$base_dir, snapshot$snapshot_id, restore_artifacts = FALSE)
  expect_match(.studyAgentSlashSummarizeWorkflowStatus(fixture$base_dir)[[1]], "Prepare cohorts \\[not_started\\]")
})

test_that("execution exploration rejects unavailable commands without running scripts", {
  fixture <- make_execution_fixture("strategus_cohort_methods")
  expect_error(
    .studyAgentSlashRunExplorationCommand(fixture$base_dir, "not-a-command"),
    "Unknown exploration command"
  )
})

test_that("execution lifecycle resolves, skips, and resets deterministic plan state", {
  fixture <- make_execution_fixture()
  expect_identical(.studyAgentSlashResolveWorkflowStepId(fixture$base_dir, "1"), "prepare")
  expect_identical(.studyAgentSlashResolveWorkflowStepId(fixture$base_dir, "diagnostics"), "diagnostics")
  expect_null(.studyAgentSlashResolveWorkflowStepId(fixture$base_dir, "99"))
  expect_error(.studyAgentSlashSkipWorkflowStep(fixture$base_dir, "prepare"), "cannot be skipped")

  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  state$execution_plan[[1]]$status <- "completed"
  .studyAgentSlashWriteProjectState(state, fixture$base_dir)
  skipped <- .studyAgentSlashSkipWorkflowStep(fixture$base_dir, "diagnostics")
  expect_identical(skipped$status, "skipped")
  expect_true(.studyAgentSlashWorkflowIsComplete(fixture$base_dir))

  reset <- .studyAgentSlashResetWorkflowStepState(fixture$base_dir, "prepare", backup = FALSE, delete_outputs = FALSE)
  expect_equal(unlist(reset$affected_steps), c("prepare", "diagnostics"))
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  expect_identical(state$execution_plan[[1]]$status, "not_started")
  expect_identical(state$execution_plan[[2]]$status, "not_started")
})
test_that("both shell resume menus recover from read-only execution commands", {
  shell_cases <- list(
    incidence = runStrategusIncidenceShell,
    cohort_method = runStrategusCohortMethodsShell
  )
  for (name in names(shell_cases)) {
    fixture <- make_execution_fixture(if (identical(name, "incidence")) "strategus_incidence" else "strategus_cohort_methods")
    transcript <- new_shell_transcript(c("y", "y", "h", "s", "art", "x", "x not-a-command", "q", "y"))
    output <- capture.output(
      shell_cases[[name]](
        outputDir = fixture$base_dir,
        interactive = TRUE,
        resume = TRUE,
        showBanner = FALSE,
        checkRuntime = FALSE,
        aiSupport = "disabled",
        inputProvider = transcript$readline,
        executionTableDisplay = "console"
      )
    )
    transcript$expect_complete()
    rendered <- paste(output, collapse = "\n")
    expect_match(rendered, "Execution commands", fixed = TRUE)
    expect_match(rendered, "Execution status", fixed = TRUE)
    expect_match(rendered, "Artifact inventory", fixed = TRUE)
    expect_match(rendered, "Unknown exploration command", fixed = TRUE)
  }
})
test_that("orphaned running steps become interrupted and block automatic advancement", {
  fixture <- make_execution_fixture()
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  state$execution_plan[[1]]$status <- "running"
  .studyAgentSlashWriteProjectState(state, fixture$base_dir)
  .studyAgentSlashWriteStepState(fixture$base_dir, "prepare", "running")
  interrupted <- .studyAgentSlashRecoverInterruptedWorkflowState(fixture$base_dir)
  expect_identical(interrupted, "prepare")
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  expect_identical(state$execution_plan[[1]]$status, "interrupted")
  expect_match(state$execution_plan[[1]]$error, "previous R session ended", fixed = TRUE)
  next_result <- .studyAgentSlashRunNextWorkflowPlanStep(fixture$base_dir)
  expect_identical(next_result$status, "interrupted")
  expect_match(next_result$message, "Inspect artifacts", fixed = TRUE)
})
test_that("successful final Strategus summary reconciles an interrupted final step", {
  fixture <- make_execution_fixture("strategus_cohort_methods")
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  state$execution_plan <- list(list(step_id = "cm_spec", label = "Final CohortMethod execution", status = "running", optional = FALSE))
  .studyAgentSlashWriteProjectState(state, fixture$base_dir)
  dir.create(file.path(fixture$base_dir, "analysis-settings"), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(overall_status = "success"), file.path(fixture$base_dir, "analysis-settings", "strategus_execute_summary.json"), auto_unbox = TRUE)
  recovered <- .studyAgentSlashRecoverInterruptedWorkflowState(fixture$base_dir)
  expect_length(recovered, 0L)
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  expect_identical(state$execution_plan[[1]]$status, "completed")
})
test_that("diagnostics recovery recognizes SQLite and DuckDB stores and rejects ambiguity", {
  fixture <- make_execution_fixture()
  diagnostics_dir <- file.path(fixture$base_dir, "cm-diagnostics")
  dir.create(diagnostics_dir, recursive = TRUE)
  sqlite_path <- file.path(diagnostics_dir, "MergedCohortDiagnosticsData.sqlite")
  writeBin(c(charToRaw("SQLite format 3"), as.raw(0), rep(as.raw(0), 64L)), sqlite_path)
  assessment <- .studyAgentSlashAssessInterruptedStepArtifacts(fixture$base_dir, "diagnostics")
  expect_identical(assessment$status, "completed")
  expect_identical(assessment$store_type, "sqlite")

  unlink(sqlite_path)
  duckdb_path <- file.path(diagnostics_dir, "MergedCohortDiagnosticsData.duckdb")
  writeBin(c(rep(as.raw(0), 32L), charToRaw("DUCKDB"), rep(as.raw(0), 64L)), duckdb_path)
  assessment <- .studyAgentSlashAssessInterruptedStepArtifacts(fixture$base_dir, "diagnostics")
  expect_identical(assessment$status, "completed")
  expect_identical(assessment$store_type, "duckdb")

  writeBin(c(charToRaw("SQLite format 3"), as.raw(0), rep(as.raw(0), 64L)), sqlite_path)
  assessment <- .studyAgentSlashAssessInterruptedStepArtifacts(fixture$base_dir, "diagnostics")
  expect_identical(assessment$status, "interrupted")
  expect_identical(assessment$reason, "ambiguous_diagnostics_result_stores")
})
test_that("invalid diagnostics stores and non-probed steps remain interrupted", {
  fixture <- make_execution_fixture()
  diagnostics_dir <- file.path(fixture$base_dir, "cm-diagnostics")
  dir.create(diagnostics_dir, recursive = TRUE)
  writeBin(as.raw(rep(1L, 64L)), file.path(diagnostics_dir, "partial.duckdb"))
  assessment <- .studyAgentSlashAssessInterruptedStepArtifacts(fixture$base_dir, "diagnostics")
  expect_identical(assessment$status, "interrupted")
  expect_identical(assessment$reason, "diagnostics_result_store_missing_or_invalid")
  expect_identical(.studyAgentSlashAssessInterruptedStepArtifacts(fixture$base_dir, "generate_cohorts")$reason, "no_safe_completion_probe")
  expect_identical(.studyAgentSlashAssessInterruptedStepArtifacts(fixture$base_dir, "keeper_concept_sets")$reason, "no_safe_completion_probe")
})

test_that("valid diagnostics reconcile while Keeper and cohort generation require explicit recovery", {
  fixture <- make_execution_fixture()
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  state$execution_plan <- list(
    list(step_id = "diagnostics", label = "Diagnostics", status = "running", optional = TRUE),
    list(step_id = "keeper_concept_sets", label = "Keeper", status = "running", optional = TRUE),
    list(step_id = "generate_cohorts", label = "Cohort generation", status = "running", optional = FALSE)
  )
  .studyAgentSlashWriteProjectState(state, fixture$base_dir)
  diagnostics_dir <- file.path(fixture$base_dir, "cm-diagnostics")
  dir.create(diagnostics_dir, recursive = TRUE)
  writeBin(c(rep(as.raw(0), 32L), charToRaw("DUCKDB"), rep(as.raw(0), 64L)), file.path(diagnostics_dir, "results.duckdb"))
  interrupted <- .studyAgentSlashRecoverInterruptedWorkflowState(fixture$base_dir)
  expect_setequal(interrupted, c("keeper_concept_sets", "generate_cohorts"))
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  expect_identical(state$execution_plan[[1]]$status, "completed")
  expect_identical(state$execution_plan[[2]]$status, "interrupted")
  expect_identical(state$execution_plan[[3]]$status, "interrupted")
})

test_that("an explicit run can retry an interrupted step", {
  fixture <- make_execution_fixture()
  script_path <- file.path(fixture$base_dir, "scripts", "retry.R")
  dir.create(dirname(script_path), recursive = TRUE)
  writeLines("invisible(NULL)", script_path)
  state <- .studyAgentSlashReadProjectState(fixture$base_dir)
  state$execution_plan <- list(list(step_id = "generate_cohorts", label = "Cohort generation", status = "interrupted", script_path = "scripts/retry.R", optional = FALSE))
  .studyAgentSlashWriteProjectState(state, fixture$base_dir)
  result <- .studyAgentSlashRunWorkflowPlanStep(fixture$base_dir, "generate_cohorts")
  expect_identical(result$status, "completed")
})
