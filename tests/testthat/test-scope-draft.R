test_that("supported ACP scope draft becomes editable checklist defaults", {
  artifact_root <- tempfile("scope-draft-")
  dir.create(artifact_root, recursive = TRUE)
  on.exit(unlink(artifact_root, recursive = TRUE, force = TRUE), add = TRUE)
  answers <- c("", "y", "y", "", "", "", "", "", "", "", "no", "")
  prompts <- character()
  calls <- 0L
  local_mocked_bindings(
    .studyAgentSlashAcpPhenotypeMakeComputable = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(list(scope_draft = list(
          status = "supported", pattern = "single_direct_entry",
          index_event = "Cirrhosis", domain = "Condition", entry_limit = "First",
          prior_observation_days = 365L, exit_strategy = "observation",
          rationale = "One direct condition event.", warnings = list()
        )))
      }
      list(status = "needs_clarification")
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  result <- .studyAgentSlashCreateComputableRoleSelectionFresh(
    role_label = "target",
    role_statement = "Earliest cirrhosis with 365 days prior observation.",
    client = slashOhdsiAcpClient::acp_client(check = FALSE),
    output_dir = artifact_root,
    imported_definition_dir = file.path(artifact_root, "imports"),
    readline_with_navigation = function(prompt) {
      prompts <<- c(prompts, prompt)
      answer <- answers[[1L]]
      answers <<- answers[-1L]
      answer
    },
    write_json = function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE),
    offer_scope_draft = TRUE
  )
  expect_identical(result$action, "retry")
  expect_equal(calls, 2L)
  expect_true(any(grepl("Index event clinical term \\[Cirrhosis\\]", prompts)))
  expect_true(any(grepl("Index event OMOP domain \\[Condition\\]", prompts)))
  expect_true(any(grepl("Entry-event limit \\[First\\]", prompts)))
  expect_true(file.exists(file.path(artifact_root, "phenotype-make-computable", "target", "scope-draft-response.json")))
})


test_that("unsupported ACP scope draft leaves the checklist unfilled", {
  artifact_root <- tempfile("scope-draft-none-")
  dir.create(artifact_root, recursive = TRUE)
  on.exit(unlink(artifact_root, recursive = TRUE, force = TRUE), add = TRUE)
  answers <- c("", "y", "", "cirrhosis", "Condition", "First", "", "", "", "no", "")
  prompts <- character()
  local_mocked_bindings(
    .studyAgentSlashAcpPhenotypeMakeComputable = function(...) {
      list(scope_draft = list(status = "needs_clarification", rationale = "Temporal logic needs review."))
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  result <- .studyAgentSlashCreateComputableRoleSelectionFresh(
    role_label = "target", role_statement = "Cirrhosis after drug exposure.",
    client = slashOhdsiAcpClient::acp_client(check = FALSE), output_dir = artifact_root,
    imported_definition_dir = file.path(artifact_root, "imports"),
    readline_with_navigation = function(prompt) {
      prompts <<- c(prompts, prompt)
      answer <- answers[[1L]]
      answers <<- answers[-1L]
      answer
    },
    write_json = function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE),
    offer_scope_draft = TRUE
  )
  expect_identical(result$action, "retry")
  expect_true(any(grepl("Index event clinical term:", prompts, fixed = TRUE)))
})


test_that("drug-plus-condition scope draft requires a human evidence window", {
  artifact_root <- tempfile("scope-draft-drug-condition-")
  dir.create(artifact_root, recursive = TRUE)
  on.exit(unlink(artifact_root, recursive = TRUE, force = TRUE), add = TRUE)
  answers <- c(
    "", "y", "y", "", "", "", "", "", "", "", "", "",
    "", "", "-180", "", ""
  )
  prompts <- character()
  calls <- 0L
  local_mocked_bindings(
    .studyAgentSlashAcpPhenotypeMakeComputable = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(list(scope_draft = list(
          status = "supported", pattern = "drug_with_supporting_condition",
          index_event = "esketamine", domain = "Drug", entry_limit = "First",
          prior_observation_days = 0L, exit_strategy = "observation",
          supporting_condition_term = "major depressive disorder",
          supporting_condition_domain = "Condition",
          rationale = "One treatment exposure with one indication condition.", warnings = list()
        )))
      }
      list(status = "needs_clarification")
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  result <- .studyAgentSlashCreateComputableRoleSelectionFresh(
    role_label = "target", role_statement = "Patients with major depressive disorder initiating esketamine treatment.",
    client = slashOhdsiAcpClient::acp_client(check = FALSE), output_dir = artifact_root,
    imported_definition_dir = file.path(artifact_root, "imports"),
    readline_with_navigation = function(prompt) {
      prompts <<- c(prompts, prompt)
      answer <- answers[[1L]]
      answers <<- answers[-1L]
      answer
    },
    write_json = function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE),
    offer_scope_draft = TRUE
  )
  expect_identical(result$action, "retry")
  expect_true(any(grepl("Require suggested supporting Condition occurrence \\[major depressive disorder\\]", prompts)))
  expect_equal(sum(grepl("Supporting-condition window start days relative to index", prompts)), 2L)
  scope <- jsonlite::read_json(file.path(artifact_root, "phenotype-make-computable", "target", "confirmed-scope.json"), simplifyVector = FALSE)
  expect_identical(scope$supporting_condition_occurrence$concept_set, "major depressive disorder")
  expect_identical(scope$supporting_condition_occurrence$start_days, -180L)
  expect_identical(scope$supporting_condition_occurrence$end_days, 0L)
})


test_that("condition-plus-Visit scope draft requires an explicit overlap mode", {
  artifact_root <- tempfile("scope-draft-condition-visit-")
  dir.create(artifact_root, recursive = TRUE)
  on.exit(unlink(artifact_root, recursive = TRUE, force = TRUE), add = TRUE)
  answers <- c("", "y", "y", "", "", "", "", "", "", "", "", "", "entry", "")
  prompts <- character()
  calls <- 0L
  local_mocked_bindings(
    .studyAgentSlashAcpPhenotypeMakeComputable = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(list(scope_draft = list(
          status = "supported", pattern = "condition_with_visit_restriction",
          index_event = "acute cystitis", domain = "Condition", entry_limit = "First",
          prior_observation_days = 0L, exit_strategy = "observation",
          visit_term = "emergency room or inpatient visit", visit_domain = "Visit",
          rationale = "One Condition event with one Visit restriction.", warnings = list()
        )))
      }
      list(status = "needs_clarification")
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  result <- .studyAgentSlashCreateComputableRoleSelectionFresh(
    role_label = "target", role_statement = "Acute cystitis with an emergency room or inpatient visit restriction.",
    client = slashOhdsiAcpClient::acp_client(check = FALSE), output_dir = artifact_root,
    imported_definition_dir = file.path(artifact_root, "imports"),
    readline_with_navigation = function(prompt) {
      prompts <<- c(prompts, prompt)
      answer <- answers[[1L]]
      answers <<- answers[-1L]
      answer
    },
    write_json = function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE),
    offer_scope_draft = TRUE
  )
  expect_identical(result$action, "retry")
  expect_true(any(grepl("Require suggested Visit restriction \\[emergency room or inpatient visit\\]", prompts)))
  expect_true(any(grepl("Visit overlap mode \\[entry or attrition\\]", prompts)))
  scope <- jsonlite::read_json(file.path(artifact_root, "phenotype-make-computable", "target", "confirmed-scope.json"), simplifyVector = FALSE)
  expect_identical(scope$criterion_domains[["emergency room or inpatient visit"]], "Visit")
  expect_true(scope$visit_overlap)
  expect_identical(scope$visit_overlap_mode, "entry")
})


test_that("direct acquisition can still offer an ACP scope draft", {
  shell_paths <- c(
    incidence = file.path("R", "strategus_incidence_shell.R"),
    cohort_methods = file.path("R", "strategus_cohort_methods_shell.R")
  )
  expected_counts <- c(incidence = 2L, cohort_methods = 3L)
  source_root <- if (file.exists(file.path("R", "strategus_incidence_shell.R"))) "." else file.path("..", "..")
  for (name in names(shell_paths)) {
    source <- paste(readLines(file.path(source_root, shell_paths[[name]]), warn = FALSE), collapse = "\n")
    expect_false(grepl("offer_scope_draft = isTRUE(ai_enabled) && !isTRUE(direct_acquisition_mode)", source, fixed = TRUE))
    expect_equal(
      length(gregexpr("offer_scope_draft = isTRUE(ai_enabled)", source, fixed = TRUE)[[1L]]),
      expected_counts[[name]]
    )
  }
})
