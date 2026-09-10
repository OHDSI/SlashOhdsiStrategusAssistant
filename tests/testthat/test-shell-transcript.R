testthat::test_that("scripted dialogue input preserves back navigation", {
  transcript <- new_shell_transcript(c("/back", "answer"))
  session <- new_workflow_dialogue_session(
    interactive = TRUE,
    input_provider = transcript$readline,
    study_intent_getter = function() "test study",
    build_stage_context = function(studyIntent, dialogue_state) list(),
    call_dialogue = function(stage_context, message) list(status = "ok")
  )
  back <- session$readline("Choose: ", allow_back = TRUE)
  testthat::expect_true(inherits(back, "workflow_navigation_signal"))
  testthat::expect_identical(back$action, "back")
  testthat::expect_identical(session$readline("Next: "), "answer")
  testthat::expect_true(transcript$expect_complete())
})


testthat::test_that("slash guidance and deferred back re-prompt an atomic wizard field", {
  transcript <- new_shell_transcript(c("/ohdsi why does this setting matter?", "/back", "42"))
  dialogue_calls <- list()
  session <- new_workflow_dialogue_session(
    interactive = TRUE,
    input_provider = transcript$readline,
    study_intent_getter = function() "test study",
    build_stage_context = function(studyIntent, dialogue_state) list(step = "atomic_wizard"),
    call_dialogue = function(stage_context, message) {
      dialogue_calls[[length(dialogue_calls) + 1L]] <<- list(context = stage_context, message = message)
      list(status = "ok", dialogue = list(answer = "Use a clinically justified value."))
    }
  )
  value <- session$readline(
    "Setting: ", allow_back = TRUE,
    deferred_back_message = "Finish this section before using /back at its next stage boundary."
  )
  testthat::expect_identical(value, "42")
  testthat::expect_length(dialogue_calls, 1L)
  testthat::expect_identical(dialogue_calls[[1]]$message, "why does this setting matter?")
  testthat::expect_length(transcript$prompts(), 3L)
  testthat::expect_true(transcript$expect_complete())
})
testthat::test_that("fixed-width renderer removes Markdown table delimiters", {
  markdown <- paste(
    "### Concept set",
    "|Concept ID|Concept Name|Vocabulary|",
    "|:---|:---|:---|",
    "|1|A long concept name intended to be clipped in a console column|SNOMED|",
    sep = "\n"
  )
  rendered <- .studyAgentSlashCirceDefinitionConsole(markdown, max_column_width = 24L)
  testthat::expect_false(grepl("|", rendered, fixed = TRUE))
  testthat::expect_match(rendered, "Concept ID", fixed = TRUE)
  testthat::expect_match(rendered, "...", fixed = TRUE)
})

testthat::test_that("empty CohortMethod recommendations return to the source menu", {
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Statins", "Heart failure", "", "", "", "ai"
  ))
  fixture <- function(flow_name, body, url) {
    if (identical(flow_name, "phenotype_recommendation")) {
      return(list(
        recommendations = list(phenotype_recommendations = list()),
        fallback_reason = "no_direct_role_match"
      ))
    }
    stop(sprintf("Unexpected fixture flow: %s", flow_name))
  }

  testthat::expect_error(
    runStrategusCohortMethodsShell(
      outputDir = tempfile("shell-transcript-"),
      interactive = TRUE,
      showBanner = FALSE,
      checkRuntime = FALSE,
      aiSupport = "enabled",
      inputProvider = transcript$readline,
      acpFlowCaller = fixture
    ),
    "Shell transcript exhausted at prompt: Source for target cohort"
  )
  testthat::expect_match(tail(transcript$prompts(), 1L), "Source for target cohort", fixed = TRUE)
})

testthat::test_that("empty incidence recommendations return to the source menu", {
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Heart failure", "", "ai"
  ))
  fixture <- function(flow_name, body, url) {
    if (identical(flow_name, "phenotype_recommendation")) {
      return(list(
        recommendations = list(phenotype_recommendations = list()),
        fallback_reason = "no_direct_role_match"
      ))
    }
    stop(sprintf("Unexpected fixture flow: %s", flow_name))
  }

  testthat::expect_error(
    runStrategusIncidenceShell(
      outputDir = tempfile("shell-transcript-"),
      interactive = TRUE,
      showBanner = FALSE,
      checkRuntime = FALSE,
      aiSupport = "enabled",
      inputProvider = transcript$readline,
      acpFlowCaller = fixture
    ),
    "Shell transcript exhausted at prompt: Source for target cohort"
  )
  testthat::expect_match(tail(transcript$prompts(), 1L), "Source for target cohort", fixed = TRUE)
})

testthat::test_that("source menu re-prompts invalid input and preserves back", {
  invalid_then_valid <- new_shell_transcript(c("ACP", "file"))
  selected <- .studyAgentSlashChooseSelectionSourceMode(
    role_label = "target",
    allow_index = TRUE,
    interactive = TRUE,
    readline_with_navigation = invalid_then_valid$readline,
    is_back_signal = function(value) inherits(value, "workflow_navigation_signal")
  )
  testthat::expect_identical(selected, "file")
  testthat::expect_length(invalid_then_valid$prompts(), 2L)
  testthat::expect_true(invalid_then_valid$expect_complete())

  back <- new_shell_transcript("/back")
  selected_back <- .studyAgentSlashChooseSelectionSourceMode(
    role_label = "outcome",
    allow_index = TRUE,
    interactive = TRUE,
    readline_with_navigation = back$readline,
    is_back_signal = function(value) identical(value, "/back")
  )
  testthat::expect_identical(selected_back, "/back")
  testthat::expect_true(back$expect_complete())
})

testthat::test_that("invalid concept-set JSON re-prompts and accepts back", {
  transcript <- new_shell_transcript(c("json", "missing.json", "/back"))
  artifact_dir <- tempfile("review-transcript-")
  dir.create(artifact_dir, recursive = TRUE)
  review <- list(
    status = "needs_concept_review",
    candidate_count = 0L,
    review_urls = list(),
    concept_provenance = list(search_runs = list())
  )
  result <- .studyAgentSlashPmcReviewHandoff(
    role_label = "target",
    narrative = "test cohort",
    scope = list(),
    review = review,
    client = NULL,
    artifact_dir = artifact_dir,
    imported_definition_dir = tempfile("imports-"),
    readline_with_navigation = transcript$readline,
    is_back_signal = function(value) identical(value, "/back"),
    write_json = function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE)
  )
  testthat::expect_identical(result$action, "retry")
  testthat::expect_length(transcript$prompts(), 3L)
  testthat::expect_match(transcript$prompts()[[2]], "concept-set JSON", fixed = TRUE)
  testthat::expect_true(transcript$expect_complete())
})

testthat::test_that("incidence role-statement back returns to target entry", {
  transcript <- new_shell_transcript(c("", "ACE inhibitor", "/back"))
  testthat::expect_error(
    runStrategusIncidenceShell(
      outputDir = tempfile("shell-transcript-"), interactive = TRUE,
      showBanner = FALSE, checkRuntime = FALSE, aiSupport = "disabled",
      inputProvider = transcript$readline
    ),
    "Shell transcript exhausted at prompt: Target cohort statement"
  )
  testthat::expect_match(tail(transcript$prompts(), 1L), "Target cohort statement", fixed = TRUE)
})

testthat::test_that("CohortMethod role-statement back returns to target entry", {
  transcript <- new_shell_transcript(c("", "ACE inhibitor", "/back"))
  testthat::expect_error(
    runStrategusCohortMethodsShell(
      outputDir = tempfile("shell-transcript-"), interactive = TRUE,
      showBanner = FALSE, checkRuntime = FALSE, aiSupport = "disabled",
      inputProvider = transcript$readline
    ),
    "Shell transcript exhausted at prompt: Target statement"
  )
  testthat::expect_match(tail(transcript$prompts(), 1L), "Target statement", fixed = TRUE)
})

testthat::test_that("CohortMethod keeps two independently acquired outcome cohorts", {
  fixture_dir <- tempfile("multi-outcome-fixtures-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 101L, "ACE inhibitor")
  comparator_path <- write_shell_circe_fixture(file.path(fixture_dir, "comparator.json"), 102L, "Statins")
  outcome_one_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome-one.json"), 103L, "Cystitis")
  outcome_two_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome-two.json"), 104L, "Renal failure")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Statins", "Cystitis", "y", "Renal failure", "n", "", "",
    "file", target_path, "n", "y", "", "file", comparator_path, "y", "", "file",
    paste(outcome_one_path, outcome_two_path, sep = ","), "y", "n"
  ))
  improvement_roles <- character(0)
  outcome_item_counts <- integer(0)
  improvement_fixture <- function(flow_name, body, url) {
    if (!identical(flow_name, "phenotype_improvements")) stop(sprintf("Unexpected fixture flow: %s", flow_name))
    role <- as.character(body$role %||% "")
    improvement_roles <<- c(improvement_roles, role)
    if (identical(role, "outcome")) {
      outcome_item_counts <<- c(outcome_item_counts, length(outcome_item_counts) == 0L)
      return(list(phenotype_improvements = if (length(outcome_item_counts) == 1L) list(list(summary = "Review first outcome")) else list()))
    }
    list(phenotype_improvements = list())
  }
  testthat::expect_error(
    runStrategusCohortMethodsShell(
      outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
      checkRuntime = FALSE, aiSupport = "enabled", inputProvider = transcript$readline,
      acpFlowCaller = improvement_fixture
    ),
    "Shell transcript exhausted at prompt: Press Enter to continue to study configuration"
  )
  testthat::expect_identical(improvement_roles, c("target", "comparator", "outcome", "outcome"))
  testthat::expect_identical(outcome_item_counts, c(1L, 0L))
  selected_outcomes <- list.files(file.path(output_dir, "selected-outcome-cohorts"), pattern = "\\.json$", full.names = TRUE)
  testthat::expect_length(selected_outcomes, 2L)
  selected_names <- vapply(selected_outcomes, function(path) jsonlite::read_json(path, simplifyVector = FALSE)$name, character(1))
  testthat::expect_setequal(selected_names, c("Cystitis", "Renal failure"))
  testthat::expect_length(transcript$prompts(), 23L)
})

testthat::test_that("CohortMethod free-text settings use the injected ACP recommendation", {
  fixture_dir <- tempfile("analytic-settings-fixtures-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 201L, "ACE inhibitor")
  comparator_path <- write_shell_circe_fixture(file.path(fixture_dir, "comparator.json"), 202L, "Statins")
  outcome_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome.json"), 203L, "Heart failure")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Statins", "Heart failure", "n", "", "", "file", target_path,
    "n", "n", "", "file", comparator_path, "n", "", "file", outcome_path, "n", "",
    "", "", "", "", "n", "n", "", ""
  ))
  calls <- character(0)
  fixture <- function(flow_name, body, url) {
    calls <<- c(calls, flow_name)
    testthat::expect_identical(flow_name, "cohort_methods_specifications_recommendation")
    list(
      status = "ok",
      recommendation = list(
        profile_name = "Fixture-recommended analytic settings",
        study_population = list(), time_at_risk = list(),
        propensity_score_adjustment = list(), outcome_model = list()
      )
    )
  }

  testthat::expect_error(
    runStrategusCohortMethodsShell(
      outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
      checkRuntime = FALSE, aiSupport = "enabled",
      analyticSettingsDescription = "New-user comparative study",
      inputProvider = transcript$readline, acpFlowCaller = fixture
    ),
    "Shell transcript exhausted at prompt: Press Enter to continue to Keeper review options"
  )
  recommendation_path <- file.path(output_dir, "outputs", "cm_analytic_settings_recommendation.json")
  testthat::expect_true(file.exists(recommendation_path))
  recommendation <- jsonlite::read_json(recommendation_path, simplifyVector = FALSE)
  testthat::expect_identical(recommendation$profile_name, "Fixture-recommended analytic settings")
  testthat::expect_identical(calls, "cohort_methods_specifications_recommendation")
  testthat::expect_length(transcript$prompts(), 29L)
})

testthat::test_that("incidence keeps two outcomes when only the first has improvements", {
  fixture_dir <- tempfile("incidence-multi-outcome-fixtures-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 301L, "ACE inhibitor")
  outcome_one_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome-one.json"), 302L, "Cough")
  outcome_two_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome-two.json"), 303L, "Renal failure")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Cough", "", "file", target_path, "n", "n", "file",
    paste(outcome_one_path, outcome_two_path, sep = ","), "y", "n"
  ))
  improvement_ids <- integer(0)
  outcome_item_counts <- integer(0)
  improvement_fixture <- function(flow_name, body, url) {
    if (!identical(flow_name, "phenotype_improvements")) stop(sprintf("Unexpected fixture flow: %s", flow_name))
    cohort_id <- as.integer(body$cohorts[[1]]$id)
    improvement_ids <<- c(improvement_ids, cohort_id)
    outcome_item_counts <<- c(outcome_item_counts, cohort_id == 302L)
    list(phenotype_improvements = if (cohort_id == 302L) list(list(summary = "Review first outcome")) else list())
  }
  testthat::expect_error(
    runStrategusIncidenceShell(
      outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
      checkRuntime = FALSE, aiSupport = "enabled", inputProvider = transcript$readline,
      acpFlowCaller = improvement_fixture
    ),
    "Shell transcript exhausted at prompt: Use these time-at-risk and strata settings"
  )
  testthat::expect_identical(improvement_ids, c(302L, 303L))
  testthat::expect_identical(outcome_item_counts, c(1L, 0L))
  selected_outcomes <- list.files(file.path(output_dir, "selected-outcome-cohorts"), pattern = "\\.json$", full.names = TRUE)
  testthat::expect_length(selected_outcomes, 2L)
  selected_names <- vapply(selected_outcomes, function(path) jsonlite::read_json(path, simplifyVector = FALSE)$name, character(1))
  testthat::expect_setequal(selected_names, c("Cough", "Renal failure"))
})

testthat::test_that("incidence time-at-risk wizard recovers invalid values and persists strata", {
  fixture_dir <- tempfile("incidence-tar-fixtures-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 501L, "ACE inhibitor")
  outcome_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome.json"), 502L, "Cough")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Cough", "", "file", target_path, "n", "n", "file", outcome_path,
    "n", "n", "bad", "/back", "1", "7", "Thirty days", "start", "0", "start", "30", "7", "n", "y", "y", "18,65"
  ))
  testthat::expect_error(
    runStrategusIncidenceShell(
      outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
      checkRuntime = FALSE, aiSupport = "enabled", inputProvider = transcript$readline
    ),
    "Shell transcript exhausted at prompt: Run Keeper review now"
  )
  settings <- jsonlite::read_json(file.path(output_dir, "analysis-settings", "time_at_risk_settings.json"), simplifyVector = FALSE)
  testthat::expect_identical(settings$time_at_risk_defs[[1]]$id, 7L)
  testthat::expect_identical(settings$time_at_risk_defs[[1]]$name, "Thirty days")
  testthat::expect_identical(settings$time_at_risk_defs[[1]]$endOffset, 30L)
  testthat::expect_identical(settings$analysis_tar_ids, 7L)
  testthat::expect_false(settings$strata_settings$byYear)
  testthat::expect_true(settings$strata_settings$byGender)
  testthat::expect_true(settings$strata_settings$byAge)
  testthat::expect_identical(settings$strata_settings$ageBreaks, list(18L, 65L))
  testthat::expect_equal(sum(grepl("Number of time-at-risk definitions", transcript$prompts(), fixed = TRUE)), 3L)
})
testthat::test_that("accepted incidence improvement patches only its selected outcome", {
  fixture_dir <- tempfile("incidence-improvement-mutation-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 601L, "ACE inhibitor")
  first_path <- write_shell_circe_fixture(file.path(fixture_dir, "first.json"), 602L, "Cough")
  second_path <- write_shell_circe_fixture(file.path(fixture_dir, "second.json"), 603L, "Renal failure")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Cough", "", "file", target_path, "n", "n", "file",
    paste(first_path, second_path, sep = ","), "y", "y", "y"
  ))
  fixture <- function(flow_name, body, url) {
    testthat::expect_identical(flow_name, "phenotype_improvements")
    cohort_id <- as.integer(body$cohorts[[1]]$id)
    list(phenotype_improvements = if (cohort_id == 602L) {
      list(list(summary = "Rename selected outcome", actions = list(list(type = "set", path = "/name", value = "Cough reviewed"))))
    } else list())
  }
  testthat::expect_error(
    runStrategusIncidenceShell(
      outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
      checkRuntime = FALSE, aiSupport = "enabled", inputProvider = transcript$readline,
      acpFlowCaller = fixture
    ),
    "Shell transcript exhausted at prompt: Run Keeper review now"
  )
  patched_first <- jsonlite::read_json(file.path(output_dir, "patched-outcome-cohorts", "602.json"), simplifyVector = FALSE)
  testthat::expect_identical(patched_first$name, "Cough reviewed")
  testthat::expect_false(file.exists(file.path(output_dir, "patched-outcome-cohorts", "603.json")))
  original_second <- jsonlite::read_json(file.path(output_dir, "selected-outcome-cohorts", "603.json"), simplifyVector = FALSE)
  testthat::expect_identical(original_second$name, "Renal failure")
})



testthat::test_that("CohortMethod step-by-step wizard recovers an invalid parameter", {
  values <- c("", "", "", "", "invalid", "7")
  position <- 0L
  prompts <- character(0)
  next_value <- function(prompt) {
    position <<- position + 1L
    prompts <<- c(prompts, prompt)
    if (position > length(values)) return(if (identical(prompt, "Analytic settings profile name")) "Stepwise fixture" else "")
    values[[position]]
  }
  io <- list(
    section_header = function(label) invisible(NULL),
    text = function(prompt, default = "", allow_blank = FALSE) next_value(prompt),
    yesno = function(prompt, default = TRUE) {
      value <- tolower(trimws(next_value(prompt)))
      if (!nzchar(value)) return(default)
      value %in% c("y", "yes", "true", "1")
    },
    choice = function(prompt, choices, default, labels = choices) {
      value <- trimws(next_value(prompt))
      if (!nzchar(value)) return(default)
      index <- suppressWarnings(as.integer(value))
      if (!is.na(index) && index >= 1L && index <= length(choices)) return(choices[[index]])
      if (value %in% choices) return(value)
      stop(sprintf("Invalid choice fixture value: %s", value))
    },
    integer = function(prompt, default, min_value = NULL, allow_negative = TRUE) {
      repeat {
        value <- trimws(next_value(prompt))
        parsed <- if (!nzchar(value)) as.integer(default) else suppressWarnings(as.integer(value))
        if (is.na(parsed)) next
        if (!allow_negative && parsed < 0L) next
        if (!is.null(min_value) && parsed < min_value) next
        return(parsed)
      }
    },
    numeric = function(prompt, default, min_value = NULL) as.numeric(default)
  )
  defaults <- .studyAgentDefaultCohortMethodAnalyticSettings()
  result <- .studyAgentCollectStepByStepAnalyticSettings(
    default_settings = defaults, seed_settings = defaults, interactive = TRUE, io = io
  )
  testthat::expect_identical(result$settings$create_study_population$riskWindowStart, 7L)
  testthat::expect_gte(position, length(values))
  testthat::expect_identical(result$settings$profile_name, "Stepwise fixture")
  testthat::expect_equal(sum(prompts == "Risk window start (days)"), 2L)
})

testthat::test_that("incidence no-AI workflow acquires local cohorts and generates scripts", {
  fixture_dir <- tempfile("incidence-no-ai-fixtures-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 701L, "ACE inhibitor")
  outcome_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome.json"), 702L, "Cough")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Cough", "", "file", target_path, "n", "file", outcome_path, "y", "n"
  ))
  no_acp_call <- function(...) stop("ACP must not be called when aiSupport is disabled.")

  result <- runStrategusIncidenceShell(
    outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
    checkRuntime = FALSE, aiSupport = "disabled", inputProvider = transcript$readline,
    acpFlowCaller = no_acp_call
  )

  testthat::expect_true(transcript$expect_complete())
  testthat::expect_true(file.exists(file.path(output_dir, "selected-target-cohorts", "701.json")))
  testthat::expect_true(file.exists(file.path(output_dir, "selected-outcome-cohorts", "702.json")))
  testthat::expect_true(file.exists(file.path(output_dir, "analysis-settings", "time_at_risk_settings.json")))
  testthat::expect_true(file.exists(file.path(output_dir, "scripts", "03_generate_cohorts.R")))
  testthat::expect_true(file.exists(file.path(output_dir, "scripts", "07_incidence_spec.R")))
  state <- jsonlite::read_json(file.path(output_dir, "outputs", "study_agent_state.json"), simplifyVector = FALSE)
  testthat::expect_identical(state$acp_capability_status, "disabled_by_user")
  testthat::expect_true(state$skip_phenotype_improvements)
})

testthat::test_that("CohortMethod no-AI workflow acquires local cohorts and generates scripts", {
  fixture_dir <- tempfile("cohort-method-no-ai-fixtures-")
  dir.create(fixture_dir, recursive = TRUE)
  target_path <- write_shell_circe_fixture(file.path(fixture_dir, "target.json"), 711L, "ACE inhibitor")
  comparator_path <- write_shell_circe_fixture(file.path(fixture_dir, "comparator.json"), 712L, "Statins")
  outcome_path <- write_shell_circe_fixture(file.path(fixture_dir, "outcome.json"), 713L, "Heart failure")
  output_dir <- file.path(fixture_dir, "workflow")
  transcript <- new_shell_transcript(c(
    "", "ACE inhibitor", "Statins", "Heart failure", "", "", "file", target_path,
    "n", "", "file", comparator_path, "", "file", outcome_path, rep("", 24L)
  ))
  no_acp_call <- function(...) stop("ACP must not be called when aiSupport is disabled.")

  result <- runStrategusCohortMethodsShell(
    outputDir = output_dir, interactive = TRUE, showBanner = FALSE,
    checkRuntime = FALSE, aiSupport = "disabled", inputProvider = transcript$readline,
    acpFlowCaller = no_acp_call
  )

  testthat::expect_true(transcript$expect_complete())
  testthat::expect_true(file.exists(file.path(output_dir, "selected-target-cohorts", "711.json")))
  testthat::expect_true(file.exists(file.path(output_dir, "selected-comparator-cohorts", "712.json")))
  testthat::expect_true(file.exists(file.path(output_dir, "selected-outcome-cohorts", "713.json")))
  state <- jsonlite::read_json(file.path(output_dir, "outputs", "study_agent_state.json"), simplifyVector = FALSE)
  testthat::expect_true(file.exists(state$manual_inputs_path))
  testthat::expect_true(file.exists(file.path(output_dir, "scripts", "03_generate_cohorts.R")))
  testthat::expect_true(file.exists(file.path(output_dir, "scripts", "07_cm_spec.R")))
  testthat::expect_identical(state$acp_capability_status, "disabled_by_user")
  testthat::expect_true(state$skip_phenotype_improvements)
  testthat::expect_identical(state$analytic_settings_mode, "step_by_step")
  testthat::expect_true(isTRUE(state$analytic_settings_confirmed))
})

testthat::test_that("AI-enabled incidence ACP failures are safe", {
  transcript <- new_shell_transcript(c("", "ACE inhibitor", "Cough", "", "ai"))
  result <- tryCatch(
    runStrategusIncidenceShell(
      outputDir = (output_dir <- tempfile("incidence-acp-failure-")),
      interactive = TRUE, showBanner = FALSE, checkRuntime = FALSE,
      aiSupport = "enabled", inputProvider = transcript$readline,
      acpFlowCaller = function(...) stop("embedding provider connection refused")
    ),
    error = identity
  )
  testthat::expect_s3_class(result, "study_agent_acp_failure")
  testthat::expect_match(conditionMessage(result), "contact the ACP service administrator", fixed = TRUE)
  testthat::expect_false(grepl("embedding provider", conditionMessage(result), fixed = TRUE))
})

testthat::test_that("AI-enabled CohortMethod ACP failures are safe", {
  transcript <- new_shell_transcript(c("compare ACE inhibitors with statins for heart failure", ""))
  result <- tryCatch(
    runStrategusCohortMethodsShell(
      outputDir = tempfile("cohort-method-acp-failure-"),
      studyIntent = "compare ACE inhibitors with statins for heart failure",
      interactive = TRUE, showBanner = FALSE, checkRuntime = FALSE,
      aiSupport = "enabled", inputProvider = transcript$readline,
      acpFlowCaller = function(...) stop("embedding provider connection refused")
    ),
    error = identity
  )
  testthat::expect_s3_class(result, "study_agent_acp_failure")
  testthat::expect_match(conditionMessage(result), "contact the ACP service administrator", fixed = TRUE)
  testthat::expect_false(grepl("embedding provider", conditionMessage(result), fixed = TRUE))
})
