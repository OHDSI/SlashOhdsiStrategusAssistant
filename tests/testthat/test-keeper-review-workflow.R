test_that("profile input is bounded independently for each frozen Keeper lane", {
  bound <- getFromNamespace(".studyAgentSlashBoundKeeperProfileConceptSets", "slashOhdsiStrategusAssistant")
  items <- list(
    list(conceptId = 1L, conceptSetName = "doi"),
    list(conceptId = 2L, conceptSetName = "doi"),
    list(conceptId = 3L, conceptSetName = "doi"),
    list(conceptId = 4L, conceptSetName = "drugs"),
    list(conceptId = 5L, conceptSetName = "drugs"),
    list(conceptId = 6L)
  )

  actual <- bound(items, 2L)

  expect_equal(actual$approved_count, 6L)
  expect_equal(actual$selected_count, 5L)
  expect_equal(vapply(actual$concept_sets, function(x) x$conceptId, integer(1)), c(1L, 2L, 4L, 5L, 6L))
  expect_equal(actual$lane_counts$doi, list(approved_count = 3L, selected_count = 2L))
  expect_equal(actual$lane_counts$drugs, list(approved_count = 2L, selected_count = 2L))
  expect_equal(actual$lane_counts$unlabeled, list(approved_count = 1L, selected_count = 1L))
})

test_that("Keeper recovers direct-acquisition labels from durable study state", {
  infer <- getFromNamespace(".studyAgentSlashInferPhenotypeName", "slashOhdsiStrategusAssistant")

  expect_equal(
    infer(
      role = "outcome",
      cohort_id = 967361L,
      cohort_name = "Cohort 967361",
      intent_payload = list(),
      study_state = list(outcome_statement = "Gastrointestinal hemorrhage")
    ),
    "Gastrointestinal hemorrhage"
  )
})

test_that("Keeper refuses a generic cohort label when clinical context is unavailable", {
  infer <- getFromNamespace(".studyAgentSlashInferPhenotypeName", "slashOhdsiStrategusAssistant")

  expect_error(
    infer(
      role = "outcome",
      cohort_id = 967361L,
      cohort_name = "Cohort 967361",
      intent_payload = list(),
      study_state = list()
    ),
    "clinical phenotype label is required"
  )
})

test_that("Keeper saves a resumable terminal state when setup fails before ACP generation", {
  base_dir <- tempfile("keeper-terminal-state-")
  dir.create(base_dir, recursive = TRUE)

  result <- runKeeperConceptSetWorkflow(
    base_dir = base_dir,
    execution_settings_path = file.path(base_dir, "missing-execution-settings.json"),
    cohort_id_map_path = file.path(base_dir, "missing-cohort-id-map.json")
  )

  state_path <- file.path(base_dir, "outputs", "keeper_concept_set_state.json")
  expect_identical(result$status, "error")
  expect_true(file.exists(state_path))
  state <- jsonlite::read_json(state_path, simplifyVector = FALSE)
  expect_identical(state$status, "error")
  expect_match(state$message, "Execution settings file not found", fixed = TRUE)
  expect_true(length(state$resume$instructions) >= 2L)
  expect_match(state$resume$instructions[[3]], "04_keeper_concept_sets.R", fixed = TRUE)
})

test_that("generic-label reviews are identified and archived before replacement", {
  is_generic_review <- getFromNamespace(".studyAgentSlashReviewUsesGenericPhenotypeLabel", "slashOhdsiStrategusAssistant")
  archive <- getFromNamespace(".studyAgentSlashArchiveStaleKeeperReview", "slashOhdsiStrategusAssistant")
  review_path <- file.path(tempdir(), "outcome_967361_reviews.json")
  jsonlite::write_json(
    list(
      phenotype_name = "Cohort 967361",
      reviews = list(list(row_index = 1L, phenotype_name = "Cohort 967361", label = "unknown"))
    ),
    review_path,
    auto_unbox = TRUE
  )

  expect_true(is_generic_review(jsonlite::read_json(review_path, simplifyVector = FALSE)))
  archived_path <- archive(review_path)
  expect_true(file.exists(archived_path))
  expect_equal(
    jsonlite::read_json(archived_path, simplifyVector = FALSE)$phenotype_name,
    "Cohort 967361"
  )
})

