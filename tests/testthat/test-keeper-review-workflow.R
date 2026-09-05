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

