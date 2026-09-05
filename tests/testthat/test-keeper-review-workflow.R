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
