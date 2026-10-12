test_that("Atlas review handoff configuration and URL are explicit", {
  old <- Sys.getenv(c("STUDY_AGENT_WEBAPI_URL", "STUDY_AGENT_WEBAPI_API_KEY", "STUDY_AGENT_ATLAS_URL"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else Sys.setenv(structure(old[[name]], names = name))
    }
  }, add = TRUE)
  Sys.setenv(
    STUDY_AGENT_WEBAPI_URL = "https://webapi.example.org/WebAPI/",
    STUDY_AGENT_WEBAPI_API_KEY = "test-key",
    STUDY_AGENT_ATLAS_URL = "https://atlas.example.org/Atlas3/"
  )
  config <- .studyAgentSlashAtlasReviewConfig()
  expect_true(.studyAgentSlashAtlasReviewConfigured(config))
  expect_equal(
    .studyAgentSlashAtlasReviewUrl(config, "abc/123", 99L),
    "https://atlas.example.org/Atlas3/#/concepts?tab=sets&studyAgentExternalReviewSession=abc%2F123&conceptSetId=99"
  )
  Sys.unsetenv("STUDY_AGENT_WEBAPI_API_KEY")
  expect_false(.studyAgentSlashAtlasReviewConfigured(.studyAgentSlashAtlasReviewConfig()))
})

test_that("Atlas accepted review persists only the returned immutable snapshot", {
  artifact_dir <- file.path(tempdir(), paste0("atlas-review-", as.integer(Sys.time())))
  dir.create(artifact_dir, recursive = TRUE)
  on.exit(unlink(artifact_dir, recursive = TRUE, force = TRUE), add = TRUE)
  jsonlite::write_json(list(
    external_review_session_id = "session-1",
    concept_set_id = 42L,
    review_url = "https://atlas.example.org/Atlas3/#/concepts"
  ), file.path(artifact_dir, "atlas-review-handoff.json"), auto_unbox = TRUE)
  old <- Sys.getenv(c("STUDY_AGENT_WEBAPI_URL", "STUDY_AGENT_WEBAPI_API_KEY", "STUDY_AGENT_ATLAS_URL"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else Sys.setenv(structure(old[[name]], names = name))
    }
  }, add = TRUE)
  Sys.setenv(STUDY_AGENT_WEBAPI_URL = "https://webapi.example.org", STUDY_AGENT_WEBAPI_API_KEY = "test-key", STUDY_AGENT_ATLAS_URL = "https://atlas.example.org")
  local_mocked_bindings(
    .studyAgentSlashAtlasWebApiRequest = function(config, method, path, body = NULL) {
      expect_equal(method, "GET")
      expect_match(path, "external-review/session-1")
      list(state = "external_review_accepted", review_revision = 3L, expression_checksum = "abc123", expression = list(items = list(list(concept = list(CONCEPT_ID = 123L), isExcluded = FALSE, includeDescendants = TRUE, includeMapped = FALSE))) )
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  accepted <- .studyAgentSlashAtlasAcceptedReview(artifact_dir, function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE))
  expect_equal(accepted$status, "accepted")
  expect_equal(accepted$handoff$expression_checksum, "abc123")
  snapshot <- jsonlite::read_json(accepted$expression_path, simplifyVector = FALSE)
  expect_equal(snapshot$items[[1]]$concept$CONCEPT_ID, 123L)
})
