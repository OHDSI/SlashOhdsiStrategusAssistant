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

test_that("Atlas handoff asks for a new name after a duplicate-name conflict", {
  old <- Sys.getenv(c("STUDY_AGENT_WEBAPI_URL", "STUDY_AGENT_WEBAPI_API_KEY", "STUDY_AGENT_ATLAS_URL"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else Sys.setenv(structure(old[[name]], names = name))
    }
  }, add = TRUE)
  Sys.setenv(
    STUDY_AGENT_WEBAPI_URL = "https://webapi.example.org",
    STUDY_AGENT_WEBAPI_API_KEY = "test-key",
    STUDY_AGENT_ATLAS_URL = "https://atlas.example.org"
  )
  artifact_dir <- file.path(tempdir(), paste0("atlas-handoff-", as.integer(Sys.time())))
  dir.create(artifact_dir, recursive = TRUE)
  on.exit(unlink(artifact_dir, recursive = TRUE, force = TRUE), add = TRUE)
  answers <- c("duplicate", "ATLAS", "replacement", "ATLAS")
  request_count <- 0L
  local_mocked_bindings(
    .studyAgentSlashAtlasWebApiRequest = function(config, method, path, body = NULL) {
      request_count <<- request_count + 1L
      if (identical(path, "/conceptset") && request_count == 1L) {
        stop("Atlas/WebAPI request failed (HTTP 409): concept-set name already exists")
      }
      if (identical(path, "/conceptset")) {
        expect_equal(body$name, "replacement")
        return(list(id = 77L))
      }
      expect_match(path, "external-review\\?conceptSetId=77")
      list(session_id = "session-77")
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  handoff <- .studyAgentSlashAtlasCreateReviewHandoff(
    role_label = "target",
    narrative = "new users of esketamine",
    scope = list(index_event = "esketamine"),
    review = list(review_id = "review-77", candidate_count = 2L, concept_provenance = list(search_runs = list())),
    artifact_dir = artifact_dir,
    write_json = function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE),
    prompt = function(message) {
      answer <- answers[[1]]
      answers <<- answers[-1]
      answer
    }
  )
  expect_equal(handoff$concept_set_id, 77L)
  expect_equal(handoff$external_review_session_id, "session-77")
  expect_true(file.exists(file.path(artifact_dir, "atlas-review-handoff.json")))
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

test_that("Atlas snapshot uses its reviewed scope domain and ignores a stale handoff", {
  artifact_dir <- file.path(tempdir(), paste0("atlas-snapshot-", as.integer(Sys.time())))
  dir.create(artifact_dir, recursive = TRUE)
  on.exit(unlink(artifact_dir, recursive = TRUE, force = TRUE), add = TRUE)
  expression_path <- file.path(artifact_dir, "expression.json")
  jsonlite::write_json(
    list(items = list(list(
      concept = list(CONCEPT_ID = 123L),
      isExcluded = FALSE,
      includeDescendants = TRUE,
      includeMapped = FALSE
    ))),
    expression_path,
    auto_unbox = TRUE
  )
  sets <- .studyAgentSlashPmcExternalSets(expression_path, "atomoxetine", fallback_domain = "Drug")
  expect_equal(sets[[1]]$domain, "Drug")
  expect_equal(sets[[1]]$items[[1]]$domain, "Drug")

  jsonlite::write_json(
    list(review_id = "earlier-review", external_review_session_id = "session-earlier"),
    file.path(artifact_dir, "atlas-review-handoff.json"),
    auto_unbox = TRUE
  )
  ignored <- .studyAgentSlashAtlasAcceptedReview(
    artifact_dir,
    function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE),
    expected_review_id = "new-review"
  )
  expect_equal(ignored$status, "different_review")
})

test_that("zero-candidate review retains the configured Atlas handoff", {
  expect_match(
    .studyAgentSlashPmcReviewActionPrompt(TRUE, TRUE),
    "atlas=review in Atlas",
    fixed = TRUE
  )
  expect_false(grepl("csv=", .studyAgentSlashPmcReviewActionPrompt(TRUE, TRUE), fixed = TRUE))
  expect_false(grepl("atlas=", .studyAgentSlashPmcReviewActionPrompt(TRUE, FALSE), fixed = TRUE))
  expect_match(
    .studyAgentSlashPmcReviewActionPrompt(FALSE, TRUE, "full=request complete available set"),
    "csv=download/validate, later=download and resume later, full=request complete available set",
    fixed = TRUE
  )
})
