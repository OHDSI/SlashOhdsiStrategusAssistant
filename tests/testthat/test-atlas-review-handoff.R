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
    "`atlas`    Review in Atlas vocabulary search",
    fixed = TRUE
  )
  expect_false(grepl("csv=", .studyAgentSlashPmcReviewActionPrompt(TRUE, TRUE), fixed = TRUE))
  expect_false(grepl("atlas=", .studyAgentSlashPmcReviewActionPrompt(TRUE, FALSE), fixed = TRUE))
  expect_match(
    .studyAgentSlashPmcReviewActionPrompt(FALSE, TRUE, "full=request complete available set"),
    "`csv`      Download and validate the CSV review",
    fixed = TRUE
  )
})


test_that("multi-criterion Atlas review advances one named lane at a time", {
  old <- Sys.getenv(c("STUDY_AGENT_WEBAPI_URL", "STUDY_AGENT_WEBAPI_API_KEY", "STUDY_AGENT_ATLAS_URL"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else Sys.setenv(structure(old[[name]], names = name))
    }
  }, add = TRUE)
  Sys.setenv(STUDY_AGENT_WEBAPI_URL = "https://webapi.example.org", STUDY_AGENT_WEBAPI_API_KEY = "test-key", STUDY_AGENT_ATLAS_URL = "https://atlas.example.org")
  artifact_dir <- file.path(tempdir(), paste0("atlas-multi-", as.integer(Sys.time())))
  dir.create(artifact_dir, recursive = TRUE)
  on.exit(unlink(artifact_dir, recursive = TRUE, force = TRUE), add = TRUE)
  scope <- list(
    index_event = "esketamine",
    criterion_domains = list(esketamine = "Drug", `major depressive disorder` = "Condition"),
    criterion_vocabularies = list(esketamine = list("RxNorm")),
    supporting_condition_occurrence = list(concept_set = "major depressive disorder")
  )
  review <- list(review_id = "review-multi", concept_provenance = list(search_runs = list(
    list(concept_set_name = "esketamine", returned_count = 11L),
    list(concept_set_name = "major depressive disorder", returned_count = 20L)
  )))
  next_id <- 100L
  registrations <- list()
  local_mocked_bindings(
    .studyAgentSlashAtlasWebApiRequest = function(config, method, path, body = NULL) {
      if (identical(method, "POST") && identical(path, "/conceptset")) {
        next_id <<- next_id + 1L
        return(list(id = next_id))
      }
      if (identical(method, "POST")) {
        registrations[[length(registrations) + 1L]] <<- body
        return(list(session_id = paste0("session-", next_id)))
      }
      if (grepl("session-101", path, fixed = TRUE)) {
        return(list(
          state = "external_review_accepted", review_revision = 1L, expression_checksum = "drug",
          expression = list(items = list(list(
            concept = list(CONCEPT_ID = 111L, DOMAIN_ID = "Drug"),
            isExcluded = FALSE, includeDescendants = TRUE, includeMapped = FALSE
          )))
        ))
      }
      if (grepl("session-102", path, fixed = TRUE)) {
        return(list(
          state = "external_review_accepted", review_revision = 1L, expression_checksum = "condition",
          expression = list(items = list(list(
            concept = list(CONCEPT_ID = 222L, DOMAIN_ID = "Condition"),
            isExcluded = FALSE, includeDescendants = FALSE, includeMapped = FALSE
          )))
        ))
      }
      stop("unexpected request")
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  prompt_answers <- c("", "", "", "", "")
  prompt <- function(message) {
    answer <- prompt_answers[[1L]]
    prompt_answers <<- prompt_answers[-1L]
    answer
  }
  write_json <- function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE)
  first <- .studyAgentSlashAtlasAdvanceMultiReview("target", "MDD patients taking esketamine", scope, review, artifact_dir, write_json, prompt, initialize = TRUE)
  expect_equal(first$status, "awaiting_atlas_review")
  expect_equal(first$lane$key, "esketamine")
  expect_true(registrations[[1]]$require_item_domain_metadata)
  expect_equal(registrations[[1]]$review_lane$purpose, "Index event")
  expect_equal(registrations[[1]]$scope$criterion_domains$esketamine, "Drug")
  second <- .studyAgentSlashAtlasAdvanceMultiReview("target", "MDD patients taking esketamine", scope, review, artifact_dir, write_json, prompt, initialize = FALSE)
  expect_equal(second$status, "awaiting_atlas_review")
  expect_equal(second$lane$key, "major depressive disorder")
  expect_equal(registrations[[2]]$review_lane$purpose, "Supporting Condition")
  expect_equal(registrations[[2]]$scope$criterion_domains$`major depressive disorder`, "Condition")
  complete <- .studyAgentSlashAtlasAdvanceMultiReview("target", "MDD patients taking esketamine", scope, review, artifact_dir, write_json, prompt, initialize = FALSE)
  expect_equal(complete$status, "accepted")
  expect_equal(vapply(complete$concept_sets, `[[`, character(1), "name"), c("esketamine", "major depressive disorder"))
  expect_equal(vapply(complete$concept_sets, `[[`, character(1), "domain"), c("Drug", "Condition"))
})

test_that("validated R cohort creates a separate editable Atlas draft handoff", {
  old <- Sys.getenv(c("STUDY_AGENT_WEBAPI_URL", "STUDY_AGENT_WEBAPI_API_KEY", "STUDY_AGENT_ATLAS_URL"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else Sys.setenv(structure(old[[name]], names = name))
    }
  }, add = TRUE)
  Sys.setenv(STUDY_AGENT_WEBAPI_URL = "https://webapi.example.org", STUDY_AGENT_WEBAPI_API_KEY = "test-key", STUDY_AGENT_ATLAS_URL = "https://atlas.example.org")
  artifact_dir <- file.path(tempdir(), paste0("atlas-cohort-draft-", as.integer(Sys.time())))
  dir.create(artifact_dir, recursive = TRUE)
  on.exit(unlink(artifact_dir, recursive = TRUE, force = TRUE), add = TRUE)
  approval_path <- file.path(artifact_dir, "concept-set-approval.json")
  jsonlite::write_json(list(review_id = "review-validated"), approval_path, auto_unbox = TRUE)
  expression <- list(PrimaryCriteria = list(CriteriaList = list()), ConceptSets = list())
  local_mocked_bindings(
    .studyAgentSlashAtlasWebApiRequest = function(config, method, path, body = NULL) {
      expect_equal(method, "POST")
      expect_equal(path, "/study-agent/v1/cohort-definition-sessions/external-r-draft")
      expect_equal(body$narrative, "new users of esketamine")
      expect_equal(body$role_label, "target")
      expect_equal(body$review_id, "review-validated")
      expect_equal(body$technical_validation$status, "passed")
      expect_equal(body$expression, expression)
      list(session_id = "cohort-session-1", review_revision = 2L, expression_checksum = "checksum")
    },
    .package = "slashOhdsiStrategusAssistant"
  )
  handoff <- .studyAgentSlashAtlasCreateCohortDraft(
    "new users of esketamine", "target", expression, list(status = "passed"),
    artifact_dir, approval_path,
    function(value, target) jsonlite::write_json(value, target, auto_unbox = TRUE)
  )
  expect_match(handoff$atlas_url, "#/cohorts/new\\?studyAgentSession=cohort-session-1")
  expect_match(handoff$atlas_url, "studyAgentExternalR=true", fixed = TRUE)
  expect_true(handoff$r_artifact_immutable)
  expect_true(file.exists(file.path(artifact_dir, "atlas-cohort-definition-handoff.json")))
})
