# R-to-Atlas handoff helpers for the review-gated phenotype_make_computable flow.
# These functions intentionally live in the Strategus shell package rather than
# slashOhdsiAcpClient: WebAPI/Atlas is an optional review client, not an ACP
# service dependency.

.studyAgentSlashAtlasReviewConfig <- function() {
  list(
    webapi_url = sub("/+$", "", Sys.getenv("STUDY_AGENT_WEBAPI_URL", "")),
    api_key = Sys.getenv("STUDY_AGENT_WEBAPI_API_KEY", ""),
    atlas_url = sub("/+$", "", Sys.getenv("STUDY_AGENT_ATLAS_URL", ""))
  )
}

.studyAgentSlashAtlasReviewConfigured <- function(config = .studyAgentSlashAtlasReviewConfig()) {
  all(vapply(config, function(value) nzchar(trimws(as.character(value %||% ""))), logical(1)))
}

.studyAgentSlashAtlasWebApiRequest <- function(config, method, path, body = NULL) {
  if (!.studyAgentSlashAtlasReviewConfigured(config)) {
    stop("Atlas handoff needs STUDY_AGENT_WEBAPI_URL, STUDY_AGENT_WEBAPI_API_KEY, and STUDY_AGENT_ATLAS_URL.")
  }
  request_url <- paste0(config$webapi_url, path)
  request <- switch(
    toupper(method),
    GET = httr::GET(request_url, httr::add_headers(`X-API-KEY` = config$api_key), httr::timeout(60)),
    POST = httr::POST(request_url, body = body %||% list(), encode = "json",
      httr::add_headers(`X-API-KEY` = config$api_key), httr::timeout(60)),
    stop(sprintf("Unsupported Atlas WebAPI method: %s", method))
  )
  status <- httr::status_code(request)
  response_text <- httr::content(request, as = "text", encoding = "UTF-8")
  if (status < 200L || status >= 300L) {
    detail <- tryCatch(jsonlite::fromJSON(response_text, simplifyVector = FALSE)$message, error = function(error) "")
    detail <- trimws(as.character(detail %||% ""))
    stop(sprintf("Atlas/WebAPI request failed (HTTP %s)%s", status, if (nzchar(detail)) paste0(": ", detail) else ""))
  }
  if (!nzchar(trimws(response_text))) return(list())
  tryCatch(jsonlite::fromJSON(response_text, simplifyVector = FALSE), error = function(error) {
    stop("Atlas/WebAPI returned an invalid JSON response.")
  })
}

.studyAgentSlashAtlasReviewUrl <- function(config, session_id, concept_set_id) {
  query <- paste0(
    "tab=sets&studyAgentExternalReviewSession=", utils::URLencode(as.character(session_id), reserved = TRUE),
    "&conceptSetId=", utils::URLencode(as.character(concept_set_id), reserved = TRUE)
  )
  paste0(config$atlas_url, "/#/concepts?", query)
}

.studyAgentSlashAtlasCreateReviewHandoff <- function(role_label, narrative, scope, review, artifact_dir, write_json, prompt) {
  config <- .studyAgentSlashAtlasReviewConfig()
  if (!.studyAgentSlashAtlasReviewConfigured(config)) {
    cat("Atlas handoff is not configured for this R session. Configure STUDY_AGENT_WEBAPI_URL, STUDY_AGENT_WEBAPI_API_KEY, and STUDY_AGENT_ATLAS_URL, or use csv/json review.\n")
    return(NULL)
  }
  default_name <- paste0("R review: ", narrative)
  name <- trimws(as.character(prompt(sprintf("Name for the Atlas concept set [%s]: ", default_name)) %||% ""))
  if (!nzchar(name)) name <- default_name
  if (!identical(prompt("Create this empty Atlas concept set and open it for review [type ATLAS]: "), "ATLAS")) return(NULL)
  create_description <- paste0("Review-gated concept-set draft from R phenotype_make_computable (", role_label, ").")
  repeat {
    created <- tryCatch(
      .studyAgentSlashAtlasWebApiRequest(config, "POST", "/conceptset", list(name = name, description = create_description)),
      error = function(error) error
    )
    if (!inherits(created, "error")) break
    if (!grepl("HTTP 409", conditionMessage(created), fixed = TRUE)) stop(created)
    cat(sprintf("An Atlas concept set named '%s' already exists. The shell will not reuse, modify, or overwrite it.\n", name))
    name <- trimws(as.character(prompt("Enter a different Atlas concept-set name, or press Enter to return to review: ") %||% ""))
    if (!nzchar(name)) return(NULL)
    if (!identical(prompt("Create this empty Atlas concept set and open it for review [type ATLAS]: "), "ATLAS")) return(NULL)
  }
  concept_set_id <- suppressWarnings(as.integer(created$id %||% NA_integer_))
  if (is.na(concept_set_id) || concept_set_id <= 0L) stop("Atlas/WebAPI did not return a concept-set identifier.")
  runs <- review$concept_provenance$search_runs %||% list()
  summary <- list(
    candidate_count = as.integer(review$candidate_count %||% 0L),
    search_lanes = lapply(runs, function(run) list(
      concept_set_name = as.character(run$concept_set_name %||% ""),
      returned_count = as.integer(run$returned_count %||% run$count %||% 0L),
      matched_count = run$matched_count %||% NULL,
      target_domain = as.character(run$target_domain %||% "")
    ))
  )
  workflow_id <- paste(basename(dirname(dirname(artifact_dir))), tolower(role_label), sep = ":")
  registration <- .studyAgentSlashAtlasWebApiRequest(
    config, "POST", paste0("/study-agent/v1/concept-set-sessions/external-review?conceptSetId=", concept_set_id),
    list(
      narrative = narrative,
      review_id = as.character(review$review_id %||% ""),
      workflow_id = workflow_id,
      role_label = role_label,
      candidate_count = as.integer(review$candidate_count %||% 0L),
      candidate_summary = summary,
      scope = scope
    )
  )
  session_id <- as.character(registration$session_id %||% "")
  if (!nzchar(session_id)) stop("Atlas/WebAPI did not return an external review session identifier.")
  handoff <- list(
    schema_version = 1L,
    status = "awaiting_atlas_review",
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    review_id = as.character(review$review_id %||% ""),
    role_label = role_label,
    narrative_statement = narrative,
    scope = scope,
    candidate_count = as.integer(review$candidate_count %||% 0L),
    webapi_url = config$webapi_url,
    atlas_url = config$atlas_url,
    concept_set_id = concept_set_id,
    external_review_session_id = session_id,
    review_url = .studyAgentSlashAtlasReviewUrl(config, session_id, concept_set_id)
  )
  write_json(handoff, file.path(artifact_dir, "atlas-review-handoff.json"))
  handoff
}

.studyAgentSlashAtlasAcceptedReview <- function(artifact_dir, write_json, expected_review_id = "") {
  handoff_path <- file.path(artifact_dir, "atlas-review-handoff.json")
  if (!file.exists(handoff_path)) return(NULL)
  handoff <- tryCatch(jsonlite::read_json(handoff_path, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(handoff)) return(NULL)
  expected_review_id <- trimws(as.character(expected_review_id %||% ""))
  handoff_review_id <- trimws(as.character(handoff$review_id %||% ""))
  if (nzchar(expected_review_id) && !identical(handoff_review_id, expected_review_id)) {
    return(list(status = "different_review", handoff = handoff))
  }
  config <- .studyAgentSlashAtlasReviewConfig()
  if (!.studyAgentSlashAtlasReviewConfigured(config)) return(list(status = "configuration_required", handoff = handoff))
  session_id <- as.character(handoff$external_review_session_id %||% "")
  if (!nzchar(session_id)) return(NULL)
  status <- .studyAgentSlashAtlasWebApiRequest(config, "GET", paste0("/study-agent/v1/concept-set-sessions/external-review/", utils::URLencode(session_id, reserved = TRUE)))
  if (!identical(as.character(status$state %||% ""), "external_review_accepted")) {
    return(list(status = "awaiting_atlas_review", handoff = handoff, response = status))
  }
  expression <- status$expression %||% NULL
  if (!is.list(expression) || !length(expression$items %||% list())) stop("Atlas accepted the review without a usable concept-set expression.")
  snapshot_path <- file.path(artifact_dir, "atlas-accepted-expression.json")
  write_json(expression, snapshot_path)
  handoff$status <- "accepted"
  handoff$accepted_at <- as.character(status$accepted_at %||% format(Sys.time(), tz = "UTC", usetz = TRUE))
  handoff$review_revision <- status$review_revision %||% NULL
  handoff$expression_checksum <- as.character(status$expression_checksum %||% "")
  handoff$accepted_expression_artifact <- basename(snapshot_path)
  write_json(handoff, handoff_path)
  list(status = "accepted", handoff = handoff, expression_path = snapshot_path)
}
