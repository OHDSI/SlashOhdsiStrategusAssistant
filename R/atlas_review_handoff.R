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

.studyAgentSlashAtlasCohortDraftUrl <- function(config, session_id) {
  paste0(config$atlas_url, "/#/cohorts/new?studyAgentSession=", utils::URLencode(as.character(session_id), reserved = TRUE), "&studyAgentExternalR=true")
}

.studyAgentSlashAtlasCreateCohortDraft <- function(narrative, role_label, cohort_expression, validation, artifact_dir, approval_path, write_json) {
  config <- .studyAgentSlashAtlasReviewConfig()
  if (!.studyAgentSlashAtlasReviewConfigured(config)) return(NULL)
  approval <- tryCatch(jsonlite::read_json(approval_path, simplifyVector = FALSE), error = function(error) list())
  request <- list(
    narrative = narrative,
    role_label = role_label,
    review_id = as.character(approval$review_id %||% ""),
    expression = cohort_expression,
    technical_validation = list(status = as.character(validation$status %||% ""))
  )
  response <- .studyAgentSlashAtlasWebApiRequest(config, "POST", "/study-agent/v1/cohort-definition-sessions/external-r-draft", request)
  session_id <- as.character(response$session_id %||% "")
  if (!nzchar(session_id)) stop("Atlas/WebAPI did not return an R cohort-draft session identifier.")
  handoff <- list(
    source = "r_phenotype_make_computable",
    role_label = role_label,
    narrative_statement = narrative,
    session_id = session_id,
    review_revision = response$review_revision %||% NULL,
    expression_checksum = as.character(response$expression_checksum %||% ""),
    atlas_url = .studyAgentSlashAtlasCohortDraftUrl(config, session_id),
    r_artifact_immutable = TRUE
  )
  write_json(handoff, file.path(artifact_dir, "atlas-cohort-definition-handoff.json"))
  handoff
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
  confirm_create <- function() {
    answer <- tolower(trimws(as.character(prompt("Create an empty Atlas concept set and open it for review? [Y/n] (n returns to review options): ") %||% "")))
    answer %in% c("", "y", "yes", "atlas")
  }
  if (!confirm_create()) return(NULL)
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
    if (!confirm_create()) return(NULL)
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


# A simple scope has one Atlas asset.  Multi-criterion scopes instead use one
# normal Atlas concept set per named criterion binding, reviewed sequentially.
.studyAgentSlashAtlasReviewLanes <- function(scope, review = list()) {
  domains <- scope$criterion_domains %||% list()
  if (!is.list(domains) || !length(domains)) return(list())
  terms <- names(domains)
  if (is.null(terms)) return(list())
  index_term <- trimws(as.character(scope$index_event %||% ""))
  ordered <- c(if (index_term %in% terms) index_term else character(), setdiff(terms, index_term))
  vocabularies <- scope$criterion_vocabularies %||% list()
  runs <- (review$concept_provenance %||% list())$search_runs %||% list()
  lapply(ordered, function(term) {
    domain <- trimws(as.character(domains[[term]] %||% ""))
    if (!nzchar(term) || !nzchar(domain)) stop("Every Atlas review lane needs a named criterion and OMOP domain.")
    purpose <- if (identical(term, index_term)) "Index event" else if (identical(term, as.character((scope$supporting_condition_occurrence %||% list())$concept_set %||% ""))) "Supporting Condition" else if (identical(domain, "Visit")) "Visit restriction" else "Supporting criterion"
    vocabulary <- as.character(unlist(vocabularies[[term]] %||% character(), use.names = FALSE))
    run <- Filter(function(value) identical(trimws(as.character(value$concept_set_name %||% "")), term), runs)
    list(
      key = term,
      term = term,
      domain = domain,
      vocabulary = vocabulary[nzchar(vocabulary)],
      purpose = purpose,
      candidate_count = if (length(run)) as.integer(run[[1]]$returned_count %||% run[[1]]$count %||% 0L) else NA_integer_,
      status = "pending"
    )
  })
}

.studyAgentSlashAtlasLaneScope <- function(lane) {
  criterion_domains <- stats::setNames(list(as.character(lane$domain)), as.character(lane$key))
  scope <- list(index_event = as.character(lane$term), criterion_domains = criterion_domains)
  if (length(lane$vocabulary %||% character())) {
    scope$criterion_vocabularies <- stats::setNames(list(as.list(as.character(lane$vocabulary))), as.character(lane$key))
  }
  scope
}

.studyAgentSlashAtlasCreateReviewLane <- function(config, role_label, narrative, review_id, workflow_id, lane, base_name, prompt) {
  default_name <- paste(base_name, lane$purpose, lane$term, sep = " · ")
  name <- trimws(as.character(prompt(sprintf("Name for the Atlas concept set (%s) [%s]: ", tolower(lane$purpose), default_name)) %||% ""))
  if (!nzchar(name)) name <- default_name
  confirm <- tolower(trimws(as.character(prompt(sprintf("Create the Atlas concept set for %s? [Y/n]: ", tolower(lane$purpose))) %||% "")))
  if (!confirm %in% c("", "y", "yes", "atlas")) return(NULL)
  repeat {
    created <- tryCatch(
      .studyAgentSlashAtlasWebApiRequest(config, "POST", "/conceptset", list(
        name = name,
        description = sprintf("Review-gated %s concept-set draft from R phenotype_make_computable (%s).", tolower(lane$purpose), role_label)
      )),
      error = function(error) error
    )
    if (!inherits(created, "error")) break
    if (!grepl("HTTP 409", conditionMessage(created), fixed = TRUE)) stop(created)
    cat(sprintf("An Atlas concept set named '%s' already exists. The shell will not reuse, modify, or overwrite it.\n", name))
    name <- trimws(as.character(prompt(sprintf("Enter a different name for %s, or press Enter to return to review: ", tolower(lane$purpose))) %||% ""))
    if (!nzchar(name)) return(NULL)
  }
  concept_set_id <- suppressWarnings(as.integer(created$id %||% NA_integer_))
  if (is.na(concept_set_id) || concept_set_id <= 0L) stop("Atlas/WebAPI did not return a concept-set identifier.")
  registration <- .studyAgentSlashAtlasWebApiRequest(
    config, "POST", paste0("/study-agent/v1/concept-set-sessions/external-review?conceptSetId=", concept_set_id),
    list(
      narrative = narrative,
      review_id = review_id,
      workflow_id = workflow_id,
      role_label = role_label,
      candidate_count = as.integer(lane$candidate_count %||% 0L),
      scope = .studyAgentSlashAtlasLaneScope(lane),
      review_lane = list(key = lane$key, term = lane$term, purpose = lane$purpose, expected_domain = lane$domain),
      require_item_domain_metadata = TRUE
    )
  )
  session_id <- as.character(registration$session_id %||% "")
  if (!nzchar(session_id)) stop("Atlas/WebAPI did not return an external review session identifier.")
  lane$status <- "awaiting_atlas_review"
  lane$concept_set_name <- name
  lane$concept_set_id <- concept_set_id
  lane$external_review_session_id <- session_id
  lane$review_url <- .studyAgentSlashAtlasReviewUrl(config, session_id, concept_set_id)
  lane
}

.studyAgentSlashAtlasMultiHandoffPath <- function(artifact_dir) file.path(artifact_dir, "atlas-review-handoff.json")

.studyAgentSlashAtlasWriteMultiHandoff <- function(handoff, artifact_dir, write_json) {
  write_json(handoff, .studyAgentSlashAtlasMultiHandoffPath(artifact_dir))
  handoff
}

.studyAgentSlashAtlasReadMultiHandoff <- function(artifact_dir, expected_review_id = "") {
  path <- .studyAgentSlashAtlasMultiHandoffPath(artifact_dir)
  if (!file.exists(path)) return(NULL)
  handoff <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(error) NULL)
  if (!is.list(handoff) || !identical(as.integer(handoff$schema_version %||% 0L), 2L) || !is.list(handoff$lanes)) return(NULL)
  if (nzchar(expected_review_id) && !identical(as.character(handoff$review_id %||% ""), as.character(expected_review_id))) return(list(status = "different_review", handoff = handoff))
  handoff
}

.studyAgentSlashAtlasAcceptedLane <- function(config, lane, artifact_dir, write_json) {
  status <- .studyAgentSlashAtlasWebApiRequest(config, "GET", paste0("/study-agent/v1/concept-set-sessions/external-review/", utils::URLencode(as.character(lane$external_review_session_id), reserved = TRUE)))
  if (!identical(as.character(status$state %||% ""), "external_review_accepted")) return(list(status = "awaiting_atlas_review", lane = lane))
  expression <- status$expression %||% NULL
  if (!is.list(expression) || !length(expression$items %||% list())) stop(sprintf("Atlas accepted the %s lane without a usable concept-set expression.", tolower(lane$purpose)))
  has_expected_domain <- all(vapply(expression$items, function(item) {
    identical(trimws(as.character((item$concept %||% list())$DOMAIN_ID %||% "")), trimws(as.character(lane$domain)))
  }, logical(1)))
  if (!has_expected_domain) stop(sprintf("Atlas accepted the %s lane without verified %s concept domains. The review remains available for correction.", tolower(lane$purpose), lane$domain))
  slug <- gsub("[^A-Za-z0-9]+", "-", tolower(paste(lane$purpose, lane$key, sep = "-")))
  snapshot <- file.path(artifact_dir, paste0("atlas-accepted-expression-", sub("^-|-$", "", slug), ".json"))
  write_json(expression, snapshot)
  lane$status <- "accepted"
  lane$accepted_expression_artifact <- basename(snapshot)
  lane$review_revision <- status$review_revision %||% NULL
  lane$expression_checksum <- as.character(status$expression_checksum %||% "")
  lane
}

.studyAgentSlashAtlasAdvanceMultiReview <- function(role_label, narrative, scope, review, artifact_dir, write_json, prompt, initialize = FALSE) {
  expected_review_id <- as.character(review$review_id %||% "")
  handoff <- .studyAgentSlashAtlasReadMultiHandoff(artifact_dir, expected_review_id)
  if (is.list(handoff) && identical(handoff$status %||% "", "different_review")) return(handoff)
  if (is.null(handoff)) {
    if (!isTRUE(initialize)) return(NULL)
    lanes <- .studyAgentSlashAtlasReviewLanes(scope, review)
    if (length(lanes) < 2L) return(NULL)
    base_default <- paste0("R review · ", role_label, " · ", narrative)
    base_name <- trimws(as.character(prompt(sprintf("Name prefix for %s Atlas concept sets [%s]: ", length(lanes), base_default)) %||% ""))
    if (!nzchar(base_name)) base_name <- base_default
    handoff <- list(schema_version = 2L, status = "awaiting_atlas_review", created_at = format(Sys.time(), tz = "UTC", usetz = TRUE), review_id = expected_review_id, role_label = role_label, narrative_statement = narrative, scope = scope, base_name = base_name, lanes = lanes)
  }
  config <- .studyAgentSlashAtlasReviewConfig()
  if (!.studyAgentSlashAtlasReviewConfigured(config)) return(list(status = "configuration_required", handoff = handoff))
  workflow_id <- paste(basename(dirname(dirname(artifact_dir))), tolower(role_label), sep = ":")
  for (index in seq_along(handoff$lanes)) {
    lane <- handoff$lanes[[index]]
    if (identical(as.character(lane$status %||% ""), "accepted")) next
    if (nzchar(as.character(lane$external_review_session_id %||% ""))) {
      accepted <- .studyAgentSlashAtlasAcceptedLane(config, lane, artifact_dir, write_json)
      if (!identical(accepted$status, "accepted")) return(c(accepted, list(handoff = handoff, lane_index = index)))
      handoff$lanes[[index]] <- accepted
      .studyAgentSlashAtlasWriteMultiHandoff(handoff, artifact_dir, write_json)
      next
    }
    created <- .studyAgentSlashAtlasCreateReviewLane(config, role_label, narrative, expected_review_id, workflow_id, lane, as.character(handoff$base_name), prompt)
    if (is.null(created)) return(list(status = "cancelled", handoff = handoff))
    handoff$lanes[[index]] <- created
    .studyAgentSlashAtlasWriteMultiHandoff(handoff, artifact_dir, write_json)
    return(list(status = "awaiting_atlas_review", handoff = handoff, lane = created, lane_index = index))
  }
  sets <- lapply(handoff$lanes, function(lane) {
    snapshot <- file.path(artifact_dir, as.character(lane$accepted_expression_artifact %||% ""))
    .studyAgentSlashPmcExternalSets(snapshot, as.character(lane$key), fallback_domain = as.character(lane$domain))[[1]]
  })
  list(status = "accepted", handoff = handoff, concept_sets = sets)
}

.studyAgentSlashAtlasPrintLaneInstructions <- function(role_label, narrative, progress) {
  lane <- progress$lane
  cat(sprintf("\nAtlas review request created for %s (%s of %s concept-set reviews).\n", lane$purpose, progress$lane_index, length(progress$handoff$lanes)))
  cat(sprintf("Working criterion: %s (%s)\n\n", lane$term, lane$domain))
  cat("*** NEXT STEPS ***\n")
  cat("1. Copy this entire Atlas URL and paste it into a new browser tab or window:\n\n")
  cat(sprintf("%s\n\n", lane$review_url))
  cat(sprintf("2. Curate the %s concept set using Atlas features and the Atlas `/ohdsi` helper.\n", tolower(lane$purpose)))
  cat("3. Save the concept set in Atlas.\n")
  cat(sprintf("4. Return here and type `resume`; R will then continue this %s review.\n\n", tolower(role_label)))
  cat("R will use only the immutable Atlas snapshot recorded after each save.\n")
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
