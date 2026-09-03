.studyAgentSlashPmcMark <- function(x) identical(tolower(trimws(as.character(x %||% "")[1])), "x")

.studyAgentSlashPmcStatePath <- function(artifact_dir) file.path(artifact_dir, "review-state.json")

.studyAgentSlashPmcSaveState <- function(artifact_dir, role_label, narrative, scope, review, status, write_json) {
  write_json(list(schema_version = 1L, status = status, role_label = role_label,
    narrative_statement = narrative, scope = scope, review = review,
    artifacts = list(candidate_csv = "concept-review.csv", manifest = "concept-review-manifest.json",
      approval = "concept-set-approval.json", emission = "emission-response.json")),
    .studyAgentSlashPmcStatePath(artifact_dir))
}

.studyAgentSlashPmcReviewCsv <- function(csv_path, manifest_path, review_id) {
  if (!file.exists(csv_path)) stop(sprintf("Reviewed CSV was not found: %s", csv_path))
  if (!file.exists(manifest_path)) stop(sprintf("Review manifest was not found: %s", manifest_path))
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  if (!identical(as.character(manifest$review_id %||% ""), as.character(review_id %||% ""))) stop("The review manifest is not for this ACP review session.")
  rows <- utils::read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", na.strings = c("", "NA"))
  needed <- c("concept_set_name", "concept_id", "concept_name", "domain", "standard_concept", "standard_concept_status",
    "assessment_status", "precision_eligible", "relationship_evidence", "review_include_concept",
    "review_include_descendants", "review_include_mapped", "review_exclude_concepts",
    "review_exclude_descendants", "review_exclude_mapped")
  missing <- setdiff(needed, names(rows)); if (length(missing)) stop(sprintf("Reviewed CSV is missing columns: %s", paste(missing, collapse = ", ")))
  groups <- list(); preview <- list()
  if (nrow(rows)) for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    inc <- .studyAgentSlashPmcMark(row$review_include_concept); exc <- .studyAgentSlashPmcMark(row$review_exclude_concepts)
    inc_d <- .studyAgentSlashPmcMark(row$review_include_descendants); inc_m <- .studyAgentSlashPmcMark(row$review_include_mapped)
    exc_d <- .studyAgentSlashPmcMark(row$review_exclude_descendants); exc_m <- .studyAgentSlashPmcMark(row$review_exclude_mapped)
    if (inc && exc) stop(sprintf("CSV row %s marks both include and exclude.", i))
    if ((inc_d || inc_m) && !inc) stop(sprintf("CSV row %s has inclusion policy marks without review_include_concept.", i))
    if ((exc_d || exc_m) && !exc) stop(sprintf("CSV row %s has exclusion policy marks without review_exclude_concepts.", i))
    if (!inc && !exc) next
    id <- suppressWarnings(as.integer(row$concept_id)); name <- trimws(as.character(row$concept_set_name)); domain <- trimws(as.character(row$domain))
    if (is.na(id) || id <= 0L || !nzchar(name) || !nzchar(domain)) stop(sprintf("CSV row %s has invalid frozen concept-set name, domain, or id.", i))
    key <- paste(name, domain, sep = "\r")
    if (is.null(groups[[key]])) groups[[key]] <- list(name = name, domain = domain, items = list())
    item <- list(concept_id = id, domain = domain, include_descendants = if (inc) inc_d else exc_d,
      include_mapped = if (inc) inc_m else exc_m, is_excluded = exc)
    groups[[key]]$items[[length(groups[[key]]$items) + 1L]] <- item
    preview[[length(preview) + 1L]] <- list(concept_set_name = name, concept_id = id, concept_name = as.character(row$concept_name),
      domain = domain, standard_concept = as.character(row$standard_concept), standard_concept_status = as.character(row$standard_concept_status),
      policy = if (exc) "Exclude" else paste0("Include", if (inc_d) " + descendants" else "", if (inc_m) " + mapped" else ""),
      assessment_status = as.character(row$assessment_status), precision_eligible = as.character(row$precision_eligible), relationship_evidence = as.character(row$relationship_evidence))
  }
  sets <- unname(lapply(groups, identity)); if (!length(sets)) stop("No concepts were selected in the reviewed CSV.")
  list(concept_sets = sets, approval_preview = preview)
}

.studyAgentSlashPmcExternalSets <- function(path, fallback_name) {
  if (!file.exists(path)) stop(sprintf("Concept-set JSON was not found: %s", path))
  value <- jsonlite::read_json(path, simplifyVector = FALSE)
  direct <- value$concept_sets %||% NULL
  if (is.list(direct) && length(direct)) return(direct)
  if (is.list(value) && length(value) && !is.null(value[[1]]$items)) return(value)
  atlas <- value$items %||% NULL
  if (!is.list(atlas) || !length(atlas)) stop("JSON must contain ACP concept_sets or an Atlas items array.")
  items <- lapply(atlas, function(x) {
    concept <- x$concept %||% list(); id <- suppressWarnings(as.integer(x$concept_id %||% x$conceptId %||% concept$CONCEPT_ID))
    domain <- as.character(x$domain %||% concept$DOMAIN_ID %||% "")
    if (is.na(id) || id <= 0L || !nzchar(domain)) stop("Every Atlas item needs a concept id and domain.")
    list(concept_id = id, domain = domain, include_descendants = isTRUE(x$includeDescendants %||% FALSE),
      include_mapped = isTRUE(x$includeMapped %||% FALSE), is_excluded = isTRUE(x$isExcluded %||% FALSE))
  })
  domains <- unique(vapply(items, `[[`, character(1), "domain")); if (length(domains) != 1L) stop("Mixed-domain Atlas exports require separate reviewed sets.")
  list(list(name = as.character(value$name %||% fallback_name), domain = domains[[1]], items = items))
}

.studyAgentSlashPmcPrintPreview <- function(preview) {
  cat("\nExact concept-set policy preview:\n")
  for (x in preview) cat(sprintf("- %s | %s | %s | %s | %s\n", x$concept_set_name, x$concept_id, x$concept_name, x$domain, x$policy))
}

.studyAgentSlashPmcEmit <- function(client, narrative, scope, concept_sets, artifact_dir, imported_definition_dir, write_json) {
  emitted <- .studyAgentSlashAcpPhenotypeMakeComputable(client, narrative_statement = narrative, confirmed_scope = TRUE,
    scope = scope, concept_review_mode = "provided_only", concept_sets = concept_sets)
  write_json(emitted, file.path(artifact_dir, "emission-response.json"))
  if (!identical(emitted$status %||% "", "ok")) { cat(sprintf("ACP did not emit a definition (%s); review state is preserved in %s.\n", emitted$status %||% "unknown", artifact_dir)); return(list(action = "retry")) }
  capr <- emitted$capr %||% list(); writeLines(as.character(capr$source %||% ""), file.path(artifact_dir, "phenotype_definition.R"))
  circe <- emitted$circe_json %||% emitted$circeJson; cohort <- if (is.character(circe)) jsonlite::fromJSON(circe, simplifyVector = FALSE) else circe
  .studyAgentSlashValidateCohortDefinitionJson(cohort, "phenotype_make_computable result")
  id <- .studyAgentSlashStableImportedCohortId(.studyAgentSlashCanonicalCohortJson(cohort))
  imported <- .studyAgentSlashImportAcpCohortDefinition(list(phenotype_id = as.character(id), phenotype_name = narrative,
    justification = "Created through the review-gated phenotype_make_computable ACP flow.", circe_json = cohort), imported_definition_dir)
  imported$metadata$source_type <- "phenotype_make_computable"; imported$metadata$artifact_dir <- artifact_dir; imported$metadata$validation <- emitted$validation %||% NULL
  list(action = "handled", imported = list(imported), selected_source_ids = imported$source_id, selected_ids = imported$cohort_definition_id, records = list(imported$metadata))
}

.studyAgentSlashPmcReviewHandoff <- function(role_label, narrative, scope, review, client, artifact_dir, imported_definition_dir, readline_with_navigation, is_back_signal, write_json, download = TRUE) {
  prompt <- function(text) { x <- trimws(as.character(readline_with_navigation(text) %||% "")); if (is_back_signal(x)) return(x); sub("^['\\\"](.*)['\\\"]$", "\\1", x) }
  if (!identical(review$status %||% "", "needs_concept_review")) { cat(sprintf("ACP returned %s; inspect %s before retrying.\n", review$status %||% "an unexpected response", artifact_dir)); return(list(action = "retry")) }
  urls <- review$review_urls %||% list(); csv <- file.path(artifact_dir, "concept-review.csv"); manifest <- file.path(artifact_dir, "concept-review-manifest.json")
  if (isTRUE(download) && nzchar(as.character(urls$candidates_csv %||% ""))) slashOhdsiAcpClient::acp_download(client, urls$candidates_csv, csv)
  if (isTRUE(download) && nzchar(as.character(urls$manifest %||% ""))) slashOhdsiAcpClient::acp_download(client, urls$manifest, manifest)
  .studyAgentSlashPmcSaveState(artifact_dir, role_label, narrative, scope, review, "awaiting_review", write_json)
  cat(sprintf("\nReview state and frozen ACP artifacts are saved in %s.\n", artifact_dir))
  for (run in review$concept_provenance$search_runs %||% list()) cat(sprintf("- %s: returned %s; matched %s (%s); truncated %s; ordering %s.\n", run$concept_set_name %||% "lane", run$returned_count %||% run$count %||% 0, run$matched_count %||% "not available", run$matched_count_status %||% "not available", run$truncated %||% "not available", run$ordering %||% "provider defined"))
  zero <- identical(as.integer(review$candidate_count %||% 0L), 0L)
  if (zero) cat("No candidates were returned, so CSV review is unavailable.\n") else cat(sprintf("Edit only review_* columns in %s; use x for deliberate selections, then save it. Preserve %s beside it.\n", csv, manifest))
  action <- tolower(prompt(if (zero) "Next [json=Atlas/ACP concept-set JSON, source=choose another cohort source, scope=restart, /back]: " else "Review [csv=validate edited CSV now, later=resume later, json=Atlas/ACP concept-set JSON, source=choose another cohort source, /back]: "))
  if (is_back_signal(action)) return(action)
  if (action %in% c("later", "source", "scope", "")) return(list(action = "retry"))
  if (identical(action, "csv") && !zero) {
    chosen <- prompt(sprintf("Reviewed CSV path [%s]: ", csv)); if (is_back_signal(chosen)) return(chosen); if (!nzchar(chosen)) chosen <- csv
    converted <- .studyAgentSlashPmcReviewCsv(chosen, manifest, review$review_id %||% ""); sets <- converted$concept_sets; preview <- converted$approval_preview
  } else if (identical(action, "json")) {
    chosen <- prompt("Atlas or ACP concept-set JSON path: "); if (is_back_signal(chosen)) return(chosen); sets <- .studyAgentSlashPmcExternalSets(chosen, narrative)
    preview <- unlist(lapply(sets, function(set) lapply(set$items, function(item) list(concept_set_name = set$name, concept_id = item$concept_id, concept_name = "external JSON", domain = item$domain, policy = if (isTRUE(item$is_excluded)) "Exclude" else "Include"))), recursive = FALSE)
  } else return(list(action = "retry"))
  .studyAgentSlashPmcPrintPreview(preview); approval_path <- file.path(artifact_dir, "concept-set-approval.json")
  write_json(list(review_id = review$review_id %||% NULL, concept_sets = sets, approval_preview = preview), approval_path)
  cat(sprintf("Exact policy object saved to %s.\n", approval_path))
  if (!identical(prompt("I explicitly approve this exact concept-set policy [type APPROVE]: "), "APPROVE")) return(list(action = "retry"))
  .studyAgentSlashPmcEmit(client, narrative, scope, sets, artifact_dir, imported_definition_dir, write_json)
}

.studyAgentSlashCreateComputableRoleSelection <- function(role_label, role_statement, client, output_dir, imported_definition_dir, interactive = TRUE, readline_with_navigation = readline, is_back_signal = function(value) FALSE, write_json = function(x, path) jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE)) {
  artifact_dir <- file.path(output_dir, "phenotype-make-computable", tolower(role_label)); state_path <- .studyAgentSlashPmcStatePath(artifact_dir)
  if (!file.exists(state_path)) {
    legacy_response <- file.path(artifact_dir, "concept-review-response.json")
    legacy_scope <- file.path(artifact_dir, "confirmed-scope.json")
    if (file.exists(legacy_response) && file.exists(legacy_scope)) {
      recovered_review <- tryCatch(jsonlite::read_json(legacy_response, simplifyVector = FALSE), error = function(e) NULL)
      recovered_scope <- tryCatch(jsonlite::read_json(legacy_scope, simplifyVector = FALSE), error = function(e) NULL)
      if (is.list(recovered_review) && is.list(recovered_scope) && identical(recovered_review$status %||% "", "needs_concept_review")) {
        .studyAgentSlashPmcSaveState(artifact_dir, role_label, recovered_review$narrative_statement %||% role_statement,
          recovered_scope, recovered_review, "awaiting_review", write_json)
      }
    }
  }
  if (file.exists(state_path)) {
    state <- tryCatch(jsonlite::read_json(state_path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.list(state) && identical(state$status %||% "", "awaiting_review")) {
      choice <- tolower(trimws(as.character(readline_with_navigation(sprintf("Saved %s review found [resume, new, source, /back]: ", role_label)) %||% "")))
      if (is_back_signal(choice)) return(choice)
      if (choice %in% c("resume", "")) return(.studyAgentSlashPmcReviewHandoff(role_label, state$narrative_statement, state$scope, state$review, client, artifact_dir, imported_definition_dir, readline_with_navigation, is_back_signal, write_json, download = FALSE))
      if (identical(choice, "source")) return(list(action = "retry"))
    }
  }
  .studyAgentSlashCreateComputableRoleSelectionFresh(role_label, role_statement, client, output_dir, imported_definition_dir, interactive, readline_with_navigation, is_back_signal, write_json)
}

.studyAgentSlashPmcPrintScope <- function(scope) {
  cat("\nScope to confirm:\n")
  cat(sprintf("- Index event: %s\n", scope$index_event %||% ""))
  for (name in names(scope$criterion_domains %||% list())) cat(sprintf("- Criterion: %s (%s)\n", name, scope$criterion_domains[[name]]))
  for (name in names(scope$criterion_vocabularies %||% list())) cat(sprintf("- Vocabulary restriction: %s = %s\n", name, paste(unlist(scope$criterion_vocabularies[[name]]), collapse = ", ")))
  cat(sprintf("- Entry-event limit: %s\n- Prior observation: %s days\n- Index-day boundary: %s\n- Windows: %s\n- Exit strategy: %s\n- Visit overlap: %s\n",
    scope$entry_limit %||% "", scope$prior_observation %||% "", scope$index_day_boundary %||% "", scope$windows %||% "",
    if (is.list(scope$exit_strategy)) jsonlite::toJSON(scope$exit_strategy, auto_unbox = TRUE) else scope$exit_strategy %||% "", scope$visit_overlap %||% FALSE))
  if (is.list(scope$supporting_condition_occurrence)) {
    x <- scope$supporting_condition_occurrence
    cat(sprintf("- Supporting Condition: %s, %s to %s days relative to %s\n", x$concept_set %||% "", x$start_days %||% "", x$end_days %||% "", x$anchor %||% ""))
  }
  if (!is.null(scope$multi_domain_entry_policy)) cat(sprintf("- Multi-domain policy: %s\n", scope$multi_domain_entry_policy))
}
