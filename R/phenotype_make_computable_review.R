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
    if (identical(as.character(manifest$source %||% ""), "phenotype_code_mapping_evidence")) {
      eligible_keys <- as.character(unlist(manifest$eligible_candidate_keys %||% character(0), use.names = FALSE))
      candidate_key <- paste(as.character(row$concept_id), trimws(as.character(row$domain)), sep = "|")
      if ((inc || exc) && !candidate_key %in% eligible_keys) stop(sprintf("CSV row %s is not eligible for the confirmed-domain mapping review.", i))
    }
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


.studyAgentSlashPmcWriteAtlasMappingExports <- function(rows, source_title, artifact_dir, write_json) {
  eligible <- rows[rows$precision_eligible == "TRUE", , drop = FALSE]
  if (!nrow(eligible)) return(character(0))
  paths <- character(0)
  for (domain in unique(eligible$domain)) {
    domain_rows <- eligible[eligible$domain == domain, , drop = FALSE]
    items <- lapply(seq_len(nrow(domain_rows)), function(i) {
      row <- domain_rows[i, , drop = FALSE]
      list(
        concept = list(
          conceptId = suppressWarnings(as.integer(row$concept_id)),
          conceptName = as.character(row$concept_name),
          domainId = as.character(row$domain),
          vocabularyId = as.character(row$vocabulary_id %||% ""),
          standardConcept = "S"
        ),
        isExcluded = FALSE,
        includeDescendants = FALSE,
        includeMapped = FALSE
      )
    })
    safe_domain <- gsub("[^A-Za-z0-9_-]+", "_", tolower(domain))
    path <- file.path(artifact_dir, paste0("atlas-mapping-review-", safe_domain, ".json"))
    write_json(list(
      name = paste0("Mapped source evidence: ", source_title, " (", domain, ")"),
      description = "Unapproved StudyAgent mapping evidence. Review in Atlas; export corrected JSON and return it for explicit approval.",
      expression = list(items = items),
      source = "StudyAgent phenotype_code_mapping_evidence",
      domain = domain
    ), path)
    paths <- c(paths, path)
  }
  paths
}

.studyAgentSlashPmcWriteMappingEvidenceReview <- function(mapping_evidence, source_title, artifact_dir, write_json) {
  if (!identical(as.character(mapping_evidence$status %||% ""), "ok")) return(NULL)
  results <- mapping_evidence$code_results %||% list()
  rows <- unlist(lapply(results, function(result) {
    candidates <- result$standard_candidates %||% list()
    if (!identical(result$status %||% "", "mapped") && !identical(result$status %||% "", "ambiguous_mapping")) return(list())
    lapply(candidates, function(candidate) data.frame(
      concept_set_name = paste0("Mapped source evidence: ", source_title),
      concept_id = as.character(candidate$concept_id %||% ""),
      concept_name = as.character(candidate$concept_name %||% ""),
      domain = as.character(candidate$domain_id %||% ""),
      vocabulary_id = as.character(candidate$vocabulary_id %||% ""),
      standard_concept = "S", standard_concept_status = as.character(candidate$mapping_method %||% "mapped_source_evidence"),
      assessment_status = paste0("mapping_evidence:", as.character(candidate$domain_policy_status %||% "expected_domain_required")),
      precision_eligible = if (identical(as.character(candidate$domain_policy_status %||% ""), "eligible_for_review")) "TRUE" else "FALSE",
      relationship_evidence = sprintf("%s from %s:%s", as.character(candidate$mapping_method %||% "Maps to"), result$source_vocabulary_id %||% "", result$source_code %||% ""),
      review_include_concept = "", review_include_descendants = "", review_include_mapped = "",
      review_exclude_concepts = "", review_exclude_descendants = "", review_exclude_mapped = "",
      stringsAsFactors = FALSE
    ))
  }), recursive = FALSE)
  if (!length(rows)) return(NULL)
  rows <- do.call(rbind, rows)
  rows <- rows[!is.na(rows$concept_id) & nzchar(rows$concept_id) & nzchar(rows$domain), , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  rows <- rows[!duplicated(rows[, c("concept_id", "domain")]), , drop = FALSE]
  eligible_rows <- rows$precision_eligible == "TRUE"
  eligible_rows[is.na(eligible_rows)] <- FALSE
  eligible_keys <- unique(paste(rows$concept_id[eligible_rows], rows$domain[eligible_rows], sep = "|"))
  if (!length(eligible_keys)) return(NULL)
  review_id <- paste0("mapping-", as.integer(Sys.time()))
  csv <- file.path(artifact_dir, "mapping-concept-review.csv")
  manifest <- file.path(artifact_dir, "mapping-concept-review-manifest.json")
  utils::write.csv(rows, csv, row.names = FALSE, na = "")
  atlas_exports <- .studyAgentSlashPmcWriteAtlasMappingExports(rows, source_title, artifact_dir, write_json)
  write_json(list(
    schema_version = 1L,
    review_id = review_id,
    source = "phenotype_code_mapping_evidence",
    selection_guardrail = "Rows are unapproved mapping evidence. Only confirmed-domain eligible rows may be selected; edit only review_* columns and explicitly approve an exact policy before emission.",
    eligible_candidate_keys = eligible_keys,
    atlas_exports = atlas_exports,
    atlas_recommendation = if (nrow(rows) > 500L) "required" else if (nrow(rows) > 100L) "strongly_recommended" else "optional"
  ), manifest)
  list(review_id = review_id, csv = csv, manifest = manifest, candidate_count = nrow(rows), eligible_candidate_count = length(eligible_keys), atlas_exports = atlas_exports, atlas_recommendation = if (nrow(rows) > 500L) "required" else if (nrow(rows) > 100L) "strongly_recommended" else "optional")
}

.studyAgentSlashPmcExternalSets <- function(path, fallback_name) {
  if (!file.exists(path)) stop(sprintf("Concept-set JSON was not found: %s", path))
  value <- jsonlite::read_json(path, simplifyVector = FALSE)
  direct <- value$concept_sets %||% NULL
  if (is.list(direct) && length(direct)) return(direct)
  if (is.list(value) && length(value) && is.list(value[[1]]) && !is.null(value[[1]]$items)) return(value)
  atlas <- value$items %||% value$expression$items %||% NULL
  if (!is.list(atlas) || !length(atlas)) stop("JSON must contain ACP concept_sets, a bare Atlas items array, or an Atlas expression.items array.")
  items <- lapply(atlas, function(x) {
    concept <- x$concept %||% list(); id <- suppressWarnings(as.integer(x$concept_id %||% x$conceptId %||% concept$CONCEPT_ID %||% concept$conceptId))
    domain <- as.character(x$domain %||% concept$DOMAIN_ID %||% concept$domainId %||% "")
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

.studyAgentSlashPmcLocalValidationEnvironment <- function() {
  packages <- c("Capr", "CirceR", "SqlRender")
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    validation_packages = stats::setNames(lapply(packages, function(package) {
      if (requireNamespace(package, quietly = TRUE)) as.character(utils::packageVersion(package)) else "not_installed"
    }), packages)
  )
}

.studyAgentSlashPmcCompareValidationEnvironment <- function(server, local) {
  server_packages <- server$validation_packages %||% list(); local_packages <- local$validation_packages %||% list()
  package_comparison <- lapply(c("Capr", "CirceR", "SqlRender"), function(package) {
    server_version <- as.character(server_packages[[package]] %||% "unavailable")
    local_version <- as.character(local_packages[[package]] %||% "not_installed")
    server_major <- strsplit(server_version, "\\.")[[1]][1]
    local_major <- strsplit(local_version, "\\.")[[1]][1]
    list(package = package, server_version = server_version, local_version = local_version,
      major_match = identical(server_major, local_major) && !server_version %in% c("unavailable", "not_installed") && !local_version %in% c("unavailable", "not_installed"))
  })
  issues <- Filter(function(x) !isTRUE(x$major_match), package_comparison)
  list(status = if (length(issues)) "warning" else "compatible", server = server, local = local,
    package_comparison = package_comparison,
    warnings = if (length(issues)) lapply(issues, function(x) sprintf("%s server=%s local=%s", x$package, x$server_version, x$local_version)) else list())
}

.studyAgentSlashPmcConversionProvenance <- function(artifact_dir) {
  output_dir <- dirname(dirname(artifact_dir))
  role_dir <- basename(artifact_dir)
  conversion_dir <- file.path(output_dir, "phenotype-conversion", role_dir)
  snapshot_path <- file.path(conversion_dir, "source-snapshot.json")
  if (!file.exists(snapshot_path)) return(NULL)
  snapshot <- tryCatch(jsonlite::read_json(snapshot_path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(snapshot)) return(NULL)
  composition_path <- file.path(conversion_dir, "composition-seed.json")
  mapping_path <- file.path(conversion_dir, "mapping-evidence.json")
  composition <- if (file.exists(composition_path)) tryCatch(jsonlite::read_json(composition_path, simplifyVector = FALSE), error = function(e) NULL) else NULL
  mapping <- if (file.exists(mapping_path)) tryCatch(jsonlite::read_json(mapping_path, simplifyVector = FALSE), error = function(e) NULL) else NULL
  list(source_phenotype_id = snapshot$phenotype_id %||% "", source_title = snapshot$title %||% "",
    source_revision = snapshot$source_revision %||% "", source_payload_sha256 = snapshot$source_payload_sha256 %||% "",
    composition_type = composition$composition_type %||% NULL,
    mapping_evidence_status = mapping$status %||% NULL,
    conversion_artifact_dir = conversion_dir)
}

.studyAgentSlashPmcEmit <- function(client, narrative, scope, concept_sets, artifact_dir, imported_definition_dir, write_json, readline_with_navigation = readline) {
  emitted <- .studyAgentSlashAcpPhenotypeMakeComputable(client, narrative_statement = narrative, confirmed_scope = TRUE,
    scope = scope, concept_review_mode = "provided_only", concept_sets = concept_sets)
  write_json(emitted, file.path(artifact_dir, "emission-response.json"))
  if (!identical(emitted$status %||% "", "ok")) { cat(sprintf("ACP did not emit a definition (%s); review state is preserved in %s.\n", emitted$status %||% "unknown", artifact_dir)); return(list(action = "retry")) }
  validation <- emitted$validation %||% list(); server_environment <- validation$r_environment %||% list()
  local_environment <- .studyAgentSlashPmcLocalValidationEnvironment()
  conversion_provenance <- .studyAgentSlashPmcConversionProvenance(artifact_dir)
  if (!is.null(conversion_provenance)) write_json(conversion_provenance, file.path(artifact_dir, "conversion-provenance.json"))
  comparison <- .studyAgentSlashPmcCompareValidationEnvironment(server_environment, local_environment)
  write_json(local_environment, file.path(artifact_dir, "local-validation-environment.json"))
  write_json(comparison, file.path(artifact_dir, "validation-environment-comparison.json"))
  cat(sprintf("ACP technical validation: %s. R sourced the generated function, Capr wrote Circe JSON, and CirceR generated SQL.\n", validation$status %||% "reported"))
  if (identical(comparison$status, "warning")) cat(sprintf("Validation environment warning: %s. See %s.\n", paste(unlist(comparison$warnings), collapse = "; "), file.path(artifact_dir, "validation-environment-comparison.json")))
  capr <- emitted$capr %||% list(); writeLines(as.character(capr$source %||% ""), file.path(artifact_dir, "phenotype_definition.R"))
  circe <- emitted$circe_json %||% emitted$circeJson; cohort <- if (is.character(circe)) jsonlite::fromJSON(circe, simplifyVector = FALSE) else circe
  .studyAgentSlashValidateCohortDefinitionJson(cohort, "phenotype_make_computable result")
  readable <- tryCatch(CirceR::cohortPrintFriendly(cohort), error = function(error) error)
  if (inherits(readable, "error")) cat(sprintf("Could not render a print-friendly Circe definition: %s\n", conditionMessage(readable))) else {
    readable <- gsub("\r\n?", "\n", paste(as.character(readable), collapse = ""), perl = TRUE)
    readable_action <- tolower(trimws(as.character(readline_with_navigation("Readable Circe definition [v=view, s=save, Enter=skip]: ") %||% "")))
    if (identical(readable_action, "v")) cat(readable, "\n", sep = "")
    if (identical(readable_action, "s")) { readable_path <- file.path(artifact_dir, "cohort-definition-readable.txt"); writeLines(readable, readable_path, useBytes = TRUE); cat(sprintf("Saved print-friendly Circe definition to %s.\n", readable_path)) }
  }
  id <- .studyAgentSlashStableImportedCohortId(.studyAgentSlashCanonicalCohortJson(cohort))
  imported <- .studyAgentSlashImportAcpCohortDefinition(list(phenotype_id = as.character(id), phenotype_name = narrative,
    justification = "Created through the review-gated phenotype_make_computable ACP flow.", circe_json = cohort), imported_definition_dir)
  imported$metadata$source_type <- "phenotype_make_computable"; imported$metadata$source_label <- "ACP phenotype_make_computable"; imported$metadata$artifact_dir <- artifact_dir; imported$metadata$validation <- validation; imported$metadata$validation_environment_comparison <- comparison
  imported$metadata$conversion_source_provenance <- conversion_provenance
  list(action = "handled", imported = list(imported), selected_source_ids = imported$source_id, selected_ids = imported$cohort_definition_id, records = list(imported$metadata))
}

.studyAgentSlashPmcReviewHandoff <- function(role_label, narrative, scope, review, client, artifact_dir, imported_definition_dir, readline_with_navigation, is_back_signal, write_json, download = TRUE) {
  prompt <- function(text) { x <- trimws(as.character(readline_with_navigation(text) %||% "")); if (is_back_signal(x)) return(x); sub("^['\\\"](.*)['\\\"]$", "\\1", x) }
  if (!identical(review$status %||% "", "needs_concept_review")) { cat(sprintf("ACP returned %s; inspect %s before retrying.\n", review$status %||% "an unexpected response", artifact_dir)); return(list(action = "retry")) }
  urls <- review$review_urls %||% list(); csv <- file.path(artifact_dir, "concept-review.csv"); manifest <- file.path(artifact_dir, "concept-review-manifest.json")
  download_review <- function() {
    if (!isTRUE(download)) return(invisible(NULL))
    if (nzchar(as.character(urls$candidates_csv %||% ""))) slashOhdsiAcpClient::acp_download(client, urls$candidates_csv, csv)
    if (nzchar(as.character(urls$manifest %||% ""))) slashOhdsiAcpClient::acp_download(client, urls$manifest, manifest)
  }
  .studyAgentSlashPmcSaveState(artifact_dir, role_label, narrative, scope, review, "awaiting_review", write_json)
  cat(sprintf("\nReview state and frozen ACP artifacts are saved in %s.\n", artifact_dir))
  runs <- review$concept_provenance$search_runs %||% list()
  for (run in runs) cat(sprintf("- %s: returned %s of %s matched (%s); limit %s; truncated %s; ordering %s.\n", run$concept_set_name %||% "lane", run$returned_count %||% run$count %||% 0, run$matched_count %||% "not available", run$matched_count_status %||% "not available", run$limit %||% "not available", run$truncated %||% "not available", run$ordering %||% "provider defined"))
  zero <- identical(as.integer(review$candidate_count %||% 0L), 0L)
  exact_truncated <- Filter(function(run) isTRUE(run$truncated) && identical(run$matched_count_status %||% "", "exact") && !is.null(run$matched_count), runs)
  max_exact <- if (length(exact_truncated)) max(vapply(exact_truncated, function(run) as.integer(run$matched_count), integer(1))) else 0L
  expansion <- if (max_exact > 0L && max_exact <= 500L) "full=request complete available set" else if (max_exact > 500L) "max500=request largest ACP slice" else ""
  if (zero) cat("No candidates were returned, so CSV review is unavailable.\n") else {
    if (nzchar(expansion)) cat(sprintf("The current review is bounded. Use %s before downloading if you want more candidates.\n", expansion))
    cat(sprintf("CSV review writes %s and %s. Edit only review_* columns; use x for deliberate selections.\n", csv, manifest))
  }
  action_prompt <- if (zero) "Next [json=Atlas/ACP concept-set JSON, source=choose another cohort source, scope=restart, /back]: " else paste0("Review [csv=download/validate, later=download and resume later", if (nzchar(expansion)) paste0(", ", expansion) else "", ", json=Atlas/ACP concept-set JSON, source=choose another cohort source, /back]: ")
  action <- tolower(prompt(action_prompt))
  if (is_back_signal(action)) return(action)
  request_expansion <- (identical(action, "full") && max_exact > 0L && max_exact <= 500L) || (identical(action, "max500") && max_exact > 500L)
  if (request_expansion) {
    requested_limit <- if (identical(action, "full")) max_exact else 500L
    request <- list(narrative_statement = narrative, confirmed_scope = TRUE, scope = scope,
      concept_review_mode = "required", concept_build_mode = "search_only", review_delivery = "session",
      candidate_limit = as.integer(requested_limit), concept_sets = list())
    write_json(request, file.path(artifact_dir, "concept-review-expanded-request.json"))
    expanded <- .studyAgentSlashAcpPhenotypeMakeComputable(client, narrative_statement = narrative,
      confirmed_scope = TRUE, scope = scope, concept_review_mode = "required",
      review_delivery = "session", candidate_limit = requested_limit)
    write_json(expanded, file.path(artifact_dir, "concept-review-expanded-response.json"))
    return(.studyAgentSlashPmcReviewHandoff(role_label, narrative, scope, expanded, client, artifact_dir,
      imported_definition_dir, readline_with_navigation, is_back_signal, write_json, download = TRUE))
  }
  if (identical(action, "later")) { download_review(); return(list(action = "retry")) }
  if (action %in% c("source", "scope", "")) return(list(action = "retry"))
  if (identical(action, "csv") && !zero) {
    if (as.integer(review$candidate_count %||% 0L) > 500L && !identical(prompt("This CSV has more than 500 candidates; Atlas is recommended. Download anyway [type DOWNLOAD]: "), "DOWNLOAD")) return(list(action = "retry"))
    download_review()
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
  .studyAgentSlashPmcEmit(client, narrative, scope, sets, artifact_dir, imported_definition_dir, write_json, readline_with_navigation)
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
  cat(sprintf("- Entry-event limit: %s\n- Prior observation: %s days\n- Index-day boundary: %s\n- Windows: %s\n- Exit strategy: %s\n- Visit overlap: %s\n", scope$entry_limit %||% "", scope$prior_observation %||% "", scope$index_day_boundary %||% "", scope$windows %||% "", if (is.list(scope$exit_strategy)) jsonlite::toJSON(scope$exit_strategy, auto_unbox = TRUE) else scope$exit_strategy %||% "", scope$visit_overlap %||% FALSE))
  if (is.list(scope$supporting_condition_occurrence)) {
    x <- scope$supporting_condition_occurrence
    cat(sprintf("- Supporting Condition: %s, %s to %s days relative to %s\n", x$concept_set %||% "", x$start_days %||% "", x$end_days %||% "", x$anchor %||% ""))
  }
  if (!is.null(scope$multi_domain_entry_policy)) cat(sprintf("- Multi-domain policy: %s\n", scope$multi_domain_entry_policy))
}

.studyAgentSlashPrintPhenotypePresentation <- function(preparation) {
.studyAgentSlashPrintPhenotypePresentation <- function(preparation) {
  presentation <- preparation$presentation %||% list(); readiness <- preparation$readiness %||% list()
  cat(sprintf("\n%s\n", as.character(presentation$title %||% preparation$phenotype_id %||% "Phenotype candidate")))
  cat(sprintf("Source: %s\n", as.character(presentation$source %||% "Unknown")))
  cat(sprintf("Use path: %s\n", as.character(readiness$action_class %||% presentation$use_mode %||% "unknown")))
  source_payload <- (preparation$source_snapshot %||% list())$source_payload %||% list()
  if (identical(as.character(presentation$source %||% ""), "OHDSI Phenotype Library") && is.list(source_payload) && is.list(source_payload$PrimaryCriteria)) {
    readable <- tryCatch(CirceR::cohortPrintFriendly(source_payload), error = function(error) NULL)
    if (!is.null(readable)) {
      readable <- gsub("\r\n?", "\n", paste(as.character(readable), collapse = ""), perl = TRUE)
      cat("Executable OHDSI definition (deterministic Circe rendering):\n", readable, "\n", sep = "")
    } else {
      summary <- trimws(as.character(presentation$plain_language_summary %||% "")); if (nzchar(summary)) cat(sprintf("Definition summary:\n%s\n", summary))
    }
  } else {
    summary <- trimws(as.character(presentation$plain_language_summary %||% ""))
    if (nzchar(summary)) cat(sprintf("Source algorithm narrative (evidence only; it may contain source-specific code-list names or record-type fields and is not executable OHDSI logic):\n%s\n", summary))
  }
  mapping <- preparation$mapping_evidence %||% list(); coverage <- mapping$coverage %||% list()
    if (length(candidates)) { cat(sprintf("Suggested phenotypes for %s (%s):\n", as.character(group$role %||% "component"), as.character(group$query %||% ""))); for (candidate in candidates) { card <- candidate$presentation %||% list(); cat(sprintf("- %s [%s; %s] %s\n", as.character(candidate$phenotype_name %||% candidate$phenotype_id %||% ""), as.character(candidate$phenotype_id %||% ""), as.character(candidate$computability_status %||% ""), as.character(card$plain_language_summary %||% candidate$short_description %||% ""))) } }
    else if (identical(group$status %||% "", "no_candidates")) cat(sprintf("No indexed phenotype suggestions were returned for %s (%s); continue with the confirmed scope and concept review.\n", as.character(group$role %||% "component"), as.character(group$query %||% "")))
    else if (identical(group$status %||% "", "unavailable")) cat(sprintf("Follow-on phenotype search was unavailable for %s (%s); no substitute was selected.\n", as.character(group$role %||% "component"), as.character(group$query %||% "")))
  }
  invisible(preparation)
}

.studyAgentSlashPreviewPhenotypeCandidate <- function(client, phenotype_id, role_label, workflow_type,
                                                      check_vocabulary_database = TRUE) {
  phenotype_id <- trimws(as.character(phenotype_id %||% ""))
  if (!nzchar(phenotype_id)) stop("Selected ACP recommendation has no stable phenotype_id.")
  preparation <- .studyAgentSlashAcpPhenotypeConversionPrepare(
    client = client,
    phenotype_id = phenotype_id,
    recommendation_context = list(recommendation_role = tolower(role_label), workflow_type = workflow_type),
    check_vocabulary_database = check_vocabulary_database
  )
  if (!identical(as.character(preparation$status %||% ""), "ok")) {
    stop("ACP could not prepare a candidate preview.")
  }
  .studyAgentSlashPrintPhenotypePresentation(preparation)
  invisible(preparation)
}

.studyAgentSlashPreparePhenotypeConversion <- function(client, phenotype_id, role_label,
                                                        output_dir, workflow_type,
                                                        check_vocabulary_database = TRUE, display = TRUE,
                                                        write_json = function(x, path) jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE)) {
  phenotype_id <- trimws(as.character(phenotype_id %||% ""))
  if (!nzchar(phenotype_id)) stop("Provide a non-empty phenotype_id.")
  artifact_dir <- file.path(output_dir, "phenotype-conversion", tolower(role_label))
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  preparation <- .studyAgentSlashAcpPhenotypeConversionPrepare(
    client = client, phenotype_id = phenotype_id,
    recommendation_context = list(recommendation_role = tolower(role_label), workflow_type = workflow_type),
    check_vocabulary_database = check_vocabulary_database
  )
  if (!identical(as.character(preparation$status %||% ""), "ok")) stop("ACP could not prepare the selected phenotype for review.")
  write_json(preparation, file.path(artifact_dir, "preparation-package.json"))
  snapshot <- preparation$source_snapshot %||% list()
  write_json(snapshot, file.path(artifact_dir, "source-snapshot.json"))
  write_json(preparation$presentation %||% list(), file.path(artifact_dir, "presentation.json"))
  write_json(preparation$readiness %||% list(), file.path(artifact_dir, "readiness.json"))
  write_json(preparation$mapping_evidence %||% list(), file.path(artifact_dir, "mapping-evidence.json"))
  write_json(preparation$composition_seed %||% list(), file.path(artifact_dir, "composition-seed.json"))
  write_json(preparation$component_recommendations %||% list(), file.path(artifact_dir, "component-recommendations.json"))
  write_json(list(schema_version = 1L, phenotype_id = phenotype_id, role_label = role_label,
    workflow_type = workflow_type, next_action = preparation$next_action %||% "",
    source_payload_sha256 = snapshot$source_payload_sha256 %||% ""),
    file.path(artifact_dir, "conversion-state.json"))
  if (isTRUE(display)) .studyAgentSlashPrintPhenotypePresentation(preparation)
  preparation$artifact_dir <- artifact_dir
  preparation
}
