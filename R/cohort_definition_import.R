`%||%` <- function(x, y) if (is.null(x)) y else x

.studyAgentSlashValidateCohortDefinitionSchema <- function(schema) {
  schema <- trimws(as.character(schema %||% ""))
  if (!nzchar(schema)) {
    stop("Provide a cohort definition schema name.")
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", schema)) {
    stop("Schema names may contain only letters, numbers, and underscores, and must start with a letter.")
  }
  schema
}

.studyAgentSlashDatabaseCohortSourceId <- function(schema, cohort_definition_id) {
  schema <- .studyAgentSlashValidateCohortDefinitionSchema(schema)
  cohort_definition_id <- suppressWarnings(as.integer(cohort_definition_id))
  if (is.na(cohort_definition_id) || cohort_definition_id <= 0L) {
    stop("cohort_definition_id must be a positive integer.")
  }
  sprintf("db:%s:%s", schema, cohort_definition_id)
}

.studyAgentSlashImportedCohortDefinitionPath <- function(source_id, imported_def_dir) {
  source_id <- as.character(source_id %||% "")
  imported_def_dir <- as.character(imported_def_dir %||% "")
  if (!nzchar(imported_def_dir)) {
    stop("Provide imported_def_dir for cached acquired cohort definitions.")
  }
  file.path(imported_def_dir, sprintf("%s.json", gsub(":", "__", source_id, fixed = TRUE)))
}

.studyAgentSlashImportedCohortAliasPath <- function(cohort_definition_id, imported_def_dir) {
  cohort_definition_id <- suppressWarnings(as.integer(cohort_definition_id))
  imported_def_dir <- as.character(imported_def_dir %||% "")
  if (is.na(cohort_definition_id) || cohort_definition_id <= 0L) {
    stop("cohort_definition_id must be a positive integer.")
  }
  if (!nzchar(imported_def_dir)) {
    stop("Provide imported_def_dir for cached cohort definition aliases.")
  }
  file.path(imported_def_dir, sprintf("%s.json", cohort_definition_id))
}

.studyAgentSlashImportedCohortAliasConflictPath <- function(cohort_definition_id, imported_def_dir) {
  file.path(imported_def_dir, sprintf("%s.alias-conflict", as.integer(cohort_definition_id)))
}

.studyAgentSlashCanonicalCohortJson <- function(cohort_json) {
  jsonlite::toJSON(cohort_json, pretty = FALSE, auto_unbox = TRUE, null = "null")
}

.studyAgentSlashSameCohortJsonFile <- function(path, cohort_json) {
  if (!file.exists(path)) return(FALSE)
  existing <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
  !is.null(existing) && identical(
    .studyAgentSlashCanonicalCohortJson(existing),
    .studyAgentSlashCanonicalCohortJson(cohort_json)
  )
}

# Numeric aliases are conveniences only. When providers reuse a numeric ID with
# different JSON, namespaced source IDs remain usable and bare IDs become unsafe.
.studyAgentSlashCacheCohortDefinition <- function(imported, imported_def_dir) {
  dir.create(imported_def_dir, recursive = TRUE, showWarnings = FALSE)
  cache_path <- .studyAgentSlashImportedCohortDefinitionPath(imported$source_id, imported_def_dir)
  alias_path <- .studyAgentSlashImportedCohortAliasPath(imported$cohort_definition_id, imported_def_dir)
  if (file.exists(cache_path) && !.studyAgentSlashSameCohortJsonFile(cache_path, imported$cohort_json)) {
    stop(sprintf("Cached cohort source %s already exists with different Circe JSON.", imported$source_id))
  }
  if (!file.exists(cache_path)) {
    jsonlite::write_json(imported$cohort_json, cache_path, pretty = TRUE, auto_unbox = TRUE)
  }
  alias_status <- "created"
  if (file.exists(alias_path) && !.studyAgentSlashSameCohortJsonFile(alias_path, imported$cohort_json)) {
    conflict_path <- .studyAgentSlashImportedCohortAliasConflictPath(imported$cohort_definition_id, imported_def_dir)
    writeLines(sprintf("Numeric cohort ID %s has more than one acquired definition. Use a namespaced source ID.", imported$cohort_definition_id), conflict_path)
    alias_path <- NA_character_
    alias_status <- "conflict"
  } else if (!file.exists(alias_path)) {
    jsonlite::write_json(imported$cohort_json, alias_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    alias_status <- "reused"
  }
  list(cache_path = cache_path, alias_path = alias_path, alias_status = alias_status)
}

.studyAgentSlashFormatDbConnectError <- function(error, connectionDetails = NULL) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  base_message <- conditionMessage(error)
  details <- character(0)
  if (inherits(connectionDetails, "connectionDetails")) {
    details <- c(
      details,
      sprintf(
        "dbms=%s server=%s port=%s pathToDriver=%s",
        as.character(connectionDetails$dbms %||% ""),
        as.character(connectionDetails$server %||% ""),
        as.character(connectionDetails$port %||% ""),
        as.character(connectionDetails$pathToDriver %||% "")
      )
    )
  }
  error_classes <- paste(class(error), collapse = ",")
  if (nzchar(error_classes)) {
    details <- c(details, sprintf("error_classes=%s", error_classes))
  }
  java_detail <- tryCatch({
    captured <- capture.output(str(error))
    captured <- trimws(captured)
    captured <- captured[nzchar(captured)]
    if (length(captured) == 0) return("")
    paste(utils::head(captured, 6L), collapse = " | ")
  }, error = function(e) "")
  if (nzchar(java_detail)) {
    details <- c(details, sprintf("detail=%s", java_detail))
  }
  if (length(details) == 0) return(base_message)
  sprintf("%s [%s]", base_message, paste(details, collapse = "; "))
}

.studyAgentSlashConnectWithDiagnostics <- function(connectionDetails) {
  tryCatch(
    DatabaseConnector::connect(connectionDetails),
    error = function(e) {
      stop(.studyAgentSlashFormatDbConnectError(e, connectionDetails = connectionDetails), call. = FALSE)
    }
  )
}

.studyAgentSlashListDatabaseCohortDefinitions <- function(connectionDetails,
                                                           cohort_database_schema,
                                                           search_term = NULL,
                                                           limit = NULL,
                                                           sort_by = c("id", "name")) {
  schema <- .studyAgentSlashValidateCohortDefinitionSchema(cohort_database_schema)
  sort_by <- match.arg(sort_by)
  limit <- suppressWarnings(as.integer(limit))
  if (length(limit) == 0L || is.na(limit) || limit <= 0L) limit <- NULL
  sql <- sprintf(
    paste(
      "SELECT id AS cohort_definition_id, name AS cohort_name, expression_type",
      "FROM %s.cohort_definition"
    ),
    schema
  )
  connection <- .studyAgentSlashConnectWithDiagnostics(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  rows <- DatabaseConnector::querySql(connection, sql)
  if (is.null(rows) || nrow(rows) == 0) {
    return(data.frame(
      cohort_definition_id = integer(0),
      cohort_name = character(0),
      expression_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  keep <- intersect(c("cohort_definition_id", "cohort_name", "expression_type"), names(rows))
  rows <- rows[, keep, drop = FALSE]
  rows$cohort_definition_id <- suppressWarnings(as.integer(rows$cohort_definition_id))
  rows$cohort_name <- as.character(rows$cohort_name %||% "")
  rows$expression_type <- as.character(rows$expression_type %||% "")
  search_term <- trimws(as.character(search_term %||% ""))
  if (nzchar(search_term)) {
    lowered <- tolower(search_term)
    id_match <- suppressWarnings(as.integer(search_term))
    keep_rows <- grepl(lowered, tolower(rows$cohort_name), fixed = TRUE)
    if (!is.na(id_match)) {
      keep_rows <- keep_rows | rows$cohort_definition_id == id_match
    }
    rows <- rows[keep_rows, , drop = FALSE]
  }
  if (nrow(rows) == 0) return(rows)
  if (identical(sort_by, "name")) {
    rows <- rows[order(rows$cohort_name, rows$cohort_definition_id), , drop = FALSE]
  } else {
    rows <- rows[order(rows$cohort_definition_id, rows$cohort_name), , drop = FALSE]
  }
  if (!is.null(limit)) {
    rows <- utils::head(rows, n = limit)
  }
  rows
}

.studyAgentSlashReadDatabaseCohortDefinition <- function(connectionDetails,
                                                         cohort_database_schema,
                                                         cohort_definition_id) {
  schema <- .studyAgentSlashValidateCohortDefinitionSchema(cohort_database_schema)
  cohort_definition_id <- suppressWarnings(as.integer(cohort_definition_id))
  if (is.na(cohort_definition_id) || cohort_definition_id <= 0L) {
    stop("cohort_definition_id must be a positive integer.")
  }
  sql <- sprintf(
    paste(
      "SELECT cd.id AS cohort_definition_id,",
      "cd.name AS cohort_name,",
      "cd.expression_type AS expression_type,",
      "cdd.expression AS expression",
      "FROM %s.cohort_definition cd",
      "LEFT JOIN %s.cohort_definition_details cdd",
      "  ON cd.id = cdd.id",
      "WHERE cd.id = %s"
    ),
    schema,
    schema,
    cohort_definition_id
  )
  connection <- .studyAgentSlashConnectWithDiagnostics(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  rows <- DatabaseConnector::querySql(connection, sql)
  if (is.null(rows) || nrow(rows) == 0) {
    stop(sprintf("No cohort_definition row found for id %s in schema %s.", cohort_definition_id, schema))
  }
  row <- rows[1, , drop = FALSE]
  expression_type <- toupper(trimws(as.character(row$expression_type[[1]] %||% "")))
  if (!identical(expression_type, "SIMPLE_EXPRESSION")) {
    stop(sprintf(
      "Cohort definition %s in schema %s uses expression_type '%s'. Only SIMPLE_EXPRESSION is currently supported.",
      cohort_definition_id,
      schema,
      as.character(row$expression_type[[1]] %||% "")
    ))
  }
  expression_text <- as.character(row$expression[[1]] %||% "")
  if (!nzchar(trimws(expression_text))) {
    stop(sprintf(
      "Cohort definition %s in schema %s has no cohort_definition_details.expression payload.",
      cohort_definition_id,
      schema
    ))
  }
  cohort_json <- tryCatch(
    jsonlite::fromJSON(expression_text, simplifyVector = FALSE),
    error = function(e) {
      stop(sprintf(
        "Cohort definition %s in schema %s does not contain valid JSON in cohort_definition_details.expression: %s",
        cohort_definition_id,
        schema,
        conditionMessage(e)
      ))
    }
  )
  if (!is.list(cohort_json) || is.null(cohort_json$PrimaryCriteria) || is.null(cohort_json$ConceptSets)) {
    stop(sprintf(
      "Cohort definition %s in schema %s does not look like a Circe SIMPLE_EXPRESSION JSON payload.",
      cohort_definition_id,
      schema
    ))
  }
  source_id <- .studyAgentSlashDatabaseCohortSourceId(schema, cohort_definition_id)
  list(
    source_id = source_id,
    cohort_definition_id = cohort_definition_id,
    cohort_name = as.character(row$cohort_name[[1]] %||% sprintf("Cohort %s", cohort_definition_id)),
    expression_type = expression_type,
    expression_text = expression_text,
    cohort_json = cohort_json,
    metadata = list(
      source_type = "database",
      source_id = source_id,
      source_schema = schema,
      cohort_definition_id = cohort_definition_id,
      cohort_name = as.character(row$cohort_name[[1]] %||% sprintf("Cohort %s", cohort_definition_id)),
      logic_description = sprintf("Imported from %s.cohort_definition (%s).", schema, cohort_definition_id),
      expression_type = expression_type
    )
  )
}

.studyAgentSlashImportDatabaseCohortDefinition <- function(connectionDetails,
                                                           cohort_database_schema,
                                                           cohort_definition_id,
                                                           imported_def_dir) {
  imported <- .studyAgentSlashReadDatabaseCohortDefinition(
    connectionDetails = connectionDetails,
    cohort_database_schema = cohort_database_schema,
    cohort_definition_id = cohort_definition_id
  )
  cached <- .studyAgentSlashCacheCohortDefinition(imported, imported_def_dir)
  imported$cache_path <- cached$cache_path
  imported$alias_path <- cached$alias_path
  imported$metadata$cache_path <- cached$cache_path
  imported$metadata$alias_status <- cached$alias_status
  imported
}


.studyAgentSlashValidateCohortDefinitionJson <- function(cohort_json, source_label = "cohort definition") {
  if (!is.list(cohort_json) || is.null(cohort_json$PrimaryCriteria) || is.null(cohort_json$ConceptSets)) {
    stop(sprintf(
      "%s does not look like a Circe SIMPLE_EXPRESSION JSON payload.",
      source_label
    ))
  }
  invisible(cohort_json)
}

.studyAgentSlashSanitizeCohortSourceSlug <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) value <- "cohort"
  value <- gsub("\\.[Jj][Ss][Oo][Nn]$", "", value)
  value <- gsub("[^A-Za-z0-9_.-]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) value <- "cohort"
  value
}

.studyAgentSlashStableImportedCohortId <- function(value) {
  chars <- utf8ToInt(as.character(value %||% "cohort"))
  hash <- 0L
  for (ch in chars) {
    hash <- (hash * 131L + as.integer(ch)) %% 900000L
  }
  as.integer(100000L + hash)
}

.studyAgentSlashInferImportedCohortDefinitionId <- function(cohort_json,
                                                            source_value,
                                                            explicit_id = NULL) {
  explicit_id <- suppressWarnings(as.integer(explicit_id))
  explicit_id <- explicit_id[!is.na(explicit_id)]
  if (length(explicit_id) > 0L && explicit_id[[1]] > 0L) return(as.integer(explicit_id[[1]]))
  json_id <- suppressWarnings(as.integer(cohort_json$id %||% cohort_json$cohortDefinitionId %||% cohort_json$cohort_definition_id))
  json_id <- json_id[!is.na(json_id)]
  if (length(json_id) > 0L && json_id[[1]] > 0L) return(as.integer(json_id[[1]]))
  .studyAgentSlashStableImportedCohortId(source_value)
}

.studyAgentSlashImportedFileSourceId <- function(source_type,
                                                 cohort_definition_id,
                                                 slug,
                                                 source_path = NULL) {
  source_type <- trimws(as.character(source_type %||% "file"))
  if (!(source_type %in% c("file", "directory"))) {
    stop("source_type must be 'file' or 'directory'.")
  }
  cohort_definition_id <- suppressWarnings(as.integer(cohort_definition_id))
  if (is.na(cohort_definition_id) || cohort_definition_id <= 0L) {
    stop("cohort_definition_id must be a positive integer.")
  }
  slug <- .studyAgentSlashSanitizeCohortSourceSlug(slug)
  if (!is.null(source_path) && file.exists(source_path)) {
    token <- substr(unname(tools::md5sum(source_path))[[1]], 1L, 12L)
    slug <- sprintf("%s-%s", slug, token)
  }
  sprintf("%s:%s:%s", if (identical(source_type, "directory")) "dir" else "file", cohort_definition_id, slug)
}

.studyAgentSlashReadFileCohortDefinition <- function(path,
                                                     source_type = "file",
                                                     cohort_definition_id = NULL) {
  source_path <- normalizePath(as.character(path %||% ""), winslash = "/", mustWork = FALSE)
  if (!file.exists(source_path)) {
    stop(sprintf("Cohort definition file not found: %s", source_path))
  }
  cohort_json <- tryCatch(
    jsonlite::read_json(source_path, simplifyVector = FALSE),
    error = function(e) {
      stop(sprintf("Failed to parse cohort definition JSON at %s: %s", source_path, conditionMessage(e)))
    }
  )
  .studyAgentSlashValidateCohortDefinitionJson(cohort_json, source_label = sprintf("Cohort definition file %s", source_path))
  inferred_id <- .studyAgentSlashInferImportedCohortDefinitionId(
    cohort_json = cohort_json,
    source_value = source_path,
    explicit_id = cohort_definition_id
  )
  file_slug <- .studyAgentSlashSanitizeCohortSourceSlug(basename(source_path))
  source_id <- .studyAgentSlashImportedFileSourceId(source_type, inferred_id, file_slug, source_path = source_path)
  cohort_name <- as.character(
    cohort_json$name %||%
      cohort_json$Name %||%
      cohort_json$cohortName %||%
      cohort_json$cohort_name %||%
      sprintf("Cohort %s", inferred_id)
  )
  list(
    source_id = source_id,
    cohort_definition_id = inferred_id,
    cohort_name = cohort_name,
    cohort_json = cohort_json,
    metadata = list(
      source_type = if (identical(source_type, "directory")) "directory" else "file",
      source_id = source_id,
      source_path = source_path,
      source_schema = NA_character_,
      cohort_definition_id = inferred_id,
      cohort_name = cohort_name,
      logic_description = sprintf(
        "Imported from local cohort JSON file %s.",
        source_path
      )
    )
  )
}

.studyAgentSlashImportFileCohortDefinition <- function(path,
                                                       imported_def_dir,
                                                       source_type = "file",
                                                       cohort_definition_id = NULL) {
  imported <- .studyAgentSlashReadFileCohortDefinition(
    path = path,
    source_type = source_type,
    cohort_definition_id = cohort_definition_id
  )
  cached <- .studyAgentSlashCacheCohortDefinition(imported, imported_def_dir)
  imported$cache_path <- cached$cache_path
  imported$alias_path <- cached$alias_path
  imported$metadata$cache_path <- cached$cache_path
  imported$metadata$alias_status <- cached$alias_status
  imported
}


.studyAgentSlashImportPhenotypeLibraryCohortDefinitions <- function(cohort_ids,
                                                                     imported_def_dir) {
  if (!requireNamespace("PhenotypeLibrary", quietly = TRUE)) {
    stop("PhenotypeLibrary must be installed to select a Phenotype Library cohort.")
  }
  cohort_ids <- unique(suppressWarnings(as.integer(cohort_ids)))
  cohort_ids <- cohort_ids[!is.na(cohort_ids) & cohort_ids > 0L]
  if (length(cohort_ids) == 0L) stop("Select at least one positive Phenotype Library cohort ID.")
  definitions <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds = cohort_ids)
  required <- c("cohortId", "cohortName", "json", "sql")
  missing <- setdiff(required, names(definitions %||% list()))
  if (is.null(definitions) || length(missing) > 0L) {
    stop(sprintf("PhenotypeLibrary response is missing required columns: %s", paste(missing, collapse = ", ")))
  }
  returned_ids <- suppressWarnings(as.integer(definitions$cohortId))
  if (anyNA(returned_ids) || anyDuplicated(returned_ids) || !setequal(returned_ids, cohort_ids)) {
    stop("PhenotypeLibrary did not return exactly one definition for every selected cohort ID.")
  }
  definitions <- definitions[match(cohort_ids, returned_ids), , drop = FALSE]
  phenotype_log <- tryCatch(PhenotypeLibrary::getPhenotypeLog(), error = function(e) NULL)
  log_ids <- if (!is.null(phenotype_log) && "cohortId" %in% names(phenotype_log)) suppressWarnings(as.integer(phenotype_log$cohortId)) else integer(0)
  lapply(seq_len(nrow(definitions)), function(i) {
    row <- definitions[i, , drop = FALSE]
    cohort_id <- as.integer(row$cohortId[[1]])
    cohort_json <- tryCatch(jsonlite::fromJSON(as.character(row$json[[1]]), simplifyVector = FALSE), error = function(e) {
      stop(sprintf("PhenotypeLibrary cohort %s returned invalid JSON: %s", cohort_id, conditionMessage(e)))
    })
    .studyAgentSlashValidateCohortDefinitionJson(cohort_json, sprintf("PhenotypeLibrary cohort %s", cohort_id))
    source_id <- sprintf("pl:%s", cohort_id)
    log_row <- if (length(log_ids)) phenotype_log[match(cohort_id, log_ids), , drop = FALSE] else NULL
    logic_description <- if (!is.null(log_row) && "logicDescription" %in% names(log_row)) as.character(log_row$logicDescription[[1]]) else NA_character_
    imported <- list(source_id = source_id, cohort_definition_id = cohort_id,
      cohort_name = as.character(row$cohortName[[1]]), cohort_json = cohort_json)
    cached <- .studyAgentSlashCacheCohortDefinition(imported, imported_def_dir)
    list(source_id = source_id, cohort_definition_id = cohort_id, cohort_name = imported$cohort_name,
      cohort_json = cohort_json, cache_path = cached$cache_path, alias_path = cached$alias_path,
      metadata = list(source_type = "phenotype_library", source_id = source_id, cohort_definition_id = cohort_id,
        cohort_name = imported$cohort_name, logic_description = logic_description, sql = as.character(row$sql[[1]]),
        cache_path = cached$cache_path, alias_status = cached$alias_status, computability_status = "circe_json"))
  })
}

.studyAgentSlashAcpRecommendationJson <- function(recommendation) {
  json_value <- recommendation$circe_json %||% recommendation$circeJson %||% recommendation$json %||% recommendation$cohort_json
  if (is.null(json_value)) return(NULL)
  if (is.character(json_value) && !nzchar(trimws(json_value[[1]]))) return(NULL)
  cohort_json <- tryCatch(if (is.character(json_value)) jsonlite::fromJSON(json_value[[1]], simplifyVector = FALSE) else json_value, error = function(e) NULL)
  if (is.null(cohort_json)) return(NULL)
  tryCatch({ .studyAgentSlashValidateCohortDefinitionJson(cohort_json, "ACP recommendation"); cohort_json }, error = function(e) NULL)
}

.studyAgentSlashAcpRecommendationCohortId <- function(recommendation, cohort_json = NULL) {
  values <- c(recommendation$cohort_id, recommendation$cohortId, recommendation$id, recommendation$phenotype_id)
  for (value in values) {
    text <- trimws(as.character(value %||% ""))
    if (grepl("^ohdsi:[0-9]+$", text)) text <- sub("^ohdsi:", "", text)
    id <- suppressWarnings(as.integer(text))
    if (!is.na(id) && id > 0L) return(id)
  }
  .studyAgentSlashStableImportedCohortId(.studyAgentSlashCanonicalCohortJson(cohort_json))
}

.studyAgentSlashImportAcpCohortDefinition <- function(recommendation, imported_def_dir) {
  cohort_json <- .studyAgentSlashAcpRecommendationJson(recommendation)
  if (is.null(cohort_json)) stop("ACP recommendation did not include a computable Circe JSON definition.")
  cohort_id <- .studyAgentSlashAcpRecommendationCohortId(recommendation, cohort_json)
  source_id <- sprintf("acp:%s", cohort_id)
  imported <- list(source_id = source_id, cohort_definition_id = cohort_id, cohort_json = cohort_json)
  cached <- .studyAgentSlashCacheCohortDefinition(imported, imported_def_dir)
  upstream_id <- as.character(recommendation$phenotype_id %||% recommendation$cohort_id %||% recommendation$cohortId %||% NA_character_)
  list(source_id = source_id, cohort_definition_id = cohort_id, cohort_json = cohort_json,
    cache_path = cached$cache_path, alias_path = cached$alias_path,
    metadata = list(source_type = "acp", source_id = source_id, upstream_id = upstream_id,
      cohort_definition_id = cohort_id, cohort_name = as.character(recommendation$phenotype_name %||% recommendation$cohortName %||% sprintf("Cohort %s", cohort_id)),
      logic_description = as.character(recommendation$justification %||% NA_character_), cache_path = cached$cache_path,
      alias_status = cached$alias_status, computability_status = "circe_json"))
}

.studyAgentSlashListLocalCohortDefinitionFiles <- function(directory,
                                                           limit = 100L) {
  dir_path <- normalizePath(as.character(directory %||% ""), winslash = "/", mustWork = FALSE)
  if (!dir.exists(dir_path)) {
    stop(sprintf("Cohort definition directory not found: %s", dir_path))
  }
  limit <- suppressWarnings(as.integer(limit %||% 100L))
  if (is.na(limit) || limit <= 0L) limit <- 100L
  files <- list.files(dir_path, pattern = "\\.[Jj][Ss][Oo][Nn]$", full.names = TRUE)
  if (length(files) == 0L) {
    return(data.frame(
      cohort_definition_id = integer(0),
      cohort_name = character(0),
      path = character(0),
      source_id = character(0),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(files, function(file_path) {
    parsed <- tryCatch(
      .studyAgentSlashReadFileCohortDefinition(file_path, source_type = "directory"),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(NULL)
    data.frame(
      cohort_definition_id = as.integer(parsed$cohort_definition_id),
      cohort_name = as.character(parsed$cohort_name %||% basename(file_path)),
      path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
      source_id = as.character(parsed$source_id %||% ""),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame(
      cohort_definition_id = integer(0),
      cohort_name = character(0),
      path = character(0),
      source_id = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$cohort_name, out$cohort_definition_id, out$path), , drop = FALSE]
  utils::head(out, n = limit)
}
