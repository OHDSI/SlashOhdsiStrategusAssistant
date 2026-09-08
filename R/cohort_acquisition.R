`%||%` <- function(x, y) if (is.null(x)) y else x

.studyAgentSlashImportedSourceIdPattern <- function() {
  "^(db:[A-Za-z][A-Za-z0-9_]*:[0-9]+|file:[0-9]+:[A-Za-z0-9_.-]+|dir:[0-9]+:[A-Za-z0-9_.-]+|pl:[0-9]+|acp:[0-9]+)$"
}

.studyAgentSlashPhenotypeDefinitionPath <- function(phenotype_id, imported_def_dir = NULL) {
  phenotype_id <- as.character(phenotype_id %||% "")
  if (grepl(.studyAgentSlashImportedSourceIdPattern(), phenotype_id)) {
    return(.studyAgentSlashImportedCohortDefinitionPath(phenotype_id, imported_def_dir))
  }
  alias_conflict <- .studyAgentSlashImportedCohortAliasConflictPath(phenotype_id, imported_def_dir)
  if (file.exists(alias_conflict)) {
    stop(sprintf("Cohort ID %s is ambiguous across acquired providers; use its namespaced source ID.", phenotype_id))
  }
  .studyAgentSlashImportedCohortAliasPath(phenotype_id, imported_def_dir)
}

.studyAgentSlashStopIfUnsupportedSelected <- function(phenotype_ids, role_label) {
  supported <- grepl("^(ohdsi|pl|acp):[0-9]+$", phenotype_ids %||% character(0)) |
    grepl(.studyAgentSlashImportedSourceIdPattern(), phenotype_ids %||% character(0))
  unsupported <- phenotype_ids[!supported]
  if (length(unsupported) > 0) {
    stop(
      sprintf(
        paste0(
          "Selected %s cohort source ids include unsupported values (%s). ",
          "Supported ids are OHDSI phenotype ids, imported database cohort ids, and imported local cohort JSON ids."
        ),
        role_label,
        paste(unique(unsupported), collapse = ", ")
      )
    )
  }
}

.studyAgentSlashDefaultCohortIdFromSource <- function(source_id) {
  source_id <- trimws(as.character(source_id %||% ""))
  if (!nzchar(source_id)) return(NA_integer_)
  if (grepl("^(ohdsi|pl|acp):[0-9]+$", source_id)) {
    return(suppressWarnings(as.integer(sub("^(ohdsi|pl|acp):", "", source_id))))
  }
  if (grepl("^db:[A-Za-z][A-Za-z0-9_]*:[0-9]+$", source_id)) {
    return(suppressWarnings(as.integer(sub("^db:[A-Za-z][A-Za-z0-9_]*:([0-9]+)$", "\\1", source_id))))
  }
  if (grepl("^(file|dir):[0-9]+:[A-Za-z0-9_.-]+$", source_id)) {
    return(suppressWarnings(as.integer(sub("^(file|dir):([0-9]+):[A-Za-z0-9_.-]+$", "\\2", source_id))))
  }
  suppressWarnings(as.integer(source_id))
}

.studyAgentSlashDefaultCohortIdsFromSources <- function(source_ids, role_label = "selected") {
  source_ids <- as.character(source_ids %||% character(0))
  if (length(source_ids) == 0) return(integer(0))
  derived <- vapply(source_ids, .studyAgentSlashDefaultCohortIdFromSource, integer(1))
  if (any(is.na(derived))) {
    bad <- source_ids[is.na(derived)]
    stop(sprintf(
      "Could not derive numeric cohort IDs for %s phenotype(s): %s",
      role_label,
      paste(unique(bad), collapse = ", ")
    ))
  }
  as.integer(derived)
}

.studyAgentSlashCopyCohortJsonMulti <- function(source_id, dest_id, dest_dirs, imported_def_dir = NULL, ensure_dir) {
  src <- .studyAgentSlashPhenotypeDefinitionPath(source_id, imported_def_dir = imported_def_dir)
  if (!file.exists(src)) stop(sprintf("Cohort JSON not found: %s", src))
  dests <- character(0)
  for (dest_dir in dest_dirs) {
    ensure_dir(dest_dir)
    dest <- file.path(dest_dir, sprintf("%s.json", dest_id))
    file.copy(src, dest, overwrite = TRUE)
    dests <- c(dests, dest)
  }
  dests
}

.studyAgentSlashSelectionRecordFromRecommendation <- function(rec) {
  list(
    source_type = "index",
    source_id = as.character(rec$phenotype_id %||% ""),
    source_schema = NA_character_,
    cohort_definition_id = .studyAgentSlashDefaultCohortIdFromSource(rec$phenotype_id %||% NULL),
    cohort_name = as.character(rec$phenotype_name %||% ""),
    logic_description = rec$justification %||% NA_character_
  )
}

.studyAgentSlashSelectionRecordFromImport <- function(imported) {
  imported$metadata
}

.studyAgentSlashSeedDbDetailsTemplate <- function(path, write_json) {
  if (!file.exists(path)) {
    write_json(list(
      dbms = "postgresql",
      authType = "username_password",
      DB_SERVER = "",
      DB_PORT = "5432",
      DB_USER = "",
      DB_PASS = "",
      DB_DRIVER_PATH = "",
      DATABASECONNECTOR_JAR_FOLDER = "",
      extraSettings = "sslmode=disable"
    ), path)
  }
  invisible(path)
}

.studyAgentSlashSeedRuntimeTemplates <- function(base_dir, write_json) {
  db_details_path <- file.path(base_dir, "strategus-db-details.json")
  cohort_source_db_details_path <- file.path(base_dir, "strategus-cohort-source-db-details.json")
  execution_settings_path <- file.path(base_dir, "strategus-execution-settings.json")

  .studyAgentSlashSeedDbDetailsTemplate(db_details_path, write_json = write_json)
  .studyAgentSlashSeedDbDetailsTemplate(cohort_source_db_details_path, write_json = write_json)

  if (!file.exists(execution_settings_path)) {
    write_json(list(
      cdmDatabaseSchema = "",
      workDatabaseSchema = "",
      resultsDatabaseSchema = "",
      vocabularyDatabaseSchema = "",
      cohortTable = "cohort",
      workFolder = file.path(base_dir, "work"),
      resultsFolder = file.path(base_dir, "results"),
      cohortIdFieldName = "cohort_definition_id",
      maxCores = 4,
      incremental = FALSE
    ), execution_settings_path)
  }

  list(
    db_details_path = db_details_path,
    cohort_source_db_details_path = cohort_source_db_details_path,
    execution_settings_path = execution_settings_path
  )
}

.studyAgentSlashCohortSourceDbDetailsNeedConfiguration <- function(path, readStrategusDbDetails) {
  db_config <- tryCatch(readStrategusDbDetails(path), error = function(e) NULL)
  if (is.null(db_config)) return(TRUE)
  auth_type <- tolower(trimws(as.character(
    db_config$authType %||%
      db_config$authenticationType %||%
      if (isTRUE(db_config$useWindowsAuth %||% FALSE)) "windows" else "username_password"
  )))
  if (!nzchar(auth_type)) auth_type <- "username_password"
  server <- trimws(as.character(db_config$DB_SERVER %||% db_config$server %||% ""))
  if (!nzchar(server)) return(TRUE)
  if (auth_type %in% c("windows", "integrated", "integrated_windows")) return(FALSE)
  user <- trimws(as.character(db_config$DB_USER %||% db_config$user %||% ""))
  raw_password <- db_config$DB_PASS %||% db_config$password
  is.null(raw_password) || !nzchar(user)
}

.studyAgentSlashChooseSelectionSourceMode <- function(role_label, allow_index = TRUE, interactive = TRUE, readline_with_navigation, is_back_signal) {
  if (!isTRUE(interactive)) {
    if (isTRUE(allow_index)) return("index")
    stop("Non-interactive direct cohort acquisition requires explicit source handling and is not implemented for this shell.")
  }
  repeat {
    prompt <- if (isTRUE(allow_index)) {
      sprintf("Source for %s cohort [ai=agentic search (default), create=new definition, pl=Phenotype Library, file=JSON file, dir=directory, db=database cohort]: ", role_label)
    } else {
      sprintf("Source for %s cohort [pl=Phenotype Library, file=JSON file, dir=directory, db=database cohort]: ", role_label)
    }
    entered <- trimws(readline_with_navigation(prompt))
    if (is_back_signal(entered)) return(entered)
    lowered <- tolower(entered)
    if (isTRUE(allow_index) && lowered %in% c("create", "new", "make", "computable")) return("create")
    if (isTRUE(allow_index) && (!nzchar(lowered) || lowered %in% c("ai", "index", "search", "s", "recommend", "agentic"))) return("index")
    if (lowered %in% c("db", "database", "existing")) return("database")
    if (lowered %in% c("pl", "phenotypelibrary", "phenotype_library", "library")) return("phenotype_library")
    if (lowered %in% c("file", "json", "local")) return("file")
    if (lowered %in% c("dir", "directory", "folder")) return("directory")
    cat(if (isTRUE(allow_index)) "Choose ai, create, pl, file, dir, or db, or press Enter for the default.\n" else "Choose pl, file, dir, or db.\n")
  }
}

.studyAgentSlashPromptDatabaseCohortImports <- function(role_label,
                                                        allow_multiple = FALSE,
                                                        base_dir,
                                                        imported_definition_dir,
                                                        interactive = TRUE,
                                                        readline_with_navigation,
                                                        readline_with_dialogue,
                                                        is_back_signal,
                                                        write_json,
                                                        readStrategusDbDetails,
                                                        normalizeStrategusDbConfig,
                                                        createStrategusConnectionDetails) {
  db_details_path <- file.path(base_dir, "strategus-cohort-source-db-details.json")
  .studyAgentSlashSeedDbDetailsTemplate(db_details_path, write_json = write_json)
  if (isTRUE(.studyAgentSlashCohortSourceDbDetailsNeedConfiguration(db_details_path, readStrategusDbDetails = readStrategusDbDetails))) {
    cat(sprintf(
      "Database cohort import requires a populated %s. Fill in the cohort-source DB connection details there and then retry the db import option.\n",
      db_details_path
    ))
    return(NULL)
  }
  repeat {
    if (isTRUE(.studyAgentSlashCohortSourceDbDetailsNeedConfiguration(db_details_path, readStrategusDbDetails = readStrategusDbDetails))) {
      cat(sprintf(
        "Database cohort import requires a populated %s. Fill in the cohort-source DB connection details there and then retry the db import option.\n",
        db_details_path
      ))
      return(NULL)
    }
    normalized_db_config <- tryCatch(
      normalizeStrategusDbConfig(path = db_details_path),
      error = function(e) e
    )
    if (inherits(normalized_db_config, "error")) {
      cat(sprintf(
        "Cannot use database cohort import until %s is populated: %s\n",
        db_details_path,
        conditionMessage(normalized_db_config)
      ))
      return(NULL)
    }
    cat(sprintf(
      "Using %s connection details: server=%s, port=%s, authType=%s\n",
      as.character(normalized_db_config$dbms %||% "database"),
      as.character(normalized_db_config$server %||% ""),
      as.character(normalized_db_config$port %||% ""),
      as.character(normalized_db_config$authType %||% "")
    ))
    connectionDetails <- tryCatch(
      createStrategusConnectionDetails(path = db_details_path, dbDetails = normalized_db_config$dbConfig),
      error = function(e) e
    )
    if (inherits(connectionDetails, "error")) {
      cat(sprintf(
        "Cannot use database cohort import until %s is populated: %s\n",
        db_details_path,
        conditionMessage(connectionDetails)
      ))
      return(NULL)
    }
    schema_value <- readline_with_navigation(sprintf(
      "Schema containing cohort_definition and cohort_definition_details for the %s cohort: ",
      role_label
    ))
    if (is_back_signal(schema_value)) return(schema_value)
    schema_value <- trimws(as.character(schema_value %||% ""))
    if (!nzchar(schema_value)) {
      cat("Enter a schema name.\n")
      next
    }
    search_term <- trimws(readline_with_dialogue(sprintf(
      "Optional %s cohort name search term [Enter=list candidates]: ",
      role_label
    )))
    candidates <- tryCatch(
      .studyAgentSlashListDatabaseCohortDefinitions(
        connectionDetails = connectionDetails,
        cohort_database_schema = schema_value,
        search_term = search_term,
        sort_by = "id"
      ),
      error = function(e) e
    )
    if (inherits(candidates, "error")) {
      cat(sprintf("Database cohort lookup failed: %s\n", conditionMessage(candidates)))
      next
    }
    if (nrow(candidates) == 0) {
      cat("No matching cohort definitions were found. Try a different schema or search term.\n")
      next
    }
    preview <- data.frame(
      cohort_definition_id = candidates$cohort_definition_id,
      cohort_name = candidates$cohort_name,
      stringsAsFactors = FALSE
    )
    preview <- preview[order(preview$cohort_definition_id, preview$cohort_name), , drop = FALSE]
    rownames(preview) <- as.character(preview$cohort_definition_id)
    cat(sprintf("\nAvailable %s cohort definitions from %s\n", role_label, schema_value))
    print(preview, row.names = TRUE)
    labels <- sprintf("[%s] %s", preview$cohort_definition_id, preview$cohort_name)
    selected_ids <- integer(0)
    invalid <- character(0)
    if (isTRUE(interactive)) {
      menu_pick <- tryCatch(
        utils::select.list(
          labels,
          multiple = isTRUE(allow_multiple),
          title = sprintf("Select %s cohort definition%s", role_label, if (isTRUE(allow_multiple)) "(s)" else "")
        ),
        error = function(e) NULL
      )
      if (length(menu_pick) > 0 && any(nzchar(menu_pick))) {
        selected_ids <- unique(vapply(menu_pick[nzchar(menu_pick)], function(label) {
          idx <- which(labels == label)[1]
          preview$cohort_definition_id[[idx]]
        }, integer(1)))
      }
    }
    if (length(selected_ids) == 0) {
      selection_prompt <- if (isTRUE(allow_multiple)) {
        sprintf("Select %s cohort row numbers or cohort_definition ids (comma-separated): ", role_label)
      } else {
        sprintf("Select the %s cohort row number or cohort_definition id: ", role_label)
      }
      selected_raw <- trimws(readline_with_dialogue(selection_prompt))
      if (!nzchar(selected_raw)) {
        cat("No cohort selected.\n")
        next
      }
      selected_parts <- trimws(strsplit(selected_raw, ",", fixed = TRUE)[[1]])
      selected_parts <- selected_parts[nzchar(selected_parts)]
      if (length(selected_parts) == 0) {
        cat("No cohort selected.\n")
        next
      }
      for (part in selected_parts) {
        parsed <- suppressWarnings(as.integer(part))
        if (is.na(parsed)) {
          invalid <- c(invalid, part)
          next
        }
        if (parsed %in% preview$cohort_definition_id) {
          selected_ids <- c(selected_ids, parsed)
        } else if (parsed >= 1L && parsed <= nrow(preview)) {
          selected_ids <- c(selected_ids, preview$cohort_definition_id[[parsed]])
        } else {
          invalid <- c(invalid, part)
        }
      }
    }
    selected_ids <- unique(selected_ids)
    if (length(invalid) > 0 || length(selected_ids) == 0) {
      cat(sprintf("Invalid selection: %s\n", paste(unique(invalid), collapse = ", ")))
      next
    }
    imported <- lapply(selected_ids, function(id) {
      .studyAgentSlashImportDatabaseCohortDefinition(
        connectionDetails = connectionDetails,
        cohort_database_schema = schema_value,
        cohort_definition_id = id,
        imported_def_dir = imported_definition_dir
      )
    })
    return(imported)
  }
}

.studyAgentSlashPromptFileCohortImports <- function(role_label,
                                                    allow_multiple = FALSE,
                                                    imported_definition_dir,
                                                    readline_with_navigation,
                                                    is_back_signal) {
  repeat {
    prompt <- if (isTRUE(allow_multiple)) {
      sprintf("Path to %s cohort JSON file(s) [comma-separated]: ", role_label)
    } else {
      sprintf("Path to %s cohort JSON file: ", role_label)
    }
    entered <- trimws(readline_with_navigation(prompt))
    if (is_back_signal(entered)) return(entered)
    if (!nzchar(entered)) {
      cat("Enter a cohort JSON file path.\n")
      next
    }
    parts <- trimws(strsplit(entered, ",", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    if (!isTRUE(allow_multiple) && length(parts) > 1L) {
      cat("Select exactly one cohort JSON file for this role.\n")
      next
    }
    imported <- tryCatch(
      lapply(parts, function(file_path) {
        .studyAgentSlashImportFileCohortDefinition(
          path = file_path,
          imported_def_dir = imported_definition_dir,
          source_type = "file"
        )
      }),
      error = function(e) e
    )
    if (inherits(imported, "error")) {
      cat(sprintf("File cohort import failed: %s\n", conditionMessage(imported)))
      next
    }
    return(imported)
  }
}

.studyAgentSlashPromptDirectoryCohortImports <- function(role_label,
                                                         allow_multiple = FALSE,
                                                         imported_definition_dir,
                                                         interactive = TRUE,
                                                         readline_with_navigation,
                                                         readline_with_dialogue,
                                                         is_back_signal) {
  repeat {
    directory <- trimws(readline_with_navigation(sprintf(
      "Directory containing %s cohort JSON files: ",
      role_label
    )))
    if (is_back_signal(directory)) return(directory)
    if (!nzchar(directory)) {
      cat("Enter a directory path.\n")
      next
    }
    candidates <- tryCatch(
      .studyAgentSlashListLocalCohortDefinitionFiles(directory, limit = 100L),
      error = function(e) e
    )
    if (inherits(candidates, "error")) {
      cat(sprintf("Directory cohort lookup failed: %s\n", conditionMessage(candidates)))
      next
    }
    if (nrow(candidates) == 0) {
      cat("No readable cohort JSON files were found in that directory.\n")
      next
    }
    preview <- data.frame(
      cohort_definition_id = candidates$cohort_definition_id,
      cohort_name = candidates$cohort_name,
      path = candidates$path,
      stringsAsFactors = FALSE
    )
    rownames(preview) <- seq_len(nrow(preview))
    cat(sprintf("\nAvailable %s cohort JSON files from %s\n", role_label, normalizePath(directory, winslash = "/", mustWork = FALSE)))
    print(preview, row.names = TRUE)
    labels <- sprintf("[%s] %s", preview$cohort_definition_id, preview$cohort_name)
    selected_rows <- integer(0)
    invalid <- character(0)
    if (isTRUE(interactive)) {
      menu_pick <- tryCatch(
        utils::select.list(
          labels,
          multiple = isTRUE(allow_multiple),
          title = sprintf("Select %s cohort JSON%s", role_label, if (isTRUE(allow_multiple)) "(s)" else "")
        ),
        error = function(e) NULL
      )
      if (length(menu_pick) > 0 && any(nzchar(menu_pick))) {
        selected_rows <- unique(vapply(menu_pick[nzchar(menu_pick)], function(label) {
          which(labels == label)[1]
        }, integer(1)))
      }
    }
    if (length(selected_rows) == 0) {
      selection_prompt <- if (isTRUE(allow_multiple)) {
        sprintf("Select %s cohort row numbers (comma-separated): ", role_label)
      } else {
        sprintf("Select the %s cohort row number: ", role_label)
      }
      selected_raw <- trimws(readline_with_dialogue(selection_prompt))
      if (!nzchar(selected_raw)) {
        cat("No cohort selected.\n")
        next
      }
      selected_parts <- trimws(strsplit(selected_raw, ",", fixed = TRUE)[[1]])
      selected_parts <- selected_parts[nzchar(selected_parts)]
      for (part in selected_parts) {
        parsed <- suppressWarnings(as.integer(part))
        if (is.na(parsed) || parsed < 1L || parsed > nrow(preview)) {
          invalid <- c(invalid, part)
        } else {
          selected_rows <- c(selected_rows, parsed)
        }
      }
    }
    selected_rows <- unique(selected_rows)
    if (length(invalid) > 0 || length(selected_rows) == 0) {
      cat(sprintf("Invalid selection: %s\n", paste(unique(invalid), collapse = ", ")))
      next
    }
    imported <- tryCatch(
      lapply(selected_rows, function(row_idx) {
        .studyAgentSlashImportFileCohortDefinition(
          path = preview$path[[row_idx]],
          imported_def_dir = imported_definition_dir,
          source_type = "directory"
        )
      }),
      error = function(e) e
    )
    if (inherits(imported, "error")) {
      cat(sprintf("Directory cohort import failed: %s\n", conditionMessage(imported)))
      next
    }
    return(imported)
  }
}

.studyAgentSlashAcquireImportedRoleSelection <- function(source_mode,
                                                         role_label,
                                                         allow_multiple = FALSE,
                                                         interactive = TRUE,
                                                         step_messages = list(database = NULL, file = NULL, directory = NULL, phenotype_library = NULL),
                                                         prompt_database_imports,
                                                         prompt_file_imports,
                                                         prompt_directory_imports,
                                                         prompt_phenotype_library_imports = NULL,
                                                         prompt_create_computable = NULL,
                                                         selection_record_from_import = .studyAgentSlashSelectionRecordFromImport) {
  if (!(source_mode %in% c("database", "file", "directory", "phenotype_library", "create"))) return(NULL)
  if (isTRUE(interactive)) {
    step_message <- step_messages[[source_mode]] %||% NULL
    if (nzchar(trimws(as.character(step_message %||% "")))) {
      cat(sprintf("\n== %s ==\n", step_message))
    }
  }
  imported <- switch(
    source_mode,
    database = prompt_database_imports(role_label, allow_multiple = allow_multiple),
    file = prompt_file_imports(role_label, allow_multiple = allow_multiple),
    directory = prompt_directory_imports(role_label, allow_multiple = allow_multiple),
    phenotype_library = prompt_phenotype_library_imports(role_label, allow_multiple = allow_multiple),
    create = prompt_create_computable(role_label, allow_multiple = allow_multiple)
  )
  if (inherits(imported, "workflow_navigation_signal")) return(imported)
  if (is.list(imported) && identical(imported$action %||% "", "handled")) return(imported)
  if (is.list(imported) && identical(imported$action %||% "", "retry")) return(imported)
  if (is.null(imported) || length(imported) == 0) {
    return(list(action = "retry", imported = imported))
  }
  imported_items <- imported
  list(
    action = "handled",
    imported = imported_items,
    selected_source_ids = as.character(vapply(imported_items, function(item) item$source_id %||% "", character(1))),
    selected_ids = as.integer(vapply(imported_items, function(item) item$cohort_definition_id, integer(1))),
    records = lapply(imported_items, selection_record_from_import)
  )
}

.studyAgentSlashCreateComputableRoleSelectionFresh <- function(role_label,
                                                          role_statement,
                                                          client,
                                                          output_dir,
                                                          imported_definition_dir,
                                                          interactive = TRUE,
                                                          readline_with_navigation = readline,
                                                          is_back_signal = function(value) FALSE,
                                                          write_json = function(x, path) jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE)) {
  if (!isTRUE(interactive)) stop("Creating a phenotype definition requires interactive scope and concept-set review.")
  if (!slashOhdsiAcpClient::acp_is_connected(client)) stop("ACP bridge unavailable; connect ACP before creating a phenotype definition.")
  artifact_dir <- file.path(output_dir, "phenotype-make-computable", tolower(role_label))
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  prompt <- function(text) {
    value <- readline_with_navigation(text)
    if (is_back_signal(value)) return(value)
    value <- trimws(as.character(value %||% ""))
    sub("^['\\\"](.*)['\\\"]$", "\\1", value)
  }
  default_narrative <- trimws(as.character(role_statement %||% ""))
  narrative <- prompt(sprintf("Working local OMOP cohort statement for the new %s [%s]: ", tolower(role_label), default_narrative))
  cat("You are authoring a new local OMOP cohort definition. This statement is not an imported source phenotype definition.\n")
  if (!nzchar(narrative)) narrative <- default_narrative
  if (!nzchar(narrative)) return(list(action = "retry"))
  checklist <- .studyAgentSlashAcpPhenotypeMakeComputable(client, narrative_statement = narrative, confirmed_scope = FALSE)
  write_json(checklist, file.path(artifact_dir, "scope-checklist.json"))
  source_conversion_dir <- file.path(output_dir, "phenotype-conversion", tolower(role_label))
  source_presentation_path <- file.path(source_conversion_dir, "presentation.json")
  source_mapping_path <- file.path(source_conversion_dir, "mapping-evidence.json")
  if (file.exists(source_presentation_path)) {
    source_presentation <- tryCatch(jsonlite::read_json(source_presentation_path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.list(source_presentation)) {
      source_title <- trimws(as.character(source_presentation$title %||% ""))
      source_summary <- gsub("\\s+", " ", trimws(as.character(source_presentation$plain_language_summary %||% "")), perl = TRUE)
      mapped_domains <- character(0)
      if (file.exists(source_mapping_path)) {
        source_mapping <- tryCatch(jsonlite::read_json(source_mapping_path, simplifyVector = FALSE), error = function(e) NULL)
        mapped_domains <- unique(unlist(lapply(source_mapping$code_results %||% list(), function(result) {
          vapply(result$standard_candidates %||% list(), function(candidate) as.character(candidate$domain_id %||% ""), character(1))
        }), use.names = FALSE))
        mapped_domains <- mapped_domains[nzchar(mapped_domains)]
      }
      cat("\nSource-informed scope suggestions (unconfirmed; nothing is prefilled):\n")
      if (nzchar(source_title)) cat(sprintf("- Consider whether the source candidate title helps name the index event: %s\n", source_title))
      if (length(mapped_domains)) cat(sprintf("- Mapped source evidence contains candidate OMOP domain(s): %s\n", paste(mapped_domains, collapse = ", ")))
      if (nzchar(source_summary)) cat(sprintf("- Source narrative evidence: %s\n", substr(source_summary, 1L, 600L)))
      cat("Use these only as review context. You must still choose and confirm every scope value.\n")
    }
  }
  cat("\nACP returned a scope checklist. No definition has been created.\n")
  cat("Answer each scope question deliberately. Press /back to return to cohort-source selection.\n")
  index_event <- prompt("Index event clinical term: ")
  if (is_back_signal(index_event)) return(index_event)
  domain <- prompt("Index event OMOP domain (Condition, Drug, Procedure, Measurement, Observation, Visit, or Device): ")
  if (is_back_signal(domain)) return(domain)
  entry_limit <- prompt("Entry-event limit [First or All]: ")
  if (is_back_signal(entry_limit)) return(entry_limit)
  if (!entry_limit %in% c("First", "All")) stop("Entry-event limit must be First or All.")
  repeat {
    prior_raw <- prompt("Required prior continuous observation days [0]: ")
    if (is_back_signal(prior_raw)) return(prior_raw)
    if (!nzchar(trimws(prior_raw))) {
      prior_observation <- 0L
      break
    }
    prior_observation <- suppressWarnings(as.integer(prior_raw))
    if (!is.na(prior_observation) && prior_observation >= 0L) break
    cat("Enter a non-negative whole number, or press Enter to use 0.\n")
  }
  vocabulary <- prompt("Optional index-event vocabulary restriction (for example RxNorm; press Enter for none): ")
  if (is_back_signal(vocabulary)) return(vocabulary)
  exit_strategy <- prompt("Exit strategy [observation]: ")
  if (is_back_signal(exit_strategy)) return(exit_strategy)
  if (!nzchar(exit_strategy)) exit_strategy <- "observation"
  if (!identical(exit_strategy, "observation")) stop("This guided path currently supports exit strategy observation only.")
  criterion_domains <- setNames(list(domain), index_event)
  scope <- list(index_event = index_event, criterion_domains = criterion_domains, entry_limit = entry_limit,
                prior_observation = prior_observation, index_day_boundary = "included", windows = "none",
                exit_strategy = exit_strategy, visit_overlap = FALSE)
  if (nzchar(vocabulary)) scope$criterion_vocabularies <- setNames(list(list(vocabulary)), index_event)
  composition_path <- file.path(output_dir, "phenotype-conversion", tolower(role_label), "composition-seed.json")
  composition <- if (file.exists(composition_path)) tryCatch(jsonlite::read_json(composition_path, simplifyVector = FALSE), error = function(e) NULL) else NULL
  emitter_support <- composition$emitter_support %||% list()
  use_temporal <- FALSE
  if (is.list(composition) && identical(composition$status %||% "", "unconfirmed") && identical(emitter_support$status %||% "", "supported")) {
    use_temporal <- tolower(prompt("Use the proposed exposure-followed-by-outcome template? [y/N] (y = require the follow-on Condition within the selected post-exposure window; Enter/N = continue without that temporal requirement): "))
    if (is_back_signal(use_temporal)) return(use_temporal)
    if (use_temporal %in% c("y", "yes")) {
      if (!identical(domain, "Drug")) stop("The supported exposure-followed-by-outcome template requires a Drug index event.")
      outcome_term <- prompt("Follow-on Condition clinical term [Cough]: ")
      if (is_back_signal(outcome_term)) return(outcome_term)
      if (!nzchar(outcome_term)) outcome_term <- "Cough"
      followup_raw <- prompt("Maximum days after exposure for the follow-on Condition [30]: ")
      if (is_back_signal(followup_raw)) return(followup_raw)
      followup_days <- if (!nzchar(followup_raw)) 30L else suppressWarnings(as.integer(followup_raw))
      if (is.na(followup_days) || followup_days < 0L) stop("Follow-on window must be a non-negative integer.")
      washout_raw <- prompt(sprintf("Clean-window days for exposure and outcome [%s]: ", prior_observation))
      if (is_back_signal(washout_raw)) return(washout_raw)
      washout_days <- if (!nzchar(washout_raw)) prior_observation else suppressWarnings(as.integer(washout_raw))
      if (is.na(washout_days) || washout_days < 1L || washout_days != prior_observation) stop("Clean-window days must be at least 1 and equal the confirmed prior-observation days.")
      scope$criterion_domains[[outcome_term]] <- "Condition"
      scope$temporal_followup <- list(index_concept_set = index_event, trigger_concept_set = outcome_term,
        followup_days = followup_days, washout_days = washout_days)
      scope$exit_strategy <- list(type = "fixed", index = "startDate", offset_days = 1L)
      cat("The temporal template defines the follow-on Condition; a separate supporting Condition is not requested.\n")
    }
  }
  supporting <- "no"
  if (!use_temporal %in% c("y", "yes")) {
    supporting <- tolower(prompt("Require a supporting Condition occurrence around the index event? [y/N]: "))
    if (is_back_signal(supporting)) return(supporting)
  }
  if (supporting %in% c("y", "yes")) {
    supporting_term <- prompt("Supporting Condition clinical term: ")
    if (is_back_signal(supporting_term)) return(supporting_term)
    start_days_raw <- prompt("Supporting-condition window start days relative to index (for example -180): ")
    if (is_back_signal(start_days_raw)) return(start_days_raw)
    end_days_raw <- prompt("Supporting-condition window end days relative to index [0]: ")
    if (is_back_signal(end_days_raw)) return(end_days_raw)
    start_days <- suppressWarnings(as.integer(start_days_raw))
    end_days <- if (!nzchar(end_days_raw)) 0L else suppressWarnings(as.integer(end_days_raw))
    if (is.na(start_days) || is.na(end_days) || start_days > end_days || end_days > 0L) stop("Use integer supporting-condition bounds with start <= end <= 0.")
    scope$criterion_domains[[supporting_term]] <- "Condition"
    scope$supporting_condition_occurrence <- list(concept_set = supporting_term, start_days = start_days, end_days = end_days, anchor = "index_start")
    scope$multi_domain_entry_policy <- "supporting_evidence_only"
  }
  write_json(scope, file.path(artifact_dir, "confirmed-scope.json"))
  cat(sprintf("Confirmed scope draft written to %s.\n", file.path(artifact_dir, "confirmed-scope.json")))
  .studyAgentSlashPmcPrintScope(scope)
  approved_scope <- prompt("Confirm scope [type CONFIRM; Enter or /back returns to cohort-source selection]: ")
  if (!identical(approved_scope, "CONFIRM")) {
    cat("Scope was not confirmed; returning to cohort-source selection.\n")
    return(list(action = "retry"))
  }
  conversion_dir <- file.path(output_dir, "phenotype-conversion", tolower(role_label))
  conversion_state_path <- file.path(conversion_dir, "conversion-state.json")
  conversion_state <- if (file.exists(conversion_state_path)) tryCatch(jsonlite::read_json(conversion_state_path, simplifyVector = FALSE), error = function(e) NULL) else NULL
  conversion_phenotype_id <- trimws(as.character(conversion_state$phenotype_id %||% ""))
  confirmed_domains <- unique(as.character(unlist(scope$criterion_domains %||% list(), use.names = FALSE)))
  confirmed_domains <- confirmed_domains[nzchar(confirmed_domains)]
  if (nzchar(conversion_phenotype_id) && length(confirmed_domains)) {
    refreshed_preparation <- .studyAgentSlashAcpPhenotypeConversionPrepare(
      client = client, phenotype_id = conversion_phenotype_id,
      recommendation_context = list(recommendation_role = tolower(role_label), workflow_type = "strategus"),
      expected_domains = confirmed_domains, check_vocabulary_database = TRUE
    )
    if (identical(as.character(refreshed_preparation$status %||% ""), "ok")) {
      write_json(refreshed_preparation$mapping_evidence %||% list(), file.path(conversion_dir, "mapping-evidence-confirmed-domains.json"))
      write_json(refreshed_preparation$mapping_evidence %||% list(), file.path(conversion_dir, "mapping-evidence.json"))
      cat(sprintf("Refreshed mapping evidence for confirmed OMOP domain(s): %s. This only updates the review evidence; no concepts have been selected.\n", paste(confirmed_domains, collapse = ", ")))
    } else {
      cat("Could not refresh mapping evidence for the confirmed domain; continuing without inferred mapping eligibility.\n")
    }
  }
  mapping_path <- file.path(conversion_dir, "mapping-evidence.json")
  source_path <- file.path(conversion_dir, "source-snapshot.json")
  if (file.exists(mapping_path) && file.exists(source_path)) {
    mapping_evidence <- tryCatch(jsonlite::read_json(mapping_path, simplifyVector = FALSE), error = function(e) NULL)
    source_snapshot <- tryCatch(jsonlite::read_json(source_path, simplifyVector = FALSE), error = function(e) NULL)
    mapping_review <- .studyAgentSlashPmcWriteMappingEvidenceReview(mapping_evidence %||% list(),
      as.character(source_snapshot$title %||% narrative), artifact_dir, write_json)
    if (is.list(mapping_review)) {
      coverage <- mapping_evidence$coverage %||% list()
      cat(sprintf("Mapping reconciliation: %s source code(s) checked; %s mapped, %s ambiguous, and %s unmatched. After deduplication, %s candidate concept(s) are available; %s are eligible for the confirmed domain in %s. These are review options only; no concepts have been selected.\n", coverage$requested_code_count %||% 0L, coverage$mapped_code_count %||% 0L, coverage$ambiguous_mapping_count %||% 0L, coverage$unmatched_source_code_count %||% 0L, mapping_review$candidate_count %||% 0L, mapping_review$eligible_candidate_count %||% 0L, mapping_review$csv))
      atlas_exports <- as.character(mapping_review$atlas_exports %||% character(0))
      atlas_mode <- as.character(mapping_review$atlas_recommendation %||% "optional")
      if (length(atlas_exports)) cat(sprintf("Atlas import file(s): %s\n", paste(atlas_exports, collapse = ", ")))
      if (identical(atlas_mode, "required")) {
        cat("More than 500 mapping candidates were returned. Atlas review is required before mapped source evidence can be used.\n")
        review_choice <- tolower(prompt("Concept review source [atlas=import in Atlas and return corrected JSON, search=run ACP vocabulary search]: "))
      } else {
        if (identical(atlas_mode, "strongly_recommended")) cat("More than 100 mapping candidates were returned. Atlas review is strongly recommended.\n")
        review_choice <- tolower(prompt("Concept review source [mapping=review mapped CSV, atlas=import in Atlas and return corrected JSON, search=run ACP vocabulary search]: "))
      }
      if (is_back_signal(review_choice)) return(review_choice)
      if (identical(review_choice, "atlas")) {
        chosen <- prompt("Corrected Atlas concept-set JSON path: ")
        if (is_back_signal(chosen)) return(chosen)
        corrected_sets <- .studyAgentSlashPmcExternalSets(chosen, narrative)
        approval_path <- file.path(artifact_dir, "mapping-concept-set-approval.json")
        write_json(list(review_id = mapping_review$review_id, source = "atlas_corrected_export", atlas_import_path = chosen,
          concept_sets = corrected_sets), approval_path)
        cat(sprintf("Atlas-corrected concept-set policy saved to %s.\n", approval_path))
        if (!identical(prompt("I explicitly approve this exact Atlas-corrected concept-set policy [type APPROVE]: "), "APPROVE")) return(list(action = "retry"))
        return(.studyAgentSlashPmcEmit(client, narrative, scope, corrected_sets, artifact_dir,
          imported_definition_dir, write_json, readline_with_navigation, approval_path = approval_path))
      }
      if (identical(review_choice, "mapping") && !identical(atlas_mode, "required")) {
        chosen <- prompt(sprintf("Reviewed mapping CSV path [%s]: ", mapping_review$csv))
        if (is_back_signal(chosen)) return(chosen)
        if (!nzchar(chosen)) chosen <- mapping_review$csv
        converted <- .studyAgentSlashPmcReviewCsv(chosen, mapping_review$manifest, mapping_review$review_id)
        .studyAgentSlashPmcPrintPreview(converted$approval_preview)
        approval_path <- file.path(artifact_dir, "mapping-concept-set-approval.json")
        write_json(list(review_id = mapping_review$review_id, concept_sets = converted$concept_sets,
          approval_preview = converted$approval_preview), approval_path)
        cat(sprintf("Exact mapping-derived policy saved to %s.\n", approval_path))
        if (!identical(prompt("I explicitly approve this exact concept-set policy [type APPROVE]: "), "APPROVE")) return(list(action = "retry"))
        return(.studyAgentSlashPmcEmit(client, narrative, scope, converted$concept_sets, artifact_dir,
          imported_definition_dir, write_json, readline_with_navigation, approval_path = approval_path))
      }
    }
  }
  write_json(list(narrative_statement = narrative, confirmed_scope = TRUE,
    concept_review_mode = "required", concept_build_mode = "search_only", review_delivery = "session",
    candidate_limit = 20L, concept_sets = list(), scope = scope),
    file.path(artifact_dir, "concept-review-request.json"))
  review <- .studyAgentSlashAcpPhenotypeMakeComputable(
    client, narrative_statement = narrative, confirmed_scope = TRUE, scope = scope,
    concept_review_mode = "required", review_delivery = "session", candidate_limit = 20
  )
  write_json(review, file.path(artifact_dir, "concept-review-response.json"))
  .studyAgentSlashPmcReviewHandoff(
    role_label = role_label, narrative = narrative, scope = scope, review = review,
    client = client, artifact_dir = artifact_dir, imported_definition_dir = imported_definition_dir,
    readline_with_navigation = readline_with_navigation, is_back_signal = is_back_signal,
    write_json = write_json
  )
}
