#' Validate Strategus execution configuration before database work begins
#'
#' This check validates local configuration and cohort role mappings without
#' opening a database connection. It is intentionally required before database
#' dependent workflow scripts run.
#' @param base_dir workflow project directory
#' @return a list containing validated database, execution, and cohort mapping settings
#' @export
validateStrategusExecutionConfiguration <- function(base_dir = getwd()) {
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  db_details_path <- file.path(base_dir, "strategus-db-details.json")
  execution_settings_path <- file.path(base_dir, "strategus-execution-settings.json")
  cohort_id_map_path <- file.path(base_dir, "outputs", "cohort_id_map.json")
  cohort_roles_path <- file.path(base_dir, "outputs", "cohort_roles.json")
  problems <- character(0)
  is_blank <- function(value) {
    is.null(value) || length(value) == 0L || !nzchar(trimws(as.character(value[[1]])))
  }

  raw_db_details <- tryCatch(readStrategusDbDetails(db_details_path), error = function(e) e)
  db_details <- NULL
  if (inherits(raw_db_details, "error")) {
    problems <- c(problems, conditionMessage(raw_db_details))
  } else {
    if (is_blank(raw_db_details$DB_SERVER %||% raw_db_details$server)) {
      problems <- c(problems, "Database server must be provided in strategus-db-details.json (DB_SERVER or server).")
    }
    auth_type <- tolower(trimws(as.character(raw_db_details$authType %||% raw_db_details$authenticationType %||% "username_password")))
    integrated_auth <- auth_type %in% c("windows", "integrated", "integrated_windows")
    if (!integrated_auth && is_blank(raw_db_details$DB_USER %||% raw_db_details$user)) {
      problems <- c(problems, "Database username must be provided unless authType requests integrated Windows authentication.")
    }
    if (!integrated_auth && is_blank(raw_db_details$DB_PASS %||% raw_db_details$password)) {
      problems <- c(problems, "Database password must be provided unless authType requests integrated Windows authentication.")
    }
    if (!length(problems)) {
      db_details <- tryCatch(createStrategusConnectionDetails(path = db_details_path), error = function(e) e)
      if (inherits(db_details, "error")) {
        problems <- c(problems, conditionMessage(db_details))
        db_details <- NULL
      }
    }
  }

  raw_execution <- tryCatch(readStrategusExecutionSettings(execution_settings_path), error = function(e) e)
  execution <- NULL
  if (inherits(raw_execution, "error")) {
    problems <- c(problems, conditionMessage(raw_execution))
  } else {
    required_execution_fields <- c("cdmDatabaseSchema", "workDatabaseSchema", "resultsDatabaseSchema", "vocabularyDatabaseSchema", "cohortTable", "workFolder", "resultsFolder")
    missing_execution_fields <- required_execution_fields[vapply(required_execution_fields, function(name) is_blank(raw_execution[[name]]), logical(1))]
    if (length(missing_execution_fields)) {
      problems <- c(problems, paste0("strategus-execution-settings.json has blank required field(s): ", paste(missing_execution_fields, collapse = ", ")))
    } else {
      execution <- tryCatch(createStrategusExecutionSettings(path = execution_settings_path), error = function(e) e)
      if (inherits(execution, "error")) {
        problems <- c(problems, conditionMessage(execution))
        execution <- NULL
      }
    }
  }

  mapping <- NULL
  roles <- NULL
  if (!file.exists(cohort_id_map_path)) {
    problems <- c(problems, paste0("Cohort id map not found: ", cohort_id_map_path))
  } else {
    mapping_payload <- tryCatch(jsonlite::read_json(cohort_id_map_path, simplifyVector = TRUE), error = function(e) e)
    mapping <- if (inherits(mapping_payload, "error")) NULL else mapping_payload$mapping %||% mapping_payload
    if (inherits(mapping_payload, "error") || is.null(mapping) || NROW(mapping) == 0) {
      problems <- c(problems, "cohort_id_map.json must contain one or more role-to-cohort mappings.")
    } else {
      mapping <- as.data.frame(mapping, stringsAsFactors = FALSE)
      required_mapping_columns <- c("cohort_id", "role")
      missing_columns <- setdiff(required_mapping_columns, names(mapping))
      if (length(missing_columns)) {
        problems <- c(problems, paste0("cohort_id_map.json is missing column(s): ", paste(missing_columns, collapse = ", ")))
      } else {
        cohort_ids <- suppressWarnings(as.integer(mapping$cohort_id))
        if (anyNA(cohort_ids) || any(cohort_ids <= 0L)) {
          problems <- c(problems, "cohort_id_map.json contains a missing or non-positive cohort_id.")
        }
      }
    }
  }
  if (!file.exists(cohort_roles_path)) {
    problems <- c(problems, paste0("Cohort roles file not found: ", cohort_roles_path))
  } else {
    roles <- tryCatch(jsonlite::read_json(cohort_roles_path, simplifyVector = TRUE), error = function(e) e)
    if (inherits(roles, "error") || !is.list(roles)) {
      problems <- c(problems, "cohort_roles.json must contain target and outcome role assignments.")
      roles <- NULL
    }
  }

  if (!is.null(mapping) && !is.null(roles) && all(c("cohort_id", "role") %in% names(mapping))) {
    mapped_ids <- suppressWarnings(as.integer(mapping$cohort_id))
    mapped_roles <- tolower(trimws(as.character(mapping$role)))
    role_ids <- function(name) suppressWarnings(as.integer(unlist(roles[[name]] %||% integer(0), use.names = FALSE)))
    required_role_names <- c("targets", "outcomes")
    if (!is.null(roles$comparators)) required_role_names <- c(required_role_names, "comparators")
    expected_mapping_roles <- c(targets = "target", comparators = "comparator", outcomes = "outcome")
    for (role_name in required_role_names) {
      ids <- role_ids(role_name)
      if (!length(ids) || anyNA(ids) || any(ids <= 0L)) {
        problems <- c(problems, sprintf("cohort_roles.json must contain at least one positive %s cohort id.", role_name))
      } else if (!all(ids %in% mapped_ids)) {
        problems <- c(problems, sprintf("cohort_roles.json %s id(s) are absent from cohort_id_map.json.", role_name))
      } else if (!all(ids %in% mapped_ids[mapped_roles == expected_mapping_roles[[role_name]]])) {
        problems <- c(problems, sprintf("cohort_roles.json %s id(s) do not have the matching role in cohort_id_map.json.", role_name))
      }
    }
  }

  if (length(problems)) {
    stop(
      paste0(
        "Pre-execution configuration validation failed. Review strategus-db-details.json, strategus-execution-settings.json, and cohort role mappings:\n- ",
        paste(unique(problems), collapse = "\n- ")
      ),
      call. = FALSE
    )
  }
  list(
    base_dir = base_dir,
    db_details_path = db_details_path,
    execution_settings_path = execution_settings_path,
    cohort_id_map_path = cohort_id_map_path,
    cohort_roles_path = cohort_roles_path,
    connection_details = db_details,
    execution_settings = execution,
    mapping = mapping,
    roles = roles
  )
}
