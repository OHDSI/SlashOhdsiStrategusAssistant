test_that("pre-execution validation reports missing database, schema, and mapping settings together", {
  base_dir <- tempfile("pre-execution-validation-")
  dir.create(base_dir, recursive = TRUE)
  jsonlite::write_json(list(), file.path(base_dir, "strategus-db-details.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(), file.path(base_dir, "strategus-execution-settings.json"), auto_unbox = TRUE)

  failure <- tryCatch(
    {
      validateStrategusExecutionConfiguration(base_dir)
      NULL
    },
    error = identity
  )

  expect_s3_class(failure, "error")
  message <- conditionMessage(failure)
  expect_match(message, "Database server", fixed = TRUE)
  expect_match(message, "Database username", fixed = TRUE)
  expect_match(message, "cdmDatabaseSchema", fixed = TRUE)
  expect_match(message, "cohort_id_map.json", fixed = TRUE)
  expect_match(message, "cohort_roles.json", fixed = TRUE)
})

test_that("pre-execution validation rejects role ids absent from the cohort map", {
  base_dir <- tempfile("pre-execution-role-validation-")
  dir.create(file.path(base_dir, "outputs"), recursive = TRUE)
  jsonlite::write_json(
    list(dbms = "postgresql", server = "localhost", authType = "integrated"),
    file.path(base_dir, "strategus-db-details.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(
      cdmDatabaseSchema = "cdm",
      workDatabaseSchema = "work",
      resultsDatabaseSchema = "results",
      vocabularyDatabaseSchema = "vocabulary",
      cohortTable = "cohort",
      workFolder = "work",
      resultsFolder = "results"
    ),
    file.path(base_dir, "strategus-execution-settings.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(mapping = data.frame(cohort_id = 1L, role = "target")),
    file.path(base_dir, "outputs", "cohort_id_map.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(targets = 1L, comparators = 2L, outcomes = 3L),
    file.path(base_dir, "outputs", "cohort_roles.json"), auto_unbox = TRUE
  )

  failure <- tryCatch(
    {
      validateStrategusExecutionConfiguration(base_dir)
      NULL
    },
    error = identity
  )

  expect_s3_class(failure, "error")
  message <- conditionMessage(failure)
  expect_match(message, "comparators id(s) are absent", fixed = TRUE)
  expect_match(message, "outcomes id(s) are absent", fixed = TRUE)
})

test_that("pre-execution validation requires cohort roles to agree with the cohort map", {
  base_dir <- tempfile("pre-execution-role-alignment-")
  dir.create(file.path(base_dir, "outputs"), recursive = TRUE)
  jsonlite::write_json(
    list(dbms = "postgresql", server = "localhost", authType = "integrated"),
    file.path(base_dir, "strategus-db-details.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(
      cdmDatabaseSchema = "cdm", workDatabaseSchema = "work",
      resultsDatabaseSchema = "results", vocabularyDatabaseSchema = "vocabulary",
      cohortTable = "cohort", workFolder = "work", resultsFolder = "results"
    ),
    file.path(base_dir, "strategus-execution-settings.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(mapping = data.frame(cohort_id = c(1L, 2L, 3L), role = c("target", "comparator", "target"))),
    file.path(base_dir, "outputs", "cohort_id_map.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(targets = 1L, comparators = 2L, outcomes = 3L),
    file.path(base_dir, "outputs", "cohort_roles.json"), auto_unbox = TRUE
  )

  expect_error(
    validateStrategusExecutionConfiguration(base_dir),
    "outcomes id\\(s\\) do not have the matching role"
  )
})
