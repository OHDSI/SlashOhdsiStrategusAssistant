test_that("ACP Circe JSON is cached with durable provenance", {
  cache_dir <- tempfile("acp-cache-")
  definition <- list(PrimaryCriteria = list(), ConceptSets = list())
  imported <- slashOhdsiStrategusAssistant:::.studyAgentSlashImportAcpCohortDefinition(
    list(
      phenotype_id = "42",
      phenotype_name = "Example ACP cohort",
      justification = "Recommended for the stated role.",
      json = jsonlite::toJSON(definition, auto_unbox = TRUE)
    ),
    cache_dir
  )

  expect_identical(imported$source_id, "acp:42")
  expect_identical(imported$metadata$source_type, "acp")
  expect_identical(imported$metadata$computability_status, "circe_json")
  expect_true(file.exists(imported$cache_path))
  expect_true(file.exists(imported$alias_path))
})

test_that("local Circe JSON is cached and copied from the acquired source", {
  source_dir <- tempfile("circe-source-")
  cache_dir <- tempfile("circe-cache-")
  destination_dir <- tempfile("circe-destination-")
  dir.create(source_dir)
  definition_path <- file.path(source_dir, "example.json")
  jsonlite::write_json(list(id = 73, name = "Example local cohort", PrimaryCriteria = list(), ConceptSets = list()), definition_path, auto_unbox = TRUE)

  imported <- slashOhdsiStrategusAssistant:::.studyAgentSlashImportFileCohortDefinition(definition_path, cache_dir)
  copied <- slashOhdsiStrategusAssistant:::.studyAgentSlashCopyCohortJsonMulti(
    imported$source_id,
    9001L,
    destination_dir,
    imported_def_dir = cache_dir,
    ensure_dir = function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  )

  expect_true(file.exists(imported$cache_path))
  expect_identical(copied, file.path(destination_dir, "9001.json"))
  expect_true(file.exists(copied))
})

test_that("acquired Phenotype Library source IDs resolve locally without an index", {
  cache_dir <- tempfile("pl-cache-")
  dir.create(cache_dir)
  source_path <- slashOhdsiStrategusAssistant:::.studyAgentSlashImportedCohortDefinitionPath("pl:123", cache_dir)
  alias_path <- slashOhdsiStrategusAssistant:::.studyAgentSlashImportedCohortAliasPath(123L, cache_dir)
  jsonlite::write_json(list(PrimaryCriteria = list(), ConceptSets = list()), source_path, auto_unbox = TRUE)
  file.copy(source_path, alias_path)

  expect_identical(slashOhdsiStrategusAssistant:::.studyAgentSlashPhenotypeDefinitionPath("pl:123", cache_dir), source_path)
  expect_identical(slashOhdsiStrategusAssistant:::.studyAgentSlashPhenotypeDefinitionPath("123", cache_dir), alias_path)
  expect_identical(slashOhdsiStrategusAssistant:::.studyAgentSlashDefaultCohortIdFromSource("pl:123"), 123L)
})


test_that("ACP OHDSI identifiers preserve their numeric cohort ID", {
  cache_dir <- tempfile("acp-ohdsi-")
  definition <- list(PrimaryCriteria = list(), ConceptSets = list())
  imported <- slashOhdsiStrategusAssistant:::.studyAgentSlashImportAcpCohortDefinition(
    list(phenotype_id = "ohdsi:42", circe_json = jsonlite::toJSON(definition, auto_unbox = TRUE)), cache_dir
  )
  expect_identical(imported$source_id, "acp:42")
  expect_identical(imported$metadata$upstream_id, "ohdsi:42")
  expect_true(is.null(slashOhdsiStrategusAssistant:::.studyAgentSlashAcpRecommendationJson(list(phenotype_id = "cipher:alpha"))))
  expect_true(is.list(slashOhdsiStrategusAssistant:::.studyAgentSlashAcpRecommendationJson(list(
    phenotype_id = "cipher:alpha", json = jsonlite::toJSON(definition, auto_unbox = TRUE)
  ))))
  expect_identical(slashOhdsiStrategusAssistant:::.studyAgentSlashAcpRecommendationCohortId(
    list(cohort_id = 43L), definition
  ), 43L)
})

test_that("conflicting numeric aliases do not overwrite acquired definitions", {
  cache_dir <- tempfile("alias-conflict-")
  first <- list(source_id = "acp:77", cohort_definition_id = 77L,
                cohort_json = list(PrimaryCriteria = list(), ConceptSets = list()))
  second <- list(source_id = "pl:77", cohort_definition_id = 77L,
                 cohort_json = list(PrimaryCriteria = list(ObservationWindow = list()), ConceptSets = list()))
  one <- slashOhdsiStrategusAssistant:::.studyAgentSlashCacheCohortDefinition(first, cache_dir)
  two <- slashOhdsiStrategusAssistant:::.studyAgentSlashCacheCohortDefinition(second, cache_dir)
  expect_true(file.exists(one$alias_path))
  expect_true(is.na(two$alias_path))
  expect_true(file.exists(slashOhdsiStrategusAssistant:::.studyAgentSlashImportedCohortDefinitionPath("acp:77", cache_dir)))
  expect_true(file.exists(slashOhdsiStrategusAssistant:::.studyAgentSlashImportedCohortDefinitionPath("pl:77", cache_dir)))
  expect_error(slashOhdsiStrategusAssistant:::.studyAgentSlashPhenotypeDefinitionPath("77", cache_dir), "ambiguous")
})

test_that("local source identifiers include a content token", {
  source_dir <- tempfile("source-token-")
  dir.create(source_dir)
  path <- file.path(source_dir, "same.json")
  jsonlite::write_json(list(id = 88, PrimaryCriteria = list(), ConceptSets = list()), path, auto_unbox = TRUE)
  source_id <- slashOhdsiStrategusAssistant:::.studyAgentSlashReadFileCohortDefinition(path)$source_id
  expect_match(source_id, "^file:88:same-[a-f0-9]{12}$")
})
