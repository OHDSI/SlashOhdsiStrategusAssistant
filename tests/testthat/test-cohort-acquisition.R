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

test_that("source selector offers resume for a pending concept review", {
  prompts <- character()
  choice <- slashOhdsiStrategusAssistant:::.studyAgentSlashChooseSelectionSourceMode(
    role_label = "outcome",
    allow_index = TRUE,
    interactive = TRUE,
    resume_pending = TRUE,
    readline_with_navigation = function(prompt) {
      prompts <<- c(prompts, prompt)
      "resume"
    },
    is_back_signal = function(value) FALSE
  )
  expect_identical(choice, "resume")
  expect_match(prompts[[1L]], "`resume`   Continue the saved Atlas review", fixed = TRUE)
})


test_that("AI cohort source selector exposes reviewed phenotype creation", {
  choice <- slashOhdsiStrategusAssistant:::.studyAgentSlashChooseSelectionSourceMode(
    role_label = "target",
    allow_index = TRUE,
    interactive = TRUE,
    readline_with_navigation = function(prompt) "create",
    is_back_signal = function(value) FALSE
  )
  expect_identical(choice, "create")
})

test_that("create acquisition dispatch preserves a handled reviewed result", {
  result <- slashOhdsiStrategusAssistant:::.studyAgentSlashAcquireImportedRoleSelection(
    source_mode = "create",
    role_label = "target",
    interactive = TRUE,
    prompt_database_imports = function(...) NULL,
    prompt_file_imports = function(...) NULL,
    prompt_directory_imports = function(...) NULL,
    prompt_phenotype_library_imports = function(...) NULL,
    prompt_create_computable = function(...) list(action = "handled", imported = list(), selected_source_ids = "acp:99", selected_ids = 99L, records = list()),
    selection_record_from_import = function(x) x
  )
  expect_identical(result$selected_source_ids, "acp:99")
})

test_that("reviewed concept CSV becomes an explicit policy object", {
  review_dir <- tempfile("pmc-review-")
  dir.create(review_dir)
  csv_path <- file.path(review_dir, "concept-review.csv")
  manifest_path <- file.path(review_dir, "concept-review-manifest.json")
  columns <- c("concept_set_name", "concept_id", "concept_name", "domain", "standard_concept", "standard_concept_status", "assessment_status", "precision_eligible", "relationship_evidence", "review_include_concept", "review_include_descendants", "review_include_mapped", "review_exclude_concepts", "review_exclude_descendants", "review_exclude_mapped")
  row <- as.list(setNames(rep("", length(columns)), columns))
  row$concept_set_name <- "Warfarin"; row$concept_id <- "1310149"; row$concept_name <- "warfarin"; row$domain <- "Drug"; row$standard_concept <- "S"; row$standard_concept_status <- "Standard"; row$review_include_concept <- "x"; row$review_include_descendants <- "x"
  utils::write.csv(as.data.frame(row, check.names = FALSE), csv_path, row.names = FALSE, na = "")
  jsonlite::write_json(list(review_id = "review-1"), manifest_path, auto_unbox = TRUE)
  result <- slashOhdsiStrategusAssistant:::.studyAgentSlashPmcReviewCsv(csv_path, manifest_path, "review-1")
  expect_equal(result$concept_sets[[1]]$name, "Warfarin")
  expect_true(result$concept_sets[[1]]$items[[1]]$include_descendants)
  expect_false(result$concept_sets[[1]]$items[[1]]$is_excluded)
})

test_that("reviewed concept CSV rejects policy marks without a root decision", {
  review_dir <- tempfile("pmc-review-invalid-")
  dir.create(review_dir)
  csv_path <- file.path(review_dir, "concept-review.csv")
  manifest_path <- file.path(review_dir, "concept-review-manifest.json")
  columns <- c("concept_set_name", "concept_id", "concept_name", "domain", "standard_concept", "standard_concept_status", "assessment_status", "precision_eligible", "relationship_evidence", "review_include_concept", "review_include_descendants", "review_include_mapped", "review_exclude_concepts", "review_exclude_descendants", "review_exclude_mapped")
  row <- as.list(setNames(rep("", length(columns)), columns))
  row$concept_set_name <- "Warfarin"; row$concept_id <- "1310149"; row$concept_name <- "warfarin"; row$domain <- "Drug"; row$review_include_descendants <- "x"
  utils::write.csv(as.data.frame(row, check.names = FALSE), csv_path, row.names = FALSE, na = "")
  jsonlite::write_json(list(review_id = "review-2"), manifest_path, auto_unbox = TRUE)
  expect_error(slashOhdsiStrategusAssistant:::.studyAgentSlashPmcReviewCsv(csv_path, manifest_path, "review-2"), "without review_include_concept")
})


test_that("source selector presents readable choices and a pending Atlas resume", {
  prompts <- character()
  choice <- slashOhdsiStrategusAssistant:::.studyAgentSlashChooseSelectionSourceMode(
    role_label = "outcome",
    allow_index = TRUE,
    interactive = TRUE,
    resume_pending = TRUE,
    readline_with_navigation = function(prompt) {
      prompts <<- c(prompts, prompt)
      "resume"
    },
    is_back_signal = function(value) FALSE
  )
  expect_identical(choice, "resume")
  expect_match(prompts[[1L]], "Source for outcome cohort", fixed = TRUE)
  expect_match(prompts[[1L]], "Options:", fixed = TRUE)
  expect_match(prompts[[1L]], "`resume`   Continue the saved Atlas review", fixed = TRUE)
  expect_false(grepl("\\[ai=agentic", prompts[[1L]]))
})

test_that("cohort role summary retains all known role statements", {
  output <- capture.output(
    slashOhdsiStrategusAssistant:::.studyAgentSlashPrintCohortRoleSummary(
      "comparator",
      list(target = "new users of atomoxetine", comparator = "new users of methylphenidate", outcome = "venous thromboembolism")
    )
  )
  expect_match(paste(output, collapse = "\n"), "Study plan", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "Target: new users of atomoxetine", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "Comparator: new users of methylphenidate", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "Now selecting the comparator cohort.", fixed = TRUE)
})

test_that("numbered phenotype review shows details and requires explicit acceptance", {
  values <- c("2", "y")
  position <- 0L
  read_value <- function(prompt) {
    position <<- position + 1L
    values[[position]]
  }
  recommendations <- list(
    list(phenotype_name = "First candidate", phenotype_id = "ohdsi:1"),
    list(phenotype_name = "Second candidate", phenotype_id = "ohdsi:2", computability_status = "Circe available", justification = "Matches the requested outcome.")
  )
  output <- capture.output(
    selected <- slashOhdsiStrategusAssistant:::.studyAgentSlashChoosePhenotypeRecommendationIndices(
      recommendations = recommendations,
      role_label = "outcome",
      allow_multiple = FALSE,
      readline_with_navigation = read_value,
      is_back_signal = function(value) FALSE
    )
  )
  expect_identical(selected, 2L)
  rendered <- paste(output, collapse = "\n")
  expect_match(rendered, "Candidate details", fixed = TRUE)
  expect_match(rendered, "Second candidate", fixed = TRUE)
})

test_that("numbered phenotype review permits no relevant candidate", {
  selected <- slashOhdsiStrategusAssistant:::.studyAgentSlashChoosePhenotypeRecommendationIndices(
    recommendations = list(list(phenotype_name = "Only candidate", phenotype_id = "ohdsi:1")),
    role_label = "target",
    readline_with_navigation = function(prompt) "0",
    is_back_signal = function(value) FALSE
  )
  expect_identical(selected, integer(0))
})
