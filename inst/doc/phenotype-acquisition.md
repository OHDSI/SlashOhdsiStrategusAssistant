# Cohort-definition acquisition

## Purpose

`runStrategusIncidenceShell()` and `runStrategusCohortMethodsShell()` acquire an
executable OHDSI Circe cohort definition before generating Strategus artifacts. The
workflow records and uses its own saved JSON copy; later execution does not need the
original provider.

## Providers

### Phenotype Library

When the user selects `pl`, the shell requires the optional `PhenotypeLibrary` package.
It uses `PhenotypeLibrary::getPhenotypeLog()` to present available cohort definitions,
then retrieves selected IDs with `PhenotypeLibrary::getPlCohortDefinitionSet()`. The
returned `cohortName`, JSON, and SQL metadata are retained in workflow provenance; the
validated Circe JSON is saved locally.

### Local files and directories

A local source must contain a Circe `SIMPLE_EXPRESSION` JSON object. The package
validates the JSON and copies it into the workflow. Directory selection presents valid
JSON definitions found in the requested directory.

### Existing database definitions

Database acquisition reads a cohort definition from the configured cohort source. Only
`SIMPLE_EXPRESSION` definitions with a valid Circe JSON payload are executable in this
workflow and can be imported.

### ACP recommendations

With AI enabled, ACP recommendations are usable only when they include a computable
Circe JSON payload. The shell validates and saves that payload as an ACP-acquired
workflow artifact. Returning only a PhenotypeLibrary ID is deliberately insufficient:
it would make reproducibility depend on an unverified ACP/PhenotypeLibrary version
relationship.

Non-computable recommendations require an ACP conversion workflow that returns
validated Capr-derived Circe JSON. Until that ACP endpoint and response contract are
available to this package, such recommendations must not be selected.

## Output location and Strategus execution roots

Set `outputDir` to an absolute workflow directory. The shell writes its selected cohort
artifacts, generated scripts, and configuration files there. Its initial
`strategus-execution-settings.json` also sets `workFolder` to `<outputDir>/work` and
`resultsFolder` to `<outputDir>/results`. To use separate execution storage, edit those
two settings in that generated JSON file before running generated Strategus scripts.

## Workflow artifacts

The shell stores acquired definitions in `imported-cohort-definitions/` under a
namespaced source ID (`pl:`, `acp:`, `db:`, `file:`, or `dir:`), and then copies
role-specific definitions into `selected-*-cohorts/` and the combined
`selected-cohorts/` directory. Numeric JSON filenames are convenience aliases only.
If different providers supply different JSON for the same numeric ID, the alias is
marked ambiguous and the workflow requires the namespaced source ID rather than
silently choosing one definition. `selected_cohort_sources.json` records source type,
source ID, cohort name, logic description, and local cache path. Generated replay
scripts copy only from these workflow-local artifacts.

## No-AI use

`aiSupport = "disabled"` does not require ACP, a phenotype index, or an `indexDir`.
Users choose a Phenotype Library definition, local JSON file/directory, or a database
cohort definition. `PhenotypeLibrary` is required only for the Phenotype Library option.

### Creating a new computable phenotype

In AI-enabled workflows, select `create` at a role's cohort-source prompt when the
recommendations are not suitable. The shell calls the review-gated
It writes a scope checklist and then asks for each supported scope decision one at a
time. The resulting confirmed scope JSON, ACP candidate CSV and manifest are saved
under `phenotype-make-computable/<role>/`. The shell requires explicit approval of a
policy-bearing `concept_sets` JSON file before emission. The generated Capr source,
Circe JSON response, and technical validation evidence are persisted; the Circe
definition is imported as a normal local cohort source. Technical validation is not
clinical validation.
