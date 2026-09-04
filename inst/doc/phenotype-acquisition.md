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
`phenotype_make_computable` flow and asks for each supported scope decision one at a
time. It saves the confirmed scope, ACP response, frozen candidate CSV, manifest, and
`review-state.json` under `phenotype-make-computable/<role>/`.

Review can continue in the same shell by editing only the CSV `review_*` columns and
returning its path, or by leaving the shell and later selecting `create` again to resume
from those local artifacts. The shell converts the marked CSV to an exact policy object,
displays it, saves an approval record, and requires `APPROVE` before emission. If ACP
returns no candidates, the user can provide an Atlas/ACP concept-set JSON object or
return to the normal cohort-source menu. Remote review URLs are not needed to resume a
downloaded package. Generated Capr source, Circe JSON, and technical validation evidence
are also persisted. The shell saves local and ACP validation-environment reports, warns on major Capr/CirceR/SqlRender version mismatches, and can save `cohort-definition-readable.txt` from `CirceR::cohortPrintFriendly()`. Technical validation is not clinical validation.

Vocabulary review begins with 20 candidates per lane. The shell displays returned and exact matched counts plus truncation, and can request a fresh complete session through 500 candidates. For broader sets it can request a 500-candidate slice, or users should narrow the search or review in Atlas.
