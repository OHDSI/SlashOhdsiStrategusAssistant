# slashOhdsiStrategusAssistant

## Introduction

`slashOhdsiStrategusAssistant` is an R package for designing,
configuring, and executing OHDSI Strategus workflows. It creates
durable workflow artifacts and generated scripts, and optionally
integrates with ACP-based assistance from the [OHDSI
StudyAgent](https://github.com/OHDSI/StudyAgent) project.

## Features

- Interactive Strategus incidence and CohortMethod shell entrypoints (more are planned).
- Workflow-stage context, checkpoints, resumable execution, and artifact tracking.
- Cohort-definition acquisition from the [OHDSI Phenotype Library](https://github.com/OHDSI/PhenotypeLibrary), local [Circe JSON](https://github.com/OHDSI/CirceR) files,
  directories of Circe JSON, existing database cohort definitions, or StudyAgent ACP responses.
- Workflow-local cohort JSON artifacts, so generated Strategus scripts do not depend
  on the original cohort provider at execution time.
- Review-gated narrative phenotype creation retains durable CSV/manifest review state, supports complete review sessions through 500 candidates, records ACP/local technical-validation environment comparisons, and can save a print-friendly Circe definition.
- Keeper concept-set preparation is separate from row-level Keeper case review; the latter is deferred until generated cohort rows are available.
- Optional StudyAgent ACP assistance for phenotype recommendations, conversion to validated [CapR](https://github.com/OHDSI/Capr) and Circe JSON of non-computable phenotypes indexed by the ACP or narrative phenotype descriptions, and [Keeper](https://github.com/OHDSI/Keeper) phenotype definition validation workflows.
- Strategus database/execution-settings helpers and a read-only Shiny artifact browser.
- Runtime compatibility checks against the tested HADES profile.

## Examples

Repository example scripts include:

- `extras/demo_strategus_incidence_rate_no_ai.R`
- `extras/demo_strategus_incidence_rate.R`
- `extras/demo_strategus_cohort_method_no_ai.R`
- `extras/demo_strategus_cohort_method.R`

For example, start an interactive no-AI incidence workflow with:

```r
slashOhdsiStrategusAssistant::runStrategusIncidenceShell(
  outputDir = "my-incidence-workflow",
  aiSupport = "disabled"
)
```

## Technology

The package is written in R and uses the [OHDSI HADES ecosystem](https://ohdsi.github.io/Hades/), including [Strategus](https://github.com/OHDSI/Strategus),
[CohortGenerator](https://github.com/OHDSI/CohortGenerator), [CohortIncidence](https://github.com/OHDSI/CohortIncidence), and [CohortMethod](https://github.com/OHDSI/CohortMethod). ACP features are optional and are provided through the optional [slashOhdsiAcpClient](https://github.com/OHDSI/slashOhdsiAcpClient) package.

## System Requirements

- R 4.4 or later.
- The HADES packages declared in `DESCRIPTION`; the tested runtime profile is recorded
  in `inst/hades-runtime.json`.
- Optional: `PhenotypeLibrary` to acquire cohorts from the OHDSI Phenotype Library.
- Optional: `slashOhdsiAcpClient` for ACP-assisted workflows.
- Optional: `shiny` for the artifact browser.

## Getting Started

Install the package and its HADES dependencies, then inspect the active runtime:

```r
# install.packages("devtools")
devtools::install_github("ohdsi/slashOhdsiStrategusAssistant")
library(slashOhdsiStrategusAssistant)
strategusRuntimeReport()
```

Start either `runStrategusIncidenceShell()` or
`runStrategusCohortMethodsShell()`. Both default to `aiSupport = "disabled"`; set it
to `"enabled"` to require ACP or `"auto"` to use ACP when its optional client package
is installed.

(Advanced) If you are manually updating to a newer version rather than the recommend standard  renv approach: 
```
package_name <- "slashOhdsiStrategusAssistant"
library_loc <- ""  # fill in the location
if (paste0("package:", package_name) %in% search()) {
  detach(paste0("package:", package_name), unload = TRUE, character.only = TRUE)
}

if (package_name %in% loadedNamespaces()) {
  unloadNamespace(package_name)
}

if (dir.exists(file.path(library_loc, package_name))) {
  remove.packages(package_name, lib = library_loc)
}
```


## User Documentation

A PDF version of the package manual will be available after the package is published:

- Package manual: [GitHub Pages package site](https://ohdsi.github.io/SlashOhdsiStrategusAssistant/)
- [Incidence shell workflow guide](https://github.com/OHDSI/SlashOhdsiStrategusAssistant/blob/main/inst/doc/R_STRATEGUS_INCIDENCE_SHELL.md)
- [Cohort Methods shell workflow guide](https://github.com/OHDSI/SlashOhdsiStrategusAssistant/blob/main/inst/doc/R_STRATEGUS_COHORT_METHODS_SHELL.md)
- [Cohort-definition acquisition guide](https://github.com/OHDSI/SlashOhdsiStrategusAssistant/blob/main/inst/doc/phenotype-acquisition.md)

The three Markdown guides are installed with the package and can also be located from
R with `system.file("doc", ..., package = "slashOhdsiStrategusAssistant")`.

### Cohort-definition acquisition

The shells acquire every usable cohort definition into the workflow directory before
Strategus scripts are generated. Consequently, generated workflows do not depend on an
ACP index, an installed PhenotypeLibrary package, a source database, or the original
local JSON file when they are executed later.

| Provider | Selection and acquisition | Stored workflow artifact |
| --- | --- | --- |
| Phenotype Library | Browse `PhenotypeLibrary::getPhenotypeLog()` and retrieve selected IDs with `PhenotypeLibrary::getPlCohortDefinitionSet()` | Validated Circe JSON in `imported-cohort-definitions/` |
| Local JSON | Choose a path to a Circe cohort definition JSON file | Validated copy in `imported-cohort-definitions/` |
| Database | Choose an existing `SIMPLE_EXPRESSION` cohort definition from the configured source database | Retrieved and validated Circe JSON in `imported-cohort-definitions/` |
| ACP | Select an AI recommendation that includes computable Circe JSON | Saved ACP Circe JSON in `imported-cohort-definitions/` |

With `aiSupport = "disabled"`, the available choices are Phenotype Library, local JSON,
a JSON directory, and a configured database cohort. PhenotypeLibrary is optional until a
user selects that provider. ACP recommendations are intentionally self-contained: the
assistant must return Circe JSON rather than only a PhenotypeLibrary ID, avoiding a
hidden version/synchronization dependency.

Set `outputDir` to an absolute workflow directory. It contains the generated scripts,
workflow artifacts, and the default Strategus execution roots: `<outputDir>/work` and
`<outputDir>/results`. Edit `strategus-execution-settings.json` in that directory before
execution if work or results must be stored elsewhere.

The installed guide gives the acquisition contract, artifact layout, and reproducibility
notes:

```r
system.file("doc", "phenotype-acquisition.md", package = "slashOhdsiStrategusAssistant")
```

## Support

Developer questions, comments, and feedback: [OHDSI Forum](http://forums.ohdsi.org/c/developers)

Use the [GitHub issue tracker](https://github.com/OHDSI/SlashOhdsiStrategusAssistant/issues/)
for bugs, issues, and enhancement requests.

## Contributing

Read the [HADES contribution guide](https://ohdsi.github.io/Hades/contribute.html) to
learn how to contribute to this package.

## License

slashOhdsiStrategusAssistant is licensed under the MIT license. See `LICENSE`.

## Development

slashOhdsiStrategusAssistant is developed in R using OpenAI Codex, Emacs, and [agent-shell](https://github.com/xenodium/agent-shell). RStudio
may be the preferred IDE for many contributors.

**Development status:** Beta; use at your own risk.
