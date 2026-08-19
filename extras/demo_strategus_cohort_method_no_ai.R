# Demo: no-AI Strategus CohortMethod workflow
#
# Run this script from any writable directory after installing
# slashOhdsiStrategusAssistant. During cohort selection choose `pl` to browse an
# installed PhenotypeLibrary, `file`/`dir` for Circe JSON, or `db` for an existing
# SIMPLE_EXPRESSION database cohort definition.

library(slashOhdsiStrategusAssistant)

# outputDir controls the workflow artifacts and the default Strategus work/results roots.
demo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
banner_path <- system.file("banner", "ohdsi-logo-ascii.txt", package = "slashOhdsiStrategusAssistant")
if (!nzchar(banner_path)) stop("Installed package banner asset was not found.")

output_dir <- file.path(demo_root, "demo-strategus-cohort-method-no-ai")
incidence_output_dir <- file.path(demo_root, "demo-strategus-cohort-incidence-no-ai")

# To start from scratch, uncomment the next line. It removes only this demo output.
# unlink(output_dir, recursive = TRUE, force = TRUE)

slashOhdsiStrategusAssistant::runStrategusCohortMethodsShell(
  outputDir = output_dir,
  incidenceOutputDir = incidence_output_dir,
  aiSupport = "disabled",
  bannerPath = banner_path,
  showBanner = TRUE,
  executionTableDisplay = "console"
)

# Resume a prior run:
# slashOhdsiStrategusAssistant::runStrategusCohortMethodsShell(
#   outputDir = output_dir,
#   incidenceOutputDir = incidence_output_dir,
#   aiSupport = "disabled",
#   resume = TRUE,
#   allowCache = TRUE,
#   promptOnCache = TRUE,
#   bannerPath = banner_path,
#   showBanner = TRUE,
#   executionTableDisplay = "console"
# )
