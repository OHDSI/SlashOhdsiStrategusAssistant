# Demo: ACP-assisted Strategus CohortMethod workflow
#
# Run this script from any writable directory after installing
# slashOhdsiStrategusAssistant and slashOhdsiAcpClient. ACP must be available at
# ACP_URL (or http://127.0.0.1:8765 by default).

library(slashOhdsiStrategusAssistant)

# outputDir controls the workflow artifacts and the default Strategus work/results roots.
demo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
banner_path <- system.file("banner", "ohdsi-logo-ascii.txt", package = "slashOhdsiStrategusAssistant")
if (!nzchar(banner_path)) stop("Installed package banner asset was not found.")
library(slashOhdsiAcpClient)

acp_url <- Sys.getenv("ACP_URL", "http://127.0.0.1:8765")
Sys.setenv(ACP_TIMEOUT = "1800", ACP_URL = acp_url)
invisible(connect_study_agent_acp())

output_dir <- file.path(demo_root, "demo-strategus-cohort-method")
incidence_output_dir <- file.path(demo_root, "demo-strategus-cohort-incidence")

# To start from scratch, uncomment the next line. It removes only this demo output.
# unlink(output_dir, recursive = TRUE, force = TRUE)

slashOhdsiStrategusAssistant::runStrategusCohortMethodsShell(
  outputDir = output_dir,
  incidenceOutputDir = incidence_output_dir,
  acpUrl = acp_url,
  aiSupport = "enabled",
  bannerPath = banner_path,
  showBanner = TRUE,
  executionTableDisplay = "console"
)

# Resume a prior run:
# slashOhdsiStrategusAssistant::runStrategusCohortMethodsShell(
#   outputDir = output_dir,
#   incidenceOutputDir = incidence_output_dir,
#   acpUrl = acp_url,
#   aiSupport = "enabled",
#   resume = TRUE,
#   allowCache = TRUE,
#   promptOnCache = TRUE,
#   bannerPath = banner_path,
#   showBanner = TRUE,
#   executionTableDisplay = "console"
# )
