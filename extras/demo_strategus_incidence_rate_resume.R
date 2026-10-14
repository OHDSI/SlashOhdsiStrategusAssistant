source(file.path(dirname(sys.frame(1)$ofile), "conference_demo_common.R"))

acp_url <- .studyAgentDemoAcpUrl()
demo_root <- .studyAgentDemoRoot()

slashOhdsiStrategusAssistant::runStrategusIncidenceShell(
  outputDir = file.path(demo_root, "demo-strategus-cohort-incidence"),
  acpUrl = acp_url,
  aiSupport = "enabled",
  resume = TRUE,
  allowCache = TRUE,
  promptOnCache = TRUE,
  bannerPath = .studyAgentDemoBanner(),
  showBanner = FALSE,
  executionTableDisplay = "console"
)
