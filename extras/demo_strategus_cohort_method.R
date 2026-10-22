source(file.path(dirname(sys.frame(1)$ofile), "conference_demo_common.R"))

acp_url <- .studyAgentDemoAcpUrl()
demo_root <- .studyAgentDemoRoot()

slashOhdsiStrategusAssistant::runStrategusCohortMethodsShell(
  outputDir = file.path(demo_root, "demo-strategus-cohort-method"),
  incidenceOutputDir = file.path(demo_root, "demo-strategus-cohort-incidence"),
  acpUrl = acp_url,
  aiSupport = "enabled",
  bannerPath = .studyAgentDemoBanner(),
  showBanner = FALSE,
  executionTableDisplay = "console"
)
