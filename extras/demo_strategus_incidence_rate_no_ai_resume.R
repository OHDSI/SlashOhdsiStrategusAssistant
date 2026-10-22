source(file.path(dirname(sys.frame(1)$ofile), "conference_demo_common.R"))

demo_root <- .studyAgentDemoRoot()

slashOhdsiStrategusAssistant::runStrategusIncidenceShell(
  outputDir = file.path(demo_root, "demo-strategus-cohort-incidence-no-ai"),
  aiSupport = "disabled",
  resume = TRUE,
  allowCache = TRUE,
  promptOnCache = TRUE,
  bannerPath = .studyAgentDemoBanner(),
  showBanner = FALSE,
  executionTableDisplay = "console"
)
