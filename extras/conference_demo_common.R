# Conference demo launcher support. Site configuration belongs in the R environment:
# ACP_URL, ACP_TIMEOUT, STUDY_AGENT_WEBAPI_URL, STUDY_AGENT_WEBAPI_API_KEY,
# STUDY_AGENT_ATLAS_URL, and optionally STUDY_AGENT_DEMO_ROOT.

.studyAgentDemoRoot <- function() {
  configured <- trimws(Sys.getenv("STUDY_AGENT_DEMO_ROOT", ""))
  if (nzchar(configured)) return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

.studyAgentDemoAcpUrl <- function() {
  url <- trimws(Sys.getenv("ACP_URL", "http://127.0.0.1:8765"))
  if (!nzchar(url)) stop("Set ACP_URL before starting the demo shell.", call. = FALSE)
  Sys.setenv(ACP_URL = url)
  if (!nzchar(Sys.getenv("ACP_TIMEOUT", ""))) Sys.setenv(ACP_TIMEOUT = "1800")
  url
}

.studyAgentDemoBanner <- function() {
  banner <- system.file("banner", "ohdsi-logo-ascii.txt", package = "slashOhdsiStrategusAssistant")
  if (!nzchar(banner)) stop("Installed package banner asset was not found.", call. = FALSE)
  banner
}
