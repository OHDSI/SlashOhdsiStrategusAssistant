#' Resolve optional AI/ACP support for a Strategus workflow
#'
#' `disabled` is intentionally the default: it makes the workflow a local,
#' deterministic wizard and guarantees that it will not create an ACP client
#' or make an ACP request. `enabled` requires the optional ACP client package;
#' `auto` uses ACP when the client package is installed and otherwise behaves as
#' the local wizard.
#'
#' @param aiSupport one of `disabled`, `enabled`, or `auto`
#' @return a list describing the resolved support policy
.studyAgentSlashResolveAiSupport <- function(aiSupport = c("disabled", "enabled", "auto")) {
  if (is.null(aiSupport)) aiSupport <- "disabled"
  requested <- match.arg(as.character(aiSupport), c("disabled", "enabled", "auto"))
  client_installed <- requireNamespace("slashOhdsiAcpClient", quietly = TRUE)
  if (identical(requested, "enabled") && !client_installed) {
    stop(
      "aiSupport='enabled' requires the optional slashOhdsiAcpClient package. ",
      "Install it or use aiSupport='disabled' for the local workflow wizard."
    )
  }
  list(
    requested = requested,
    enabled = !identical(requested, "disabled") && client_installed,
    client_installed = client_installed,
    mode = if (identical(requested, "auto") && !client_installed) "disabled" else requested,
    reason = if (identical(requested, "disabled")) "disabled_by_user" else if (!client_installed) "optional_acp_client_not_installed" else "enabled"
  )
}

.studyAgentSlashAiSupportAllowsAcp <- function(policy) {
  isTRUE(policy$enabled)
}

.studyAgentSlashAiSupportDisabledMessage <- function(policy, capability = "AI support") {
  sprintf(
    "%s is unavailable because aiSupport is %s (%s).",
    capability,
    as.character(policy$mode %||% "disabled"),
    as.character(policy$reason %||% "disabled_by_user")
  )
}

.studyAgentSlashAcpFailureCondition <- function(error, flow_name = NULL) {
  technical_message <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error %||% "unknown ACP error")
  stage <- if (is.null(flow_name) || !nzchar(as.character(flow_name))) "the current AI-assisted step" else sprintf("ACP flow '%s'", as.character(flow_name))
  structure(
    list(
      message = sprintf("The AI-assisted workflow cannot continue because %s is unavailable or failed upstream. Stop this shell and contact the ACP service administrator before resuming.", stage),
      call = NULL,
      flow_name = flow_name,
      technical_message = technical_message
    ),
    class = c("study_agent_acp_failure", "error", "condition")
  )
}

.studyAgentSlashStopForAcpFailure <- function(error, flow_name = NULL) {
  if (inherits(error, "study_agent_acp_failure")) stop(error)
  stop(.studyAgentSlashAcpFailureCondition(error, flow_name))
}

.studyAgentSlashValidateAcpResponse <- function(response, flow_name = NULL) {
  core <- response$full_result %||% response
  status <- tolower(trimws(as.character(core$status %||% response$status %||% "")))
  error_message <- core$error %||% response$error %||% NULL
  if (!is.null(error_message) && nzchar(trimws(as.character(error_message)))) {
    .studyAgentSlashStopForAcpFailure(as.character(error_message), flow_name)
  }
  if (status %in% c("error", "failed", "unavailable")) {
    .studyAgentSlashStopForAcpFailure(sprintf("ACP returned status '%s'.", status), flow_name)
  }
  response
}
