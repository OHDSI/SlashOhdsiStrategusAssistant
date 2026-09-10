testthat::test_that("scripted dialogue input preserves back navigation", {
  transcript <- new_shell_transcript(c("/back", "answer"))
  session <- new_workflow_dialogue_session(
    interactive = TRUE,
    input_provider = transcript$readline,
    study_intent_getter = function() "test study",
    build_stage_context = function(studyIntent, dialogue_state) list(),
    call_dialogue = function(stage_context, message) list(status = "ok")
  )
  back <- session$readline("Choose: ", allow_back = TRUE)
  testthat::expect_true(inherits(back, "workflow_navigation_signal"))
  testthat::expect_identical(back$action, "back")
  testthat::expect_identical(session$readline("Next: "), "answer")
  testthat::expect_true(transcript$expect_complete())
})

testthat::test_that("fixed-width renderer removes Markdown table delimiters", {
  markdown <- paste(
    "### Concept set",
    "|Concept ID|Concept Name|Vocabulary|",
    "|:---|:---|:---|",
    "|1|A long concept name intended to be clipped in a console column|SNOMED|",
    sep = "\n"
  )
  rendered <- .studyAgentSlashCirceDefinitionConsole(markdown, max_column_width = 24L)
  testthat::expect_false(grepl("|", rendered, fixed = TRUE))
  testthat::expect_match(rendered, "Concept ID", fixed = TRUE)
  testthat::expect_match(rendered, "...", fixed = TRUE)
})
