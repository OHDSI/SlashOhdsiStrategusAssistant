new_shell_transcript <- function(responses) {
  state <- new.env(parent = emptyenv())
  state$responses <- as.character(responses)
  state$index <- 0L
  state$prompts <- character(0)
  list(
    readline = function(prompt) {
      state$prompts <- c(state$prompts, as.character(prompt))
      state$index <- state$index + 1L
      if (state$index > length(state$responses)) stop(sprintf("Shell transcript exhausted at prompt: %s", prompt))
      state$responses[[state$index]]
    },
    prompts = function() state$prompts,
    remaining = function() state$responses[seq.int(state$index + 1L, length(state$responses))],
    expect_complete = function() {
      if (state$index != length(state$responses)) stop(sprintf("Shell transcript has %s unused response(s).", length(state$responses) - state$index))
      invisible(TRUE)
    }
  )
}
