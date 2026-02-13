#' TUI Resource
#'
#' Provides methods for TUI-related API endpoints.
#' @keywords internal
NULL

#' TuiResource
#'
#' Resource for TUI-related API endpoints.
#' @export
#' @keywords internal
TuiResource <- R6::R6Class(
  "TuiResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' Append a prompt to the TUI
    #'
    #' @param prompt The prompt to append
    #' @return TuiAppendPromptResponse object
    append_prompt = function(prompt) {
      body <- list(prompt = prompt)
      self$.post("/tui/append-prompt", body = body, type = "TuiAppendPromptResponse")
    },

    #' Open help in the TUI
    #'
    #' @return TuiOpenHelpResponse object
    open_help = function() {
      self$.post("/tui/open-help", type = "TuiOpenHelpResponse")
    }
  )
)

#' AsyncTuiResource
#'
#' Async resource for TUI-related API endpoints.
#' @export
#' @keywords internal
AsyncTuiResource <- R6::R6Class(
  "AsyncTuiResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    append_prompt_async = function(prompt) {
      body <- list(prompt = prompt)
      self$.post_async("/tui/append-prompt", body = body, type = "TuiAppendPromptResponse")
    },

    open_help_async = function() {
      self$.post_async("/tui/open-help", type = "TuiOpenHelpResponse")
    }
  )
)
