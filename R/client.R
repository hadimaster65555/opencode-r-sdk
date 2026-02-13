#' OpenCode API Client
#'
#' Main client class for accessing the OpenCode API.
#'
#' @export
#' @examples
#' # Synchronous client
#' client <- Opencode$new()
#' sessions <- client$session$list()
#'
#' # With custom configuration
#' client <- Opencode$new(
#'   base_url = "http://localhost:54321",
#'   timeout = 30,
#'   max_retries = 3
#' )
Opencode <- R6::R6Class(
  "Opencode",
  public = list(
    session = NULL,
    app = NULL,
    event = NULL,
    find = NULL,
    file = NULL,
    config_resource = NULL,
    tui = NULL,
    .config = NULL,

    initialize = function(base_url = NULL, timeout = NULL, max_retries = NULL,
                          api_key = NULL, default_headers = NULL, default_query = NULL) {
      self$.config <- ClientConfig$new(
        base_url = base_url,
        timeout = timeout,
        max_retries = max_retries,
        api_key = api_key,
        default_headers = default_headers,
        default_query = default_query
      )

      # Initialize resources
      self$session <- SessionResource$new(self)
      self$app <- AppResource$new(self)
      self$event <- EventResource$new(self)
      self$find <- FindResource$new(self)
      self$file <- FileResource$new(self)
      self$config_resource <- ConfigResource$new(self)
      self$tui <- TuiResource$new(self)
    },

    # Create a copy with modified options
    with_options = function(base_url = NULL, timeout = NULL, max_retries = NULL,
                            api_key = NULL, default_headers = NULL, default_query = NULL) {
      new_config <- self$.config$with_options(
        base_url = base_url,
        timeout = timeout,
        max_retries = max_retries,
        api_key = api_key,
        default_headers = default_headers,
        default_query = default_query
      )

      client <- self$clone()
      client$.config <- new_config
      client
    },

    # Close the client and release resources
    close = function() {
      invisible(TRUE)
    },

    print = function(...) {
      cli::cli_h1("Opencode Client")
      cli::cli_text("Base URL: {self$.config$base_url}")
      cli::cli_text("Timeout: {self$.config$timeout}s")
      cli::cli_text("Max retries: {self$.config$max_retries}")
    }
  )
)

#' Async OpenCode API Client
#'
#' Asynchronous client class for accessing the OpenCode API.
#'
#' @export
#' @examples
#' # Async client
#' client <- AsyncOpencode$new()
#' future_result <- client$session$list_async()
AsyncOpencode <- R6::R6Class(
  "AsyncOpencode",
  inherit = Opencode,
  public = list(
    initialize = function(base_url = NULL, timeout = NULL, max_retries = NULL,
                          api_key = NULL, default_headers = NULL, default_query = NULL) {
      self$.config <- ClientConfig$new(
        base_url = base_url,
        timeout = timeout,
        max_retries = max_retries,
        api_key = api_key,
        default_headers = default_headers,
        default_query = default_query
      )

      # Initialize async resources
      self$session <- AsyncSessionResource$new(self)
      self$app <- AsyncAppResource$new(self)
      self$event <- AsyncEventResource$new(self)
      self$find <- AsyncFindResource$new(self)
      self$file <- AsyncFileResource$new(self)
      self$config_resource <- AsyncConfigResource$new(self)
      self$tui <- AsyncTuiResource$new(self)
    },

    print = function(...) {
      cli::cli_h1("AsyncOpencode Client")
      cli::cli_text("Base URL: {self$.config$base_url}")
      cli::cli_text("Timeout: {self$.config$timeout}s")
      cli::cli_text("Max retries: {self$.config$max_retries}")
      cli::cli_text("Note: This is an async client using the 'future' package")
    }
  )
)

# Context manager support
#' @export
print.Opencode <- function(x, ...) {
  x$print()
}

#' @export
print.AsyncOpencode <- function(x, ...) {
  x$print()
}
