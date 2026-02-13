#' Streaming Support (Server-Side Events)
#'
#' Provides support for streaming responses using SSE.
#' @keywords internal
NULL

#' Stream a response and yield events
#' @param client Opencode client
#' @param path API path
#' @param query Query parameters
#' @param method HTTP method
#' @return An iterator that yields parsed events
#' @keywords internal
stream_events <- function(client, path, query = NULL, method = "GET") {
  config <- client$.config

  if (is.null(config)) {
    rlang::abort("Client not properly initialized")
  }

  SSEStream$new(config, path, query = query, method = method)
}

#' SSE Stream Iterator
#'
#' Handles SSE stream parsing and iteration.
#' @keywords internal
SSEStream <- R6::R6Class(
  "SSEStream",
  public = list(
    config = NULL,
    path = NULL,
    query = NULL,
    method = NULL,
    con = NULL,
    initialized = FALSE,

    initialize = function(config, path, query = NULL, method = "GET") {
      self$config <- config
      self$path <- path
      self$query <- query
      self$method <- method

      # Set up the connection
      url <- paste0(config$base_url, path)
      req <- httr2::request(url) %>%
        httr2::req_method(method) %>%
        httr2::req_timeout(config$get_timeout())

      # Open connection for streaming
      self$con <- req %>%
        httr2::req_perform_sequential() %>%
        httr2::resp_body_raw()

      self$initialized <- TRUE
    },

    # Get next event from stream
    next_event = function() {
      if (!self$initialized || is.null(self$con)) {
        return(NULL)
      }

      line <- readLines(self$con, n = 1, warn = FALSE)

      if (length(line) == 0) {
        return(NULL)
      }

      self$parse_sse_line(line)
    },

    # Parse a single SSE line
    parse_sse_line = function(line) {
      if (nchar(line) == 0 || line == "") {
        return(NULL)
      }

      # Parse SSE format: "event: type" or "data: {...}"
      if (startsWith(line, "event:")) {
        event_type <- sub("^event: ", "", line)
        return(list(type = event_type, data = NULL))
      }

      if (startsWith(line, "data:")) {
        data_str <- sub("^data: ", "", line)

        # Try to parse as JSON
        tryCatch({
          data <- jsonlite::fromJSON(data_str, simplifyVector = FALSE)
          return(list(type = "message", data = data))
        }, error = function(e) {
          return(list(type = "message", data = data_str))
        })
      }

      # Other SSE fields (id, retry)
      if (startsWith(line, "id:")) {
        return(list(type = "id", id = sub("^id: ", "", line)))
      }

      if (startsWith(line, "retry:")) {
        return(list(type = "retry", retry = as.numeric(sub("^retry: ", "", line))))
      }

      NULL
    },

    # Close the stream
    close = function() {
      if (!is.null(self$con)) {
        close(self$con)
        self$con <- NULL
        self$initialized <- FALSE
      }
    },

    print = function(...) {
      cli::cli_text("SSE Stream to {self$path}")
    }
  )
)

#' Create a streaming response wrapper
#' @param client Client instance
#' @param path API path
#' @param query Query parameters
#' @return SSEStream object
#' @keywords internal
create_streaming_response <- function(client, path, query = NULL) {
  config <- client$.__enclos_env__$private$config
  SSEStream$new(config, path, query = query)
}

#' Check if a response should be streamed
#' @param response HTTP response
#' @return TRUE if response should be streamed
#' @keywords internal
should_stream <- function(response) {
  headers <- httr2::resp_headers(response)
  content_type <- headers[["content-type"]] %||% ""

  grepl("text/event-stream", content_type, fixed = TRUE) ||
    headers[["transfer-encoding"]] == "chunked"
}
