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

#' SSE Stream using curl multi-handle
#'
#' True streaming implementation using curl's multi interface.
#' @keywords internal
SSEStream <- R6::R6Class(
  "SSEStream",
  public = list(
    config = NULL,
    path = NULL,
    query = NULL,
    method = NULL,
    url = NULL,
    multi_handle = NULL,
    easy_handle = NULL,
    buffer = NULL,
    initialized = FALSE,

    initialize = function(config, path, query = NULL, method = "GET") {
      self$config <- config
      self$path <- path
      self$query <- query
      self$method <- method
      self$url <- paste0(config$base_url, path)
      self$buffer <- raw(0)

      tryCatch({
        self$multi_handle <- curl::new_multi_handle()
        self$easy_handle <- curl::new_handle(URL = self$url, FOLLOWLOCATION = TRUE)

        if (method == "POST") {
          curl::handle_setopt(self$easy_handle, POST = TRUE)
        }

        headers <- c(
          "Accept: text/event-stream",
          "Cache-Control: no-cache"
        )
        curl::handle_setopt(self$easy_handle, HTTPHEADER = headers)

        curl::multi_add(self$multi_handle, self$easy_handle)

        self$initialized <- TRUE
      }, error = function(e) {
        rlang::warn(paste("Failed to initialize stream:", e$message))
        self$initialized <- FALSE
      })
    },

    next_event = function(timeout = 0.1) {
      if (!self$initialized) {
        return(NULL)
      }

      tryCatch({
        done <- FALSE
        while (!done && curl::multi_run(self$multi_handle, timeout = timeout) > 0) {
          data <- curl::multi_read(self$easy_handle, timeout = timeout)

          if (length(data) > 0) {
            self$buffer <- c(self$buffer, data)

            lines <- strsplit(rawToChar(self$buffer), "\n")[[1]]

            for (i in seq_len(length(lines) - 1)) {
              line <- lines[i]
              if (nzchar(line)) {
                event <- self$parse_sse_line(line)
                if (!is.null(event)) {
                  self$buffer <- raw(0)
                  return(event)
                }
              }
            }

            self$buffer <- charToRaw(lines[length(lines)])
          }

          done <- TRUE
        }
        NULL
      }, error = function(e) {
        NULL
      })
    },

    parse_sse_line = function(line) {
      if (nchar(line) == 0 || line == "") {
        return(NULL)
      }

      line <- sub("\r$", "", line)

      if (startsWith(line, "event:")) {
        event_type <- sub("^event: ", "", line)
        return(list(type = event_type, data = NULL))
      }

      if (startsWith(line, "data:")) {
        data_str <- sub("^data: ", "", line)

        if (data_str == "") {
          return(list(type = "heartbeat", data = NULL))
        }

        tryCatch({
          data <- jsonlite::fromJSON(data_str, simplifyVector = FALSE)
          return(list(type = "message", data = data))
        }, error = function(e) {
          return(list(type = "message", data = data_str))
        })
      }

      if (startsWith(line, "id:")) {
        return(list(type = "id", id = sub("^id: ", "", line)))
      }

      if (startsWith(line, "retry:")) {
        return(list(type = "retry", retry = as.numeric(sub("^retry: ", "", line))))
      }

      NULL
    },

    close = function() {
      if (!is.null(self$multi_handle)) {
        tryCatch({
          curl::multi_remove(self$multi_handle, self$easy_handle)
        }, error = function(e) {})
        self$multi_handle <- NULL
      }
      if (!is.null(self$easy_handle)) {
        self$easy_handle <- NULL
      }
      self$initialized <- FALSE
    },

    print = function(...) {
      cli::cli_text("SSE Stream to {self$path}")
    }
  )
)

#' Stream text response with callbacks
#'
#' Simplified streaming that yields text chunks as they arrive.
#' @param url Full URL to stream from
#' @param method HTTP method
#' @param body Request body (for POST)
#' @param headers Additional headers
#' @param timeout Request timeout
#' @param callback Function to call with each chunk
#' @param on_error Error handler function
#' @return List with full_response and chunks
#' @keywords internal
stream_text <- function(url, method = "GET", body = NULL, headers = NULL,
                        timeout = 60, callback = NULL, on_error = NULL) {

  full_response <- character(0)
  chunks <- list()

  tryCatch({
    handle <- curl::new_handle(URL = url, FOLLOWLOCATION = TRUE)

    if (method == "POST") {
      curl::handle_setopt(handle, POST = TRUE)
      if (!is.null(body)) {
        curl::handle_setopt(handle, POSTFIELDS = body)
      }
    }

    all_headers <- c(
      "Accept: text/event-stream",
      "Cache-Control: no-cache",
      headers %||% character(0)
    )
    curl::handle_setopt(handle, HTTPHEADER = all_headers)

    con <- curl::curl(url, open = "rb")

    on_timeout <- function(con, timeout_sec) {
      start_time <- Sys.time()
      while (TRUE) {
        ready <- curl::is_readable(con)
        if (ready) {
          return(TRUE)
        }
        if (as.numeric(Sys.time() - start_time) > timeout_sec) {
          return(FALSE)
        }
        Sys.sleep(0.01)
      }
    }

    chunk_buffer <- ""

    while (TRUE) {
      if (!on_timeout(con, 0.1)) {
        break
      }

      chunk <- readLines(con, n = 1, warn = FALSE)

      if (length(chunk) == 0) {
        break
      }

      chunk <- sub("\r$", "", chunk)

      if (nzchar(chunk) && startsWith(chunk, "data:")) {
        data_str <- sub("^data: ", "", chunk)

        if (data_str == "[DONE]") {
          break
        }

        tryCatch({
          json_data <- jsonlite::fromJSON(data_str, simplifyVector = FALSE)

          chunk_text <- NULL
          if (is.list(json_data) && !is.null(json_data$choices)) {
            delta <- json_data$choices[[1]]$delta
            if (is.list(delta) && !is.null(delta$content)) {
              chunk_text <- delta$content
            } else if (is.list(delta) && !is.null(delta$text)) {
              chunk_text <- delta$text
            }
          } else if (is.character(json_data)) {
            chunk_text <- json_data
          }

          if (!is.null(chunk_text) && nzchar(chunk_text)) {
            full_response <- c(full_response, chunk_text)
            chunks[[length(chunks) + 1]] <- chunk_text

            if (!is.null(callback)) {
              callback(chunk_text)
            }
          }
        }, error = function(e) {
        })
      }
    }

    close(con)

  }, error = function(e) {
    if (!is.null(on_error)) {
      on_error(e)
    }
  })

  list(
    full_response = paste(full_response, collapse = ""),
    chunks = chunks
  )
}

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
