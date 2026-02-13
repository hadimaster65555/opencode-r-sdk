#' HTTP Request Building
#'
#' Utilities for building and executing HTTP requests.
#' @keywords internal
NULL

#' Build an HTTP request
#' @param config Client configuration
#' @param method HTTP method
#' @param path API path
#' @param body Request body (optional)
#' @param query Query parameters (optional)
#' @param headers Additional headers (optional)
#' @return httr2 request
#' @keywords internal
build_request <- function(config, method, path, body = NULL, query = NULL, headers = NULL) {
  url <- paste0(config$base_url, path)

  req <- httr2::request(url) %>%
    httr2::req_method(method) %>%
    httr2::req_timeout(config$timeout)

  # Add API key if provided
  if (nzchar(config$api_key)) {
    req <- req %>% httr2::req_headers(Authorization = paste0("Bearer ", config$api_key))
  }

  # Add default headers
  if (length(config$default_headers) > 0) {
    for (name in names(config$default_headers)) {
      req <- req %>% httr2::req_headers(!!name := config$default_headers[[name]])
    }
  }

  # Add extra headers
  if (length(headers) > 0) {
    for (name in names(headers)) {
      req <- req %>% httr2::req_headers(!!name := headers[[name]])
    }
  }

  # Add query parameters
  if (length(query) > 0 || length(config$default_query) > 0) {
    # Filter out NULL values from query
    query_filtered <- query[!sapply(query, is.null)]
    all_query <- c(config$default_query[!sapply(config$default_query, is.null)], query_filtered)
    if (length(all_query) > 0) {
      req <- req %>% httr2::req_url_query(!!!all_query)
    }
  }

  # Add body for POST/PUT/PATCH
  if (!is.null(body) && method %in% c("POST", "PUT", "PATCH")) {
    req <- req %>% httr2::req_body_json(body)
  }

  req
}

#' Execute a request with retry logic
#' @param config Client configuration
#' @param req httr2 request
#' @return httr2 response
#' @keywords internal
execute_request <- function(config, req) {
  max_retries <- config$max_retries
  base_delay <- 0.5
  max_delay <- 60
  jitter <- TRUE

  for (attempt in seq_len(max_retries + 1)) {
    tryCatch({
      response <- httr2::req_perform(req)

      # Check for errors
      status_code <- httr2::resp_status(response)

      if (status_code >= 400) {
        body <- httr2::resp_body_string(response)
        error <- create_error_from_response(response, body)
        stop(error)
      }

      return(response)
    }, error = function(e) {
      if (attempt > max_retries) {
        stop(e)
      }

      if (!is_retryable_error(e)) {
        stop(e)
      }

      delay <- min(base_delay * (2 ^ (attempt - 1)), max_delay)

      if (jitter) {
        delay <- delay * runif(1, 0.5, 1.5)
      }

      cli::cli_inform(c(
        "i" = "Retry {attempt}/{max_retries} after {round(delay, 2)}s",
        "!" = conditionMessage(e)
      ))

      Sys.sleep(delay)
    })
  }

  stop("Unexpected error in execute_request")
}

#' Make a GET request
#' @param config Client configuration
#' @param path API path
#' @param query Query parameters
#' @param headers Additional headers
#' @return Response list with body and parsed data
#' @keywords internal
api_get <- function(config, path, query = NULL, headers = NULL) {
  req <- build_request(config, "GET", path, query = query, headers = headers)
  response <- execute_request(config, req)

  list(
    body = httr2::resp_body_string(response),
    parsed = parse_json_response(response),
    headers = httr2::resp_headers(response),
    status_code = httr2::resp_status(response)
  )
}

#' Make a POST request
#' @param config Client configuration
#' @param path API path
#' @param body Request body
#' @param query Query parameters
#' @param headers Additional headers
#' @return Response list with body and parsed data
#' @keywords internal
api_post <- function(config, path, body = NULL, query = NULL, headers = NULL) {
  req <- build_request(config, "POST", path, body = body, query = query, headers = headers)
  response <- execute_request(config, req)

  list(
    body = httr2::resp_body_string(response),
    parsed = parse_json_response(response),
    headers = httr2::resp_headers(response),
    status_code = httr2::resp_status(response)
  )
}

#' Make a DELETE request
#' @param config Client configuration
#' @param path API path
#' @param query Query parameters
#' @param headers Additional headers
#' @return Response list with body and parsed data
#' @keywords internal
api_delete <- function(config, path, query = NULL, headers = NULL) {
  req <- build_request(config, "DELETE", path, query = query, headers = headers)
  response <- execute_request(config, req)

  list(
    body = httr2::resp_body_string(response),
    parsed = parse_json_response(response),
    headers = httr2::resp_headers(response),
    status_code = httr2::resp_status(response)
  )
}

#' Make a request and return raw response for header access
#' @param config Client configuration
#' @param method HTTP method
#' @param path API path
#' @param body Request body
#' @param query Query parameters
#' @param headers Additional headers
#' @return RawResponse wrapper
#' @keywords internal
api_raw_request <- function(config, method, path, body = NULL, query = NULL, headers = NULL) {
  req <- build_request(config, method, path, body = body, query = query, headers = headers)
  RawResponse$new(req, config)
}

#' Raw Response Wrapper
#'
#' Provides access to raw HTTP response data.
#' @keywords internal
RawResponse <- R6::R6Class(
  "RawResponse",
  public = list(
    request = NULL,
    config = NULL,
    response = NULL,
    content = NULL,
    initialize = function(req, config) {
      self$request <- req
      self$config <- config
      # Execute request immediately
      self$response <- httr2::req_perform(req)
      self$content <- httr2::resp_body_string(self$response)
    },
    parse = function() {
      parse_json_response(self$response)
    },
    headers = function() {
      httr2::resp_headers(self$response)
    }
  )
)
