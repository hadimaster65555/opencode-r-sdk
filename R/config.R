#' Configuration Management
#'
#' Handles client configuration and environment variables.
#' @keywords internal
NULL

#' Get package options
#' @param option Option name
#' @param default Default value
#' @return Option value
#' @export
get_opencode_option <- function(option, default = NULL) {
  opt_name <- paste0("opencode.", option)
  getOption(opt_name, default)
}

#' Set package option
#' @param option Option name
#' @param value Value to set
#' @return NULL
#' @export
set_opencode_option <- function(option, value) {
  opt_name <- paste0("opencode.", option)
  options(list(opt_name = value))
}

#' Client Configuration
#'
#' R6 class for managing client configuration.
#' @keywords internal
ClientConfig <- R6::R6Class(
  "ClientConfig",
  public = list(
    base_url = NULL,
    timeout = NULL,
    max_retries = NULL,
    api_key = NULL,
    default_headers = NULL,
    default_query = NULL,

    initialize = function(base_url = NULL, timeout = NULL, max_retries = NULL,
                          api_key = NULL, default_headers = NULL, default_query = NULL) {
      self$base_url <- if (is.null(base_url)) get_opencode_option("base_url", "http://localhost:54321") else base_url
      self$timeout <- if (is.null(timeout)) get_opencode_option("timeout", 60) else timeout
      self$max_retries <- if (is.null(max_retries)) get_opencode_option("max_retries", 2) else max_retries
      self$api_key <- if (is.null(api_key)) get_opencode_option("api_key", "") else api_key
      self$default_headers <- if (is.null(default_headers)) list() else default_headers
      self$default_query <- if (is.null(default_query)) list() else default_query
    },

    # Create a copy with new options
    with_options = function(base_url = NULL, timeout = NULL, max_retries = NULL,
                            api_key = NULL, default_headers = NULL, default_query = NULL) {
      new_config <- self$clone()
      new_config$base_url <- if (is.null(base_url)) self$base_url else base_url
      new_config$timeout <- if (is.null(timeout)) self$timeout else timeout
      new_config$max_retries <- if (is.null(max_retries)) self$max_retries else max_retries
      new_config$api_key <- if (is.null(api_key)) self$api_key else api_key
      new_config$default_headers <- if (is.null(default_headers)) self$default_headers else default_headers
      new_config$default_query <- if (is.null(default_query)) self$default_query else default_query
      new_config
    },

    # Get effective timeout for httr2
    get_timeout = function() {
      self$timeout
    }
  )
)

#' Null coalesce operator
#' @param a First value
#' @param b Second value (fallback)
#' @return a if not NULL, otherwise b
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
