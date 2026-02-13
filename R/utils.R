#' Utility Functions
#'
#' Common utility functions for the package.
#' @keywords internal
NULL

#' Check if a value is not given (similar to Python's NotGiven)
#' @param x Value to check
#' @return TRUE if value is not given
#' @keywords internal
is_not_given <- function(x) {
  missing(x) || identical(x, quote(expr = ))
}

#' Get default value if not given
#' @param x Value to check
#' @param default Default value
#' @return x if given, otherwise default
#' @keywords internal
if_not_given <- function(x, default) {
  if (is_not_given(x)) default else x
}

#' Validate required parameters
#' @param params List of parameters
#' @param required Names of required parameters
#' @keywords internal
validate_required <- function(params, required) {
  for (param in required) {
    if (is.null(params[[param]]) || is.na(params[[param]]) || params[[param]] == "") {
      rlang::abort(glue::glue("Required parameter '{param}' is missing or empty"))
    }
  }
}

#' Convert a list to JSON, handling special types
#' @param x List to convert
#' @return JSON string
#' @keywords internal
to_json_body <- function(x) {
  jsonlite::toJSON(x, auto_unbox = FALSE, null = "null")
}

#' Parse JSON response
#' @param response HTTP response
#' @return Parsed list
#' @keywords internal
parse_json_response <- function(response) {
  body <- httr2::resp_body_string(response)
  if (nchar(body) == 0) {
    return(list())
  }
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

#' Convert snake_case to camelCase
#' @param x Named list
#' @return Converted list
#' @keywords internal
snake_to_camel <- function(x) {
  if (!is.list(x)) return(x)

  keys <- names(x)
  new_keys <- gsub("_([a-z])", "\\U\\1", keys, perl = TRUE)
  names(x) <- new_keys
  x
}

#' Convert camelCase to snake_case
#' @param x Named list
#' @return Converted list
#' @keywords internal
camel_to_snake <- function(x) {
  if (!is.list(x)) return(x)

  keys <- names(x)
  new_keys <- gsub("([A-Z])", "_\\L\\1", keys, perl = TRUE)
  new_keys <- tolower(new_keys)
  names(x) <- new_keys
  x
}

#' Log a debug message
#' @param ... Messages to log
#' @keywords internal
log_debug <- function(...) {
  if (tolower(Sys.getenv("OPENCODE_LOG", "warning")) == "debug") {
    cli::cli_inform(c("v" = ...))
  }
}

#' Log an info message
#' @param ... Messages to log
#' @keywords internal
log_info <- function(...) {
  if (tolower(Sys.getenv("OPENCODE_LOG", "warning")) %in% c("debug", "info")) {
    cli::cli_inform(c("i" = ...))
  }
}

#' Create a temporary file for testing
#' @param content File content
#' @return Path to temporary file
#' @keywords internal
create_temp_file <- function(content = "") {
  tempfile_pattern <- file.path(tempdir(), "opencode_test_")
  tf <- tempfile(tmpdir = tempdir(), pattern = "opencode_test_")
  writeLines(content, tf)
  tf
}

#' Null coalesce operator
#' @param a First value
#' @param b Second value (fallback)
#' @return a if not NULL/empty, otherwise b
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a) || identical(a, NA) || (is.character(a) && nchar(a) == 0)) b else a
}
