#' Response Parsing and Handling
#'
#' Utilities for parsing and handling API responses.
#' @keywords internal
NULL

#' Parse API response into appropriate type
#' @param response Response from API call
#' @param type_name Name of the expected type
#' @return Parsed response with appropriate S3 class
#' @keywords internal
parse_response <- function(response, type_name = NULL) {
  parsed <- response$parsed

  if (!is.null(type_name)) {
    class(parsed) <- c(type_name, "opencode_response", class(parsed))
  } else {
    class(parsed) <- c("opencode_response", class(parsed))
  }

  parsed
}

#' Opencode Response S3 Class
#'
#' Base class for all API responses.
#' @keywords internal
NULL

#' Print method for opencode_response
#' @param x Response object
#' @param ... Additional arguments
#' @method print opencode_response
#' @keywords internal
print.opencode_response <- function(x, ...) {
  cli::cli_h2(class(x)[1])
  print(as.list(x))
}

#' Convert response to JSON
#' @param x Response object
#' @param ... Additional arguments
#' @method as.json opencode_response
#' @keywords internal
as.json.opencode_response <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
}

#' Convert response to list
#' @param x Response object
#' @param ... Additional arguments
#' @method as.list opencode_response
#' @keywords internal
as.list.opencode_response <- function(x, ...) {
  as.list(x)
}

#' Create a typed response object
#' @param data Response data as list
#' @param type Type name
#' @return Typed response object
#' @keywords internal
create_typed_response <- function(data, type) {
  structure(data, class = c(type, "opencode_response", class(data)))
}

#' Validate response has required fields
#' @param response Response object
#' @param required_fields Vector of required field names
#' @keywords internal
validate_response <- function(response, required_fields) {
  for (field in required_fields) {
    if (is.null(response[[field]])) {
      rlang::abort(glue::glue("Response missing required field: {field}"))
    }
  }
  response
}
