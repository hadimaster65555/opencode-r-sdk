#' OpenCode Error Classes
#'
#' A hierarchy of error classes for handling API errors.
#' @keywords internal
NULL

#' @rdname OpencodeError
#' @export
OpencodeError <- R6::R6Class(
  "OpencodeError",
  public = list(
    message = NULL,
    status_code = NULL,
    response = NULL,
    body = NULL,
    initialize = function(message, status_code = NULL, response = NULL, body = NULL) {
      self$message <- message
      self$status_code <- status_code
      self$response <- response
      self$body <- body
    },
    print = function(...) {
      cli::cli_h2("OpencodeError")
      cli::cli_text(self$message)
      if (!is.null(self$status_code)) {
        cli::cli_text("Status code: {self$status_code}")
      }
    }
  )
)

#' @rdname APIConnectionError
#' @export
APIConnectionError <- R6::R6Class(
  "APIConnectionError",
  inherit = OpencodeError,
  public = list(
    initialize = function(message = "Could not connect to the API",
                         status_code = NULL, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname APITimeoutError
#' @export
APITimeoutError <- R6::R6Class(
  "APITimeoutError",
  inherit = OpencodeError,
  public = list(
    initialize = function(message = "Request timed out",
                         status_code = NULL, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname APIStatusError
#' @export
APIStatusError <- R6::R6Class(
  "APIStatusError",
  inherit = OpencodeError,
  public = list(
    initialize = function(message, status_code = NULL, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname BadRequestError
#' @export
BadRequestError <- R6::R6Class(
  "BadRequestError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Bad request (400)",
                         status_code = 400L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname AuthenticationError
#' @export
AuthenticationError <- R6::R6Class(
  "AuthenticationError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Authentication failed (401)",
                         status_code = 401L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname PermissionDeniedError
#' @export
PermissionDeniedError <- R6::R6Class(
  "PermissionDeniedError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Permission denied (403)",
                         status_code = 403L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname NotFoundError
#' @export
NotFoundError <- R6::R6Class(
  "NotFoundError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Resource not found (404)",
                         status_code = 404L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname UnprocessableEntityError
#' @export
UnprocessableEntityError <- R6::R6Class(
  "UnprocessableEntityError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Unprocessable entity (422)",
                         status_code = 422L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname RateLimitError
#' @export
RateLimitError <- R6::R6Class(
  "RateLimitError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Rate limit exceeded (429)",
                         status_code = 429L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' @rdname InternalServerError
#' @export
InternalServerError <- R6::R6Class(
  "InternalServerError",
  inherit = APIStatusError,
  public = list(
    initialize = function(message = "Internal server error (5xx)",
                         status_code = 500L, response = NULL, body = NULL) {
      super$initialize(message, status_code, response, body)
    }
  )
)

#' Create an error from an HTTP response
#' @keywords internal
create_error_from_response <- function(response, body = NULL) {
  status_code <- httr2::resp_status(response)

  error_class <- switch(
    as.character(status_code),
    "400" = BadRequestError,
    "401" = AuthenticationError,
    "403" = PermissionDeniedError,
    "404" = NotFoundError,
    "422" = UnprocessableEntityError,
    "429" = RateLimitError,
    "500" = InternalServerError,
    "501" = InternalServerError,
    "502" = InternalServerError,
    "503" = InternalServerError,
    "504" = InternalServerError,
    APIStatusError
  )

  message <- if (!is.null(body) && is.character(body)) {
    tryCatch({
      json_body <- jsonlite::fromJSON(body, simplifyVector = FALSE)
      if (is.character(json_body$message)) {
        json_body$message
      } else if (is.character(json_body$error)) {
        json_body$error
      } else {
        glue::glue("HTTP {status_code}")
      }
    }, error = function(e) {
      glue::glue("HTTP {status_code}")
    })
  } else {
    glue::glue("HTTP {status_code}")
  }

  error_class$new(message = message, status_code = status_code, response = response, body = body)
}

#' Check if an error is retryable
#' @keywords internal
is_retryable_error <- function(error) {
  if (inherits(error, "APIConnectionError") || inherits(error, "APITimeoutError")) {
    return(TRUE)
  }

  if (inherits(error, "APIStatusError") && !is.null(error$status_code)) {
    retry_codes <- c(408, 409, 429, 500, 501, 502, 503, 504)
    return(error$status_code %in% retry_codes)
  }

  FALSE
}
