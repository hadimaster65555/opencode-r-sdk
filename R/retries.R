#' Retry Logic with Exponential Backoff
#'
#' Implements automatic retry logic for transient errors.
#' @keywords internal
NULL

#' Execute a function with retry logic
#' @param expr The expression to execute
#' @param max_retries Maximum number of retries (default: 2)
#' @param base_delay Base delay in seconds (default: 0.5)
#' @param max_delay Maximum delay in seconds (default: 60)
#' @param jitter Add random jitter to delays (default: TRUE)
#' @return The result of the expression
#' @keywords internal
execute_with_retry <- function(expr,
                                max_retries = getOption("opencode.max_retries", 2),
                                base_delay = 0.5,
                                max_delay = 60,
                                jitter = TRUE) {
  result <- NULL
  last_error <- NULL

  for (attempt in seq_len(max_retries + 1)) {
    tryCatch({
      result <- eval(expr)
      return(result)
    }, error = function(e) {
      last_error <<- e

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

  stop(last_error)
}

#' Create a retry config object
#' @param max_retries Maximum retries
#' @param base_delay Base delay in seconds
#' @param max_delay Maximum delay in seconds
#' @param jitter Enable jitter
#' @keywords internal
RetryConfig <- R6::R6Class(
  "RetryConfig",
  public = list(
    max_retries = NULL,
    base_delay = NULL,
    max_delay = NULL,
    jitter = NULL,
    initialize = function(max_retries = 2, base_delay = 0.5, max_delay = 60, jitter = TRUE) {
      self$max_retries <- max_retries
      self$base_delay <- base_delay
      self$max_delay <- max_delay
      self$jitter <- jitter
    },
    with_retry = function(expr) {
      execute_with_retry(
        expr,
        max_retries = self$max_retries,
        base_delay = self$base_delay,
        max_delay = self$max_delay,
        jitter = self$jitter
      )
    }
  )
)
