#' Event Resource
#'
#' Provides methods for event-related API endpoints.
#' @keywords internal
NULL

#' EventResource
#'
#' Resource for event-related API endpoints.
#' @export
#' @keywords internal
EventResource <- R6::R6Class(
  "EventResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' List events
    #'
    #' Note: The /event endpoint may be a streaming SSE endpoint that waits
    #' for events. Returns NULL if it times out.
    #' @param timeout Timeout in seconds (default: 2)
    #' @return EventListResponse object or NULL
    list = function(timeout = 2) {
      tryCatch({
        # The /event endpoint is likely a streaming SSE endpoint
        # It will wait for events, so we need a short timeout
        config <- self$get_config()

        url <- paste0(config$base_url, "/event")
        req <- httr2::request(url) %>%
          httr2::req_method("GET") %>%
          httr2::req_timeout(timeout)

        response <- httr2::req_perform(req)
        body <- httr2::resp_body_string(response)

        if (nchar(body) > 0 && substr(body, 1, 1) == "[") {
          # It's a JSON array
          parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
          EventListResponse(parsed)
        } else {
          # Empty or non-JSON response
          EventListResponse(list(events = list()))
        }
      }, error = function(e) {
        # Timeout or error - return empty response
        cli::cli_inform("Event endpoint timed out or not available: {e$message}")
        EventListResponse(list(events = list()))
      })
    }
  )
)

#' AsyncEventResource
#'
#' Async resource for event-related API endpoints.
#' @export
#' @keywords internal
AsyncEventResource <- R6::R6Class(
  "AsyncEventResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    list_async = function(timeout = 2) {
      config <- self$get_config()

      future::future({
        tryCatch({
          url <- paste0(config$base_url, "/event")
          req <- httr2::request(url) %>%
            httr2::req_method("GET") %>%
            httr2::req_timeout(timeout)

          response <- httr2::req_perform(req)
          body <- httr2::resp_body_string(response)

          if (nchar(body) > 0 && substr(body, 1, 1) == "[") {
            parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
            EventListResponse(parsed)
          } else {
            EventListResponse(list(events = list()))
          }
        }, error = function(e) {
          EventListResponse(list(events = list()))
        })
      }, seed = TRUE)
    }
  )
)
