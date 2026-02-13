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
    #' @return EventListResponse object
    list = function() {
      self$.get("/event", type = "EventListResponse")
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

    list_async = function() {
      self$.get_async("/event", type = "EventListResponse")
    }
  )
)
