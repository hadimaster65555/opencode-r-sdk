#' Config Resource
#'
#' Provides methods for configuration-related API endpoints.
#' @keywords internal
NULL

#' ConfigResource
#'
#' Resource for config-related API endpoints.
#' @export
#' @keywords internal
ConfigResource <- R6::R6Class(
  "ConfigResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' Get configuration
    #'
    #' @return Config object
    get = function() {
      self$.get("/config", type = "Config")
    }
  )
)

#' AsyncConfigResource
#'
#' Async resource for config-related API endpoints.
#' @export
#' @keywords internal
AsyncConfigResource <- R6::R6Class(
  "AsyncConfigResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    get_async = function() {
      self$.get_async("/config", type = "Config")
    }
  )
)
