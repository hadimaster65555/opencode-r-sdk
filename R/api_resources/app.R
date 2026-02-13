#' App Resource
#'
#' Provides methods for app-related API endpoints.
#' @keywords internal
NULL

#' AppResource
#'
#' Resource for app-related API endpoints.
#' @export
#' @keywords internal
AppResource <- R6::R6Class(
  "AppResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' Get app information
    #'
    #' @return App object
    get = function() {
      self$.get("/app", type = "App")
    },

    #' Initialize the app
    #'
    #' @return AppInitResponse object
    init = function() {
      self$.post("/app/init", type = "AppInitResponse")
    },

    #' Log a message
    #'
    #' @param message Message to log
    #' @param level Log level (info, warning, error)
    #' @return AppLogResponse object
    log = function(message, level = "info") {
      body <- list(message = message, level = level)
      self$.post("/log", body = body, type = "AppLogResponse")
    },

    #' Get available modes
    #'
    #' @return AppModesResponse object
    modes = function() {
      self$.get("/mode", type = "AppModesResponse")
    },

    #' Get available providers
    #'
    #' @return AppProvidersResponse object
    providers = function() {
      self$.get("/config/providers", type = "AppProvidersResponse")
    }
  )
)

#' AsyncAppResource
#'
#' Async resource for app-related API endpoints.
#' @export
#' @keywords internal
AsyncAppResource <- R6::R6Class(
  "AsyncAppResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    get_async = function() {
      self$.get_async("/app", type = "App")
    },

    init_async = function() {
      self$.post_async("/app/init", type = "AppInitResponse")
    },

    log_async = function(message, level = "info") {
      body <- list(message = message, level = level)
      self$.post_async("/log", body = body, type = "AppLogResponse")
    },

    modes_async = function() {
      self$.get_async("/mode", type = "AppModesResponse")
    },

    providers_async = function() {
      self$.get_async("/config/providers", type = "AppProvidersResponse")
    }
  )
)
