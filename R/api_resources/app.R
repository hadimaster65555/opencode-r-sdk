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
    #' Note: The /app endpoint may not return JSON in all implementations.
    #' @return App object or NULL
    get = function() {
      tryCatch({
        response <- self$.get("/app", type = "App")
        if (inherits(response, "App") && !is.null(response$id)) {
          response
        } else {
          NULL
        }
      }, error = function(e) {
        NULL
      })
    },

    #' Initialize the app
    #'
    #' Note: The /app/init endpoint may not return JSON in all implementations.
    #' @return AppInitResponse object or NULL
    init = function() {
      tryCatch({
        response <- self$.post("/app/init", type = "AppInitResponse")
        if (inherits(response, "AppInitResponse")) {
          response
        } else {
          NULL
        }
      }, error = function(e) {
        NULL
      })
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
    #' Note: The /mode endpoint may not return JSON in all implementations.
    #' @return AppModesResponse object or NULL
    modes = function() {
      tryCatch({
        response <- self$.get("/mode", type = "AppModesResponse")
        if (inherits(response, "AppModesResponse")) {
          response
        } else {
          NULL
        }
      }, error = function(e) {
        NULL
      })
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
      tryCatch({
        self$.get_async("/app", type = "App")
      }, error = function(e) {
        NULL
      })
    },

    init_async = function() {
      tryCatch({
        self$.post_async("/app/init", type = "AppInitResponse")
      }, error = function(e) {
        NULL
      })
    },

    log_async = function(message, level = "info") {
      body <- list(message = message, level = level)
      self$.post_async("/log", body = body, type = "AppLogResponse")
    },

    modes_async = function() {
      tryCatch({
        self$.get_async("/mode", type = "AppModesResponse")
      }, error = function(e) {
        NULL
      })
    },

    providers_async = function() {
      self$.get_async("/config/providers", type = "AppProvidersResponse")
    }
  )
)
