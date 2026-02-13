#' Base Resource Class
#'
#' Provides base functionality for all API resources.
#' @keywords internal
NULL

#' API Resource (Base Class)
#'
#' Base class for all API resource classes.
#' @keywords internal
APIResource <- R6::R6Class(
  "APIResource",
  public = list(
    client = NULL,

    initialize = function(client) {
      self$client <- client
    },

    # Internal GET request
    .get = function(path, query = NULL, headers = NULL, type = NULL) {
      config <- self$client$.config
      response <- api_get(config, path, query = query, headers = headers)
      parse_response(response, type)
    },

    # Internal POST request
    .post = function(path, body = NULL, query = NULL, headers = NULL, type = NULL) {
      config <- self$client$.config
      response <- api_post(config, path, body = body, query = query, headers = headers)
      parse_response(response, type)
    },

    # Internal DELETE request
    .delete = function(path, query = NULL, headers = NULL, type = NULL) {
      config <- self$client$.config
      response <- api_delete(config, path, query = query, headers = headers)
      parse_response(response, type)
    },

    # Internal raw request (for .with_raw_response)
    .raw = function(method, path, body = NULL, query = NULL, headers = NULL) {
      config <- self$client$.config
      api_raw_request(config, method, path, body = body, query = query, headers = headers)
    },

    # Internal streaming request (for .with_streaming_response)
    .stream = function(path, query = NULL, method = "GET") {
      stream_events(self$client, path, query = query, method = method)
    },

    # Get config (for use by methods)
    get_config = function() {
      self$client$.config
    }
  )
)

#' Async API Resource (Base Class)
#'
#' Base class for all async API resource classes.
#' @keywords internal
AsyncAPIResource <- R6::R6Class(
  "AsyncAPIResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    # Async GET request
    .get_async = function(path, query = NULL, headers = NULL, type = NULL) {
      config <- self$client$.config

      future::future({
        response <- api_get(config, path, query = query, headers = headers)
        parse_response(response, type)
      }, seed = TRUE)
    },

    # Async POST request
    .post_async = function(path, body = NULL, query = NULL, headers = NULL, type = NULL) {
      config <- self$client$.config

      future::future({
        response <- api_post(config, path, body = body, query = query, headers = headers)
        parse_response(response, type)
      }, seed = TRUE)
    },

    # Async DELETE request
    .delete_async = function(path, query = NULL, headers = NULL, type = NULL) {
      config <- self$client$.config

      future::future({
        response <- api_delete(config, path, query = query, headers = headers)
        parse_response(response, type)
      }, seed = TRUE)
    }
  )
)

#' Resource with Raw Response
#'
#' Wrapper that provides access to raw HTTP responses.
#' @keywords internal
ResourceWithRawResponse <- R6::R6Class(
  "ResourceWithRawResponse",
  public = list(
    resource = NULL,
    initialize = function(resource) {
      self$resource <- resource
    }
  )
)

#' Resource with Streaming Response
#'
#' Wrapper that provides streaming access to responses.
#' @keywords internal
ResourceWithStreamingResponse <- R6::R6Class(
  "ResourceWithStreamingResponse",
  public = list(
    resource = NULL,
    initialize = function(resource) {
      self$resource <- resource
    }
  )
)
