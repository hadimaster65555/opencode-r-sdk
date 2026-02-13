#' Find Resource
#'
#' Provides methods for finding files and symbols.
#' @keywords internal
NULL

#' FindResource
#'
#' Resource for find-related API endpoints.
#' @export
#' @keywords internal
FindResource <- R6::R6Class(
  "FindResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' Find files matching a pattern
    #'
    #' @param pattern Pattern to match
    #' @param path Path to search (optional)
    #' @return FindFilesResponse object
    files = function(pattern, path = NULL) {
      query <- list(pattern = pattern)
      if (!is.null(path)) query$path <- path
      self$.get("/find/file", query = query, type = "FindFilesResponse")
    },

    #' Find symbols matching a pattern
    #'
    #' @param pattern Pattern to match
    #' @return FindSymbolsResponse object
    symbols = function(pattern) {
      self$.get("/find/symbol", query = list(pattern = pattern), type = "FindSymbolsResponse")
    },

    #' Find text matching a pattern
    #'
    #' @param pattern Pattern to match
    #' @return FindTextResponse object
    text = function(pattern) {
      self$.get("/find", query = list(pattern = pattern), type = "FindTextResponse")
    }
  )
)

#' AsyncFindResource
#'
#' Async resource for find-related API endpoints.
#' @export
#' @keywords internal
AsyncFindResource <- R6::R6Class(
  "AsyncFindResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    files_async = function(pattern, path = NULL) {
      query <- list(pattern = pattern)
      if (!is.null(path)) query$path <- path
      self$.get_async("/find/file", query = query, type = "FindFilesResponse")
    },

    symbols_async = function(pattern) {
      self$.get_async("/find/symbol", query = list(pattern = pattern), type = "FindSymbolsResponse")
    },

    text_async = function(pattern) {
      self$.get_async("/find", query = list(pattern = pattern), type = "FindTextResponse")
    }
  )
)
