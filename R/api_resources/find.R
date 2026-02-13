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
      tryCatch({
        query <- list(pattern = as.character(pattern))
        if (!is.null(path)) query$path <- as.character(path)
        self$.get("/find/file", query = query, type = "FindFilesResponse")
      }, error = function(e) {
        cli::cli_warn("Find files failed: {e$message}")
        FindFilesResponse(list(files = list(), pattern = pattern))
      })
    },

    #' Find symbols matching a pattern
    #'
    #' @param pattern Pattern to match
    #' @return FindSymbolsResponse object
    symbols = function(pattern) {
      tryCatch({
        self$.get("/find/symbol", query = list(pattern = as.character(pattern)), type = "FindSymbolsResponse")
      }, error = function(e) {
        cli::cli_warn("Find symbols failed: {e$message}")
        FindSymbolsResponse(list(symbols = list(), pattern = pattern))
      })
    },

    #' Find text matching a pattern
    #'
    #' @param pattern Pattern to match
    #' @return FindTextResponse object
    text = function(pattern) {
      tryCatch({
        self$.get("/find", query = list(pattern = as.character(pattern)), type = "FindTextResponse")
      }, error = function(e) {
        cli::cli_warn("Find text failed: {e$message}")
        FindTextResponse(list(results = list(), pattern = pattern))
      })
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
      tryCatch({
        query <- list(pattern = as.character(pattern))
        if (!is.null(path)) query$path <- as.character(path)
        self$.get_async("/find/file", query = query, type = "FindFilesResponse")
      }, error = function(e) {
        FindFilesResponse(list(files = list(), pattern = pattern))
      })
    },

    symbols_async = function(pattern) {
      tryCatch({
        self$.get_async("/find/symbol", query = list(pattern = as.character(pattern)), type = "FindSymbolsResponse")
      }, error = function(e) {
        FindSymbolsResponse(list(symbols = list(), pattern = pattern))
      })
    },

    text_async = function(pattern) {
      tryCatch({
        self$.get_async("/find", query = list(pattern = as.character(pattern)), type = "FindTextResponse")
      }, error = function(e) {
        FindTextResponse(list(results = list(), pattern = pattern))
      })
    }
  )
)
