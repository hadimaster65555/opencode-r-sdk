#' File Resource
#'
#' Provides methods for file-related API endpoints.
#' @keywords internal
NULL

#' FileResource
#'
#' Resource for file-related API endpoints.
#' @export
#' @keywords internal
FileResource <- R6::R6Class(
  "FileResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' Read a file
    #'
    #' @param path File path
    #' @return FileReadResponse object
    read = function(path) {
      self$.get("/file", query = list(path = path), type = "FileReadResponse")
    },

    #' Get file status
    #'
    #' @return FileStatusResponse object
    status = function() {
      self$.get("/file/status", type = "FileStatusResponse")
    }
  )
)

#' AsyncFileResource
#'
#' Async resource for file-related API endpoints.
#' @export
#' @keywords internal
AsyncFileResource <- R6::R6Class(
  "AsyncFileResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    read_async = function(path) {
      self$.get_async("/file", query = list(path = path), type = "FileReadResponse")
    },

    status_async = function() {
      self$.get_async("/file/status", type = "FileStatusResponse")
    }
  )
)
