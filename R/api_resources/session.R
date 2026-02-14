#' Session Resource
#'
#' Provides methods for managing sessions.
#' @keywords internal
NULL

#' Session S3 Class
#'
#' Represents a session object from the API.
#' @keywords internal
NULL

#' SessionResource
#'
#' Resource for session-related API endpoints.
#' @export
#' @keywords internal
SessionResource <- R6::R6Class(
  "SessionResource",
  inherit = APIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    #' Create a new session
    #'
    #' @return Session object
    create = function() {
      self$.post("/session", type = "Session")
    },

    #' List all sessions
    #'
    #' @return SessionListResponse object
    list = function() {
      self$.get("/session", type = "SessionListResponse")
    },

    #' Delete a session
    #'
    #' @param id Session ID
    #' @return SessionDeleteResponse object
    delete = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.delete(paste0("/session/", id), type = "SessionDeleteResponse")
    },

    #' Abort a session
    #'
    #' @param id Session ID
    #' @return SessionAbortResponse object
    abort = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.post(paste0("/session/", id, "/abort"), type = "SessionAbortResponse")
    },

    #' Send a message to a session
    #'
    #' @param id Session ID
    #' @param model_id Model ID to use
    #' @param parts Message parts (list of text/tool parts)
    #' @param provider_id Provider ID
    #' @param message_id Message ID (optional)
    #' @param mode Mode (optional)
    #' @param system System prompt (optional)
    #' @param tools Tools configuration (optional)
    #' @return AssistantMessage object
    chat = function(id, model_id, parts, provider_id, message_id = NULL,
                    mode = NULL, system = NULL, tools = NULL) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(
        model_id = model_id,
        parts = parts,
        provider_id = provider_id
      )

      if (!is.null(message_id)) body$message_id <- message_id
      if (!is.null(mode)) body$mode <- mode
      if (!is.null(system)) body$system <- system
      if (!is.null(tools)) body$tools <- tools

      self$.post(paste0("/session/", id, "/message"), body = body, type = "AssistantMessage")
    },

    #' Initialize a session
    #'
    #' @param id Session ID
    #' @param message_id Message ID
    #' @param model_id Model ID
    #' @param provider_id Provider ID
    #' @return SessionInitResponse object
    init = function(id, message_id, model_id, provider_id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(
        message_id = message_id,
        model_id = model_id,
        provider_id = provider_id
      )

      self$.post(paste0("/session/", id, "/init"), body = body, type = "SessionInitResponse")
    },

    #' Get messages for a session
    #'
    #' @param id Session ID
    #' @return SessionMessagesResponse object
    messages = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.get(paste0("/session/", id, "/message"), type = "SessionMessagesResponse")
    },

    #' Revert a message in a session
    #'
    #' @param id Session ID
    #' @param message_id Message ID to revert
    #' @param part_id Part ID (optional)
    #' @return Session object
    revert = function(id, message_id, part_id = NULL) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      if (is.null(message_id)) {
        rlang::abort("Required parameter 'message_id' is missing or empty")
      }

      body <- list(message_id = message_id)
      if (!is.null(part_id)) body$part_id <- part_id

      self$.post(paste0("/session/", id, "/revert"), body = body, type = "Session")
    },

    #' Share a session
    #'
    #' @param id Session ID
    #' @return Session object
    share = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.post(paste0("/session/", id, "/share"), type = "Session")
    },

    #' Summarize a session
    #'
    #' @param id Session ID
    #' @param model_id Model ID for summarization
    #' @param provider_id Provider ID
    #' @return SessionSummarizeResponse object
    summarize = function(id, model_id, provider_id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(
        model_id = model_id,
        provider_id = provider_id
      )

      self$.post(paste0("/session/", id, "/summarize"), body = body, type = "SessionSummarizeResponse")
    },

    #' Unrevert messages in a session
    #'
    #' @param id Session ID
    #' @return Session object
    unrevert = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.post(paste0("/session/", id, "/unrevert"), type = "Session")
    },

    #' Unshare a session
    #'
    #' @param id Session ID
    #' @return Session object
    unshare = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.delete(paste0("/session/", id, "/share"), type = "Session")
    },

    #' Stream chat response (real-time)
    #'
    #' Sends a message and yields streamed response chunks as they arrive.
    #' This provides real-time feedback similar to CLI experiences.
    #'
    #' @param id Session ID
    #' @param model_id Model ID to use
    #' @param parts Message parts (list of text/tool parts)
    #' @param provider_id Provider ID
    #' @param message_id Message ID (optional)
    #' @param mode Mode (optional)
    #' @param system System prompt (optional)
    #' @param tools Tools configuration (optional)
    #' @param callback Optional callback function(chunk) for each chunk received
    #' @return Invisible list with full_response and chunks
    chat_stream = function(id, model_id, parts, provider_id, message_id = NULL,
                           mode = NULL, system = NULL, tools = NULL,
                           callback = NULL) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      config <- self$get_config()
      url <- paste0(config$base_url, "/session/", id, "/message")

      body <- list(
        model_id = model_id,
        parts = parts,
        provider_id = provider_id
      )

      if (!is.null(message_id)) body$message_id <- message_id
      if (!is.null(mode)) body$mode <- mode
      if (!is.null(system)) body$system <- system
      if (!is.null(tools)) body$tools <- tools

      body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

      cli::cli_inform("Streaming response from {model_id}...")

      result <- stream_text(
        url = url,
        method = "POST",
        body = body_json,
        headers = "Content-Type: application/json",
        timeout = config$timeout,
        callback = callback,
        on_error = function(e) {
          cli::cli_warn("Streaming error: {e$message}")
        }
      )

      result$model_id <- model_id
      result$provider_id <- provider_id

      invisible(result)
    }
  )
)

#' AsyncSessionResource
#'
#' Async resource for session-related API endpoints.
#' @export
#' @keywords internal
AsyncSessionResource <- R6::R6Class(
  "AsyncSessionResource",
  inherit = AsyncAPIResource,
  public = list(
    initialize = function(client) {
      super$initialize(client)
    },

    create_async = function() {
      self$.get_async("/session", type = "Session")
    },

    list_async = function() {
      self$.get_async("/session", type = "SessionListResponse")
    },

    delete_async = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.delete_async(paste0("/session/", id), type = "SessionDeleteResponse")
    },

    abort_async = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.post_async(paste0("/session/", id, "/abort"), type = "SessionAbortResponse")
    },

    chat_async = function(id, model_id, parts, provider_id, message_id = NULL,
                          mode = NULL, system = NULL, tools = NULL) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(
        model_id = model_id,
        parts = parts,
        provider_id = provider_id
      )

      if (!is.null(message_id)) body$message_id <- message_id
      if (!is.null(mode)) body$mode <- mode
      if (!is.null(system)) body$system <- system
      if (!is.null(tools)) body$tools <- tools

      self$.post_async(paste0("/session/", id, "/message"), body = body, type = "AssistantMessage")
    },

    init_async = function(id, message_id, model_id, provider_id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(
        message_id = message_id,
        model_id = model_id,
        provider_id = provider_id
      )

      self$.post_async(paste0("/session/", id, "/init"), body = body, type = "SessionInitResponse")
    },

    messages_async = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.get_async(paste0("/session/", id, "/message"), type = "SessionMessagesResponse")
    },

    revert_async = function(id, message_id, part_id = NULL) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(message_id = message_id)
      if (!is.null(part_id)) body$part_id <- part_id

      self$.post_async(paste0("/session/", id, "/revert"), body = body, type = "Session")
    },

    share_async = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.post_async(paste0("/session/", id, "/share"), type = "Session")
    },

    summarize_async = function(id, model_id, provider_id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }

      body <- list(
        model_id = model_id,
        provider_id = provider_id
      )

      self$.post_async(paste0("/session/", id, "/summarize"), body = body, type = "SessionSummarizeResponse")
    },

    unrevert_async = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.post_async(paste0("/session/", id, "/unrevert"), type = "Session")
    },

    unshare_async = function(id) {
      if (missing(id) || !nzchar(id)) {
        rlang::abort("Session ID is required")
      }
      self$.delete_async(paste0("/session/", id, "/share"), type = "Session")
    },

    chat_stream_async = function(id, model_id, parts, provider_id, message_id = NULL,
                                  mode = NULL, system = NULL, tools = NULL,
                                  callback = NULL) {
      config <- self$get_config()

      future::future({
        result <- self$client$session$chat_stream(
          id = id,
          model_id = model_id,
          parts = parts,
          provider_id = provider_id,
          message_id = message_id,
          mode = mode,
          system = system,
          tools = tools,
          callback = callback
        )
        result
      }, seed = TRUE)
    }
  )
)
