#' Type Definitions
#'
#' S3 classes for API response types.
#' @keywords internal
NULL

#' Session Type
#'
#' Represents a session object.
#' @export
Session <- function(.data = list()) {
  if (is.null(.data$id)) {
    rlang::abort("Session must have an 'id' field")
  }
  structure(.data, class = c("Session", "opencode_response"))
}

#' SessionListResponse Type
#'
#' Response from listing sessions.
#' @export
SessionListResponse <- function(.data = list()) {
  structure(.data, class = c("SessionListResponse", "opencode_response"))
}

#' SessionDeleteResponse Type
#'
#' Response from deleting a session.
#' @export
SessionDeleteResponse <- function(.data = list()) {
  structure(.data, class = c("SessionDeleteResponse", "opencode_response"))
}

#' SessionAbortResponse Type
#'
#' Response from aborting a session.
#' @export
SessionAbortResponse <- function(.data = list()) {
  structure(.data, class = c("SessionAbortResponse", "opencode_response"))
}

#' AssistantMessage Type
#'
#' Represents an assistant message.
#' @export
AssistantMessage <- function(.data = list()) {
  structure(.data, class = c("AssistantMessage", "opencode_response"))
}

#' SessionInitResponse Type
#'
#' Response from initializing a session.
#' @export
SessionInitResponse <- function(.data = list()) {
  structure(.data, class = c("SessionInitResponse", "opencode_response"))
}

#' SessionMessagesResponse Type
#'
#' Response from getting session messages.
#' @export
SessionMessagesResponse <- function(.data = list()) {
  structure(.data, class = c("SessionMessagesResponse", "opencode_response"))
}

#' SessionSummarizeResponse Type
#'
#' Response from summarizing a session.
#' @export
SessionSummarizeResponse <- function(.data = list()) {
  structure(.data, class = c("SessionSummarizeResponse", "opencode_response"))
}

#' App Type
#'
#' Represents an app object.
#' @export
App <- function(.data = list()) {
  structure(.data, class = c("App", "opencode_response"))
}

#' AppInitResponse Type
#'
#' Response from initializing an app.
#' @export
AppInitResponse <- function(.data = list()) {
  structure(.data, class = c("AppInitResponse", "opencode_response"))
}

#' AppLogResponse Type
#'
#' Response from logging a message.
#' @export
AppLogResponse <- function(.data = list()) {
  structure(.data, class = c("AppLogResponse", "opencode_response"))
}

#' AppModesResponse Type
#'
#' Response from getting app modes.
#' @export
AppModesResponse <- function(.data = list()) {
  structure(.data, class = c("AppModesResponse", "opencode_response"))
}

#' AppProvidersResponse Type
#'
#' Response from getting app providers.
#' @export
AppProvidersResponse <- function(.data = list()) {
  structure(.data, class = c("AppProvidersResponse", "opencode_response"))
}

#' EventListResponse Type
#'
#' Response from listing events.
#' @export
EventListResponse <- function(.data = list()) {
  structure(.data, class = c("EventListResponse", "opencode_response"))
}

#' FindFilesResponse Type
#'
#' Response from finding files.
#' @export
FindFilesResponse <- function(.data = list()) {
  structure(.data, class = c("FindFilesResponse", "opencode_response"))
}

#' FindSymbolsResponse Type
#'
#' Response from finding symbols.
#' @export
FindSymbolsResponse <- function(.data = list()) {
  structure(.data, class = c("FindSymbolsResponse", "opencode_response"))
}

#' FindTextResponse Type
#'
#' Response from finding text.
#' @export
FindTextResponse <- function(.data = list()) {
  structure(.data, class = c("FindTextResponse", "opencode_response"))
}

#' File Type
#'
#' Represents a file object.
#' @export
File <- function(.data = list()) {
  structure(.data, class = c("File", "opencode_response"))
}

#' FileReadResponse Type
#'
#' Response from reading a file.
#' @export
FileReadResponse <- function(.data = list()) {
  structure(.data, class = c("FileReadResponse", "opencode_response"))
}

#' FileStatusResponse Type
#'
#' Response from getting file status.
#' @export
FileStatusResponse <- function(.data = list()) {
  structure(.data, class = c("FileStatusResponse", "opencode_response"))
}

#' Config Type
#'
#' Represents a config object.
#' @export
Config <- function(.data = list()) {
  structure(.data, class = c("Config", "opencode_response"))
}

#' TuiAppendPromptResponse Type
#'
#' Response from appending a prompt.
#' @export
TuiAppendPromptResponse <- function(.data = list()) {
  structure(.data, class = c("TuiAppendPromptResponse", "opencode_response"))
}

#' TuiOpenHelpResponse Type
#'
#' Response from opening help.
#' @export
TuiOpenHelpResponse <- function(.data = list()) {
  structure(.data, class = c("TuiOpenHelpResponse", "opencode_response"))
}

#' Message Types for Parts
#'
#' Different types of message parts.
#' @export
TextPart <- function(.data = list()) {
  structure(.data, class = c("TextPart", "message_part"))
}

ToolPart <- function(.data = list()) {
  structure(.data, class = c("ToolPart", "message_part"))
}

SymbolSource <- function(.data = list()) {
  structure(.data, class = c("SymbolSource", "message_source"))
}

FileSource <- function(.data = list()) {
  structure(.data, class = c("FileSource", "message_source"))
}

FilePartSourceText <- function(.data = list()) {
  structure(.data, class = c("FilePartSourceText", "message_source"))
}

FilePartSource <- function(.data = list()) {
  structure(.data, class = c("FilePartSource", "message_source"))
}

FilePartInput <- function(.data = list()) {
  structure(.data, class = c("FilePartInput", "message_part"))
}

SnapshotPart <- function(.data = list()) {
  structure(.data, class = c("SnapshotPart", "message_part"))
}

StepStartPart <- function(.data = list()) {
  structure(.data, class = c("StepStartPart", "message_part"))
}

StepFinishPart <- function(.data = list()) {
  structure(.data, class = c("StepFinishPart", "message_part"))
}

ToolStatePending <- function(.data = list()) {
  structure(.data, class = c("ToolStatePending", "tool_state"))
}

ToolStateRunning <- function(.data = list()) {
  structure(.data, class = c("ToolStateRunning", "tool_state"))
}

ToolStateCompleted <- function(.data = list()) {
  structure(.data, class = c("ToolStateCompleted", "tool_state"))
}

ToolStateError <- function(.data = list()) {
  structure(.data, class = c("ToolStateError", "tool_state"))
}

UserMessage <- function(.data = list()) {
  structure(.data, class = c("UserMessage", "message"))
}

Message <- function(.data = list()) {
  structure(.data, class = c("Message", "message"))
}

Symbol <- function(.data = list()) {
  structure(.data, class = c("Symbol", "opencode_response"))
}

Mode <- function(.data = list()) {
  structure(.data, class = c("Mode", "opencode_response"))
}

Model <- function(.data = list()) {
  structure(.data, class = c("Model", "opencode_response"))
}

Provider <- function(.data = list()) {
  structure(.data, class = c("Provider", "opencode_response"))
}

TextPartInput <- function(.data = list()) {
  structure(.data, class = c("TextPartInput", "message_part"))
}

Part <- function(.data = list()) {
  structure(.data, class = c("Part", "message_part"))
}

MessageAbortedError <- function(.data = list()) {
  structure(.data, class = c("MessageAbortedError", "opencode_error"))
}

ProviderAuthError <- function(.data = list()) {
  structure(.data, class = c("ProviderAuthError", "opencode_error"))
}

UnknownError <- function(.data = list()) {
  structure(.data, class = c("UnknownError", "opencode_error"))
}
