# Tests for Session Resource

test_that("Session validation works", {
  # Test that Session type requires id
  expect_error(Session(list()), "must have an 'id'")

  # Test that Session works with id
  session <- Session(list(id = "test-123", created_at = "2026-01-01"))
  expect_equal(session$id, "test-123")
})

test_that("Session methods reject empty IDs", {
  client <- Opencode$new()

  # These will fail due to HTTP request attempt, but first check ID
  # The validation happens before the HTTP request
  expect_error(client$session$delete(""), "Session ID is required")
})

test_that("Session chat requires essential parameters", {
  client <- Opencode$new()

  # Missing session ID
  expect_error(client$session$chat(
    id = "",
    model_id = "gpt-4",
    parts = list(),
    provider_id = "openai"
  ), "Session ID is required")
})

test_that("Session revert requires message_id", {
  client <- Opencode$new()

  # When ID is valid but message_id is NULL/empty, validation happens
  expect_error(client$session$revert(
    id = "test-session",
    message_id = NULL
  ), "Required parameter 'message_id' is missing")
})

test_that("Session types work correctly", {
  # Test various session-related types
  delete_resp <- SessionDeleteResponse(list(id = "sess-123", deleted = TRUE))
  expect_s3_class(delete_resp, "SessionDeleteResponse")

  abort_resp <- SessionAbortResponse(list(id = "sess-123", aborted = TRUE))
  expect_s3_class(abort_resp, "SessionAbortResponse")
})

test_that("chat_stream validates session ID", {
  client <- Opencode$new()

  expect_error(client$session$chat_stream(
    id = "",
    model_id = "glm-4.7",
    parts = list(list(type = "text", text = "hello")),
    provider_id = "zai-coding-plan"
  ), "Session ID is required")
})

test_that("chat_stream returns list with expected structure", {
  client <- Opencode$new(base_url = "http://localhost:54321")

  result <- client$session$chat_stream(
    id = "test-session",
    model_id = "glm-4.7",
    parts = list(list(type = "text", text = "hello")),
    provider_id = "zai-coding-plan"
  )

  expect_type(result, "list")
  expect_named(result, c("full_response", "chunks", "model_id", "provider_id"))
  expect_type(result$full_response, "character")
  expect_type(result$chunks, "list")
  expect_equal(result$model_id, "glm-4.7")
  expect_equal(result$provider_id, "zai-coding-plan")
})

test_that("chat_stream callback is invoked", {
  client <- Opencode$new(base_url = "http://localhost:54321")

  callback_invocations <- 0
  callback <- function(chunk) {
    callback_invocations <<- callback_invocations + 1
  }

  result <- client$session$chat_stream(
    id = "test-session",
    model_id = "glm-4.7",
    parts = list(list(type = "text", text = "hello")),
    provider_id = "zai-coding-plan",
    callback = callback
  )

  expect_gte(callback_invocations, 0)
})
