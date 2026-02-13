# Tests for Types

test_that("Session type works", {
  session <- Session(list(id = "sess-123", created_at = "2026-01-01"))
  expect_s3_class(session, "Session")
  expect_s3_class(session, "opencode_response")
  expect_equal(session$id, "sess-123")
})

test_that("SessionListResponse type works", {
  response <- SessionListResponse(list(sessions = list()))
  expect_s3_class(response, "SessionListResponse")
})

test_that("AssistantMessage type works", {
  msg <- AssistantMessage(list(role = "assistant", content = "Hello"))
  expect_s3_class(msg, "AssistantMessage")
})

test_that("App type works", {
  app <- App(list(name = "test-app", version = "1.0"))
  expect_s3_class(app, "App")
})

test_that("EventListResponse type works", {
  events <- EventListResponse(list(events = list()))
  expect_s3_class(events, "EventListResponse")
})

test_that("FindFilesResponse type works", {
  result <- FindFilesResponse(list(files = list()))
  expect_s3_class(result, "FindFilesResponse")
})

test_that("FileReadResponse type works", {
  result <- FileReadResponse(list(path = "/test.R", content = "x <- 1"))
  expect_s3_class(result, "FileReadResponse")
})

test_that("Config type works", {
  config <- Config(list(setting = "value"))
  expect_s3_class(config, "Config")
})

test_that("Message types work", {
  user_msg <- UserMessage(list(role = "user", content = "Hello"))
  expect_s3_class(user_msg, "UserMessage")
  expect_s3_class(user_msg, "message")

  assistant_msg <- AssistantMessage(list(role = "assistant", content = "Hi"))
  expect_s3_class(assistant_msg, "AssistantMessage")
})

test_that("Part types work", {
  text_part <- TextPart(list(type = "text", text = "Hello"))
  expect_s3_class(text_part, "TextPart")
  expect_s3_class(text_part, "message_part")

  tool_part <- ToolPart(list(type = "tool", tool = "bash"))
  expect_s3_class(tool_part, "ToolPart")
})
