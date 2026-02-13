# Tests for Error Handling

test_that("Error classes exist", {
  err <- opencode:::OpencodeError$new("test error")
  expect_equal(err$message, "test error")

  err2 <- opencode:::APIConnectionError$new()
  expect_equal(err2$message, "Could not connect to the API")

  err3 <- opencode:::BadRequestError$new()
  expect_equal(err3$status_code, 400L)
})

test_that("Error classes have correct messages", {
  conn_err <- opencode:::APIConnectionError$new("connection failed")
  expect_equal(conn_err$message, "connection failed")

  auth_err <- opencode:::AuthenticationError$new()
  expect_equal(auth_err$message, "Authentication failed (401)")
})

test_that("APIStatusError has correct status codes", {
  expect_equal(opencode:::BadRequestError$new()$status_code, 400L)
  expect_equal(opencode:::AuthenticationError$new()$status_code, 401L)
  expect_equal(opencode:::PermissionDeniedError$new()$status_code, 403L)
  expect_equal(opencode:::NotFoundError$new()$status_code, 404L)
  expect_equal(opencode:::RateLimitError$new()$status_code, 429L)
  expect_equal(opencode:::InternalServerError$new()$status_code, 500L)
})

test_that("is_retryable_error works correctly", {
  # Connection errors are retryable
  expect_true(is_retryable_error(opencode:::APIConnectionError$new()))

  # Rate limit is retryable
  expect_true(is_retryable_error(opencode:::RateLimitError$new()))

  # 5xx errors are retryable
  expect_true(is_retryable_error(opencode:::InternalServerError$new(status_code = 503)))

  # 4xx client errors (except 429) are not retryable
  expect_false(is_retryable_error(opencode:::BadRequestError$new()))
  expect_false(is_retryable_error(opencode:::NotFoundError$new()))
})
