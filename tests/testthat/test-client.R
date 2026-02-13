# Tests for the Opencode client

test_that("Client can be created with default settings", {
  client <- Opencode$new()

  expect_s3_class(client, "Opencode")
  expect_false(is.null(client$session))
  expect_false(is.null(client$app))
  expect_false(is.null(client$event))
  expect_false(is.null(client$find))
  expect_false(is.null(client$file))
  expect_false(is.null(client$config_resource))
  expect_false(is.null(client$tui))
})

test_that("Client can be created with custom settings", {
  client <- Opencode$new(
    base_url = "http://custom:8080",
    timeout = 30,
    max_retries = 5
  )

  expect_s3_class(client, "Opencode")
})

test_that("Client can be copied with with_options", {
  client1 <- Opencode$new(timeout = 30)
  client2 <- client1$with_options(timeout = 60)

  expect_s3_class(client2, "Opencode")
})

test_that("Async client can be created", {
  client <- AsyncOpencode$new()

  expect_s3_class(client, "AsyncOpencode")
  expect_s3_class(client, "Opencode")
})

test_that("Client has all expected resources", {
  client <- Opencode$new()

  expect_true("session" %in% names(client))
  expect_true("app" %in% names(client))
  expect_true("event" %in% names(client))
  expect_true("find" %in% names(client))
  expect_true("file" %in% names(client))
  expect_true("config_resource" %in% names(client))
  expect_true("tui" %in% names(client))
})

test_that("Client config works", {
  client <- Opencode$new(timeout = 45)
  expect_equal(client$.config$timeout, 45)
})
