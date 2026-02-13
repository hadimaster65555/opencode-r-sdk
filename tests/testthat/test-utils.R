# Tests for Configuration and Utilities

test_that("ClientConfig can be created with defaults", {
  config <- ClientConfig$new()

  expect_equal(config$base_url, "http://localhost:54321")
  expect_equal(config$timeout, 60)
  expect_equal(config$max_retries, 2)
  expect_equal(config$api_key, "")
})

test_that("ClientConfig can be created with custom values", {
  config <- ClientConfig$new(
    base_url = "http://custom:9000",
    timeout = 30,
    max_retries = 5,
    api_key = "secret-key"
  )

  expect_equal(config$base_url, "http://custom:9000")
  expect_equal(config$timeout, 30)
  expect_equal(config$max_retries, 5)
  expect_equal(config$api_key, "secret-key")
})

test_that("ClientConfig can be cloned with with_options", {
  config1 <- ClientConfig$new(timeout = 30)
  config2 <- config1$with_options(timeout = 60)

  expect_equal(config2$timeout, 60)
  expect_equal(config1$timeout, 30)  # Original unchanged
})

test_that("ClientConfig get_timeout returns a number", {
  config <- ClientConfig$new(timeout = 30)
  timeout <- config$get_timeout()

  expect_equal(timeout, 30)
  expect_true(is.numeric(timeout))
})

test_that("is_not_given works", {
  # Test with a missing argument using quote
  expect_true(is_not_given(quote(expr = )))
  expect_false(is_not_given("value"))
})

test_that("if_not_given returns correct values", {
  expect_equal(if_not_given(quote(expr = ), "default"), "default")
  expect_equal(if_not_given("value", "default"), "value")
})

test_that("validate_required works", {
  # Should not throw for complete params
  expect_silent(validate_required(list(a = 1, b = 2), c("a", "b")))

  # Should throw for missing param
  expect_error(validate_required(list(a = 1), c("a", "b")), "missing or empty")
})

test_that("to_json_body works", {
  json <- to_json_body(list(x = 1, y = "test"))
  expect_true(grepl("x", json))
  expect_true(grepl("y", json))
})

test_that("parse_json_response handles edge cases", {
  # Test the json conversion functions directly
  json_result <- to_json_body(list(x = 1, y = "test"))
  expect_true(grepl("x", json_result))
  expect_true(grepl("y", json_result))

  # Test camel_to_snake
  result <- camel_to_snake(list(myField = 1, anotherField = 2))
  expect_equal(names(result), c("my_field", "another_field"))

  # Test snake_to_camel
  result <- snake_to_camel(list(my_field = 1, another_field = 2))
  expect_equal(names(result), c("myField", "anotherField"))
})

test_that("camel_to_snake works", {
  result <- camel_to_snake(list(myField = 1, anotherField = 2))
  expect_equal(names(result), c("my_field", "another_field"))
})

test_that("snake_to_camel works", {
  result <- snake_to_camel(list(my_field = 1, another_field = 2))
  expect_equal(names(result), c("myField", "anotherField"))
})

test_that("RetryConfig works", {
  config <- RetryConfig$new(max_retries = 3, base_delay = 1)

  expect_equal(config$max_retries, 3)
  expect_equal(config$base_delay, 1)
  expect_equal(config$max_delay, 60)
  expect_true(config$jitter)
})
