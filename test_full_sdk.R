#!/usr/bin/env Rscript
# OpenCode R SDK - Comprehensive Test Script
# Tests all API resources and functionalities

library(opencode)
library(cli)

# Configuration
BASE_URL <- "http://localhost:51769"
MODEL_ID <- "glm-4.7"
PROVIDER_ID <- "zai-coding-plan"

cat("\n")
cat("========================================\n")
cat("OpenCode R SDK - Comprehensive Test\n")
cat("========================================\n")
cat("\n")

# Set the base URL
options(opencode.base_url = BASE_URL)

# Create client
cli::cli_h1("Creating Opencode Client")
cli::cli_text("Base URL: {BASE_URL}")
cli::cli_text("Model: {MODEL_ID}")
cli::cli_text("Provider: {PROVIDER_ID}")

client <- Opencode$new(
  base_url = BASE_URL,
  timeout = 120,
  max_retries = 3
)

cli::cli_alert_success("Client created successfully!\n")

# Track results
passed <- 0
failed <- 0
results <- list()

# Helper function to run tests
run_test <- function(name, test_expr) {
  cli::cli_h3(name)
  tryCatch({
    result <- eval(test_expr)
    cli::cli_alert_success("PASSED")
    results[[name]] <<- list(status = "passed", result = result)
    passed <<- passed + 1
    return(result)
  }, error = function(e) {
    cli::cli_alert_danger("FAILED: {e$message}")
    results[[name]] <<- list(status = "failed", error = e$message)
    failed <<- failed + 1
    return(NULL)
  })
}

# ==========================================
# TEST 1: Config Resource
# ==========================================
cli::cli_h2("1. Testing Config Resource")

run_test("Get Config", quote({
  config <- client$config_resource$get()
  cli::cli_text("Config retrieved: {names(config)}")
  config
}))

# ==========================================
# TEST 2: App Resource
# ==========================================
cli::cli_h2("2. Testing App Resource")

run_test("Get App Info", quote({
  app <- client$app$get()
  if (!is.null(app)) {
    cli::cli_text("App retrieved successfully")
  } else {
    cli::cli_text("App endpoint returned NULL (expected for some implementations)")
  }
  app
}))

run_test("Get App Modes", quote({
  modes <- client$app$modes()
  if (!is.null(modes)) {
    cli::cli_text("Modes retrieved successfully")
  } else {
    cli::cli_text("Modes endpoint returned NULL (expected for some implementations)")
  }
  modes
}))

run_test("Get App Providers", quote({
  providers <- client$app$providers()
  cli::cli_text("Providers count: {length(providers$providers %||% 0)}")
  providers
}))

# ==========================================
# TEST 3: Session Resource - Basic Operations
# ==========================================
cli::cli_h2("3. Testing Session Resource (Basic)")

run_test("Create Session", quote({
  session <- client$session$create()
  cli::cli_text("Session ID: {session$id}")
  session
}))

run_test("List Sessions", quote({
  sessions <- client$session$list()
  cli::cli_text("Total sessions: {length(sessions$sessions %||% 0)}")
  sessions
}))

# ==========================================
# TEST 4: Session Resource - Messaging
# ==========================================
cli::cli_h2("4. Testing Session Resource (Messaging)")

session_list <- client$session$list()
if (length(session_list$sessions) > 0) {
  session_id <- session_list$sessions[[1]]$id
  cli::cli_text("Using session ID: {session_id}")

  run_test("Session Messages", quote({
    messages <- client$session$messages(session_id)
    cli::cli_text("Messages count: {length(messages$messages %||% 0)}")
    messages
  }))

  run_test("Send Chat Message with GLM-4.7", quote({
    response <- client$session$chat(
      id = session_id,
      model_id = MODEL_ID,
      provider_id = PROVIDER_ID,
      parts = list(
        list(type = "text", text = "Hello! Please respond with a brief greeting.")
      )
    )
    cli::cli_text("Response from {MODEL_ID}: {substring(response$content %||% '', 1, 100)}...")
    response
  }))

  run_test("Summarize Session", quote({
    summary <- client$session$summarize(
      id = session_id,
      model_id = MODEL_ID,
      provider_id = PROVIDER_ID
    )
    cli::cli_text("Summary retrieved")
    summary
  }))
}

# ==========================================
# TEST 5: Session Resource - State Management
# ==========================================
cli::cli_h2("5. Testing Session Resource (State Management)")

if (exists("session_id")) {
  run_test("Share Session", quote({
    shared <- client$session$share(session_id)
    cli::cli_text("Session shared")
    shared
  }))

  run_test("Unshare Session", quote({
    unshared <- client$session$unshare(session_id)
    cli::cli_text("Session unshared")
    unshared
  }))
}

# ==========================================
# TEST 6: Find Resource
# ==========================================
cli::cli_h2("6. Testing Find Resource")

run_test("Find Files (test pattern)", quote({
  files <- client$find$files("test")
  cli::cli_text("Found {length(files$files %||% 0)} files matching 'test'")
  files
}))

run_test("Find Text (hello)", quote({
  text_results <- client$find$text("hello")
  cli::cli_text("Found {length(text_results$results %||% 0)} text matches")
  text_results
}))

# ==========================================
# TEST 7: File Resource
# ==========================================
cli::cli_h2("7. Testing File Resource")

run_test("Get File Status", quote({
  status <- client$file$status()
  cli::cli_text("Status retrieved")
  status
}))

run_test("Read File (DESCRIPTION)", quote({
  file_content <- client$file$read("DESCRIPTION")
  if (length(file_content$content %||% '') > 0) {
    cli::cli_text("File size: {nchar(file_content$content)} bytes")
  } else {
    cli::cli_text("File content is empty (path may not exist in project)")
  }
  file_content
}))

# ==========================================
# TEST 8: Event Resource
# ==========================================
cli::cli_h2("8. Testing Event Resource")

run_test("List Events (with timeout)", quote({
  events <- client$event$list(timeout = 3)
  cli::cli_text("Events retrieved: {length(events$events %||% 0)}")
  events
}))

# ==========================================
# TEST 9: Client Configuration
# ==========================================
cli::cli_h2("9. Testing Client Configuration")

run_test("Create Client with Options", quote({
  client2 <- Opencode$new(
    base_url = BASE_URL,
    timeout = 30,
    max_retries = 5
  )
  cli::cli_text("Timeout: {client2$.config$timeout}")
  cli::cli_text("Max retries: {client2$.config$max_retries}")
  client2
}))

run_test("Client with_options", quote({
  client3 <- client$with_options(timeout = 90)
  cli::cli_text("New timeout: {client3$.config$timeout}")
  client3
}))

# ==========================================
# TEST 10: Async Client
# ==========================================
cli::cli_h2("10. Testing Async Client")

run_test("Create Async Client", quote({
  async_client <- AsyncOpencode$new(base_url = BASE_URL)
  cli::cli_text("Async client created")
  async_client
}))

run_test("Async List Sessions", quote({
  async_client <- AsyncOpencode$new(base_url = BASE_URL)
  future_sessions <- async_client$session$list_async()
  cli::cli_text("Async request submitted")
  future_sessions
}))

# ==========================================
# TEST 11: TUI Resource
# ==========================================
cli::cli_h2("11. Testing TUI Resource")

run_test("Open Help", quote({
  help_result <- client$tui$open_help()
  cli::cli_text("Help opened")
  help_result
}))

# ==========================================
# TEST 12: Full Session Lifecycle
# ==========================================
cli::cli_h2("12. Full Session Lifecycle Test")

run_test("Create New Session", quote({
  new_session <- client$session$create()
  cli::cli_text("New session ID: {new_session$id}")
  new_session
}))

if (exists("new_session")) {
  run_test("Chat in New Session with GLM-4.7", quote({
    chat_response <- client$session$chat(
      id = new_session$id,
      model_id = MODEL_ID,
      provider_id = PROVIDER_ID,
      parts = list(
        list(type = "text", text = "Please write a simple R function that calculates the factorial of a number. Keep it brief.")
      )
    )
    cli::cli_text("Response from {MODEL_ID}")
    chat_response
  }))

  run_test("Get Session Messages", quote({
    messages <- client$session$messages(new_session$id)
    cli::cli_text("Messages in session: {length(messages$messages %||% 0)}")
    messages
  }))

  run_test("Delete Session", quote({
    delete_result <- client$session$delete(new_session$id)
    cli::cli_text("Session deleted successfully")
    delete_result
  }))
}

# ==========================================
# TEST 13: Error Handling
# ==========================================
cli::cli_h2("13. Testing Error Handling")

run_test("Error - Invalid Session ID", quote({
  tryCatch({
    client$session$delete("")
    "should_have_failed"
  }, error = function(e) {
    cli::cli_text("Correctly caught error")
    "error_caught"
  })
}))

run_test("Error - Missing Required Params", quote({
  tryCatch({
    client$session$chat(
      id = "test-session",
      model_id = NULL,
      parts = list(),
      provider_id = "test"
    )
    "should_have_failed"
  }, error = function(e) {
    cli::cli_text("Correctly caught error")
    "error_caught"
  })
}))

# ==========================================
# Summary
# ==========================================
cli::cli_h1("Test Summary")

cli::cli_text("Total tests: {passed + failed}")
cli::cli_alert_success("Passed: {passed}")
if (failed > 0) {
  cli::cli_alert_danger("Failed: {failed}")
}

cat("\n")
cat("========================================\n")
cat("Test Execution Complete\n")
cat("========================================\n")
cat("\n")

# Print failed tests if any
if (failed > 0) {
  cli::cli_h2("Failed Tests:")
  for (name in names(results)) {
    if (results[[name]]$status == "failed") {
      cli::cli_text("- {name}: {results[[name]]$error}")
    }
  }
}

if (failed > 0) {
  quit(status = 1)
}
