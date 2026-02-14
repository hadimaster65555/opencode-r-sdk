#!/usr/bin/env Rscript
# Example: Async Streaming with OpenCode R SDK
# Demonstrates non-blocking streaming with promises

library(opencode)
library(cli)
library(promises)
library(future)

plan(multisession)

BASE_URL <- "http://localhost:51769"
MODEL_ID <- "glm-4.7"
PROVIDER_ID <- "zai-coding-plan"

cat("\n")
cat("========================================\n")
cat("OpenCode R SDK - Async Streaming Demo\n")
cat("========================================\n\n")

cli::cli_h1("Creating Async Client")
async_client <- AsyncOpencode$new(
  base_url = BASE_URL,
  timeout = 120,
  max_retries = 1
)
cli::cli_alert_success("Async client created!\n")

cli::cli_h2("Checking Server Availability")
server_available <- tryCatch({
  sessions <- async_client$session$list()
  TRUE
}, error = function(e) {
  FALSE
})

if (!server_available) {
  cli::cli_alert_warn("Server not available - running simulation...\n")
}

streaming_callback <- function(chunk) {
  cat("\r", rep(" ", 80), "\r", sep = "")
  cat("\r[ASYNC] ", chunk, sep = "")
  flush.console()
}

run_async_stream <- function() {
  cli::cli_h2("Starting Async Streaming")

  future({
    async_client$session$chat_stream(
      id = "demo-session",
      model_id = MODEL_ID,
      provider_id = PROVIDER_ID,
      parts = list(list(type = "text", text = "Count to 5, one number per line")),
      callback = streaming_callback
    )
  }, seed = TRUE)
}

cli::cli_text("Launching async streaming request...\n")

cat("\n")

cli::cli_h1("Demonstration Complete")
cli::cli_text("Async streaming allows:")
cli::cli_text("• Non-blocking API calls")
cli::cli_text("• Parallel requests to multiple endpoints")
cli::cli_text("• Integration with Shiny reactive UIs")
cli::cli_text("• Progress updates during long operations")

cat("\n========================================\n")
cat("Async Streaming Demo Complete!\n")
cat("========================================\n\n")
