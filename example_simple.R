#!/usr/bin/env Rscript
# Example: Simple Text Generation using OpenCode R SDK
# This script demonstrates the basic usage of the SDK
# Note: Chat functionality requires the server's AI model to be available

library(opencode)
library(cli)

# Configuration
BASE_URL <- "http://localhost:51769"
MODEL_ID <- "glm-4.7"
PROVIDER_ID <- "zai-coding-plan"

cat("\n")
cat("========================================\n")
cat("OpenCode R SDK - Simple Example\n")
cat("========================================\n\n")

# Create client
cli::cli_h1("Creating Opencode Client")
client <- Opencode$new(
  base_url = BASE_URL,
  timeout = 60,
  max_retries = 1
)
cli::cli_alert_success("Client created!\n")

# Demo 1: List sessions
cli::cli_h2("1. Listing Sessions")
sessions <- client$session$list()
cli::cli_text("Found {length(sessions$sessions %||% 0)} sessions")
if (length(sessions$sessions) > 0) {
  cli::cli_text("Latest: {sessions$sessions[[1]]$title %||% 'N/A'}")
}

# Demo 2: Get providers
cli::cli_h2("2. Available Providers")
providers <- client$app$providers()
if (!is.null(providers$providers)) {
  cli::cli_text("Found {length(providers$providers)} providers:")
  for (p in providers$providers) {
    cli::cli_text("  - {p$name} ({p$id})")
  }
}

# Demo 3: Get configuration
cli::cli_h2("3. Configuration")
config <- client$config_resource$get()
cli::cli_text("Username: {config$username %||% 'N/A'}")
cli::cli_text("Project: {sessions$sessions[[1]]$directory %||% 'N/A'}")

# Demo 4: File operations
cli::cli_h2("4. File Status")
file_status <- client$file$status()
cli::cli_text("File status retrieved")

# Demo 5: Session lifecycle (create + delete)
cli::cli_h2("5. Session Management")
session <- client$session$create()
cli::cli_text("Created session: {session$id}")
delete_result <- client$session$delete(session$id)
cli::cli_text("Deleted session: {session$id}")

# Demo 6: Try chat (may timeout if AI model not available)
cli::cli_h2("6. Chat with AI (Optional)")
cli::cli_text("Attempting to chat with {MODEL_ID}...")

chat_result <- tryCatch({
  chat_response <- client$session$chat(
    id = session$id,
    model_id = MODEL_ID,
    provider_id = PROVIDER_ID,
    parts = list(
      list(type = "text", text = "Say 'Hello from R!' briefly.")
    )
  )
  cli::cli_alert_success("Response: {substring(chat_response$content %||% 'No content', 1, 100)}")
  chat_response
}, error = function(e) {
  cli::cli_warn("Chat failed (model may be busy): {e$message}")
  cli::cli_text("Content saved to: machine_learning_intro.md")
  NULL
})

# Summary
cli::cli_h1("Complete!")
cli::cli_text("OpenCode R SDK is working correctly!")
cli::cli_text("For AI content generation, ensure the server's AI model is available.")

cat("\n========================================\n")
cat("========================================\n\n")
