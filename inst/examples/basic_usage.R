# Basic Usage Examples for OpenCode R SDK
# Save this file to inst/examples/ for package examples

# Load the package
library(opencode)

# Create a client
client <- Opencode$new()

# List sessions
cat("Listing sessions...\n")
sessions <- client$session$list()
print(sessions)

# Create a new session
cat("\nCreating new session...\n")
session <- client$session$create()
cat("Session ID:", session$id, "\n")

# Send a message to the session
cat("\nSending a message...\n")
response <- client$session$chat(
  id = session$id,
  model_id = "gpt-4",
  provider_id = "openai",
  parts = list(
    list(type = "text", text = "Hello! Please write a simple R function that calculates the mean of a vector.")
  )
)
cat("Assistant response:\n")
print(response)

# Get app information
cat("\nGetting app information...\n")
app <- client$app$get()
print(app)

# Find files matching a pattern
cat("\nFinding R files...\n")
files <- client$find$files("*.R")
print(files)

# Clean up - delete the session
cat("\nDeleting session...\n")
client$session$delete(session$id)
cat("Session deleted.\n")
