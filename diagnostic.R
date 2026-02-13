#!/usr/bin/env Rscript
# API Diagnostic Script
# Explore actual API endpoints

library(httr2)
library(jsonlite)

BASE_URL <- "http://localhost:51769"

cat("\n========================================\n")
cat("API Endpoint Diagnostic\n")
cat("========================================\n\n")

# Helper to test endpoint
test_endpoint <- function(method, path, body = NULL, desc = "") {
  url <- paste0(BASE_URL, path)
  cat("Testing: ", desc, " (", method, " ", path, ")\n", sep = "")
  
  tryCatch({
    req <- request(url) %>% req_method(method)
    if (!is.null(body)) {
      req <- req %>% req_body_json(body)
    }
    resp <- req_perform(req)
    status <- resp_status(resp)
    cat("  Status: ", status, "\n")
    
    if (status < 300) {
      content <- resp_body_string(resp)
      if (nchar(content) > 0 && substr(content, 1, 1) == "{") {
        data <- fromJSON(content, simplifyVector = FALSE)
        cat("  Response keys: ", paste(names(data), collapse = ", "), "\n")
      } else {
        cat("  Response type: ", substr(content, 1, 50), "...\n")
      }
    }
    return(list(status = status, success = status < 300))
  }, error = function(e) {
    cat("  Error: ", e$message, "\n")
    return(list(status = 0, success = FALSE, error = e$message))
  })
}

# Test Config endpoints
cat("=== CONFIG ENDPOINTS ===\n")
test_endpoint("GET", "/config", NULL, "Get Config")

# Test App endpoints  
cat("\n=== APP ENDPOINTS ===\n")
test_endpoint("GET", "/app", NULL, "Get App")
test_endpoint("POST", "/app/init", NULL, "Init App")
test_endpoint("GET", "/mode", NULL, "Get Modes")
test_endpoint("GET", "/config/providers", NULL, "Get Providers")

# Test Session endpoints
cat("\n=== SESSION ENDPOINTS ===\n")
test_endpoint("GET", "/session", NULL, "List Sessions")
test_endpoint("POST", "/session", NULL, "Create Session")

# Test File endpoints
cat("\n=== FILE ENDPOINTS ===\n")
test_endpoint("GET", "/file?path=DESCRIPTION", NULL, "Read File")
test_endpoint("GET", "/file/status", NULL, "File Status")

# Test Event endpoints
cat("\n=== EVENT ENDPOINTS ===\n")
test_endpoint("GET", "/event", NULL, "List Events")

# Test Find endpoints
cat("\n=== FIND ENDPOINTS ===\n")
test_endpoint("GET", "/find/file?pattern=*.R", NULL, "Find Files")
test_endpoint("GET", "/find?pattern=function", NULL, "Find Text")

# Test TUI endpoints
cat("\n=== TUI ENDPOINTS ===\n")
test_endpoint("POST", "/tui/open-help", NULL, "Open Help")
test_endpoint("POST", "/tui/append-prompt", list(prompt = "test"), "Append Prompt")

cat("\n========================================\n")
cat("Diagnostic Complete\n")
cat("========================================\n")
