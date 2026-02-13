# Package initialization
.onLoad <- function(libname, pkgname) {
  # Set up default options
  options(
    opencode.base_url = Sys.getenv("OPENCODE_BASE_URL", "http://localhost:54321"),
    opencode.timeout = as.numeric(Sys.getenv("OPENCODE_TIMEOUT", "60")),
    opencode.max_retries = as.numeric(Sys.getenv("OPENCODE_MAX_RETRIES", "2")),
    opencode.api_key = Sys.getenv("OPENCODE_API_KEY", "")
  )
}

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  cli::cli_h1(pkgname)
  cli::cli_text("Opencode R SDK v{version}")
  cli::cli_text("API base URL: {getOption('opencode.base_url')}")
}
