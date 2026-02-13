# Source all R files in the package
# This file is used by the package to load all components

# Core utilities
source("R/utils.R", local = TRUE)
source("R/config.R", local = TRUE)
source("R/errors.R", local = TRUE)
source("R/retries.R", local = TRUE)
source("R/request.R", local = TRUE)
source("R/response.R", local = TRUE)
source("R/streaming.R", local = TRUE)
source("R/resource.R", local = TRUE)
source("R/client.R", local = TRUE)

# Types
source("R/types/types.R", local = TRUE)

# API Resources
source("R/api_resources/session.R", local = TRUE)
source("R/api_resources/app.R", local = TRUE)
source("R/api_resources/event.R", local = TRUE)
source("R/api_resources/find.R", local = TRUE)
source("R/api_resources/file.R", local = TRUE)
source("R/api_resources/config.R", local = TRUE)
source("R/api_resources/tui.R", local = TRUE)
