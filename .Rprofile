# =========================================================================
# PROJECT INITIALIZATION: .Rprofile
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================

# 1. Automatically activate the reproducible renv sandbox environment
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# 2. Enforce secure CRAN repository and dynamically select binary type based on OS
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (Sys.info()["sysname"] == "Darwin" && Sys.info()["machine"] == "arm64") {
  # Native optimization exclusively for Apple Silicon architectures
  options(pkgType = "mac.binary.big-sur-arm64")
} else {
  # Default standard installation type for Windows, Linux, or Intel Macs
  options(pkgType = "default")
}

message(">>> [EpiTriage 2026] Local renv environment activated successfully.")
message(">>> [EpiTriage 2026] Global package installer configured for cross-platform compatibility.")
