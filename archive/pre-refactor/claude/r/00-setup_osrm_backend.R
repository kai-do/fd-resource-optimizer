# One-time setup: installs the osrm.backend package and the prebuilt OSRM binaries.
# To start a server, run claude/scripts/01-start_osrm_server.ps1.

if (!requireNamespace("osrm.backend", quietly = TRUE)) install.packages("osrm.backend")
library(osrm.backend)

# Downloads prebuilt OSRM binaries if osrm-routed isn't already available
osrm_installed <- tryCatch(
  !is.null(osrm_which(quiet = TRUE)$executable),
  error = function(e) FALSE
)

if (!osrm_installed) {
  osrm_install()
}

osrm_which()   # shows which osrm-routed will be used