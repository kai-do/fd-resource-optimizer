# Runs osrm-routed directly (no R process). Stop with Ctrl+C.
#   powershell -File scripts\start_osrm_server.ps1
# Override defaults with parameters, e.g. -Port 5001

param(
  [string]$OsrmBin = "C:/Users/ZemerN036438/AppData/Local/R/cache/R/osrm.backend/v26.10.0/osrm-routed.exe",
  [string]$Graph = "C:/OSMData/2026-10-13/georgia-261012.osrm",
  [int]$Port = 5000,
  # must be >= chunk_size (900) + number of stations in r/route_matrix.R
  [int]$MaxTableSize = 1500
)

if (-not (Test-Path $OsrmBin)) { Write-Error "osrm-routed not found at $OsrmBin (use -OsrmBin)"; exit 1 }
if (-not (Test-Path "$Graph.mldgr")) { Write-Error "Prepared MLD graph not found at $Graph.mldgr (use -Graph)"; exit 1 }

try {
  Invoke-WebRequest -UseBasicParsing -TimeoutSec 2 "http://127.0.0.1:$Port/nearest/v1/driving/-84.5499,33.9526" | Out-Null
  Write-Error "An OSRM server is already answering on port $Port. Stop it first."; exit 1
} catch [System.Net.WebException] {
  # nothing answering (or an error response) -> only a connection failure means the port is free
  if ($_.Exception.Response) { Write-Error "Something is already answering on port $Port."; exit 1 }
}

Write-Host "OSRM server on http://127.0.0.1:$Port/ -- press Ctrl+C to stop"
& $OsrmBin -i 127.0.0.1 -p $Port -a MLD --max-table-size $MaxTableSize $Graph
