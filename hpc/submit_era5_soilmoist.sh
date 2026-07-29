#!/bin/bash
set -euo pipefail
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(pwd)}}"
PROFILE="${PROFILE:-smoke}"
CONFIG="${CONFIG:-config/era5_soilmoist_smoke.yml}"
MODE="${MODE:-plan}"
DRY_RUN="${DRY_RUN:-true}"
OBSERVED_END="${OBSERVED_END:-}"
CDS_DATAGRAB_ROOT="${CDS_DATAGRAB_ROOT:-}"
[[ "$PROFILE" == smoke || "$PROFILE" == production ]] || { echo "PROFILE must be smoke or production" >&2; exit 2; }
[[ -n "$CDS_DATAGRAB_ROOT" && "$CDS_DATAGRAB_ROOT" != "/" ]] || { echo "CDS_DATAGRAB_ROOT must be explicitly provided and safe" >&2; exit 2; }
[[ -n "$OBSERVED_END" ]] || { echo "OBSERVED_END is required" >&2; exit 2; }
mkdir -p "$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"
[[ -w "$CDS_DATAGRAB_ROOT" ]] || { echo "CDS_DATAGRAB_ROOT is not writable" >&2; exit 2; }
marker="$CDS_DATAGRAB_ROOT/.cds-datagrab-root"
if [[ -e "$marker" ]]; then grep -q '"application"[[:space:]]*:[[:space:]]*"cds-datagrab"' "$marker" || { echo "Invalid root marker" >&2; exit 2; }; else (umask 077; printf '{"application":"cds-datagrab","schema_version":1,"created_utc":"%s"}\n' "$(date -u +%FT%TZ)" > "$marker") || { echo "Could not create root marker" >&2; exit 2; }; fi
export REPO_DIR PROFILE CONFIG MODE DRY_RUN OBSERVED_END CDS_DATAGRAB_ROOT
exec "$REPO_DIR/hpc/submit_era5_mintemp.sh"
