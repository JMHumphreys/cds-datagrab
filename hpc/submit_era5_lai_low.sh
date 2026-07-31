#!/bin/bash
set -euo pipefail
CONFIG="${CONFIG:-config/era5_lai_low_production.yml}"
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(cd "$SCRIPT_DIR/.." && pwd -P)}}"
: "${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be explicitly set to an external, variable-specific output root}"
PROFILE="${PROFILE:-smoke}"; LOG_DIR="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"; mkdir -p "$LOG_DIR"
JOB_NAME="cds_lai_low" SLURM_OUTPUT="$LOG_DIR/cds_lai_low_%j.out" SLURM_ERROR="$LOG_DIR/cds_lai_low_%j.err" CONFIG="$CONFIG" PROFILE="$PROFILE" \
  bash "$REPO_DIR/hpc/submit_era5_variable.sh"
