#!/bin/bash
set -euo pipefail
CONFIG="${CONFIG:-config/agera5_relhum_min_production.yml}"
: "${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be explicitly set to an external, variable-specific output root}"
PROFILE="${PROFILE:-smoke}"; LOG_DIR="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"; mkdir -p "$LOG_DIR"
JOB_NAME="cds_relhum_min" SLURM_OUTPUT="$LOG_DIR/cds_relhum_min_%j.out" SLURM_ERROR="$LOG_DIR/cds_relhum_min_%j.err" CONFIG="$CONFIG" PROFILE="$PROFILE" \
  "${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(pwd)}}/hpc/submit_era5_variable.sh"
