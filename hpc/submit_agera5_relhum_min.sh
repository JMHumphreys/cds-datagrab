#!/bin/bash
set -euo pipefail
CONFIG="${CONFIG:-config/agera5_relhum_min_production.yml}"
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(cd "$SCRIPT_DIR/.." && pwd -P)}}"
source "$SCRIPT_DIR/lib/cds_datagrab_env.sh"
: "${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be explicitly set to an external, variable-specific output root}"
PROFILE="${PROFILE:-$(cds_datagrab_config_profile "$REPO_DIR/$CONFIG")}"; START_DATE="${START_DATE:-}"; END_DATE="${END_DATE:-}"; LOG_DIR="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"; mkdir -p "$LOG_DIR"
CONFIG="$CONFIG" REPO_DIR="$REPO_DIR" PROFILE="$PROFILE" CDS_DATAGRAB_ROOT="$CDS_DATAGRAB_ROOT" CDS_DATAGRAB_R_LIB="${CDS_DATAGRAB_R_LIB:-}" cds_datagrab_prepare_environment
PROFILE="$PROFILE" START_DATE="$START_DATE" END_DATE="$END_DATE" cds_datagrab_print_summary
JOB_NAME="cds_relhum_min" SLURM_OUTPUT="$LOG_DIR/cds_relhum_min_%j.out" SLURM_ERROR="$LOG_DIR/cds_relhum_min_%j.err" CONFIG="$CONFIG" PROFILE="$PROFILE" START_DATE="$START_DATE" END_DATE="$END_DATE" \
  bash "$REPO_DIR/hpc/submit_era5_variable.sh"
