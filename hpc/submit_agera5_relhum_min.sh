#!/bin/bash
set -euo pipefail
CONFIG="${CONFIG:-config/agera5_relhum_min_production.yml}"
: "${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be explicitly set to an external, variable-specific output root}"
JOB_NAME="cds_rhmin" SLURM_OUTPUT="cds_relhum_min_%j.out" SLURM_ERROR="cds_relhum_min_%j.err" CONFIG="$CONFIG" \
  "${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(pwd)}}/hpc/submit_era5_variable.sh"
