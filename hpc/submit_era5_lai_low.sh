#!/bin/bash
set -euo pipefail
CONFIG="${CONFIG:-config/era5_lai_low_production.yml}"
: "${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be explicitly set to an external, variable-specific output root}"
JOB_NAME="cds_lailow" SLURM_OUTPUT="cds_lai_low_%j.out" SLURM_ERROR="cds_lai_low_%j.err" CONFIG="$CONFIG" \
  "${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(pwd)}}/hpc/submit_era5_variable.sh"
