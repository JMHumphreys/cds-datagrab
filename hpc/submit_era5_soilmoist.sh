#!/bin/bash
set -euo pipefail
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(pwd)}}"
CONFIG="${CONFIG:-config/era5_soilmoist_smoke.yml}" MODE="${MODE:-plan}" DRY_RUN="${DRY_RUN:-true}" OBSERVED_END="${OBSERVED_END:-}" FUTURE_END="${FUTURE_END:-}" CDS_DATAGRAB_ROOT="${CDS_DATAGRAB_ROOT:-}" PROFILE="${PROFILE:-smoke}" JOB_NAME="cds_soil" SLURM_OUTPUT="${SLURM_OUTPUT:-$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE/cds_soilmoist_%j.out}" SLURM_ERROR="${SLURM_ERROR:-$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE/cds_soilmoist_%j.err}" SBATCH_SCRIPT="${SBATCH_SCRIPT:-$REPO_DIR/hpc/run_era5_variable.slurm}" \
  bash "$REPO_DIR/hpc/submit_era5_variable.sh"
