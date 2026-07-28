#!/bin/bash
set -euo pipefail
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(pwd)}}"
CONFIG="${CONFIG:-config/era5_mintemp.yml}"
MODE="${MODE:-plan}"
DRY_RUN="${DRY_RUN:-true}"
OBSERVED_END="${OBSERVED_END:-auto}"
FUTURE_END="${FUTURE_END:-}"
CDS_DATAGRAB_ROOT="${CDS_DATAGRAB_ROOT:-/project/disease_ecology/cds-datagrab}"
PROFILE="${PROFILE:-}"
if [[ -z "$PROFILE" ]]; then PROFILE=$(sed -n 's/^  profile: *//p' "$REPO_DIR/$CONFIG" | head -n1); fi
case "$PROFILE" in smoke|production) ;; *) echo "PROFILE must be smoke or production" >&2; exit 2 ;; esac
mkdir -p "$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"
export CDS_DATAGRAB_ROOT CONFIG MODE DRY_RUN OBSERVED_END FUTURE_END
sbatch --export=ALL,CDS_DATAGRAB_ROOT="$CDS_DATAGRAB_ROOT",CONFIG="$CONFIG",MODE="$MODE",DRY_RUN="$DRY_RUN",OBSERVED_END="$OBSERVED_END",FUTURE_END="$FUTURE_END" --output="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE/%x_%j.out" --error="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE/%x_%j.err" "$REPO_DIR/hpc/run_era5_mintemp.slurm"
