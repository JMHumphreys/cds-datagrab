#!/bin/bash
#SBATCH --job-name=cds_mintemp
#SBATCH -A disease_ecology
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --time=12:00:00
#SBATCH --mem=32G
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -euo pipefail
module purge
module load r/4.5 udunits gdal proj geos
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR}}"; CONFIG="${CONFIG:-config/era5_mintemp.yml}"; MODE="${MODE:-full}"; OBSERVED_END="${OBSERVED_END:-auto}"; FUTURE_END="${FUTURE_END:-2027-12-31}"; DRY_RUN="${DRY_RUN:-false}"
export CDS_DATAGRAB_DATA_ROOT="${CDS_DATAGRAB_DATA_ROOT:-/project/disease_ecology/NWScrewworm/data}"; cd "$REPO_DIR"; hostname; date; git status --short || true; git rev-parse HEAD || true; module list 2>&1; R --version; Rscript scripts/check_dependencies.R
args=(--config "$CONFIG" --mode "$MODE" --future-end "$FUTURE_END"); if [[ "$OBSERVED_END" != auto ]]; then args+=(--observed-end "$OBSERVED_END"); fi; if [[ "$DRY_RUN" == true ]]; then args+=(--dry-run); else args+=(--execute); [[ -n "${ecmwfr_PAT:-}" ]] || { echo 'ecmwfr_PAT is required for execution' >&2; exit 1; }; fi
Rscript scripts/run_pipeline.R "${args[@]}"
