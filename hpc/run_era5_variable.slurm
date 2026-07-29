#!/bin/bash
# Shared ERA5 variable worker. Job identity and log paths are supplied by sbatch.
#SBATCH -A disease_ecology
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --time=12:00:00
#SBATCH --mem=32G
set -euo pipefail
module purge
module load r/4.5 udunits gdal proj geos
REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR}}"
CONFIG="${CONFIG:?CONFIG must be set}"; MODE="${MODE:-plan}"; DRY_RUN="${DRY_RUN:-true}"; OBSERVED_END="${OBSERVED_END:-auto}"; FUTURE_END="${FUTURE_END:-}"
CDS_DATAGRAB_ROOT="${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be set}"
case "${DRY_RUN,,}" in true|1|yes) EXECUTION_FLAG="--dry-run" ;; false|0|no) EXECUTION_FLAG="--execute" ;; *) echo "DRY_RUN must be true or false; received: ${DRY_RUN}" >&2; exit 2 ;; esac
cd "$REPO_DIR"
hostname; date; git status --short || true; git rev-parse HEAD || true; module list 2>&1; R --version
Rscript scripts/check_dependencies.R --mode "$MODE"
args=(scripts/run_pipeline.R --config "$CONFIG" --mode "$MODE" --output-root "$CDS_DATAGRAB_ROOT" "$EXECUTION_FLAG")
if [[ "$OBSERVED_END" != "auto" ]]; then args+=(--observed-end "$OBSERVED_END"); fi
if [[ -n "$FUTURE_END" ]]; then args+=(--future-end "$FUTURE_END"); fi
printf 'Rscript'; printf ' %q' "${args[@]}"; printf '\n'; echo "CDS token validation will occur inside R."; Rscript "${args[@]}"
