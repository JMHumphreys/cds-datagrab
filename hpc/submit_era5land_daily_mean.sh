#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"; REPO_DIR="${REPO_DIR:-${SLURM_SUBMIT_DIR:-$(cd "$SCRIPT_DIR/.." && pwd -P)}}"; source "$SCRIPT_DIR/lib/cds_datagrab_env.sh"
PROFILE="${PROFILE:-}"; CONFIG="${CONFIG:-config/era5land_daily_mean_utc06_smoke.yml}"; MODE="${MODE:-plan}"; DRY_RUN="${DRY_RUN:-true}"; START_DATE="${START_DATE:-}"; END_DATE="${END_DATE:-}"; PRODUCT="era5land_daily_mean_utc06"; PRODUCT_IDS="${PRODUCT_IDS:-era5land_tmean,era5land_soiltemp_l1_mean,era5land_soiltemp_l2_mean,era5land_soilwater_l1_mean,era5land_soilwater_l2_mean,era5land_surface_pressure_mean,era5land_lai_high_mean,era5land_lai_low_mean}"
cds_datagrab_prepare_environment
[[ -n "$START_DATE" || -n "$END_DATE" ]] && cds_datagrab_validate_window
mkdir -p "$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"
months=0; if [[ -n "$START_DATE" && -n "$END_DATE" ]]; then cursor="$(date -d "$START_DATE" +%Y-%m-01)"; finish="$(date -d "$END_DATE" +%Y-%m-01)"; while [[ "$cursor" < "$finish" || "$cursor" == "$finish" ]]; do months=$((months+1)); cursor="$(date -d "$cursor + 1 month" +%Y-%m-01)"; done; fi
cds_datagrab_print_summary
printf 'source family: era5land_daily_mean_utc06\nregistered products: %s\nrequested variables: 8\nexpected monthly request count: %s\n' "$PRODUCT_IDS" "${months:-configured}"
export REPO_DIR CONFIG PROFILE MODE DRY_RUN START_DATE END_DATE CDS_DATAGRAB_ROOT CDS_DATAGRAB_R_LIB PRODUCT_IDS
if [[ "${DIRECT_EXECUTION:-false}" == true ]]; then exec bash "$REPO_DIR/hpc/run_era5land_daily_mean.slurm"; fi
job_id=$(sbatch --parsable --job-name=cds_era5land_daily_mean --output="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE/cds_era5land_daily_mean_%j.out" --error="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE/cds_era5land_daily_mean_%j.err" --export=ALL,REPO_DIR,CONFIG,PROFILE,MODE,DRY_RUN,START_DATE,END_DATE,CDS_DATAGRAB_ROOT,CDS_DATAGRAB_R_LIB,PRODUCT_IDS "$REPO_DIR/hpc/run_era5land_daily_mean.slurm")
printf 'submitted job ID: %s\nSlurm log path: %s/logs/slurm/%s/cds_era5land_daily_mean_%%j.{out,err}\ndry-run state: %s\n' "$job_id" "$CDS_DATAGRAB_ROOT" "$PROFILE" "$DRY_RUN"
