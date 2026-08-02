#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/lib/cds_datagrab_env.sh"
product=""; year=""; mode="plan"; output_root=""
while (($#)); do
  case "$1" in
    --product) product="$2"; shift 2;;
    --year) year="$2"; shift 2;;
    --mode) mode="$2"; shift 2;;
    --output-root) output_root="$2"; shift 2;;
    *) echo "Unknown argument: $1" >&2; exit 2;;
  esac
done
case "$product" in
  era5_mintemp) wrapper="$SCRIPT_DIR/submit_era5_mintemp.sh"; config="config/era5_mintemp_production.yml"; root_default="/project/disease_ecology/cds-datagrab-mintemp-production-output";;
  era5_soilmoist) wrapper="$SCRIPT_DIR/submit_era5_soilmoist.sh"; config="config/era5_soilmoist_production.yml"; root_default="/project/disease_ecology/cds-datagrab-soilmoist-production-output";;
  era5_lai_low) wrapper="$SCRIPT_DIR/submit_era5_lai_low.sh"; config="config/era5_lai_low_production.yml"; root_default="/project/disease_ecology/cds-datagrab-lai-low-production-output";;
  agera5_relhum_min) wrapper="$SCRIPT_DIR/submit_agera5_relhum_min.sh"; config="config/agera5_relhum_min_production.yml"; root_default="/project/disease_ecology/cds-datagrab-relhum-min-production-output";;
  *) echo "Unknown product: $product" >&2; exit 2;;
esac
[[ "$year" =~ ^20[2-2][0-6]$ ]] || { echo "Year must be between 2022 and 2026" >&2; exit 2; }
case "$mode" in plan) dry_run=true;; execute) dry_run=false;; *) echo "Mode must be plan or execute" >&2; exit 2;; esac
export CONFIG="$config" PROFILE=production START_DATE="${year}-01-01" END_DATE="${year}-12-31" DRY_RUN="$dry_run" CDS_DATAGRAB_ROOT="${output_root:-$root_default}"
cds_datagrab_prepare_environment
cds_datagrab_validate_window
cds_datagrab_print_summary
exec bash "$wrapper"
