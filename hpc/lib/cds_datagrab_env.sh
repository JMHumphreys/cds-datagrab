#!/usr/bin/env bash
set -euo pipefail

cds_datagrab_load_atlas_modules() {
  module purge
  module load r/4.5 udunits gdal proj geos git
}

cds_datagrab_repo_dir() {
  local script_dir
  script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[1]}")/../.." && pwd -P)"
  printf '%s\n' "${REPO_DIR:-${SLURM_SUBMIT_DIR:-$script_dir}}"
}

cds_datagrab_config_profile() {
  local config_path="$1"
  grep -oE 'profile:[[:space:]]*[A-Za-z]+' "$config_path" | head -n 1 | awk '{print $2}'
}

cds_datagrab_prepare_environment() {
  : "${CDS_DATAGRAB_ROOT:?CDS_DATAGRAB_ROOT must be explicitly set to an external, variable-specific output root}"
  : "${CDS_DATAGRAB_R_LIB:?CDS_DATAGRAB_R_LIB must point to the external installed cdsdatagrab library}"
  REPO_DIR="${REPO_DIR:-$(cds_datagrab_repo_dir)}"
  CONFIG="${CONFIG:?CONFIG must be set}"
  PROFILE="${PROFILE:-}"
  local config_path="$REPO_DIR/$CONFIG" config_profile
  [[ -f "$config_path" ]] || { echo "Configuration does not exist: $config_path" >&2; return 2; }
  config_profile="$(cds_datagrab_config_profile "$config_path")"
  [[ "$config_profile" == smoke || "$config_profile" == production ]] || { echo "Could not resolve profile from $config_path" >&2; return 2; }
  PROFILE="${PROFILE:-$config_profile}"
  [[ "$PROFILE" == "$config_profile" ]] || { echo "PROFILE=$PROFILE conflicts with configuration profile=$config_profile" >&2; return 2; }
  local repo_abs root_abs lib_abs
  repo_abs="$(cd "$REPO_DIR" && pwd -P)"
  root_abs="$(mkdir -p "$CDS_DATAGRAB_ROOT" && cd "$CDS_DATAGRAB_ROOT" && pwd -P)"
  lib_abs="$(mkdir -p "$CDS_DATAGRAB_R_LIB" && cd "$CDS_DATAGRAB_R_LIB" && pwd -P)"
  [[ "$root_abs" != "$repo_abs" && "$root_abs" != "$repo_abs"/* ]] || { echo "CDS_DATAGRAB_ROOT must be outside the repository checkout" >&2; return 2; }
  [[ "$lib_abs" != "$repo_abs" && "$lib_abs" != "$repo_abs"/* ]] || { echo "CDS_DATAGRAB_R_LIB must be outside the repository checkout" >&2; return 2; }
  HOME_R_LIB="${HOME_R_LIB:-$HOME/R/x86_64-pc-linux-gnu-library/4.5}"
  R_LIBS_USER="$CDS_DATAGRAB_R_LIB${HOME_R_LIB:+:$HOME_R_LIB}"
  unset R_LIBS_SITE
  export REPO_DIR CONFIG PROFILE CDS_DATAGRAB_ROOT CDS_DATAGRAB_R_LIB HOME_R_LIB R_LIBS_USER
}

cds_datagrab_validate_window() {
  START_DATE="${START_DATE:?START_DATE must be set as YYYY-MM-DD}"
  END_DATE="${END_DATE:?END_DATE must be set as YYYY-MM-DD}"
  [[ "$START_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ && "$END_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || { echo "START_DATE and END_DATE must use ISO YYYY-MM-DD" >&2; return 2; }
  [[ "${START_DATE:0:4}" == "${END_DATE:0:4}" ]] || { echo "Annual submission window must remain within one calendar year" >&2; return 2; }
  (( 10#${START_DATE:0:4} >= 2022 && 10#${END_DATE:0:4} <= 2026 )) || { echo "Annual window is outside the configured 2022-2026 production period" >&2; return 2; }
  export START_DATE END_DATE
}

cds_datagrab_print_summary() {
  printf 'repository: %s\nconfiguration: %s\nprofile: %s\noutput root: %s\nwindow: %s to %s\nR_LIBS_USER: %s\n' "$REPO_DIR" "$CONFIG" "$PROFILE" "$CDS_DATAGRAB_ROOT" "${START_DATE:-configured}" "${END_DATE:-configured}" "$R_LIBS_USER"
}
