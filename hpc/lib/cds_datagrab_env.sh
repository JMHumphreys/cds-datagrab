#!/usr/bin/env bash
set -euo pipefail

CDS_DATAGRAB_PRODUCTION_DEFAULT="/project/disease_ecology/cds-datagrab-output"
CDS_DATAGRAB_SMOKE_DEFAULT="/project/disease_ecology/cds-datagrab-smoke-output"
CDS_DATAGRAB_R_LIB="${CDS_DATAGRAB_R_LIB:-/project/disease_ecology/cds-datagrab-r-library/4.5}"
HOME_R_LIB="${HOME_R_LIB:-/home/john.humphreys/R/x86_64-pc-linux-gnu-library/4.5}"
R_LIBS_USER="${R_LIBS_USER:-${CDS_DATAGRAB_R_LIB}:${HOME_R_LIB}}"
unset R_LIBS_SITE

cds_datagrab_load_atlas_modules() {
  module purge
  module load r/4.5
  module load udunits
  module load gdal
  module load proj
  module load geos
  module load git
}

cds_datagrab_repo_dir() {
  local script_dir
  script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[1]}")/../.." && pwd -P)"
  printf '%s\n' "${REPO_DIR:-${SLURM_SUBMIT_DIR:-$script_dir}}"
}

cds_datagrab_config_profile() {
  local config_path="$1" profile
  profile="$(awk '/^project:/{on=1; next} on && /^[^ ]/{exit} on && /^  profile:/{print $2; exit}' "$config_path")"
  [[ "$profile" == production || "$profile" == smoke ]] || { echo "Could not resolve profile from $config_path" >&2; return 2; }
  printf '%s\n' "$profile"
}

cds_datagrab_product_id() {
  awk '/^project:/{on=1; next} on && /^[^ ]/{exit} on && /^  dataset_id:/{print $2; exit}' "$1"
}

cds_datagrab_resolve_root() {
  local profile="$1" explicit="${CDS_DATAGRAB_ROOT:-}" profile_root source
  if [[ -n "$explicit" ]]; then source="CDS_DATAGRAB_ROOT"; profile_root="$explicit"
  elif [[ "$profile" == production && -n "${CDS_DATAGRAB_PRODUCTION_ROOT:-}" ]]; then source="CDS_DATAGRAB_PRODUCTION_ROOT"; profile_root="$CDS_DATAGRAB_PRODUCTION_ROOT"
  elif [[ "$profile" == smoke && -n "${CDS_DATAGRAB_SMOKE_ROOT:-}" ]]; then source="CDS_DATAGRAB_SMOKE_ROOT"; profile_root="$CDS_DATAGRAB_SMOKE_ROOT"
  elif [[ "$profile" == production ]]; then source="Atlas production default"; profile_root="$CDS_DATAGRAB_PRODUCTION_DEFAULT"
  else source="Atlas smoke default"; profile_root="$CDS_DATAGRAB_SMOKE_DEFAULT"; fi
  [[ "$profile_root" == /* && "$profile_root" != "/" ]] || { echo "Resolved output root must be an absolute non-root path" >&2; return 2; }
  CDS_DATAGRAB_ROOT="$profile_root"; CDS_DATAGRAB_ROOT_SOURCE="$source"
  export CDS_DATAGRAB_ROOT CDS_DATAGRAB_ROOT_SOURCE
}

cds_datagrab_validate_root_marker() {
  local marker="$CDS_DATAGRAB_ROOT/.cds-datagrab-root"
  if [[ -e "$marker" ]]; then grep -q '"application"[[:space:]]*:[[:space:]]*"cds-datagrab"' "$marker" || { echo "Invalid cds-datagrab root marker: $marker" >&2; return 2; }; fi
}

cds_datagrab_prepare_environment() {
  REPO_DIR="${REPO_DIR:-$(cds_datagrab_repo_dir)}"
  CONFIG="${CONFIG:?CONFIG must be set}"
  local config_path="$REPO_DIR/$CONFIG" config_profile
  [[ -f "$config_path" ]] || { echo "Configuration does not exist: $config_path" >&2; return 2; }
  config_profile="$(cds_datagrab_config_profile "$config_path")"
  PROFILE="${PROFILE:-$config_profile}"
  [[ "$PROFILE" == "$config_profile" ]] || { echo "PROFILE=$PROFILE conflicts with configuration profile=$config_profile" >&2; return 2; }
  cds_datagrab_resolve_root "$PROFILE"
  local repo_abs root_abs
  repo_abs="$(cd "$REPO_DIR" && pwd -P)"; root_abs="$(mkdir -p "$CDS_DATAGRAB_ROOT" && cd "$CDS_DATAGRAB_ROOT" && pwd -P)"
  [[ "$root_abs" != "$repo_abs" && "$root_abs" != "$repo_abs"/* ]] || { echo "Output root must be outside the repository checkout" >&2; return 2; }
  CDS_DATAGRAB_R_LIB="${CDS_DATAGRAB_R_LIB:?CDS_DATAGRAB_R_LIB must point to the external installed cdsdatagrab library}"
  export REPO_DIR CONFIG PROFILE CDS_DATAGRAB_ROOT CDS_DATAGRAB_R_LIB HOME_R_LIB R_LIBS_USER
  unset R_LIBS_SITE
  cds_datagrab_validate_root_marker
}

cds_datagrab_validate_window() {
  START_DATE="${START_DATE:?START_DATE must be set as YYYY-MM-DD}"; END_DATE="${END_DATE:?END_DATE must be set as YYYY-MM-DD}"
  [[ "$START_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ && "$END_DATE" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || { echo "START_DATE and END_DATE must use ISO YYYY-MM-DD" >&2; return 2; }
  [[ "${START_DATE:0:4}" == "${END_DATE:0:4}" ]] || { echo "Annual submission window must remain within one calendar year" >&2; return 2; }
  (( 10#${START_DATE:0:4} >= 2022 && 10#${END_DATE:0:4} <= 2026 )) || { echo "Annual window is outside the configured 2022-2026 production period" >&2; return 2; }
  export START_DATE END_DATE
}

cds_datagrab_print_summary() {
  local product="${PRODUCT:-$(cds_datagrab_product_id "$REPO_DIR/$CONFIG")}" data="$CDS_DATAGRAB_ROOT/data/$PROFILE/$product" run="$CDS_DATAGRAB_ROOT/runs/$PROFILE/$product" log="$CDS_DATAGRAB_ROOT/logs/slurm/$PROFILE"
  printf 'product: %s\nconfiguration: %s\nprofile: %s\nresolved root: %s (%s)\ndata directory: %s\nrun directory: %s\nSlurm log directory: %s\neffective start: %s\neffective end: %s\ndry-run state: %s\nsource commit: %s\ninstalled commit: %s\n' "$product" "$CONFIG" "$PROFILE" "$CDS_DATAGRAB_ROOT" "$CDS_DATAGRAB_ROOT_SOURCE" "$data" "$run" "$log" "${START_DATE:-configured}" "${END_DATE:-configured}" "${DRY_RUN:-true}" "$(git -C "$REPO_DIR" rev-parse HEAD 2>/dev/null || echo unavailable)" "$(cat "${CDS_DATAGRAB_R_LIB}/.cds-datagrab-installed-commit" 2>/dev/null || echo unavailable)"
}

cds_datagrab_check_credentials() { [[ -n "${ecmwfr_PAT:-}" ]] || { echo "CDS credential is not present (value is never printed)" >&2; return 2; }; echo "CDS credential is present (value not printed)."; }

cds_datagrab_verify_commits() {
  local source installed marker="${CDS_DATAGRAB_R_LIB}/.cds-datagrab-installed-commit"
  source="$(git -C "$REPO_DIR" rev-parse HEAD)"; installed="$(cat "$marker" 2>/dev/null || true)"
  [[ -n "$installed" && "$source" == "$installed" ]] || { echo "Source and installed cdsdatagrab commits do not match" >&2; return 2; }
}

cds_datagrab_check_dependencies() {
  Rscript "$REPO_DIR/scripts/check_dependencies.R" --mode "${MODE:-plan}"
}
