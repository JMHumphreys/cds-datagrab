#!/usr/bin/env bash
set -euo pipefail
usage() { echo "Usage: $0 --root /absolute/path --profile production|smoke [--execute]" >&2; exit 2; }
root=""; profile=""; execute=false
while (($#)); do case "$1" in --root) root="$2"; shift 2;; --profile) profile="$2"; shift 2;; --execute) execute=true; shift;; *) usage;; esac; done
[[ "$root" == /* && "$root" != "/" ]] || { echo "--root must be an explicit absolute non-root path" >&2; exit 2; }
[[ "$profile" == production || "$profile" == smoke ]] || { echo "--profile is required and must be production or smoke" >&2; exit 2; }
repo="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"; root_abs="$(mkdir -p "$root" && cd "$root" && pwd -P)"
[[ "$root_abs" != "$repo" && "$root_abs" != "$repo"/* ]] || { echo "Refusing the repository checkout" >&2; exit 2; }
case "$root_abs" in /project|/project/|/project/disease_ecology|/project/disease_ecology/) echo "Refusing a dangerous broad root" >&2; exit 2;; esac
marker="$root_abs/.cds-datagrab-root"; [[ ! -e "$marker" ]] || grep -q '"application"[[:space:]]*:[[:space:]]*"cds-datagrab"' "$marker" || { echo "Existing marker is not a cds-datagrab marker" >&2; exit 2; }
paths=("$root_abs/data/$profile" "$root_abs/runs/$profile" "$root_abs/logs/slurm/$profile")
printf 'root: %s\nprofile: %s\nmarker: %s\n' "$root_abs" "$profile" "$marker"
printf 'would create:\n'; printf '  %s\n' "${paths[@]}"
if [[ "$execute" != true ]]; then echo "dry run; pass --execute to create the marker and directories"; exit 0; fi
if [[ ! -e "$marker" ]]; then (umask 077; printf '{"application":"cds-datagrab","schema_version":3,"profiles":["production","smoke"]}\n' > "$marker"); fi
mkdir -p "${paths[@]}"; echo "initialized output root"
