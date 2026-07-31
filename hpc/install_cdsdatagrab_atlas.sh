#!/bin/bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 /absolute/path/to/cds-datagrab-checkout" >&2
  exit 2
fi
REPO_DIR=$(cd "$1" && pwd -P)
: "${CDS_DATAGRAB_R_LIB:?Set CDS_DATAGRAB_R_LIB to an external Atlas R library}"
R_BIN="${R_BIN:-R}"
Rscript_BIN="${RSCRIPT_BIN:-Rscript}"
git -C "$REPO_DIR" rev-parse --show-toplevel >/dev/null 2>&1 || { echo "Repository checkout is not a Git worktree: $REPO_DIR" >&2; exit 2; }
[[ "$CDS_DATAGRAB_R_LIB" != "$REPO_DIR" && "$CDS_DATAGRAB_R_LIB" != "$REPO_DIR"/* ]] || { echo "CDS_DATAGRAB_R_LIB must be outside the repository checkout" >&2; exit 2; }
SOURCE_COMMIT=$(git -C "$REPO_DIR" rev-parse HEAD)
mkdir -p "$CDS_DATAGRAB_R_LIB"
echo "Installing cdsdatagrab commit $SOURCE_COMMIT from $REPO_DIR into $CDS_DATAGRAB_R_LIB"
case ":${R_LIBS_USER:-}:" in *":$CDS_DATAGRAB_R_LIB:"*) ;; "::") R_LIBS_USER="$CDS_DATAGRAB_R_LIB" ;; *) R_LIBS_USER="$CDS_DATAGRAB_R_LIB:${R_LIBS_USER}" ;; esac
export R_LIBS_USER R_LIBS_SITE="${R_LIBS_SITE:-}"
"$R_BIN" CMD INSTALL --preclean --no-multiarch --library="$CDS_DATAGRAB_R_LIB" "$REPO_DIR"
INSTALLED_PATH=$("$Rscript_BIN" -e 'if (!requireNamespace("cdsdatagrab", quietly=TRUE)) stop("cdsdatagrab namespace failed to load"); cat(find.package("cdsdatagrab"))')
PACKAGE_VERSION=$("$Rscript_BIN" -e 'cat(as.character(packageVersion("cdsdatagrab")))')
MARKER="$CDS_DATAGRAB_R_LIB/.cds-datagrab-installed-commit"
printf '%s\n' "$SOURCE_COMMIT" > "$MARKER"
echo "find.package(cdsdatagrab): $INSTALLED_PATH"
echo "packageVersion(cdsdatagrab): $PACKAGE_VERSION"
echo "source_git_commit: $SOURCE_COMMIT"
echo "installed_package_git_commit: $(<"$MARKER")"
echo "installed_commit_marker: $MARKER"
