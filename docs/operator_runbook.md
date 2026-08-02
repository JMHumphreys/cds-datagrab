# Operator runbook

Run one product and one calendar year at a time. Keep the same external product-specific root for all years. Plan first, inspect the manifest, then execute.

## Common setup

```bash
export REPO_DIR=/project/disease_ecology/cds-datagrab
export CDS_DATAGRAB_R_LIB=/project/disease_ecology/cds-datagrab-r-library/4.5
export HOME_R_LIB=/home/john.humphreys/R/x86_64-pc-linux-gnu-library/4.5
export R_LIBS_USER="${CDS_DATAGRAB_R_LIB}:${HOME_R_LIB}"
unset R_LIBS_SITE ALLOW_MULTIYEAR
module purge; module load r/4.5 udunits gdal proj geos git
bash hpc/install_cdsdatagrab_atlas.sh "$REPO_DIR"
REPO_DIR="$REPO_DIR" Rscript hpc/preflight_cdsdatagrab.R
```

## Product matrix

| Product | Root | Config | Wrapper | Annual expected dates / requests |
|---|---|---|---|---|
| `era5_mintemp` | `/project/disease_ecology/cds-datagrab-mintemp-production-output` | `config/era5_mintemp_production.yml` | `hpc/submit_era5_mintemp.sh` | 365/366, 12 |
| `era5_soilmoist` | `/project/disease_ecology/cds-datagrab-soilmoist-production-output` | `config/era5_soilmoist_production.yml` | `hpc/submit_era5_soilmoist.sh` | 365/366, 12 |
| `era5_lai_low` | `/project/disease_ecology/cds-datagrab-lai-low-production-output` | `config/era5_lai_low_production.yml` | `hpc/submit_era5_lai_low.sh` | 365/366, 12 |
| `agera5_relhum_min` | `/project/disease_ecology/cds-datagrab-relhum-min-production-output` | `config/agera5_relhum_min_production.yml` | `hpc/submit_agera5_relhum_min.sh` | 365/366, 12 |

The 2024 annual window has 366 days; 2022, 2023, 2025, and 2026 have 365 configured days. The effective observed 2026 endpoint is determined by the configured product availability and is recorded in the manifest.

## Annual plan and execution

Replace `PRODUCT`, `YEAR`, and `ROOT` with one row from the table:

```bash
bash hpc/submit_product_year.sh --product PRODUCT --year YEAR --mode plan --output-root ROOT
bash hpc/submit_product_year.sh --product PRODUCT --year YEAR --mode execute --output-root ROOT
```

The dispatcher sets `PROFILE=production`, `START_DATE=YEAR-01-01`, and `END_DATE=YEAR-12-31`. For an incomplete current year, pass an explicit earlier observed endpoint through the product wrapper with `OBSERVED_END`; never submit unavailable dates.

## Monitoring and validation

```bash
squeue -u "$USER"
tail -f "$ROOT/logs/slurm/production/<product>_%j.out"
find "$ROOT/runs/production/<product>" -name run_manifest.json -print
find "$ROOT/data/production/<product>/daily" -name '*.tif' | wc -l
find "$ROOT/data/production/<product>/weekly" -name '*.tif' | wc -l
```

Inspect the latest `run_manifest.json` and require `pipeline_status: success`, final validation success, and zero daily/weekly failures. Keep successful raw files and outputs when retrying a failed month. A rerun reuses valid artifacts.

## Safe rerun

1. Confirm the source and installed commits match.
2. Confirm the same product root and production profile.
3. Review the failed stage and request manifest.
4. Re-run the annual plan.
5. Execute only after confirming missing/invalid inputs.

Do not delete the production root to recover from a partial failure.
