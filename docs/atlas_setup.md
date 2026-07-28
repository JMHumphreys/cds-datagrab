# Atlas setup

Load `r/4.5`, `udunits`, `gdal`, `proj`, and `geos`; run `scripts/check_dependencies.R`. Set `CDS_DATAGRAB_DATA_ROOT` to the production data root and set `ecmwfr_PAT` securely without printing it. Submit `MODE=plan DRY_RUN=true sbatch hpc/run_era5_mintemp.slurm` for planning, then use `MODE=full DRY_RUN=false` only after reviewing manifests.
