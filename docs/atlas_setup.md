# Atlas setup

Load `r/4.5`, `udunits`, `gdal`, `proj`, and `geos`; run `scripts/check_dependencies.R --mode plan`. The current template is a validated WGS84 Albers raster with kilometer units and approximately 25-km cells; do not repair or rescale it. Set `CDS_DATAGRAB_DATA_ROOT` to the production data root and set `ecmwfr_PAT` securely without printing it. Submit `MODE=plan DRY_RUN=true sbatch hpc/run_era5_mintemp.slurm` for planning. The first production request should be the three-day smoke download, inspected with `scripts/inspect_smoke_download.R`, before any historical update.
