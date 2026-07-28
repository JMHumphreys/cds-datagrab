# cdsdatagrab

`cdsdatagrab` is a configuration-driven R pipeline for ERA5 daily minimum 2-m temperature. It requests `2m_temperature` with the CDS `daily_minimum` statistic from 6-hourly samples, converts kelvin to degrees Celsius, projects directly to the protected `spatial_domain/study_area_raster.tif` geometry, and aggregates complete ISO weeks with a cellwise minimum.

Observed files use `mintemp_YYYY-MM-DD.tif` and `mintemp_YYYY-Www.tif`; future climatological analogs use `_est`. Estimated files are excluded from observed coverage, weekly aggregation, and climatological donors. The weekly output requires all seven Monday-through-Sunday observed days.

Run `Rscript scripts/run_pipeline.R --mode diagnose` or a reproducible dry plan with `--mode plan --observed-end 2026-07-20`. Local output defaults to `runtime/cds-datagrab`; Atlas output defaults to `/project/disease_ecology/cds-datagrab` through `CDS_DATAGRAB_ROOT`. CLI precedence is `--output-root`, `CDS_DATAGRAB_ROOT`, configuration `paths.root`, then the local default. `--dry-run` and `--execute` are presence-only flags; neither flag means safe dry-run. Credentials are handled by `ecmwfr`; set `ecmwfr_PAT` securely after accepting applicable CDS dataset terms, and never put tokens in YAML or manifests.

The official Atlas entry point is `hpc/submit_era5_mintemp.sh`, which places SLURM logs under the dedicated root. Each profile has isolated `data/<profile>/era5_mintemp/{raw,extracted,daily,weekly,temp,cache}`, `runs/<profile>/era5_mintemp/<run_id>`, and pipeline-log directories. Each root has a `.cds-datagrab-root` ownership marker; destructive operations must validate it. Failed runs can be resumed because existing nonempty raw targets are skipped and valid outputs are preserved.

The original template lacked CRS metadata; the user replaced it with a correctly georeferenced Albers equal-area raster using WGS84 and kilometer coordinates (approximately 25-km cells). No coordinate scaling or CRS repair is required. The template defines both target geometry and the raster analysis mask; the GPKG defines the geographic request extent. Ordinary execution validates but never modifies either spatial file. The first real CDS execution should be the three-day smoke download, which must be inspected before any historical production update.

After reviewing the smoke plan, the documented first execution is:

```bash
Rscript scripts/run_pipeline.R --config config/era5_mintemp_smoke.yml --mode download --execute
```

Inspect a downloaded target without modifying it with `Rscript scripts/inspect_smoke_download.R <path>`.

CDS returns a valid NetCDF4/HDF5 file for the smoke request. Atlas GDAL may not open that file as a raster, so `ncdf4` is the primary raw-data reader; `terra` remains the projection, masking, validation, and GeoTIFF-writing engine. The `number` variable is ignored and `t2m` is selected explicitly. Direct NetCDF files do not need archive extraction, so `extracted/` may remain empty. Processing is resumable from the existing raw file.
