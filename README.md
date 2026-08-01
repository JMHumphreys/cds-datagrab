# cdsdatagrab

`cdsdatagrab` is a configuration-driven R pipeline for ERA5 daily minimum 2-m temperature. It requests `2m_temperature` with the CDS `daily_minimum` statistic from 6-hourly samples, converts kelvin to degrees Celsius, projects directly to the protected `spatial_domain/study_area_raster.tif` geometry, and aggregates complete ISO weeks with a cellwise minimum.

Observed files use `mintemp_YYYY-MM-DD.tif` and `mintemp_YYYY-Www.tif`; future climatological analogs use `_est`. Estimated files are excluded from observed coverage, weekly aggregation, and climatological donors. The weekly output requires all seven Monday-through-Sunday observed days.

Run `Rscript scripts/run_pipeline.R --mode diagnose` or a reproducible dry plan with `--mode plan --observed-end 2026-07-20`. Atlas wrappers require an external, variable-specific `CDS_DATAGRAB_ROOT`, such as `/project/disease_ecology/cds-datagrab-lai-low-smoke-output`; the repository checkout is never a generated-data root. CLI precedence is `--output-root`, `CDS_DATAGRAB_ROOT`, configuration `paths.root`, then the local default. `--dry-run` and `--execute` are presence-only flags; neither flag means safe dry-run. Credentials are handled by `ecmwfr`; set `ecmwfr_PAT` securely after accepting applicable CDS dataset terms, and never put tokens in YAML or manifests.

The official Atlas entry point is `hpc/submit_era5_mintemp.sh`, which places SLURM logs under the dedicated root. Each profile has isolated `data/<profile>/era5_mintemp/{raw,extracted,daily,weekly,temp,cache}`, `runs/<profile>/era5_mintemp/<run_id>`, and pipeline-log directories. Each root has a `.cds-datagrab-root` ownership marker; destructive operations must validate it. Failed runs can be resumed because existing nonempty raw targets are skipped and valid outputs are preserved.

The original template lacked CRS metadata; the user replaced it with a correctly georeferenced Albers equal-area raster using WGS84 and kilometer coordinates (approximately 25-km cells). No coordinate scaling or CRS repair is required. The template defines both target geometry and the raster analysis mask; the GPKG defines the geographic request extent. Ordinary execution validates but never modifies either spatial file. The first real CDS execution should be the three-day smoke download, which must be inspected before any historical production update.

After reviewing the smoke plan, the documented first execution is:

```bash
Rscript scripts/run_pipeline.R --config config/era5_mintemp_smoke.yml --mode download --execute
```

Inspect a downloaded target without modifying it with `Rscript scripts/inspect_smoke_download.R <path>`.

CDS returns a valid NetCDF4/HDF5 file for the smoke request. Atlas GDAL may not open that file as a raster, so `ncdf4` is the primary raw-data reader; `terra` remains the projection, masking, validation, and GeoTIFF-writing engine. The `number` variable is ignored and `t2m` is selected explicitly. Direct NetCDF files do not need archive extraction, so `extracted/` may remain empty. Processing is resumable from the existing raw file.
## ERA5 spatial coverage

The raster template is the authoritative output domain. The CDS request area is derived from the union of the template's non-NA geographic footprint and `study_bbox.gpkg`, expanded by a one-degree interpolation margin and aligned outward to the 0.25-degree ERA5 grid. The original GPKG-only area (with a 0.25-degree margin) did not reach the northern template cells, producing the fixed 529-cell gap.

Standardization performs one `terra::project(..., method = "bilinear")` operation followed by the template mask. Complete cellwise template coverage is mandatory; a second `resample()` or extrapolation cannot recover values absent from the source extent. Existing incomplete daily products are invalid observations and are planned for re-request and quarantined during execution. The request-area definition is included in the deterministic raw filename hash, so changing the area creates a new raw target while preserving the old file.

Download targets in request manifests are filenames only. They resolve to absolute paths under the active profile's `raw/` directory and are downloaded through an atomic `.partial` target. A request is successful only after the resulting file exists, is nonempty, has a supported signature, and— for NetCDF—has readable ERA5 temperature and time metadata. Download failures stop the pipeline at `download`; processing is not attempted. Run manifests are written at initialization and updated through failures. Runtime logs are written below the configured output root, never to a repository-relative `logs/` directory. Spatial diagnostics record MD5 and SHA256 separately.

## Additional direct environmental products

`era5_lai_low` is direct ERA5 low-vegetation LAI (`lai_lv`), sampled at 00:00 UTC. It is a monthly climatology with seasonal variation but no interannual variability; it is not a weighted total LAI product.

`agera5_relhum_min` is the AgERA5 precomputed derived 24-hour minimum relative humidity. It uses local-time daily periods on the 0.1-degree downscaled and bias-adjusted source grid. Relative humidity is not derived locally from temperature and dewpoint, and weekly RH is the mean of the seven daily minima.

Atlas smoke plans use separate generated-data roots:

```bash
CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab-lai-low-smoke-output CONFIG=config/era5_lai_low_smoke.yml MODE=plan DRY_RUN=true bash hpc/submit_era5_lai_low.sh
CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab-relhum-min-smoke-output CONFIG=config/agera5_relhum_min_smoke.yml MODE=plan DRY_RUN=true bash hpc/submit_agera5_relhum_min.sh
```

Use `/project/disease_ecology/cds-datagrab-lai-low-weekly-smoke-output` and `/project/disease_ecology/cds-datagrab-relhum-min-weekly-smoke-output` for weekly smoke output, and `/project/disease_ecology/cds-datagrab-lai-low-production-output` and `/project/disease_ecology/cds-datagrab-relhum-min-production-output` for persistent production output.

Production environmental workflows use the canonical configured window 2022-01-01 through 2026-12-31. Run one calendar year at a time with `START_DATE` and `END_DATE`, reusing the same variable-specific production root. Valid raw, daily, and weekly products are reused on later annual runs. Observed requests stop at the configured product-availability date; any remaining 2026 period is recorded as future/estimated and is not submitted to CDS. Boundary ISO weeks are never completed with out-of-window dates and require seven in-range daily rasters.

For example, the first annual executions are:

```bash
export CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab-lai-low-production-output
START_DATE=2022-01-01 END_DATE=2022-12-31 CONFIG=config/era5_lai_low_production.yml MODE=full DRY_RUN=false bash hpc/submit_era5_lai_low.sh

export CDS_DATAGRAB_ROOT=/project/disease_ecology/cds-datagrab-relhum-min-production-output
START_DATE=2022-01-01 END_DATE=2022-12-31 CONFIG=config/agera5_relhum_min_production.yml MODE=full DRY_RUN=false bash hpc/submit_agera5_relhum_min.sh
```

Dry-run planning may cover the complete 2022-2026 configured range. Production execution spanning multiple calendar years requires `ALLOW_MULTIYEAR=true`; annual execution is the recommended default.
