# cdsdatagrab

`cdsdatagrab` is a configuration-driven R pipeline for ERA5 daily minimum 2-m temperature. It requests `2m_temperature` with the CDS `daily_minimum` statistic from 6-hourly samples, converts kelvin to degrees Celsius, projects directly to the protected `spatial_domain/study_area_raster.tif` geometry, and aggregates complete ISO weeks with a cellwise minimum.

Observed files use `mintemp_YYYY-MM-DD.tif` and `mintemp_YYYY-Www.tif`; future climatological analogs use `_est`. Estimated files are excluded from observed coverage, weekly aggregation, and climatological donors. The weekly output requires all seven Monday-through-Sunday observed days.

Run `Rscript scripts/run_pipeline.R --mode diagnose` or a reproducible dry plan with `--mode plan --observed-end 2026-07-20`. Runtime data and manifests are written under `runtime/` by default and may be redirected with `CDS_DATAGRAB_DATA_ROOT`. The CLI never downloads unless `--execute` is supplied. Credentials are handled by `ecmwfr`; set `ecmwfr_PAT` securely after accepting applicable CDS dataset terms, and never put tokens in YAML or manifests.

The Atlas job is `hpc/run_era5_mintemp.slurm`; review the dry-run request and inventory manifests before execution. Each run records request, download, processing, validation, and reconciliation information in JSON/CSV manifests. Failed runs can be resumed because existing nonempty raw targets are skipped and valid outputs are preserved.

The current implementation is intentionally scoped to ERA5 minimum temperature. NetCDF metadata must contain unambiguous time information, and the supplied template currently has no CRS metadata; the pipeline reports that condition and does not silently assign one. Future variables should add a source-specific request/extraction adapter while reusing inventory, filename, planning, validation, and manifest modules.
