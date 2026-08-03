# Production validation summary

This summary records validated behavior without inventing unavailable live-run counts.

The consolidated-root migration was verified by matching file counts, matching byte totals, checksum-mode `rsync` comparisons, successful planning tests for all four products, and zero newly planned dates after switching to the consolidated root. No live counts are invented here. The template SHA256 remains `4BE01F0ECAFF35216A72EB8F27E791311AF90D35B5A4FFF1E46A74EDB6DC633B`.

| Product | Smoke / complete-week | Production period | Units | Weekly statistic | Reuse demonstrated |
|---|---|---|---|---|---|
| ERA5 minimum temperature | validated in repository tests and established workflow | 2022-01-01 through 2026-12-31 configured | K → °C | minimum | yes |
| ERA5 soil moisture | validated in repository tests and established workflow | 2022-01-01 through 2026-12-31 configured | m³ m⁻³ | mean | yes |
| ERA5 low-vegetation LAI | Atlas smoke, complete-week, annual, rerun and raw reuse reported successful | 2022-01-01 through 2026-12-31 configured | m² m⁻² | mean | yes |
| AgERA5 minimum RH | Atlas smoke, complete-week, annual, rerun and ZIP reuse reported successful | 2022-01-01 through 2026-12-31 configured | % | mean | yes |

All products use `spatial_domain/study_area_raster.tif` and the template checksum below. Weekly outputs require seven daily rasters. The initial 2022 boundary week and any final observed-data boundary week remain intentionally incomplete when dates outside the configured/effective window would be required.

```text
4BE01F0ECAFF35216A72EB8F27E791311AF90D35B5A4FFF1E46A74EDB6DC633B
```

The validated LAI/RH code line includes ncdf4 NetCDF4/HDF5 reading, `.nc`/`.netcdf` discovery, RH ZIP extraction, decoded source dates, active raw refresh, daily/weekly reuse, and dry-run annual planning. Exact Atlas job IDs, daily counts, weekly counts, and validation commits are intentionally not repeated here unless recoverable from retained manifests or supplied operator records.

ERA5-Land daily-mean products are implemented as an additive, pre-smoke-test family. They use one shared monthly eight-variable request and product-specific daily/weekly fan-out. The LAI high/low outputs carry the monthly-climatology/no-interannual-variability caveat; this family has not been run against Atlas during implementation.
