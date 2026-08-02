# Production validation summary

This summary records validated behavior without inventing unavailable live-run counts.

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
