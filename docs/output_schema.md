# Output and provenance reference

The storage implementation creates this schema below the shared production or smoke root:

```text
<root>/
├── .cds-datagrab-root
├── data/<profile>/<product>/
│   ├── raw/                 # retained CDS responses, reusable and content-validated
│   ├── extracted/           # transactional AgERA5 members, reusable after validation
│   ├── quarantine/          # invalid or superseded artifacts
│   ├── daily/               # authoritative template-aligned daily GeoTIFFs and sidecars
│   ├── weekly/              # authoritative complete ISO-week GeoTIFFs and sidecars
│   ├── temp/
│   └── cache/
├── runs/<profile>/<product>/<run-id>/
└── logs/<pipeline|slurm>/<profile>/<product>/
```

ERA5-Land uses a shared source-family layer so the eight-variable NetCDF bundle is stored once:

```text
data/<profile>/_sources/era5land_daily_mean_utc06/
├── raw/
├── extracted/
└── cache/
```

Product daily and weekly outputs remain under `data/<profile>/<product_id>/`. Source and product run manifests record `source_family_id`, `request_hash`, `shared_raw_path`, requested variables, returned daily time zone, frequency, and source-to-product date mappings. Shared raw files must not be removed while any product lineage references their request hash.

The conceptual CDS request for each calendar month is one multi-variable request (the area is always derived from the protected template and configured buffer):

```text
dataset = derived-era5-land-daily-statistics
variable = [2m_temperature, soil_temperature_level_1, soil_temperature_level_2,
            volumetric_soil_water_layer_1, volumetric_soil_water_layer_2,
            surface_pressure, leaf_area_index_high_vegetation,
            leaf_area_index_low_vegetation]
daily_statistic = daily_mean; time_zone = utc-06:00; frequency = 1_hourly
data_format = netcdf; download_format = unarchived; request_chunk = month
area = <template-derived buffered and source-grid-aligned extent>
```

Daily filenames are `<prefix>_YYYY-MM-DD.tif`; weekly filenames are `<prefix>_YYYY-Www.tif`. LAI uses `lai_low_`; RH uses `relhum_min_`.

Run directories contain `run_manifest.json`, planned dates, request/download manifests, daily and weekly inventories, date-source maps, stage logs, and diagnostics. Provenance includes source and installed Git commits, installed package path, R library paths, variable-spec hash, request hashes, raw checksums, decoded source dates, output units, template checksum, and validation flags. New manifests also record `resolved_output_root`, `output_root_source`, `profile`, `product_id`, `data_directory`, `run_directory`, and `slurm_log_directory`.

Raw inventories distinguish active, reused matching, duplicate, superseded, unmatched, invalid, and quarantined files. `.nc` and `.netcdf` are equivalent extensions for discovery; content validation selects ncdf4. AgERA5 `date_source_map.csv` maps each requested date to an extracted member rather than the ZIP container.

Daily inventories record filename validity, observed/estimated state, raster validity, date, checksum, geometry, and value-range checks. Weekly inventories record ISO year/week, required seven dates, available dates, aggregation status, validity, and reuse state. Production and smoke profiles are isolated by directory and must use distinct external roots.

The protected template checksum is:

```text
4BE01F0ECAFF35216A72EB8F27E791311AF90D35B5A4FFF1E46A74EDB6DC633B
```
