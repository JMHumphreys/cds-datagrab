# Output and provenance reference

Coverage component records include target cells, exact and local donor counts, maximum donor distance in cells and kilometres, eligibility, attempted/repaired state, donor method, and failure reason. `repair_components_<product>_<date>.csv` accompanies the four masks. Product results are written for every requested product even on failure; each contains date results and the original condition class, message, call, and bounded call-stack summary. Family totals aggregate failed and successful date results, and unavailable counts are represented as `null`/not available rather than zero.

ERA5-Land family manifests are finalized with `family_status`, timestamps, product/date success and failure keys, raw/archive/extraction reuse flags, `CDS_contacted`, daily output counts, and aggregate coverage-repair counts. They also record the master-template checksum, support-mask checksum, unsupported-cell audit checksum, audited cell IDs/coordinates, and the 35 km support threshold. Product manifests additionally record `source_member`, `source_alias`, `failure_stage`, and `failure_message`. Daily sidecars record master-template cells, ERA5-Land-supported cells, structural exclusions, pre-repair missing supported cells, repaired supported cells, unexpected post-repair missing cells, outside-support finite cells, pre/post repair counts, component sizes, source/projection classifications, final checksums, and successful reopen validation. The authoritative GeoTIFF is created only after temporary-file validation.

The protected master template remains `spatial_domain/study_area_raster.tif`. The explicit ERA5-Land support artifacts are `spatial_domain/derived/era5land_support_mask.tif` and `spatial_domain/derived/era5land_unsupported_cells.csv`; the audit records cells 28012, 35085, and 35964 with request hash `016f79fb`, representative date `2026-02-01`, and nearest finite source distances 83.2, 108.9, and 120.9 km. Structural exclusions are accepted as `NA` in daily and weekly rasters; finite values outside the master template and missing values in supported cells are validation failures.

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

ERA5-Land raw finalization is content-aware. A response initially transferred to `.partial/` is identified by magic bytes as `zip`, NetCDF classic, or NetCDF4/HDF5, checksum-verified, and atomically promoted to the matching deterministic extension. The observed eight-member response is retained as one shared ZIP, with extraction under `extracted/<request_hash>/`. Existing `.nc`-named ZIP artifacts are normalized in place after size/checksum verification; users must not manually rename them.

Each extracted request contains `member_inventory.csv` and `source_map.csv`. The inventory records archive/member checksums, member names and sizes, extracted paths, container and NetCDF inspection status, product ID, CDS variable, environmental alias, source units, dimension names/lengths, time dimension, and decoded dates. The source map has one row per product/date (24 rows for the three-day family smoke) and records request hash, product, alias, archive member, units, time index, source date, and output date. Scalar metadata variables such as `number` are ignored.

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

Raw inventories distinguish active, reused matching, duplicate, superseded, unmatched, invalid, and quarantined files. `.nc`, `.netcdf`, and `.zip` are equivalent discovery candidates; magic-byte validation selects the reader. Stale identical `.partial` files are removed only after the finalized artifact is verified; different partials are retained for inspection. AgERA5 `date_source_map.csv` maps each requested date to an extracted member rather than the ZIP container.

Daily inventories record filename validity, observed/estimated state, raster validity, date, checksum, geometry, and value-range checks. Weekly inventories record ISO year/week, required seven dates, available dates, aggregation status, validity, and reuse state. Production and smoke profiles are isolated by directory and must use distinct external roots.

The protected template checksum is:

```text
4BE01F0ECAFF35216A72EB8F27E791311AF90D35B5A4FFF1E46A74EDB6DC633B
```
