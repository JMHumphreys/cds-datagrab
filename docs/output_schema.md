# Output and provenance reference

The storage implementation creates this schema below the external root:

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

Daily filenames are `<prefix>_YYYY-MM-DD.tif`; weekly filenames are `<prefix>_YYYY-Www.tif`. LAI uses `lai_low_`; RH uses `relhum_min_`.

Run directories contain `run_manifest.json`, planned dates, request/download manifests, daily and weekly inventories, date-source maps, stage logs, and diagnostics. Provenance includes source and installed Git commits, installed package path, R library paths, variable-spec hash, request hashes, raw checksums, decoded source dates, output units, template checksum, and validation flags.

Raw inventories distinguish active, reused matching, duplicate, superseded, unmatched, invalid, and quarantined files. `.nc` and `.netcdf` are equivalent extensions for discovery; content validation selects ncdf4. AgERA5 `date_source_map.csv` maps each requested date to an extracted member rather than the ZIP container.

Daily inventories record filename validity, observed/estimated state, raster validity, date, checksum, geometry, and value-range checks. Weekly inventories record ISO year/week, required seven dates, available dates, aggregation status, validity, and reuse state. Production and smoke profiles are isolated by directory and must use distinct external roots.

The protected template checksum is:

```text
4BE01F0ECAFF35216A72EB8F27E791311AF90D35B5A4FFF1E46A74EDB6DC633B
```
