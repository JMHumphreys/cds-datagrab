# Adding products

For each new environmental variable:

1. Assign one stable product identifier and use the shared production and smoke roots; never create a product-specific top-level root.
2. Add product-specific configuration defining source dataset, variable, source/output units, daily interpretation, and weekly aggregation.
3. Add reader/decoder, filename, inventory, and reuse tests; add smoke and production configurations.
4. Register the product in the variable registry and annual dispatcher, preserve `PROFILE=production|smoke`, and document it in the README product table.
5. Validate a short smoke run, a complete ISO week, safe annual production chunks, and reuse from the shared root.

The required directories are `data/<profile>/<product_id>/{raw,extracted,cache,daily,weekly}`, `runs/<profile>/<product_id>/<run_id>`, and `logs/slurm/<profile>`.

For ERA5-Land daily-mean additions, use `source_family_id=era5land_daily_mean_utc06` and extend the shared eight-variable registry/request family when possible. Do not make eight copies of a monthly NetCDF. The LAI high/low products are monthly climatologies with no interannual variability; identical consecutive daily layers are acceptable.
