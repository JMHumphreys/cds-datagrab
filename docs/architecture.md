# Architecture

`inventory -> date plan -> request manifest -> download -> raw normalization -> daily extraction -> template standardization -> daily validation -> weekly aggregation -> estimate reconciliation -> future analogs -> final manifest`.

ERA5 uses `2m_temperature` with `daily_minimum` over 6-hourly samples. Values are converted from kelvin to Celsius. The original CRS-deficient template was replaced by the user with a WGS84 Albers raster in kilometer coordinates. It is the immutable target geometry and raster mask; the GPKG supplies the geographic CDS request extent. Estimated (`_est`) files are excluded from observed coverage and climatological donors.

Storage is profile-isolated beneath `CDS_DATAGRAB_ROOT` (Atlas default `/project/disease_ecology/cds-datagrab`) or `runtime/cds-datagrab` locally. Smoke and production each receive separate data, run, pipeline-log, and SLURM-log subtrees. The root marker prevents destructive operations from acting outside the active dataset subtree.

The CDS smoke target is NetCDF4/HDF5. Atlas GDAL may fail to open it as a raster; `ncdf4` reads metadata, time, coordinates, fill values, scale/offset, and temperature arrays, while `terra` performs geographic-to-template projection, masking, validation, and GeoTIFF writing. Direct NetCDF files may leave `extracted/` empty.
