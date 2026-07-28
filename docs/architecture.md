# Architecture

`inventory -> date plan -> request manifest -> download -> raw normalization -> daily extraction -> template standardization -> daily validation -> weekly aggregation -> estimate reconciliation -> future analogs -> final manifest`.

ERA5 uses `2m_temperature` with `daily_minimum` over 6-hourly samples. Values are converted from kelvin to Celsius. The original CRS-deficient template was replaced by the user with a WGS84 Albers raster in kilometer coordinates. It is the immutable target geometry and raster mask; the GPKG supplies the geographic CDS request extent. Estimated (`_est`) files are excluded from observed coverage and climatological donors.

Storage is profile-isolated beneath `CDS_DATAGRAB_ROOT` (Atlas default `/project/disease_ecology/cds-datagrab`) or `runtime/cds-datagrab` locally. Smoke and production each receive separate data, run, pipeline-log, and SLURM-log subtrees. The root marker prevents destructive operations from acting outside the active dataset subtree.

The CDS smoke target is NetCDF4/HDF5. Atlas GDAL may fail to open it as a raster; `ncdf4` reads metadata, time, coordinates, fill values, scale/offset, and temperature arrays, while `terra` performs geographic-to-template projection, masking, validation, and GeoTIFF writing. Direct NetCDF files may leave `extracted/` empty.
### Spatial request extent and coverage

Request bounds come from `derive_cds_request_area()`: non-NA template footprint plus the valid, dissolved GPKG footprint, a configurable one-degree margin, and outward 0.25-degree grid alignment. The template, rather than the source GPKG rectangle, defines accepted output cells. Reprojection is done once directly to the template with bilinear interpolation and then masked. Complete cellwise coverage is validated before a daily raster can be an observed product. A second resampling operation cannot restore source cells that were never downloaded.
