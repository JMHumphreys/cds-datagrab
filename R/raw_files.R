# NetCDF dispatch lives in netcdf_processing.R.  This file retains the public
# raw-file helpers used by the pipeline while keeping dataset-specific logic
# in the variable adapter.
detect_download_format <- detect_download_format
inspect_netcdf_file <- inspect_netcdf_ncdf4
