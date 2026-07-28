library(stars)

convert_nc_to_tif_daily <- function(nc_file, out_dir, var_name = NULL, proxy = TRUE) {
  
  x <- read_ncdf(nc_file, proxy = proxy)
  
  # valid_time format as YYYY-MM-DD
  tm <- st_get_dimension_values(x, "valid_time")
  time_str <- format(tm, "%Y-%m-%d")
  
  # if needed
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (i in seq_along(tm)) {
    # per timestep
    x_step <- st_as_stars(x[longitude = 1:dim(x)$longitude,
                            latitude  = 1:dim(x)$latitude,
                            valid_time = i])
    
    # unit conversion (example for Kelvin to Celsius)
    # x_step <- x_step - 273.15
    
    if (!is.null(var_name)) {
      names(x_step) <- var_name
    }
    
    # filename: var_YYYY-MM-DD.tif
    base_name <- if (!is.null(var_name)) var_name else names(x)[1]
    out_file <- file.path(out_dir, paste0(base_name, "_", time_str[i], ".tif"))
    
    write_stars(x_step, out_file, driver = "GTiff")
  }
  
  invisible(out_dir)
}
