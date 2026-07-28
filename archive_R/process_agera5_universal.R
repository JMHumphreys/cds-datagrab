# convert .nc to .tif, designed for AgERA5 grids

library(terra)
library(ncdf4)
library(stringr)

process_agera5_universal <- function(nc_path, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  nc <- NULL
  tryCatch({
    nc <- nc_open(nc_path)
    
    all_dims <- names(nc$dim)
    lon_name <- all_dims[grep("lon", all_dims, ignore.case = TRUE)][1]
    lat_name <- all_dims[grep("lat", all_dims, ignore.case = TRUE)][1]
    
    all_vars <- names(nc$var)

    target_var <- all_vars[!all_vars %in% c(lon_name, lat_name, "time", "valid_time")][1]
    
    data_array <- ncvar_get(nc, target_var)
    lon_vals <- ncvar_get(nc, lon_name)
    lat_vals <- ncvar_get(nc, lat_name)
    nc_close(nc)
    nc <- NULL

    r <- rast(t(data_array))
    
    ext(r) <- c(min(lon_vals), max(lon_vals), min(lat_vals), max(lat_vals))
    crs(r) <- "EPSG:4326"

    r <- flip(r, direction="vertical")
    
    date_code <- str_extract(basename(nc_path), "\\d{8}")
    formatted_date <- as.Date(date_code, format = "%Y%m%d")
    out_name <- paste0("relhum_", formatted_date, ".tif")
    out_path <- file.path(out_dir, out_name)
    
    writeRaster(r, out_path, overwrite=TRUE, gdal="COMPRESS=DEFLATE")
    message("Success: ", out_name)
    
  }, error = function(e) {
    if (!is.null(nc)) nc_close(nc)
    message("Failed on ", basename(nc_path), ": ", e$message)
  })
}

#input_dir <- "/project/disease_ecology/temp_runjob/temp_clim/unzip1"
#output_dir <- "/project/disease_ecology/temp_runjob/temp_clim/tif_output"
#nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)

#lapply(nc_files, process_agera5_universal, out_dir = output_dir)



### Rename files
library(stringr)

rename_agera5_tifs <- function(dir_path, new_prefix = "relhum") {

  files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
  
  for (f in files) {
    fname <- basename(f)

    date_part <- str_extract(fname, "\\d{4}-\\d{2}-\\d{2}")
    
    if (!is.na(date_part)) {

      new_name <- paste0(new_prefix, "_", date_part, ".tif")
      new_path <- file.path(dir_path, new_name)
      
      file.rename(f, new_path)
      message(paste("Renamed:", fname, "->", new_name))
    } else {
      message(paste("Skipped (no date found):", fname))
    }
  }
}

# Run it:
# rename_agera5_tifs("/project/disease_ecology/temp_runjob/temp_clim/tif_output")