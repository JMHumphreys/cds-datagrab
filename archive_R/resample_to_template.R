library(terra)
library(yaml)

# ensures template is standardized
fix_template_crs <- function(template_path, cfg) {
  if (is.character(cfg)) cfg <- yaml::read_yaml(cfg)
  crs_yaml  <- cfg$sp_projection$crs_aea
  crs_fixed <- sub("\\+units=km\\b", "+units=m", crs_yaml)
  
  tmpl <- rast(template_path)
  
  tmpl_crs <- crs(tmpl, proj = TRUE)
  if (is.na(tmpl_crs) || !nzchar(tmpl_crs)) {
    r0 <- res(tmpl) 
    if (max(r0) < 1000) {
      ex <- ext(tmpl)
      ext(tmpl) <- ext(xmin(ex) * 1000, xmax(ex) * 1000,
                       ymin(ex) * 1000, ymax(ex) * 1000)
      res(tmpl) <- r0 * 1000
    }
    crs(tmpl) <- crs_fixed
  }
  tmpl
}


resample_to_template <- function(source_dir,
                                template_path,
                                dest_dir,
                                cfg,
                                source_crs_if_missing = "EPSG:4326",
                                method = c("bilinear","near","cubic","average"),
                                overwrite = TRUE) {
  method <- match.arg(method)
  files <- list.files(source_dir, pattern = "\\.tif$", full.names = TRUE)
  stopifnot(length(files) > 0)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  tmpl <- fix_template_crs(template_path, cfg)
  
  for (f in files) {
    r <- rast(f)
    if (is.na(crs(r)) || !nzchar(crs(r))) crs(r) <- source_crs_if_missing
    r_out <- project(r, tmpl, method = method)
    
    writeRaster(r_out, file.path(dest_dir, basename(f)), overwrite = overwrite)
  }
  invisible(dest_dir)
}

library(terra)

resample_to_template <- function(source_dir, template_raster, out_dir = NULL) {

  if (is.null(out_dir)) {
    out_dir <- file.path(source_dir, "resampled")
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  tif_files <- list.files(source_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    stop("No .tif files found in the source directory.")
  }
  
  template_crs <- crs(template_raster)
  
  results <- lapply(tif_files, function(f) {
    tryCatch({
      r_source <- rast(f)
      
      r_resampled <- project(r_source, template_raster, method = "bilinear")
      
      out_path <- file.path(out_dir, basename(f))
      
      writeRaster(r_resampled, out_path, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))
      
      return(paste("Success:", basename(f)))
      
    }, error = function(e) {
      return(paste("Error on", basename(f), ":", e$message))
    })
  })
  
  return(unlist(results))
}
