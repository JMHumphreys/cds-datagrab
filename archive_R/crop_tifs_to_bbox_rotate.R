# crops raster files in directory given a bounding box.  Also standardizes 360-longitudes

library(terra)
library(sf)

crop_tifs_to_bbox_rotate <- function(
    in_dir,
    bbox_path,  
    out_dir = file.path(dirname(in_dir), paste0(basename(in_dir), "_cropped")),
    mask = FALSE,
    overwrite = TRUE
){
  files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  stopifnot(length(files) > 0)
  
  bb0 <- sf::st_read(bbox_path, quiet = TRUE) |> sf::st_make_valid()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (f in files) {
    r <- terra::rast(f)
    if (is.na(terra::crs(r))) terra::crs(r) <- "EPSG:4326"
    
    # standardize to [-180, 180]
    if (terra::is.lonlat(r)) {
      ex <- terra::ext(r)

      if (terra::xmax(ex) > 180 || terra::xmin(ex) >= 0) {
        r <- terra::rotate(r)
      }

      ex <- terra::ext(r)
      if (terra::xmin(ex) >= 0 && terra::xmax(ex) > 180) {
        r <- terra::shift(r, dx = -360)
      }
    }
    
    bb  <- sf::st_transform(bb0, crs = terra::crs(r))
    vbb <- terra::vect(bb)
    
    rc <- terra::crop(r, terra::ext(vbb), snap = "out")
    if (mask) rc <- terra::mask(rc, vbb)
    
    terra::writeRaster(rc, file.path(out_dir, basename(f)), overwrite = overwrite)
  }
  
  invisible(out_dir)
}

