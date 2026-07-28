# odd issue with olde verison..

crop_tifs_to_bbox_rotate2 <- function(
    in_dir,
    bbox_path,  
    out_dir = file.path(dirname(in_dir), paste0(basename(in_dir), "_cropped")),
    mask = FALSE,
    overwrite = TRUE
){
  files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  target_ext <- terra::ext(-125.125, -34.875, -0.125, 40.125)
  
  if(mask) {
    bb0 <- sf::st_read(bbox_path, quiet = TRUE)
    vbb_mask <- terra::vect(sf::st_transform(bb0, "EPSG:4326"))
  }
  
  for (f in files) {
    r <- terra::rast(f)

    if (terra::xmax(r) > 180) {
      r <- terra::rotate(r)
    }
    
    terra::crs(r) <- "EPSG:4326"
    rc <- terra::crop(r, target_ext, snap = "near")
    
    terra::ext(rc) <- target_ext
    
    if (mask) {
      rc <- terra::mask(rc, vbb_mask)
    }
    

    out_path <- file.path(out_dir, basename(f))
    terra::writeRaster(rc, out_path, overwrite = overwrite)
    
    message(paste("Processed:", basename(f), 
                  "| Dim:", paste(dim(rc)[1:2], collapse="x"), 
                  "| Res:", round(terra::res(rc)[1], 3)))
  }
}