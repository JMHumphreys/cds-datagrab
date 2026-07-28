crop_tifs_to_bbox_rotate_adapt <- function(
    in_dir,
    bbox_path,  
    out_dir = file.path(dirname(in_dir), paste0(basename(in_dir), "_cropped")),
    mask = FALSE,
    overwrite = TRUE
){
  files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  bb0 <- sf::st_read(bbox_path, quiet = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  vbb_deg <- terra::vect(sf::st_transform(bb0, "EPSG:4326"))
  
  for (f in files) {
    r <- terra::rast(f)

    vbb_native <- vbb_deg
    if (terra::xmax(r) > 180 && terra::xmin(vbb_deg) < 0) {

      vbb_native <- terra::shift(vbb_deg, dx = 360)
    }
    
    target_ext <- terra::align(terra::ext(vbb_native), r)
    rc <- terra::crop(r, target_ext, snap = "near")
    
    if (terra::xmin(rc) >= 180) {
      rc <- terra::shift(rc, dx = -360)
    }

    terra::crs(rc) <- "EPSG:4326"
    
    if (mask) {
      rc <- terra::mask(rc, vbb_deg)
    }

    out_path <- file.path(out_dir, basename(f))
    terra::writeRaster(rc, out_path, overwrite = overwrite)

    vals <- terra::minmax(rc)
    message(paste("Processed:", basename(f), "| Range:", vals[1], "to", vals[2]))
  }
}