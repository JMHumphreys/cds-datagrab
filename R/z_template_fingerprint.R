# Add an exact file SHA256 when a SpatRaster retains its source path. This keeps
# the original fingerprint fields unchanged while allowing weekly sidecars to
# carry the required template checksum.
template_fingerprint <- local({
  previous <- template_fingerprint
  function(path_or_template) {
    out <- previous(path_or_template)
    source_path <- if (is.character(path_or_template)) path_or_template else tryCatch(terra::sources(path_or_template)[1], error=function(e) NA_character_)
    if (length(source_path) && !is.na(source_path) && file.exists(source_path)) out$sha256 <- digest::digest(file=source_path, algo="sha256")
    out
  }
})
