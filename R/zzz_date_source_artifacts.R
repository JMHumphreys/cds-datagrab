.run_era5_mintemp_pipeline_unannotated <- run_era5_mintemp_pipeline
run_era5_mintemp_pipeline <- function(...) {
  manifest <- .run_era5_mintemp_pipeline_unannotated(...)
  if (!is.null(manifest$run_dir)) {
    map <- manifest$date_source_map %||% data.frame(
      date = character(), selected_raw_source = character(), request_hash = character(),
      stringsAsFactors = FALSE
    )
    if (!nrow(map)) {
      map <- data.frame(date=character(), source_path=character(), request_hash=character(),
                        raw_request_start=character(), raw_request_end=character(),
                        decoded_source_start=character(), decoded_source_end=character(),
                        mapping_reason=character(), daily_output_path=character(),
                        daily_output_exists=logical(), daily_output_valid=logical(),
                        needs_processing=logical(), stringsAsFactors=FALSE)
    } else {
      source_col <- if ("selected_raw_source" %in% names(map)) "selected_raw_source" else "source_path"
      map$source_path <- map[[source_col]]
      map$date <- canonical_iso_dates(map$date, "date_source_map$date")
      map$daily_output_path <- character(nrow(map))
      map$daily_output_exists <- FALSE
      map$daily_output_valid <- FALSE
      map$needs_processing <- TRUE
    }
    utils::write.csv(map, file.path(manifest$run_dir, "date_source_map.csv"), row.names=FALSE)
  }
  manifest
}

