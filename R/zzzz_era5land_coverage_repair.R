era5land_donor_is_valid <- function(value, source_range = c(-Inf, Inf)) {
  isTRUE(length(value) == 1L && is.finite(value) && value >= source_range[1] && value <= source_range[2])
}

era5land_ring_cells <- function(r, cell, radius, exclude = integer()) {
  rc <- terra::rowColFromCell(r, cell)
  rows <- max(1L, rc[1] - radius):min(terra::nrow(r), rc[1] + radius)
  cols <- max(1L, rc[2] - radius):min(terra::ncol(r), rc[2] + radius)
  cells <- as.integer(terra::cellFromRowColCombine(r, rows, cols))
  cells <- cells[!cells %in% exclude & cells != cell]
  if (!length(cells)) return(integer())
  other <- terra::rowColFromCell(r, cells)
  cells[ pmax(abs(other[, 1] - rc[1]), abs(other[, 2] - rc[2])) <= radius ]
}

era5land_cell_distances <- function(r, origin, cells) {
  if (!length(cells)) return(numeric())
  a <- terra::xyFromCell(r, origin); b <- terra::xyFromCell(r, cells)
  dx <- b[, 1] - a[1]; dy <- b[, 2] - a[2]
  crs_text <- terra::crs(r, proj = TRUE)
  if (isTRUE(terra::is.lonlat(r))) {
    lat <- mean(c(a[2], b[, 2])) * pi / 180
    sqrt((dx * 111.32 * cos(lat))^2 + (dy * 111.32)^2)
  } else {
    units <- tolower(crs_text)
    multiplier <- if (grepl("units=m|metre|meter", units)) 0.001 else 1
    sqrt(dx^2 + dy^2) * multiplier
  }
}

era5land_component_donors <- function(target_cell, component_cells, bilinear_values, nearest_values, template, template_valid,
                                      source_range, maximum_donor_radius_cells, maximum_donor_count = 8L) {
  exact_bilinear <- if (era5land_donor_is_valid(bilinear_values[[target_cell]], source_range)) target_cell else integer()
  exact_nearest <- if (era5land_donor_is_valid(nearest_values[[target_cell]], source_range)) target_cell else integer()
  if (length(exact_nearest)) {
    return(list(value = nearest_values[[target_cell]], method = "same_cell_nearest", donor_cells = exact_nearest,
      donor_values = nearest_values[[exact_nearest]], exact_bilinear_donor_count = length(exact_bilinear),
      exact_nearest_donor_count = 1L, local_bilinear_donor_count = 0L, local_nearest_donor_count = 0L,
      maximum_donor_distance_cells = 0, maximum_donor_distance_km = 0, failure_reason = NULL))
  }
  for (radius in seq_len(maximum_donor_radius_cells)) {
    candidates <- era5land_ring_cells(template, target_cell, radius, exclude = component_cells)
    if (!length(candidates)) next
    nearest_ok <- vapply(candidates, function(cell) era5land_donor_is_valid(nearest_values[[cell]], source_range), logical(1))
    bilinear_ok <- vapply(candidates, function(cell) era5land_donor_is_valid(bilinear_values[[cell]], source_range), logical(1))
    valid_cells <- if (any(bilinear_ok)) candidates[bilinear_ok] else candidates[nearest_ok]
    if (!length(valid_cells)) next
    distances_cells <- pmax(abs(terra::rowColFromCell(template, valid_cells)[, 1] - terra::rowColFromCell(template, target_cell)[1]),
      abs(terra::rowColFromCell(template, valid_cells)[, 2] - terra::rowColFromCell(template, target_cell)[2]))
    distances_km <- era5land_cell_distances(template, target_cell, valid_cells)
    order_index <- order(distances_km, valid_cells)
    valid_cells <- valid_cells[order_index]; distances_cells <- distances_cells[order_index]; distances_km <- distances_km[order_index]
    valid_cells <- head(valid_cells, maximum_donor_count); distances_cells <- head(distances_cells, maximum_donor_count); distances_km <- head(distances_km, maximum_donor_count)
    use_bilinear <- vapply(valid_cells, function(cell) era5land_donor_is_valid(bilinear_values[[cell]], source_range), logical(1))
    use_nearest <- !use_bilinear & vapply(valid_cells, function(cell) era5land_donor_is_valid(nearest_values[[cell]], source_range), logical(1))
    donor_values <- ifelse(use_bilinear, bilinear_values[valid_cells], nearest_values[valid_cells])
    weights <- 1 / pmax(distances_km, .Machine$double.eps)
    value <- if (length(donor_values) == 1L) donor_values[[1L]] else sum(donor_values * weights) / sum(weights)
    return(list(value = value, method = if (length(donor_values) == 1L) "local_nearest_valid" else "local_idw",
      donor_cells = valid_cells, donor_values = as.numeric(donor_values), exact_bilinear_donor_count = 0L,
      exact_nearest_donor_count = 0L, local_bilinear_donor_count = sum(use_bilinear), local_nearest_donor_count = sum(use_nearest),
      maximum_donor_distance_cells = max(distances_cells), maximum_donor_distance_km = max(distances_km), failure_reason = NULL))
  }
  list(value = NA_real_, method = NULL, donor_cells = integer(), donor_values = numeric(),
    exact_bilinear_donor_count = length(exact_bilinear), exact_nearest_donor_count = 0L,
    local_bilinear_donor_count = 0L, local_nearest_donor_count = 0L,
    maximum_donor_distance_cells = NA_real_, maximum_donor_distance_km = NA_real_, failure_reason = "no_valid_donor_within_radius")
}

era5land_source_gap_classification <- function(cell, source, template, bilinear_value, nearest_value) {
  if (is.finite(nearest_value)) return("projection_created_gap")
  point <- tryCatch(terra::xyFromCell(template, cell), error = function(e) NULL)
  if (is.null(point)) return("unknown_gap")
  source_point <- tryCatch(terra::project(terra::vect(point, crs = terra::crs(template)), terra::crs(source)), error = function(e) NULL)
  if (is.null(source_point)) return("unknown_gap")
  source_cell <- tryCatch(terra::cellFromXY(source, terra::crds(source_point)), error = function(e) NA_integer_)
  source_extent <- terra::ext(source); source_xy <- terra::crds(source_point)
  if (source_xy[1] < source_extent$xmin || source_xy[1] > source_extent$xmax || source_xy[2] < source_extent$ymin || source_xy[2] > source_extent$ymax) return("outside_source_support")
  if (!length(source_cell) || is.na(source_cell)) return("outside_source_support")
  neighbours <- coverage_cell_neighbors(source, source_cell)
  source_values <- terra::values(source, mat = FALSE)
  if (is.na(source_values[[source_cell]]) || anyNA(source_values[neighbours])) "source_land_mask_gap" else "projection_created_gap"
}

era5land_write_repair_diagnostics <- function(template, missing_pre, repaired, missing_post, outside_mask, diagnostic_dir, date, prefix, component_records) {
  if (is.null(diagnostic_dir)) return(character())
  fs::dir_create(diagnostic_dir, recurse = TRUE)
  stem <- paste0(prefix %||% "daily", "_", format(as.Date(date), "%Y-%m-%d"))
  masks <- list(missing_inside_pre_repair = missing_pre, repaired_cells = repaired, missing_inside_post_repair = missing_post, outside_mask = outside_mask)
  paths <- vapply(names(masks), function(name) file.path(diagnostic_dir, paste0(stem, "_", name, ".tif")), character(1))
  for (name in names(masks)) { r <- template; terra::values(r) <- ifelse(masks[[name]], 1, NA); terra::writeRaster(r, paths[[name]], overwrite = TRUE) }
  component_path <- file.path(diagnostic_dir, paste0("repair_components_", prefix %||% "daily", "_", format(as.Date(date), "%Y-%m-%d"), ".csv"))
  component_df <- if (length(component_records)) do.call(rbind, lapply(component_records, function(x) { x$donor_method <- x$donor_method %||% NA_character_; x$repair_failure_reason <- x$repair_failure_reason %||% NA_character_; as.data.frame(x, stringsAsFactors = FALSE) })) else data.frame()
  utils::write.csv(component_df, component_path, row.names = FALSE)
  c(paths, component_csv = component_path)
}

analyze_template_coverage <- function(bilinear, nearest, source, template, mask_template = TRUE, maximum_repair_count = 4L,
                                      maximum_repair_fraction = 0.0005, maximum_component_size = 4L,
                                      maximum_donor_radius_cells = 2L, maximum_donor_radius_km = NULL,
                                      donor_count = 8L, source_range = c(-Inf, Inf), diagnostics_dir = NULL,
                                      date = NULL, prefix = NULL) {
  bilinear_unmasked <- bilinear; nearest_unmasked <- nearest
  output_masked <- if (mask_template) terra::mask(bilinear_unmasked, template) else bilinear_unmasked
  template_values <- terra::values(template, mat = FALSE); template_valid <- !is.na(template_values)
  bilinear_values <- as.numeric(terra::values(bilinear_unmasked, mat = FALSE)); nearest_values <- as.numeric(terra::values(nearest_unmasked, mat = FALSE)); output_values <- as.numeric(terra::values(output_masked, mat = FALSE))
  missing <- template_valid & is.na(output_values); missing_cells <- which(missing); components <- coverage_component_sets(template, missing_cells)
  component_lookup <- integer(max(length(template_values), 1L)); for (k in seq_along(components)) component_lookup[components[[k]]] <- k
  details <- list(); component_records <- list(); proposed_values <- list(); proposed_by_component <- vector("list", length(components))
  for (component_id in seq_along(components)) {
    component_cells <- components[[component_id]]; proposals <- vector("list", length(component_cells)); failure_reasons <- character()
    for (k in seq_along(component_cells)) {
      cell <- component_cells[[k]]; donor <- era5land_component_donors(cell, component_cells, bilinear_values, nearest_values, template, template_valid, source_range, maximum_donor_radius_cells, donor_count)
      classification <- era5land_source_gap_classification(cell, source, template, bilinear_values[[cell]], nearest_values[[cell]])
      if (!is.null(maximum_donor_radius_km) && is.finite(donor$maximum_donor_distance_km) && donor$maximum_donor_distance_km > maximum_donor_radius_km) donor$failure_reason <- "donor_exceeds_configured_km_radius"
      if (!is.null(donor$failure_reason)) failure_reasons <- c(failure_reasons, donor$failure_reason)
      if (!is.finite(donor$value)) failure_reasons <- c(failure_reasons, "no_finite_candidate_value")
      proposals[[k]] <- donor
      target_neighbors <- setdiff(coverage_cell_neighbors(template, cell), cell)
      target_coords <- terra::xyFromCell(template, target_neighbors)
      cell_xy <- terra::xyFromCell(template, cell)
      target_distances <- if (length(target_neighbors)) sqrt(rowSums((target_coords - matrix(cell_xy, nrow = nrow(target_coords), ncol = 2, byrow = TRUE))^2)) else numeric()
      target_values <- if (length(target_neighbors)) bilinear_values[target_neighbors] else numeric()
      masked_target_values <- if (length(target_neighbors)) output_values[target_neighbors] else numeric()
      nearest_target_values <- if (length(target_neighbors)) nearest_values[target_neighbors] else numeric()
      legacy_classification <- if (classification == "outside_source_support") "outside_source_support" else if (is.finite(nearest_values[[cell]])) "bilinear_interpolation_artifact" else if (classification == "source_land_mask_gap" && length(component_cells) == 1L && sum(is.finite(target_values)) >= 4L) "isolated_land_mask_mismatch" else "source_nodata"
      gap_type <- if (is.finite(nearest_values[[cell]])) "bilinear_missing_nearest_same_cell_valid" else if (!is.null(donor$method)) "bilinear_missing_nearest_same_cell_missing_local_donor" else "bilinear_missing_no_local_donor"
      details[[length(details) + 1L]] <- list(cell_id = cell, component_id = component_id, row = terra::rowFromCell(template, cell), column = terra::colFromCell(template, cell),
        bilinear_value = bilinear_values[[cell]], nearest_value = nearest_values[[cell]], classification = legacy_classification, gap_type = gap_type, source_gap_type = classification,
        exact_bilinear_donor_count = donor$exact_bilinear_donor_count, exact_nearest_donor_count = donor$exact_nearest_donor_count,
        local_bilinear_donor_count = donor$local_bilinear_donor_count, local_nearest_donor_count = donor$local_nearest_donor_count,
        maximum_donor_distance_cells = donor$maximum_donor_distance_cells, maximum_donor_distance_km = donor$maximum_donor_distance_km,
        donor_method = donor$method, donor_cells = donor$donor_cells, donor_values = donor$donor_values,
        missing_component_size = length(component_cells), target_neighbor_cell_ids = target_neighbors, target_neighbor_distances = target_distances,
        target_neighbor_values = target_values, masked_output_neighbor_values = masked_target_values, unmasked_bilinear_neighbor_finite_count = sum(is.finite(target_values)),
        unmasked_bilinear_neighbor_na_count = sum(!is.finite(target_values)), masked_output_neighbor_finite_count = sum(is.finite(masked_target_values)),
        masked_output_neighbor_na_count = sum(!is.finite(masked_target_values)), unmasked_nearest_neighbor_finite_count = sum(is.finite(nearest_target_values)),
        unmasked_nearest_neighbor_na_count = sum(!is.finite(nearest_target_values)), target_finite_neighbor_count = sum(is.finite(target_values)), target_na_neighbor_count = sum(!is.finite(target_values)))
    }
    component_reason <- if (length(component_cells) > maximum_component_size) "component_exceeds_configured_maximum" else if (length(failure_reasons)) failure_reasons[[1L]] else NULL
    component_eligible <- is.null(component_reason)
    proposed_by_component[[component_id]] <- proposals
    component_records[[component_id]] <- list(component_id = component_id, component_size = length(component_cells), target_cells = paste(component_cells, collapse = ";"),
      exact_bilinear_donor_count = sum(vapply(proposals, `[[`, integer(1), "exact_bilinear_donor_count")), exact_nearest_donor_count = sum(vapply(proposals, `[[`, integer(1), "exact_nearest_donor_count")),
      local_bilinear_donor_count = sum(vapply(proposals, `[[`, integer(1), "local_bilinear_donor_count")), local_nearest_donor_count = sum(vapply(proposals, `[[`, integer(1), "local_nearest_donor_count")),
      maximum_donor_distance_cells = if (all(!is.finite(vapply(proposals, `[[`, numeric(1), "maximum_donor_distance_cells")))) NA_real_ else max(vapply(proposals, `[[`, numeric(1), "maximum_donor_distance_cells"), na.rm = TRUE),
      maximum_donor_distance_km = if (all(!is.finite(vapply(proposals, `[[`, numeric(1), "maximum_donor_distance_km")))) NA_real_ else max(vapply(proposals, `[[`, numeric(1), "maximum_donor_distance_km"), na.rm = TRUE),
      repair_eligible = component_eligible, repair_attempted = length(component_cells) <= maximum_component_size, repaired_cell_count = 0L,
      donor_method = paste(unique(na.omit(vapply(proposals, function(x) x$method %||% NA_character_, character(1)))), collapse = ";"), repair_failure_reason = component_reason,
      repaired = FALSE)
  }
  eligible_components <- vapply(component_records, `[[`, logical(1), "repair_eligible")
  candidate_cells <- if (length(components) && any(eligible_components)) unlist(components[eligible_components], use.names = FALSE) else integer()
  candidate_fraction <- if (sum(template_valid)) length(candidate_cells) / sum(template_valid) else 0
  global_failure <- if (length(candidate_cells) > maximum_repair_count) "maximum_repair_count_exceeded" else if (candidate_fraction > maximum_repair_fraction) "maximum_repair_fraction_exceeded" else NULL
  repaired_cells <- if (is.null(global_failure)) candidate_cells else integer()
  if (length(repaired_cells)) for (component_id in which(eligible_components)) {
    cells <- components[[component_id]]; proposals <- proposed_by_component[[component_id]]
    for (k in seq_along(cells)) output_values[[cells[[k]]] ] <- proposals[[k]]$value
    component_records[[component_id]]$repaired_cell_count <- length(cells)
    component_records[[component_id]]$repaired <- TRUE
  }
  if (!is.null(global_failure)) for (component_id in which(eligible_components)) { component_records[[component_id]]$repair_eligible <- FALSE; component_records[[component_id]]$repair_failure_reason <- global_failure; component_records[[component_id]]$repaired <- FALSE }
  for (component_id in seq_along(component_records)) { component_records[[component_id]]$repaired <- isTRUE(component_records[[component_id]]$repaired %||% FALSE); if (is.null(component_records[[component_id]]$repaired)) component_records[[component_id]]$repaired <- FALSE }
  missing_post <- template_valid & is.na(output_values); outside_mask <- !template_valid & !is.na(output_values)
  repaired_mask <- rep(FALSE, length(output_values)); repaired_mask[repaired_cells] <- TRUE
  if (sum(repaired_mask, na.rm = TRUE) != sum(missing) - sum(missing_post)) stop("Coverage repair invariant failed: repaired_cells != pre_missing - post_missing", call. = FALSE)
  if (!is.null(date)) diagnostics_paths <- era5land_write_repair_diagnostics(template, missing, repaired_mask, missing_post, outside_mask, diagnostics_dir, date, prefix, component_records) else diagnostics_paths <- character()
  component_json <- lapply(component_records, function(x) { x$donor_method <- x$donor_method %||% NA_character_; x$repair_failure_reason <- x$repair_failure_reason %||% NA_character_; x })
  classification_counts <- table(vapply(details, `[[`, character(1), "classification"))
  source_nodata_count <- sum(vapply(details, function(x) identical(x$classification, "source_nodata") || identical(x$classification, "isolated_land_mask_mismatch"), logical(1)))
  projection_created_nodata_count <- sum(vapply(details, function(x) identical(x$classification, "bilinear_interpolation_artifact"), logical(1)))
  list(raster = { terra::values(output_masked) <- output_values; output_masked }, diagnostics = list(details = details, component_records = component_json, template_non_na = sum(template_valid),
    missing_inside_count = sum(missing), missing_inside_pre_repair_count = sum(missing), missing_inside_post_repair_count = sum(missing_post),
    outside_mask_count = sum(outside_mask), repair_applied = length(repaired_cells) > 0L, repair_method = if (any(vapply(details, function(x) identical(x$classification, "isolated_land_mask_mismatch"), logical(1)))) "local_final_grid_idw" else "bounded_local_donor", repair_count = length(repaired_cells),
    repair_fraction = if (sum(template_valid)) length(repaired_cells) / sum(template_valid) else 0, repaired_cell_ids = repaired_cells, unresolved_count = sum(missing_post),
    eligible = length(candidate_cells) > 0L && is.null(global_failure), source_cell_count = terra::ncell(source), source_non_na_count = sum(!is.na(terra::values(source, mat = FALSE))),
    source_na_count = sum(is.na(terra::values(source, mat = FALSE))), source_na_fraction = mean(is.na(terra::values(source, mat = FALSE))),
    classification_counts = as.list(classification_counts), source_nodata_count = source_nodata_count, projection_created_nodata_count = projection_created_nodata_count,
    land_mask_boundary_count = sum(vapply(details, function(x) identical(x$classification, "isolated_land_mask_mismatch"), logical(1))), repairable_count = length(repaired_cells), unrepairable_count = sum(missing_post), maximum_component_size = maximum_component_size,
    maximum_donor_radius_cells = maximum_donor_radius_cells, component_sizes = vapply(components, length, integer(1)), coverage_diagnostic_paths = diagnostics_paths),
    details = details, repaired = length(repaired_cells) > 0L, template_non_na = sum(template_valid), missing_inside_count = sum(missing),
    missing_inside_post_repair_count = sum(missing_post), outside_mask_count = sum(outside_mask), repair_applied = length(repaired_cells) > 0L,
    repair_count = length(repaired_cells), repair_fraction = if (sum(template_valid)) length(repaired_cells) / sum(template_valid) else 0, repaired_cell_ids = repaired_cells,
    unresolved_count = sum(missing_post), eligible = length(candidate_cells) > 0L && is.null(global_failure), component_records = component_json,
    source_cell_count = terra::ncell(source), source_non_na_count = sum(!is.na(terra::values(source, mat = FALSE))), source_na_count = sum(is.na(terra::values(source, mat = FALSE))),
    source_na_fraction = mean(is.na(terra::values(source, mat = FALSE))), coverage_diagnostic_paths = diagnostics_paths)
}
