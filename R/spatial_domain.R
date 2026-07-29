template_crs_parameters <- function(template) {
  p <- terra::crs(template, proj=TRUE)
  getp <- function(key) { z <- regexec(paste0("[+]", key, "=([-+0-9.eE]+)"), p); m <- regmatches(p,z)[[1]]; if(length(m)) as.numeric(m[2]) else NA_real_ }
  units <- sub(".*[+]units=([^ ]+).*", "\\1", p); if(identical(units,p)) units <- NA_character_
  list(projection=if(grepl("[+]proj=aea",p)) "aea" else NA_character_, datum=if(grepl("[+]datum=WGS84",p,ignore.case=TRUE)) "WGS84" else NA_character_, units=units, unit_conversion_factor=if(identical(units,"km")) 1000 else if(identical(units,"m")) 1 else NA_real_, central_meridian=getp("lon_0"), latitude_of_origin=getp("lat_0"), latitude_of_first_standard_parallel=getp("lat_1"), latitude_of_second_standard_parallel=getp("lat_2"), false_easting=getp("x_0"), false_northing=getp("y_0"), proj_string=p)
}
validate_template_crs <- function(template, expected_crs, tolerance=0.001) {
  p <- terra::crs(template, proj=TRUE); if(!nzchar(p)) stop("Template CRS is missing", call.=FALSE); if(terra::is.lonlat(template)) stop("Template CRS must be projected", call.=FALSE)
  got <- template_crs_parameters(template); numeric_fields <- c("central_meridian","latitude_of_origin","latitude_of_first_standard_parallel","latitude_of_second_standard_parallel","false_easting","false_northing")
  if(is.na(got$projection)||got$projection!="aea") stop("Template CRS is not Albers equal-area",call.=FALSE); if(is.na(got$datum)||got$datum!="WGS84") stop("Template CRS datum is not WGS84",call.=FALSE); if(is.na(got$units)||got$units!="km"||abs(got$unit_conversion_factor-1000)>tolerance*1000) stop("Template CRS must use kilometer native units",call.=FALSE)
  for(n in numeric_fields) if(is.na(got[[n]])||is.null(expected_crs[[n]])||abs(got[[n]]-as.numeric(expected_crs[[n]]))>tolerance) stop("Template CRS parameter does not match expected: ",n,call.=FALSE)
  list(valid=TRUE, parameters=got)
}
template_fingerprint <- function(path_or_template) { r <- if(is.character(path_or_template)) terra::rast(path_or_template) else path_or_template; vals <- terra::values(r,mat=FALSE); list(crs=terra::crs(r,proj=TRUE), extent=as.vector(terra::ext(r)), resolution=as.numeric(terra::res(r)), origin=as.numeric(terra::origin(r)), rows=terra::nrow(r), columns=terra::ncol(r), layers=terra::nlyr(r), non_na_count=sum(!is.na(vals)), na_pattern_hash=digest::digest(is.na(vals),algo="xxhash32"), value_hash=digest::digest(vals,algo="xxhash32"), file_checksum=if(is.character(path_or_template)) unname(tools::md5sum(path_or_template)) else NA_character_, sha256=if(is.character(path_or_template)) digest::digest(file=path_or_template,algo="sha256") else NA_character_)
}
validate_template_geometry <- function(template, expected, tolerance=0.001) { if(terra::nrow(template)!=expected$rows||terra::ncol(template)!=expected$columns||terra::nlyr(template)!=expected$layers) stop("Template dimensions do not match expected",call.=FALSE); if(any(abs(terra::res(template)-c(expected$resolution$x,expected$resolution$y))>tolerance)) stop("Template resolution does not match expected",call.=FALSE); e<-as.vector(terra::ext(template)); ee<-c(expected$extent$xmin,expected$extent$xmax,expected$extent$ymin,expected$extent$ymax); if(any(abs(e-ee)>tolerance)) stop("Template extent does not match expected",call.=FALSE); vals<-terra::values(template,mat=FALSE); if(!any(!is.na(vals))) stop("Template is all NA",call.=FALSE); if(any(vals[!is.na(vals)]!=1)) stop("Template non-NA mask values must equal 1",call.=FALSE); if(all(!is.na(vals))) stop("Template mask is globally non-NA",call.=FALSE); TRUE }
template_mask_footprint_lonlat <- function(template_path) {
  r <- terra::rast(template_path)
  v <- terra::values(r, mat = FALSE)
  if (!any(!is.na(v))) stop("Template has no non-NA cells", call. = FALSE)
  mask <- terra::ifel(!is.na(r), 1, NA)
  fp <- terra::as.polygons(mask, dissolve = TRUE, na.rm = TRUE, values = FALSE)
  fp <- terra::project(fp, "EPSG:4326")
  if (is.null(fp) || terra::nrow(fp) < 1L) stop("Could not construct template footprint", call. = FALSE)
  e <- terra::ext(fp)
  extent <- c(north = terra::ymax(e), west = terra::xmin(e), south = terra::ymin(e), east = terra::xmax(e))
  if (extent[["north"]] <= extent[["south"]] || extent[["east"]] <= extent[["west"]]) stop("Invalid template geographic footprint", call. = FALSE)
  list(geometry = fp, extent = extent, north = extent[["north"]], west = extent[["west"]], south = extent[["south"]], east = extent[["east"]], template_non_na_count = sum(!is.na(v)))
}

bbox_footprint_lonlat <- function(bbox_path) {
  b <- sf::st_read(bbox_path, quiet = TRUE)
  if (is.na(sf::st_crs(b))) stop("Bounding box has no CRS", call. = FALSE)
  b <- sf::st_union(sf::st_make_valid(b))
  b <- sf::st_transform(b, 4326)
  e <- sf::st_bbox(b)
  extent <- c(north = unname(e[["ymax"]]), west = unname(e[["xmin"]]), south = unname(e[["ymin"]]), east = unname(e[["xmax"]]))
  if (extent[["north"]] <= extent[["south"]] || extent[["east"]] <= extent[["west"]]) stop("Invalid GPKG geographic footprint", call. = FALSE)
  list(geometry = b, extent = extent, north = extent[["north"]], west = extent[["west"]], south = extent[["south"]], east = extent[["east"]])
}

combine_request_extents <- function(template_extent, bbox_extent, source = c("template", "bbox", "template_bbox_union")) {
  source <- match.arg(source); t <- template_extent; b <- bbox_extent
  if (source == "template") out <- t else if (source == "bbox") out <- b else out <- c(north=max(t[["north"]],b[["north"]]), west=min(t[["west"]],b[["west"]]), south=min(t[["south"]],b[["south"]]), east=max(t[["east"]],b[["east"]]))
  names(out) <- c("north","west","south","east")
  controllers <- c(north=if(source=="bbox") "bbox" else if(source=="template") "template" else if(t[["north"]]>=b[["north"]]) "template" else "bbox", west=if(source=="bbox") "bbox" else if(source=="template") "template" else if(t[["west"]]<=b[["west"]]) "template" else "bbox", south=if(source=="bbox") "bbox" else if(source=="template") "template" else if(t[["south"]]<=b[["south"]]) "template" else "bbox", east=if(source=="bbox") "bbox" else if(source=="template") "template" else if(t[["east"]]>=b[["east"]]) "template" else "bbox")
  attr(out, "boundary_controllers") <- controllers
  out
}

buffer_geographic_extent <- function(area, buffer_degrees) {
  stopifnot(length(area) == 4L, length(buffer_degrees) == 1L, is.finite(buffer_degrees), buffer_degrees >= 0)
  out <- c(north=min(90,area[["north"]]+buffer_degrees), west=max(-180,area[["west"]]-buffer_degrees), south=max(-90,area[["south"]]-buffer_degrees), east=min(180,area[["east"]]+buffer_degrees))
  if(out[["north"]] <= out[["south"]] || out[["east"]] <= out[["west"]]) stop("Buffered geographic extent is invalid", call.=FALSE)
  out
}

align_extent_to_source_grid <- function(area, grid_degrees = 0.25) {
  if(length(area)!=4L || !is.finite(grid_degrees) || grid_degrees<=0) stop("Invalid source grid", call.=FALSE)
  tol <- grid_degrees * 1e-9
  out <- c(north=ceiling((area[["north"]]-tol)/grid_degrees)*grid_degrees, west=floor((area[["west"]]+tol)/grid_degrees)*grid_degrees, south=floor((area[["south"]]+tol)/grid_degrees)*grid_degrees, east=ceiling((area[["east"]]-tol)/grid_degrees)*grid_degrees)
  out <- pmin(c(north=90,west=180,south=90,east=180), pmax(c(north=-90,west=-180,south=-90,east=-180), out))
  names(out) <- c("north","west","south","east")
  if(out[["north"]] <= out[["south"]] || out[["east"]] <= out[["west"]]) stop("Aligned extent is invalid", call.=FALSE)
  out
}

derive_cds_request_area <- function(template_path, bbox_path, request_extent_source="template_bbox_union", buffer_degrees=1.0, source_grid_degrees=0.25, align_to_source_grid=TRUE) {
  t <- template_mask_footprint_lonlat(template_path); b <- bbox_footprint_lonlat(bbox_path); u <- combine_request_extents(t$extent,b$extent,request_extent_source); buf <- buffer_geographic_extent(u,buffer_degrees); final <- if(align_to_source_grid) align_extent_to_source_grid(buf,source_grid_degrees) else buf
  list(template_mask_extent=t$extent, bbox_extent=b$extent, union_extent=u, buffered_extent=buf, grid_aligned_extent=final, final_cds_area=final, boundary_controllers=attr(u,"boundary_controllers"), buffer_degrees=buffer_degrees, source_grid_degrees=source_grid_degrees, request_extent_source=request_extent_source, template_non_na_count=t$template_non_na_count, template_geometry=t$geometry, bbox_geometry=b$geometry)
}

bbox_to_cds_area <- function(bbox_path, buffer_degrees=0.25) align_extent_to_source_grid(buffer_geographic_extent(bbox_footprint_lonlat(bbox_path)$extent,buffer_degrees),0.25)

diagnose_spatial_domain <- function(template_path,bbox_path,config=NULL) {
  r <- terra::rast(template_path); fp <- template_fingerprint(template_path); pars <- template_crs_parameters(r); s <- config$spatial %||% list(); area <- derive_cds_request_area(template_path,bbox_path,s$request_extent_source %||% "template_bbox_union",s$api_bbox_buffer_degrees %||% 1.0,s$source_grid_degrees %||% 0.25,isTRUE(s$align_request_to_source_grid %||% TRUE))
  old <- c(north=40.25,west=-125.25,south=-0.25,east=-34.75)
  out <- list(template_path=normalizePath(template_path,winslash="/"), template_file_md5=unname(tools::md5sum(template_path)), template_file_sha256=digest::digest(file=template_path,algo="sha256"), template_file_checksum=unname(tools::md5sum(template_path)), template_crs=terra::crs(r,proj=FALSE), template_mask_geographic_extent=area$template_mask_extent, bbox_geographic_extent=area$bbox_extent, union_extent=area$union_extent, buffered_extent=area$buffered_extent, source_grid_aligned_extent=area$grid_aligned_extent, final_cds_area=area$final_cds_area, proposed_cds_area=area$final_cds_area, boundary_controlling=area$boundary_controllers, buffer_degrees=area$buffer_degrees, source_grid_degrees=area$source_grid_degrees, request_extent_source=area$request_extent_source, template_non_na_count=area$template_non_na_count, previous_request_area=old, previous_request_covers_template=all(c(old["north"]>=area$template_mask_extent["north"],old["west"]<=area$template_mask_extent["west"],old["south"]<=area$template_mask_extent["south"],old["east"]>=area$template_mask_extent["east"])), non_na_count=fp$non_na_count, extent=as.vector(terra::ext(r)), resolution=terra::res(r), dimensions=c(rows=terra::nrow(r),columns=terra::ncol(r)), projection_method=pars$projection, datum=pars$datum, linear_units=pars$units)
  out
}
