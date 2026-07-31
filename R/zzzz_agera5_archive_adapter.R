# Content-based AgERA5 archive handling. The existing direct-NetCDF path is
# retained; this adapter only changes the AgERA5 response/container boundary.
normalize_netcdf_name <- function(x) {
  x <- tolower(as.character(x)); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x)
}
get_nc_attribute <- function(nc, variable, attribute) {
  result <- ncdf4::ncatt_get(nc, variable, attribute)
  if (!isTRUE(result$hasatt)) return(NA_character_)
  as.character(result$value)
}
agera5_archive_manifest_schema_version <- 2L
agera5_adapter_inspection_version <- "agera5-rh-inspection-v2"
agera5_alias_hash <- function(variable_spec) digest::digest(c("Derived_Relative_Humidity_2m_Min_24h","derived_relative_humidity_2m_min_24h","2m_relative_humidity_derived","relative_humidity_2m_min"), algo="sha256")
agera5_selection_rules_hash <- function() digest::digest("exact-alias>normalized-alias>percent+lon+lat+relative-humidity+minimum-metadata", algo="sha256")
agera5_manifest_fingerprints <- function(variable_spec, checksum) {
  list(archive_manifest_schema_version=agera5_archive_manifest_schema_version,
       adapter_id="agera5_relhum_min", adapter_inspection_version=agera5_adapter_inspection_version,
       dataset_id=variable_spec$id, variable_spec_hash=variable_spec$variable_spec_hash,
       variable_alias_hash=agera5_alias_hash(variable_spec), selection_rules_hash=agera5_selection_rules_hash(),
       archive_checksum=checksum, created_with_package_version=as.character(utils::packageVersion("cdsdatagrab")))
}
.resolve_netcdf_variable_before_agera5 <- resolve_netcdf_variable
resolve_netcdf_variable <- function(nc_metadata, variable_spec) {
  if (!identical(variable_spec$id, "agera5_relhum_min")) return(.resolve_netcdf_variable_before_agera5(nc_metadata, variable_spec))
  aliases <- unique(c(variable_spec$netcdf_variable_names, "Derived_Relative_Humidity_2m_Min_24h", "derived_relative_humidity_2m_min_24h", "relative_humidity_2m_min"))
  names_available <- names(nc_metadata$variables)
  exact <- intersect(aliases, names_available)
  if (length(exact) == 1L) return(exact[[1]])
  normalized <- normalize_netcdf_name(names_available)
  alias_norm <- normalize_netcdf_name(aliases)
  norm_hits <- names_available[normalized %in% alias_norm]
  if (length(norm_hits) == 1L) return(norm_hits[[1]])
  excluded <- grepl("^(crs|time|lat|latitude|lon|longitude)(_|$)|bounds|grid", normalized)
  candidates <- names_available[!excluded]
  candidates <- candidates[vapply(candidates, function(n) {
    v <- nc_metadata$variables[[n]]
    units <- normalize_source_units(v$units %||% NA_character_)
    dims <- normalize_netcdf_name(v$dimensions %||% character())
    long <- normalize_netcdf_name(v$long_name %||% "")
    identical(units, "percent") && any(dims %in% c("lon","longitude")) && any(dims %in% c("lat","latitude")) && grepl("relative_humidity", long) && grepl("minimum|min_24|24_hour", long)
  }, logical(1))]
  if (length(candidates) != 1L) stop("Unable to select a unique AgERA5 RH variable; available variables: ", paste(names_available, collapse=", "), call.=FALSE)
  candidates[[1]]
}
archive_member_date <- function(name) {
  hits <- regmatches(basename(name), gregexpr("(?<![0-9])20[0-9]{6}(?![0-9])", basename(name), perl=TRUE))[[1]]
  if (!length(hits)) return(NA_character_)
  d <- tryCatch(as.Date(hits[[1]], "%Y%m%d"), error=function(e) NA)
  if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
}

archive_member_safe <- function(name) {
  n <- gsub("\\\\", "/", as.character(name))
  nzchar(n) && !grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", n) && !grepl("(^|/)($|\\.)", n)
}

extract_agera5_archive <- function(archive_path, extracted_root, request_hash, variable_spec, run_dir=NULL) {
  checksum <- digest::digest(file=archive_path, algo="sha256")
  final_dir <- file.path(extracted_root, request_hash)
  manifest_path <- file.path(final_dir, "archive_manifest.csv")
  if (dir.exists(final_dir) && file.exists(manifest_path)) {
    old <- tryCatch(utils::read.csv(manifest_path, stringsAsFactors=FALSE), error=function(e) NULL)
    fp <- agera5_manifest_fingerprints(variable_spec, checksum)
    inventory_matches <- function(x) {
      required <- c("member_relative_path", "extracted_path", "member_size", "member_checksum")
      if (!all(required %in% names(x)) || !nrow(x) || any(!vapply(x$member_relative_path, archive_member_safe, logical(1)))) return(FALSE)
      all(vapply(seq_len(nrow(x)), function(i) {
        p <- x$extracted_path[[i]]
        file.exists(p) && identical(as.numeric(file.info(p)$size), as.numeric(x$member_size[[i]])) &&
          identical(digest::digest(file=p, algo="sha256"), as.character(x$member_checksum[[i]]))
      }, logical(1)))
    }
    current <- !is.null(old) && nrow(old) && inventory_matches(old) &&
      all(vapply(names(fp), function(n) n %in% names(old) && all(as.character(old[[n]]) == as.character(fp[[n]])), logical(1)))
    if (isTRUE(current)) {
      attr(old, "extraction_reused") <- TRUE; attr(old, "manifest_reused") <- TRUE
      return(old)
    }
    if (!is.null(old) && nrow(old) && all(file.exists(old$extracted_path)) && all(old$archive_checksum == checksum)) {
      for (n in c("variable_candidate","selected","selection_reason","validation_message","selected_netcdf_variable","date_from_content")) if (!n %in% names(old)) old[[n]] <- NA
      for (n in names(fp)) old[[n]] <- unname(fp[[n]])
      rebuilt <- lapply(seq_len(nrow(old)), function(i) {
        p <- old$extracted_path[[i]]; md <- tryCatch(inspect_netcdf_ncdf4(p), error=function(e)e)
        selected <- if (inherits(md, "error")) NA_character_ else tryCatch(resolve_netcdf_variable(md, variable_spec), error=function(e) NA_character_)
        old$variable_candidate[[i]] <- !is.na(selected); old$selected[[i]] <- FALSE; old$selection_reason[[i]] <- ""
        old$validation_message[[i]] <- if (!is.na(selected)) "candidate" else "variable not found"
        old$selected_netcdf_variable[[i]] <- selected
        old$date_from_content[[i]] <- if (!inherits(md, "error") && length(md$decoded_dates)) canonical_iso_dates(md$decoded_dates[[1]], "date_from_content") else NA_character_
        old[i,]
      })
      rebuilt <- do.call(rbind, rebuilt); utils::write.csv(rebuilt, manifest_path, row.names=FALSE)
      if (!is.null(run_dir)) utils::write.csv(rebuilt, file.path(run_dir, "archive_manifest.csv"), row.names=FALSE)
      attr(rebuilt, "extraction_reused") <- TRUE; attr(rebuilt, "manifest_reused") <- FALSE
      attr(rebuilt, "manifest_rebuild_reason") <- "adapter fingerprint changed"
      return(rebuilt)
    }
    if (!is.null(old) && nrow(old) && !all(old$archive_checksum == checksum)) stop("Existing extracted AgERA5 directory has a different archive checksum", call.=FALSE)
  }
  listing <- tryCatch(utils::unzip(archive_path, list=TRUE), error=function(e) stop("Malformed ZIP archive: ", conditionMessage(e), call.=FALSE))
  members <- as.character(listing$Name)
  if (!length(members) || any(!vapply(members, archive_member_safe, logical(1)))) stop("Archive contains unsafe or empty member names", call.=FALSE)
  if (any(listing$Length < 0)) stop("Archive contains invalid member sizes", call.=FALSE)
  partial <- file.path(extracted_root, ".partial", paste0(request_hash, "-", as.integer(Sys.time()), "-", sample.int(1e6,1)))
  fs::dir_create(partial, recurse=TRUE)
  on.exit(if (dir.exists(partial)) unlink(partial, recursive=TRUE), add=TRUE)
  utils::unzip(archive_path, exdir=partial, overwrite=FALSE)
  extracted <- file.path(partial, members)
  if (any(!file.exists(extracted))) stop("Archive extraction did not produce every member", call.=FALSE)
  nc <- grepl("\\.nc4?$", members, ignore.case=TRUE)
  fp <- agera5_manifest_fingerprints(variable_spec, checksum)
  rows <- lapply(seq_along(members), function(i) {
    p <- extracted[[i]]; fmt <- detect_download_format(p); d <- archive_member_date(members[[i]])
    variable_candidate <- FALSE; content_date <- NA_character_; validation <- "unselected"
    if (nc[[i]]) {
      md <- tryCatch(inspect_netcdf_ncdf4(p), error=function(e)e)
      if (!inherits(md, "error")) {
        variable_candidate <- !inherits(tryCatch(resolve_netcdf_variable(md, variable_spec), error=function(e)e), "error")
        content_date <- if (length(md$decoded_dates)) canonical_iso_dates(md$decoded_dates[[1]], "date_from_content") else NA_character_
        validation <- if (variable_candidate) "candidate" else "variable not found"
      } else validation <- conditionMessage(md)
    }
    data.frame(archive_manifest_schema_version=fp$archive_manifest_schema_version, adapter_id=fp$adapter_id,
      adapter_inspection_version=fp$adapter_inspection_version, dataset_id=fp$dataset_id,
      variable_spec_hash=fp$variable_spec_hash, variable_alias_hash=fp$variable_alias_hash,
      selection_rules_hash=fp$selection_rules_hash, created_with_package_version=fp$created_with_package_version,
      archive_path=normalizePath(archive_path,winslash="/",mustWork=FALSE), archive_checksum=checksum,
      request_hash=request_hash, member_name=members[[i]], member_relative_path=gsub("\\\\","/",members[[i]]),
      extracted_path=normalizePath(p,winslash="/",mustWork=FALSE), member_size=as.numeric(listing$Length[[i]]),
      member_checksum=digest::digest(file=p,algo="sha256"), detected_format=fmt, date_from_filename=d,
      date_from_content=content_date, variable_candidate=variable_candidate,
      selected_netcdf_variable=if (nc[[i]] && variable_candidate) tryCatch(resolve_netcdf_variable(inspect_netcdf_ncdf4(p),variable_spec), error=function(e) NA_character_) else NA_character_, selected=FALSE,
      selection_reason="", validation_message=validation, stringsAsFactors=FALSE)
  })
  manifest <- do.call(rbind, rows)
  if (dir.exists(final_dir)) stop("Extraction destination already exists", call.=FALSE)
  fs::dir_create(dirname(final_dir), recurse=TRUE)
  if (!file.rename(partial, final_dir)) stop("Could not atomically promote extracted archive", call.=FALSE)
  manifest$extracted_path <- file.path(final_dir, manifest$member_relative_path)
  manifest$extracted_path <- normalizePath(manifest$extracted_path,winslash="/",mustWork=FALSE)
  manifest$selected <- FALSE
  utils::write.csv(manifest, file.path(final_dir,"archive_manifest.csv"), row.names=FALSE)
  if (!is.null(run_dir)) utils::write.csv(manifest, file.path(run_dir,"archive_manifest.csv"), row.names=FALSE)
  attr(manifest, "extraction_reused") <- FALSE; attr(manifest, "manifest_reused") <- FALSE
  manifest
}

select_agera5_archive_members <- function(manifest, request_dates) {
  dates <- canonical_iso_dates(request_dates, "requested archive dates")
  out <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    z <- manifest[manifest$date_from_filename == dates[[i]] & manifest$variable_candidate & manifest$detected_format %in% c("netcdf4_hdf5","netcdf_classic"),,drop=FALSE]
    if (nrow(z) != 1L) stop("Expected exactly one AgERA5 NetCDF member for ", dates[[i]], "; found ", nrow(z), call.=FALSE)
    if (!is.na(z$date_from_content[[1]]) && z$date_from_content[[1]] != dates[[i]]) stop("Archive filename/content date disagreement for ", dates[[i]], call.=FALSE)
    z$selected <- TRUE; z$selection_reason <- "unique variable/date match"
    if (!"selected_netcdf_variable" %in% names(z)) z$selected_netcdf_variable <- NA_character_
    if (is.na(z$selected_netcdf_variable[[1]])) z$selected_netcdf_variable[[1]] <- "Derived_Relative_Humidity_2m_Min_24h"
    out[[i]] <- z
  }
  do.call(rbind, out)
}

.download_cds_requests_before_archive_adapter <- download_cds_requests
download_cds_requests <- function(requests, ...) {
  if (!length(requests) || !any(vapply(requests, function(x) identical(x$adapter, "agera5"), logical(1)))) return(.download_cds_requests_before_archive_adapter(requests, ...))
  dots <- list(...); paths <- dots$paths; run_dir <- dots$run_dir; dry_run <- isTRUE(dots$dry_run %||% TRUE); overwrite <- isTRUE(dots$overwrite %||% FALSE)
  if (length(requests) != 1L) return(.download_cds_requests_before_archive_adapter(requests, ...))
  req <- requests[[1]]; zip_target <- file.path(paths$raw_dir, sub("\\.(nc|netcdf)$", ".zip", req$target, ignore.case=TRUE));
  if (file.exists(zip_target) && !overwrite && isTRUE(validate_downloaded_target(zip_target, req)$valid)) {
    row <- data.frame(target_filename=req$target,resolved_target_path=zip_target,status="reused_existing",valid=TRUE,exists=TRUE,size=file.info(zip_target)$size,format="zip",readable=TRUE,netcdf_metadata_readable=NA,request_match=TRUE,failure_reason="",returned_path="",elapsed_seconds=0,warnings="",error_class="",error_message="",stringsAsFactors=FALSE)
    if (!is.null(run_dir)) { jsonlite::write_json(build_cds_api_payload(req),file.path(run_dir,"cds_api_payload.json"),pretty=TRUE,auto_unbox=TRUE); utils::write.csv(row,file.path(run_dir,"download_manifest.csv"),row.names=FALSE) }
    return(row)
  }
  result <- .download_cds_requests_before_archive_adapter(requests, ...)
  source <- result$resolved_target_path[[1]]
  if (!dry_run && file.exists(source) && identical(detect_download_format(source), "zip")) {
    if (file.exists(zip_target)) { if (!identical(digest::digest(file=source,algo="sha256"),digest::digest(file=zip_target,algo="sha256"))) stop("Nonidentical ZIP exists at resolved destination",call.=FALSE); unlink(source) } else if (!file.rename(source,zip_target)) stop("Could not atomically promote AgERA5 ZIP",call.=FALSE)
    result$resolved_target_path[[1]] <- zip_target; result$format[[1]] <- "zip"; result$target_filename[[1]] <- req$target
    if (!is.null(run_dir)) utils::write.csv(result,file.path(run_dir,"download_manifest.csv"),row.names=FALSE)
  }
  result
}

.read_era5_daily_layers_before_archive_adapter <- read_era5_daily_layers
read_era5_daily_layers <- function(path, expected_dates=NULL, variable_spec=NULL, raw_request_dates=NULL, dates_to_process=NULL, request_hash=NULL) {
  if (detect_download_format(path) != "zip") return(.read_era5_daily_layers_before_archive_adapter(path,expected_dates,variable_spec,raw_request_dates,dates_to_process,request_hash))
  spec <- variable_spec %||% get_variable_spec("agera5_relhum_min")
  ex <- file.path(dirname(path), "..", "extracted")
  manifest <- extract_agera5_archive(path, ex, request_hash %||% digest::digest(file=path,algo="xxhash32"), spec)
  chosen <- select_agera5_archive_members(manifest, dates_to_process %||% raw_request_dates %||% expected_dates)
  date_map <- data.frame(date=chosen$date_from_filename, source_path=chosen$extracted_path,
    archive_path=chosen$archive_path, archive_member=chosen$member_name,
    request_hash=request_hash %||% chosen$request_hash,
    raw_request_start=min(chosen$date_from_filename), raw_request_end=max(chosen$date_from_filename),
    date_from_filename=chosen$date_from_filename, date_from_content=chosen$date_from_content,
    mapping_reason=chosen$selection_reason, stringsAsFactors=FALSE)
  utils::write.csv(date_map, file.path(dirname(chosen$extracted_path[[1]]), "date_source_map.csv"), row.names=FALSE)
  pieces <- lapply(seq_len(nrow(chosen)), function(i) .read_era5_daily_layers_before_archive_adapter(chosen$extracted_path[[i]], expected_dates=as.Date(chosen$date_from_filename[[i]]), variable_spec=spec, raw_request_dates=as.Date(chosen$date_from_filename[[i]]), dates_to_process=as.Date(chosen$date_from_filename[[i]]), request_hash=request_hash))
  list(rasters=unlist(lapply(pieces, `[[`, "rasters"), recursive=FALSE), dates=as.Date(chosen$date_from_filename), decoded_dates=as.POSIXct(chosen$date_from_content), reader_used="ncdf4", source_format="zip", selected_variable=unlist(lapply(pieces, `[[`, "selected_variable")), selected_netcdf_variable=unlist(lapply(pieces, `[[`, "selected_netcdf_variable")), selected_variable_alias=unlist(lapply(pieces, `[[`, "selected_variable_alias")), source_units=spec$source_units, source_units_original=spec$source_units, source_units_normalized=spec$source_units, output_units=spec$output_units, unit_conversion=spec$unit_conversion, source_value_minimum=min(vapply(pieces,function(x)x$source_value_minimum,numeric(1))), source_value_maximum=max(vapply(pieces,function(x)x$source_value_maximum,numeric(1))), daily_statistic_source="AgERA5_precomputed_derived_indicator", daily_statistic=spec$daily_statistic, subdaily_frequency=spec$frequency)
}
