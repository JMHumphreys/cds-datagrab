resolve_download_target <- function(request, paths) {
  stopifnot(is.character(request$target), length(request$target)==1L, nzchar(request$target))
  if (basename(request$target) != request$target) stop("Request target must contain a filename only.", call.=FALSE)
  if (grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", request$target)) stop("Request target must not contain path traversal.", call.=FALSE)
  assert_storage_target(paths$raw_dir, paths, allow_root=FALSE)
  target <- normalizePath(file.path(paths$raw_dir, request$target), winslash="/", mustWork=FALSE)
  if (!.descendant(target, paths$raw_dir)) stop("Resolved download target escaped raw directory.", call.=FALSE)
  target
}

perform_cds_transfer <- function(dataset_short_name, request, target_path) {
  validate_cds_api_payload(request)
  ecmwfr::wf_request(request=c(list(dataset_short_name=dataset_short_name),request), transfer=TRUE, path=dirname(target_path))
}
format_cds_error <- function(e) {
  msg <- conditionMessage(e); status <- if(grepl("HTTP[ :]?400|status[^0-9]*400",msg,ignore.case=TRUE)) 400L else NA_integer_; detail <- trimws(gsub("[[:space:]]+"," ",msg)); if(is.finite(status)) paste0("CDS request rejected with HTTP ",status,": ",detail) else detail
}

validate_downloaded_target <- function(path, expected_request=NULL) {
  exists <- file.exists(path); size <- if(exists) as.numeric(file.info(path)$size) else 0
  fmt <- if(exists && size > 0) detect_download_format(path) else "unknown"; readable <- FALSE; nc_ok <- FALSE; request_match <- NA; reason <- ""
  if(!exists) reason <- "expected_target_not_created" else if(size <= 0) reason <- "target_zero_bytes" else if(!fmt %in% c("netcdf_classic","netcdf4_hdf5","zip","grib")) reason <- "unsupported_response_format" else {
    if(fmt %in% c("netcdf_classic","netcdf4_hdf5")) { md <- tryCatch(inspect_netcdf_ncdf4(path),error=function(e)e); nc_ok <- !inherits(md,"error"); readable <- nc_ok; if(nc_ok && (length(md$coordinate_variables$latitude)==0L || length(md$coordinate_variables$longitude)==0L || length(md$decoded_dates)==0L || !length(intersect(c("t2m","2m_temperature"),names(md$variables))))) { nc_ok <- FALSE; reason <- "netcdf_metadata_incomplete" } } else readable <- TRUE
    if(!readable && !nzchar(reason)) reason <- "target_not_readable"
  }
  if(!exists || !readable) request_match <- NA else if(!is.null(expected_request)) { request_match <- identical(basename(path),as.character(expected_request$target)); if(!request_match && !nzchar(reason)) reason <- "request_target_mismatch" } else request_match <- NA
  valid <- exists && size > 0 && fmt %in% c("netcdf_classic","netcdf4_hdf5","zip","grib") && readable && (isTRUE(request_match) || is.null(expected_request))
  if(valid) reason <- "ok"
  list(path=normalizePath(path,winslash="/",mustWork=FALSE),exists=exists,size=size,format=fmt,readable=readable,netcdf_metadata_readable=nc_ok,request_match=request_match,valid=valid,failure_reason=reason)
}

download_cds_requests <- function(requests, raw_dir=NULL, run_dir=NULL, dry_run=TRUE, overwrite=FALSE, workers=1, paths=NULL, config=NULL, run_id=NULL, transfer_fun=NULL) {
  if(is.null(paths)) { paths <- list(raw_dir=raw_dir, dataset_root=dirname(raw_dir), root_marker=file.path(dirname(dirname(dirname(raw_dir))),".cds-datagrab-root")); if(!file.exists(paths$root_marker)) stop("Storage root marker is missing",call.=FALSE) }
  fs::dir_create(paths$raw_dir,recurse=TRUE); if(is.null(run_id)) run_id <- basename(run_dir %||% "download")
  statuses <- lapply(requests,function(req) {
    validate_cds_request_structure(req); target <- resolve_download_target(req,paths); tmpdir <- file.path(paths$raw_dir,".partial"); tmp <- file.path(tmpdir,paste0(basename(target),".part")); fs::dir_create(tmpdir,recurse=TRUE)
    existing <- validate_downloaded_target(target,req)
    if(existing$exists && !existing$valid && !dry_run) { qdir <- file.path(paths$raw_quarantine_dir %||% file.path(paths$dataset_root,"quarantine","raw"),run_id); assert_storage_target(qdir,paths,allow_root=FALSE); fs::dir_create(qdir,recurse=TRUE); fs::file_move(target,file.path(qdir,basename(target))) }
    if(existing$valid && !overwrite) return(data.frame(target_filename=req$target,resolved_target_path=target,status="reused_existing",valid=TRUE,exists=existing$exists,size=existing$size,format=existing$format,readable=existing$readable,netcdf_metadata_readable=existing$netcdf_metadata_readable,request_match=existing$request_match,failure_reason="",returned_path="",elapsed_seconds=0,warnings="",error_class="",error_message="",stringsAsFactors=FALSE))
    if(dry_run) return(data.frame(target_filename=req$target,resolved_target_path=target,status="planned",valid=NA,exists=NA,size=NA,format=NA,readable=NA,netcdf_metadata_readable=NA,request_match=NA,failure_reason="",returned_path="",elapsed_seconds=NA,warnings="",error_class="",error_message="",stringsAsFactors=FALSE))
    if(file.exists(tmp)) unlink(tmp)
    api_payload <- build_cds_api_payload(req); if(!is.null(run_dir)) jsonlite::write_json(api_payload,file.path(run_dir,"cds_api_payload.json"),pretty=TRUE,auto_unbox=TRUE)
    start <- Sys.time(); warning_text <- character(); transfer <- transfer_fun %||% perform_cds_transfer; transfer_call <- function() { if(identical(transfer,perform_cds_transfer)) transfer(req$dataset_short_name,api_payload,tmp) else if(length(formals(transfer)) >= 3L) transfer(req$dataset_short_name,api_payload,tmp) else transfer(api_payload,tmp) }; ans <- tryCatch(withCallingHandlers(transfer_call(),warning=function(w){warning_text<<-c(warning_text,conditionMessage(w));invokeRestart("muffleWarning")}), error=function(e) structure(list(success=FALSE,error_class=class(e),error_message=conditionMessage(e),formatted_message=format_cds_error(e)),class="cds_download_failure")); elapsed <- as.numeric(difftime(Sys.time(),start,units="secs")); returned <- if(is.character(ans)&&length(ans)==1L) ans else if(is.list(ans)&&is.character(ans$path %||% NULL)) ans$path else NA_character_; candidate <- if(!is.na(returned) && file.exists(returned)) returned else tmp; if(!file.exists(candidate) && !is.na(returned)) candidate <- returned
    library_target <- file.path(tmpdir,basename(target)); if(!file.exists(candidate) && file.exists(library_target)) candidate <- library_target
    if(file.exists(candidate) && !identical(normalizePath(candidate,winslash="/",mustWork=FALSE),normalizePath(tmp,winslash="/",mustWork=FALSE))) { if(!.descendant(candidate,paths$root)) stop("Returned transfer path is outside approved output root.",call.=FALSE); fs::file_copy(candidate,tmp,overwrite=TRUE) }
    post <- validate_downloaded_target(tmp,NULL); if(inherits(ans,"cds_download_failure")) { post$valid <- FALSE; post$request_match <- NA; post$failure_reason <- ans$formatted_message %||% ans$error_message }
    if(post$valid) { fs::file_move(tmp,target); post <- validate_downloaded_target(target,req); status <- "downloaded" } else status <- "failed"
    row <- data.frame(target_filename=req$target,resolved_target_path=target,status=status,valid=post$valid,exists=post$exists,size=post$size,format=post$format,readable=post$readable,netcdf_metadata_readable=post$netcdf_metadata_readable,request_match=post$request_match,failure_reason=post$failure_reason,returned_path=ifelse(is.na(returned),"",returned),elapsed_seconds=elapsed,warnings=paste(warning_text,collapse=" | "),error_class=if(inherits(ans,"cds_download_failure"))paste(class(ans),collapse=";") else "",error_message=if(inherits(ans,"cds_download_failure"))ans$error_message else "",stringsAsFactors=FALSE)
    if(!post$valid) attr(row,"cds_failure") <- TRUE
    row
  })
  out <- if(length(statuses)) do.call(rbind,statuses) else data.frame(); if(!is.null(run_dir)) utils::write.csv(out,file.path(run_dir,"download_manifest.csv"),row.names=FALSE)
  if(!dry_run && nrow(out) && any(!is.na(out$valid) & !out$valid)) stop(paste0("pipeline_status=failed\nfailed_stage=download\ndownload_target_exists=FALSE\nfailure_reason=",out$failure_reason[[which(!out$valid)[1]]]),call.=FALSE)
  out
}
