assert_storage_target <- function(target, paths, allow_root = FALSE) {
  if (!file.exists(paths$root_marker)) stop("Storage root marker is missing", call.=FALSE)
  marker <- tryCatch(jsonlite::read_json(paths$root_marker, simplifyVector=TRUE), error=function(e)NULL)
  if (is.null(marker) || !identical(marker$application, "cds-datagrab")) stop("Invalid cds-datagrab storage root marker", call.=FALSE)
  allowed <- c(paths$dataset_root, paths$runs_root, paths$pipeline_log_dir, paths$slurm_log_dir)
  if (!.descendant(target, paths$root) || (!allow_root && !any(vapply(allowed, function(a) .descendant(target, a), logical(1))))) stop("Target is outside the verified active storage subtree", call.=FALSE)
  TRUE
}
initialize_run_manifest <- function(config, mode, dry_run = TRUE, execution_source = "default") {
  p <- resolve_storage_paths(config, config$project_root %||% resolve_project_root(), create=TRUE)
  id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="UTC"), "_", substr(digest::digest(list(config,mode,p),algo="xxhash32"),1,8)); p$run_dir <- file.path(p$runs_root,id); assert_storage_target(p$run_dir,p,allow_root=FALSE); fs::dir_create(p$run_dir,recurse=TRUE)
  m <- list(run_id=id, run_dir=p$run_dir, mode=mode, dry_run=as.logical(dry_run), execution_flag_source=execution_source, storage_paths=p, start_time=as.character(Sys.time()), start_utc=as.character(Sys.time()), pipeline_status="running", current_stage="initialization", failed_stage=NA_character_, failure_class=NA_character_, failure_message=NA_character_, completed_stages=character(), pending_stages=c("plan","download","process","aggregate","estimate","final_validation"), stage_results=list())
  write_run_manifest(m); m
}
normalize_manifest_dates <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  if (is.data.frame(x)) return(lapply(x, manifest_json_safe))
  if (is.list(x)) { y <- lapply(x, manifest_json_safe); names(y) <- names(x); return(y) }
  x
}
manifest_json_safe <- normalize_manifest_dates
write_run_manifest <- function(manifest) { x<-normalize_manifest_dates(manifest); for(n in c("planned_request_hashes","active_request_hashes")) if(n %in% names(x)) x[[n]]<-structure(as.character(x[[n]]%||%character()),class="AsIs"); jsonlite::write_json(x,file.path(manifest$run_dir,"run_manifest.json"),pretty=TRUE,auto_unbox=TRUE,null="null"); invisible(manifest) }
update_manifest_stage <- function(manifest, stage, status, result=list()) {
  if (is.logical(status) && length(status)==1L) status <- if(status) "success_noop" else "not_run"
  result <- result[setdiff(names(result), "status")]
  manifest$current_stage <- stage
  if(status %in% c("success","success_noop","skipped")) manifest$completed_stages <- unique(c(manifest$completed_stages,stage))
  manifest$pending_stages <- setdiff(manifest$pending_stages,manifest$completed_stages)
  manifest$stage_results[[stage]] <- c(list(status=status),result)
  write_run_manifest(manifest); manifest
}
record_manifest_event <- function(manifest, ...) { e<-data.frame(timestamp_utc=as.character(Sys.time()),event=as.character(list(...)[[1]]),stringsAsFactors=FALSE); f<-file.path(manifest$run_dir,"run_events.csv"); if(file.exists(f)) utils::write.table(e,f,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE) else utils::write.csv(e,f,row.names=FALSE); manifest }
finalize_run_manifest <- function(manifest,status="success") { manifest$end_time<-as.character(Sys.time()); manifest$end_utc<-as.character(Sys.time()); manifest$pipeline_status<-status; manifest$status<-status; if(identical(status,"success")) manifest$current_stage<-"complete"; manifest$path_safety<-list(root_marker=manifest$storage_paths$root_marker,validated=TRUE); write_run_manifest(manifest); manifest }
