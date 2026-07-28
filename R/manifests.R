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
  list(run_id=id, run_dir=p$run_dir, mode=mode, dry_run=as.logical(dry_run), execution_flag_source=execution_source, storage_paths=p, start_utc=as.character(Sys.time()))
}
record_manifest_event <- function(manifest, ...) { e<-data.frame(timestamp_utc=as.character(Sys.time()),event=as.character(list(...)[[1]]),stringsAsFactors=FALSE); f<-file.path(manifest$run_dir,"run_events.csv"); if(file.exists(f)) utils::write.table(e,f,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE) else utils::write.csv(e,f,row.names=FALSE); manifest }
finalize_run_manifest <- function(manifest,status) { manifest$end_utc<-as.character(Sys.time()); manifest$status<-status; manifest$path_safety<-list(root_marker=manifest$storage_paths$root_marker,validated=TRUE); jsonlite::write_json(manifest,file.path(manifest$run_dir,"run_manifest.json"),pretty=TRUE,auto_unbox=TRUE); manifest }
