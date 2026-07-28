write_pipeline_log <- function(paths, run_id, line) {
  fs::dir_create(paths$pipeline_log_dir, recurse=TRUE)
  f <- file.path(paths$pipeline_log_dir, paste0(run_id, ".log"))
  cat(paste(format(Sys.time(), tz="UTC"), line), "\n", file=f, append=file.exists(f))
  f
}
log_stage <- function(paths, run_id, stage, status="start", details=character()) {
  write_pipeline_log(paths, run_id, paste(c(sprintf("%s stage=%s status=%s", toupper(status), stage, status), details), collapse=" "))
}
