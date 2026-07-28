write_pipeline_log <- function(paths, run_id, line) {
  fs::dir_create(paths$pipeline_log_dir, recurse=TRUE)
  f <- file.path(paths$pipeline_log_dir, paste0(run_id, ".log"))
  cat(paste(format(Sys.time(), tz="UTC"), line), "\n", file=f, append=file.exists(f))
  f
}
log_stage <- function(paths, run_id, stage, status="start", details=character()) {
  prefix <- switch(status, success_noop="SUCCESS_NOOP", success="SUCCESS", skipped="SKIPPED", failed="FAILED", start="START", running="START", not_run="NOT_RUN", toupper(status))
  write_pipeline_log(paths, run_id, paste(c(sprintf("%s stage=%s status=%s", prefix, stage, status), details), collapse=" "))
}
