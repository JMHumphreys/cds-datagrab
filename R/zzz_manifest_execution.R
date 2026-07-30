.initialize_run_manifest_unannotated <- initialize_run_manifest
initialize_run_manifest <- function(config, mode, dry_run = TRUE, execution_source = "default") {
  manifest <- .initialize_run_manifest_unannotated(config, mode, dry_run, execution_source)
  job_id <- Sys.getenv("SLURM_JOB_ID", "")
  slurm <- nzchar(job_id)
  manifest$execution_context <- if (slurm) "slurm" else "direct"
  manifest$slurm_job_id <- if (slurm) job_id else NULL
  manifest$slurm_array_job_id <- if (slurm) Sys.getenv("SLURM_ARRAY_JOB_ID", "") else NULL
  manifest$slurm_array_task_id <- if (slurm) Sys.getenv("SLURM_ARRAY_TASK_ID", "") else NULL
  manifest$slurm_job_name <- if (slurm) Sys.getenv("SLURM_JOB_NAME", "") else NULL
  manifest$slurm_submit_dir <- if (slurm) Sys.getenv("SLURM_SUBMIT_DIR", "") else NULL
  manifest$slurm_node_list <- if (slurm) Sys.getenv("SLURM_NODELIST", "") else NULL
  write_run_manifest(manifest)
  manifest
}

