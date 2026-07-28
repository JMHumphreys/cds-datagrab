#!/usr/bin/env Rscript
root <- normalizePath(getwd(), winslash="/")
invisible(lapply(list.files(file.path(root,"R"), pattern="\\.R$", full.names=TRUE), source))
parsed <- tryCatch(parse_pipeline_args(), error=function(e) { message("Fatal: ", conditionMessage(e)); quit(status=2) })
execution <- tryCatch(resolve_execution_choice(parsed), error=function(e) { message("Fatal: ", conditionMessage(e)); quit(status=2) })
if (isTRUE(parsed$verbose)) {
  cfg0 <- read_pipeline_config(parsed$config); cfg0 <- resolve_config_paths(cfg0, root, parsed$output_root, FALSE)
  message("mode: ", parsed$mode, "\nprofile: ", cfg0$project$profile, "\ndataset ID: ", cfg0$project$dataset_id, "\neffective dry-run: ", execution$dry_run, "\noutput root: ", cfg0$paths$root, "\nraw directory: ", cfg0$paths$raw_dir, "\ndaily directory: ", cfg0$paths$daily_dir, "\nweekly directory: ", cfg0$paths$weekly_dir, "\nrun directory: ", cfg0$paths$runs_root)
}
tryCatch({
  result <- run_era5_mintemp_pipeline(config_path=parsed$config, mode=parsed$mode, dry_run=execution$dry_run, observed_end=parsed$observed_end, future_end=parsed$future_end, output_root=parsed$output_root, overwrite=isTRUE(parsed$overwrite), rebuild_all_weeks=isTRUE(parsed$rebuild_all_weeks), execution_source=execution$source)
  message("Pipeline completed: ", result$run_id, " status=", result$status)
}, error=function(e) { message("Fatal: ", conditionMessage(e)); quit(status=1) })
