test_that("runtime logs resolve below the output root", {
  base <- file.path(getwd(),".test-log-root"); if(dir.exists(base)) unlink(base,recursive=TRUE); on.exit(unlink(base,recursive=TRUE),add=TRUE); cfg <- list(project=list(profile="smoke",dataset_id="era5_mintemp"),paths=list(root=NULL)); p <- resolve_storage_paths(cfg,getwd(),base,create=TRUE); f <- write_pipeline_log(p,"run1","START stage=download"); expect_true(.descendant(f,p$root)); expect_true(grepl("logs/pipeline/smoke/era5_mintemp",f,fixed=TRUE)); expect_true(file.exists(f))
})
