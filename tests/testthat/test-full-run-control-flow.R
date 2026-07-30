test_that("full run stops at download failure and leaves durable manifest", {
  base <- test_external_root("full-control"); cfg_path <- package_file("config","era5_mintemp_smoke.yml"); expect_error(run_era5_mintemp_pipeline(cfg_path,mode="full",dry_run=FALSE,output_root=base,observed_end="2026-07-03",transfer_fun=function(...) NULL),"failed_stage=download")
  manifests <- list.files(file.path(base,"runs","smoke","era5_mintemp"),pattern="run_manifest.json",recursive=TRUE,full.names=TRUE); expect_length(manifests,1); m <- jsonlite::read_json(manifests, simplifyVector=TRUE); expect_equal(m$failed_stage,"download"); expect_equal(m$stage_results$download$status,"failed"); expect_equal(m$stage_results$process$status,"not_run")
})
