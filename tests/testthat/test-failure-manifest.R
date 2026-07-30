test_that("run manifest is durable at initialization", {
  base <- test_external_root("manifest"); cfg <- list(project=list(profile="smoke",dataset_id="era5_mintemp"),paths=list(root=base)); cfg$project_root <- package_root(); m <- initialize_run_manifest(cfg,"plan",TRUE); expect_true(file.exists(file.path(m$run_dir,"run_manifest.json"))); z <- jsonlite::read_json(file.path(m$run_dir,"run_manifest.json"),simplifyVector=TRUE); expect_equal(z$pipeline_status,"running"); expect_true("process" %in% z$pending_stages)
})
