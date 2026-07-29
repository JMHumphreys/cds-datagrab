test_that("variable prefixes, targets, and requests are isolated", {
  sm <- get_variable_spec("era5_soilmoist")
  mt <- get_variable_spec("era5_mintemp")
  expect_false(grepl("mintemp", daily_output_filename(sm, as.Date("2026-07-01"))))
  expect_false(grepl("soilmoist", daily_output_filename(mt, as.Date("2026-07-01"))))
  cfg <- list(project=list(dataset_id="era5_soilmoist"), spatial=list(api_bbox_buffer_degrees=1, align_request_to_source_grid=TRUE, source_grid_degrees=.25), cds=list(dataset_short_name=sm$dataset_short_name, product_type="reanalysis", variable=sm$cds_variable, daily_statistic=sm$daily_statistic, time_zone=sm$time_zone, frequency=sm$frequency, data_format="netcdf", download_format="unarchived"))
  soil_req <- build_variable_requests(as.Date("2026-07-01"), c(42.75,-126,-1.25,-34), cfg, sm)[[1]]
  temp_cfg <- cfg; temp_cfg$project$dataset_id <- "era5_mintemp"; temp_cfg$cds$variable <- mt$cds_variable; temp_cfg$cds$daily_statistic <- mt$daily_statistic
  temp_req <- build_variable_requests(as.Date("2026-07-01"), c(42.75,-126,-1.25,-34), temp_cfg, mt)[[1]]
  expect_false(identical(soil_req$request_hash, temp_req$request_hash))
  expect_false(identical(soil_req$target, temp_req$target))
})
