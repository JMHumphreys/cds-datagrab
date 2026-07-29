test_that("soil moisture payload is an exact sanitized CDS payload", {
  sm <- get_variable_spec("era5_soilmoist")
  cfg <- list(project=list(dataset_id="era5_soilmoist"), spatial=list(api_bbox_buffer_degrees=1, align_request_to_source_grid=TRUE, source_grid_degrees=.25), cds=list(dataset_short_name=sm$dataset_short_name, product_type="reanalysis", variable=sm$cds_variable, daily_statistic=sm$daily_statistic, time_zone=sm$time_zone, frequency=sm$frequency, data_format="netcdf", download_format="unarchived"))
  req <- build_variable_requests(as.Date(c("2026-07-01","2026-07-02","2026-07-03")), c(42.75,-126,-1.25,-34), cfg, sm)[[1]]
  payload <- build_cds_api_payload(req)
  expect_equal(payload, list(product_type="reanalysis", variable="volumetric_soil_water_layer_1", year="2026", month="07", day=c("01","02","03"), daily_statistic="daily_mean", time_zone="utc+00:00", frequency="6_hourly", data_format="netcdf", download_format="unarchived", area=c(42.75,-126,-1.25,-34)))
  expect_false(any(c("request_hash","target","variable_spec_hash","source_grid_alignment") %in% names(payload)))
})
