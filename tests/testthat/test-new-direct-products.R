test_that("new product identities and units are explicit", {
  lai <- get_variable_spec("era5_lai_low")
  rh <- get_variable_spec("agera5_relhum_min")
  expect_equal(c(lai$id,lai$short_name,lai$cds_variable,lai$daily_statistic,lai$weekly_statistic), c("era5_lai_low","lai_low","leaf_area_index_low_vegetation","instantaneous_00_utc","mean"))
  expect_equal(c(rh$id,rh$short_name,rh$cds_variable,rh$daily_statistic,rh$weekly_statistic), c("agera5_relhum_min","relhum_min","2m_relative_humidity_derived","24_hour_minimum","mean"))
  expect_equal(normalize_source_units("m**2 m**-2"), "m2 m-2")
  expect_equal(normalize_source_units("percentage"), "percent")
  expect_equal(lai$temporal_character, "monthly_climatology")
  expect_equal(rh$model_alias, "relhum")
})

test_that("LAI request is direct ERA5 at 00 UTC and uses valid calendar days", {
  spec <- get_variable_spec("era5_lai_low")
  cfg <- list(project=list(dataset_id=spec$id), spatial=list(source_grid_degrees=.25, align_request_to_source_grid=TRUE), cds=list(dataset_short_name=spec$dataset_short_name, product_type="reanalysis", variable=spec$cds_variable, daily_statistic=spec$daily_statistic, data_format="netcdf", download_format="unarchived"))
  req <- build_variable_requests(as.Date(c("2024-02-28","2024-02-29")), c(42.8,-126,-1.1,-34), cfg, spec)[[1]]
  expect_equal(req$day, c("28","29"))
  expect_equal(build_cds_api_payload(req), list(product_type="reanalysis", variable=spec$cds_variable, year="2024", month="02", day=c("28","29"), time="00:00", data_format="netcdf", download_format="unarchived", area=c(42.8,-126,-1.1,-34)))
  expect_false(any(grepl("high_vegetation|weighted|cover", names(req), ignore.case=TRUE)))
})

test_that("AgERA5 request uses its dataset-specific schema", {
  spec <- get_variable_spec("agera5_relhum_min")
  cfg <- list(project=list(dataset_id=spec$id), spatial=list(source_grid_degrees=.1, align_request_to_source_grid=TRUE), cds=list(dataset_short_name=spec$dataset_short_name, variable=spec$cds_variable, daily_statistic=spec$daily_statistic, format="netcdf"))
  req <- build_variable_requests(as.Date(c("2025-06-01","2025-06-03")), c(42.8,-126,-1.1,-34), cfg, spec)[[1]]
  expect_equal(build_cds_api_payload(req), list(format="netcdf", variable=spec$cds_variable, year="2025", month="06", day=c("01","03"), daily_statistic="24_hour_minimum", area=c(42.8,-126,-1.1,-34)))
  expect_false(any(c("temperature","dewpoint","tetens") %in% tolower(names(req))))
})

test_that("new product validation clamps only configured numerical artifacts", {
  lai <- get_variable_spec("era5_lai_low")
  rh <- get_variable_spec("agera5_relhum_min")
  expect_equal(normalize_output_values(c(-.005, 2), lai, list(validation=list(physical_minimum=0, physical_maximum=20, clamp_tolerance=.01)))$values, c(0,2))
  expect_equal(normalize_output_values(c(-.2, 100.2), rh, list(validation=list(physical_minimum_percent=0, physical_maximum_percent=100, clamp_tolerance_percent=.5)))$values, c(0,100))
  expect_error(normalize_output_values(-.02, lai, list(validation=list(physical_minimum=0, physical_maximum=20, clamp_tolerance=.01))), "physical tolerance")
  expect_error(normalize_output_values(101, rh, list(validation=list(physical_minimum_percent=0, physical_maximum_percent=100, clamp_tolerance_percent=.5))), "physical tolerance")
})

test_that("transient retry classification excludes permanent metadata failures", {
  expect_true(is_transient_cds_error(simpleError("HTTP 503 service unavailable")))
  expect_true(is_transient_cds_error(simpleError("connection reset by peer")))
  expect_false(is_transient_cds_error(simpleError("HTTP 401 authentication failed")))
  expect_false(is_transient_cds_error(simpleError("unsupported variable")))
})
