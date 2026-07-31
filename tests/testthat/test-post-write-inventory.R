test_that("post-write AgERA5 inventory uses percent range and excludes nodata", {
  skip_if_not_installed("terra")
  base <- test_external_root("post-write-rh"); daily <- file.path(base,"daily"); dir.create(daily,recursive=TRUE)
  template <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326"); terra::values(template) <- c(1,1,NA,1); tp <- file.path(base,"template.tif"); terra::writeRaster(template,tp,overwrite=TRUE)
  output <- template; terra::values(output) <- c(75,86,NA,6); path <- file.path(daily,"relhum_min_2025-06-01.tif"); terra::writeRaster(output,path,overwrite=TRUE,wopt=list(NAflag=-9999))
  cfg <- list(project=list(dataset_id="agera5_relhum_min"),validation=list(),spatial=list(require_complete_template_coverage=TRUE))
  inv <- inventory_daily_products(daily,"relhum_min",tp,TRUE,cfg); expect_true(inv$value_range_valid); expect_true(inv$template_coverage_complete); expect_true(inv$observed_valid); expect_true(inv$valid); expect_equal(inv$validation_message,"ok")
  check <- validate_variable_value_range(path,get_variable_spec("agera5_relhum_min")); expect_equal(check$finite_non_nodata_cells,3); expect_equal(check$na_nodata_cells,1); expect_equal(check$observed_minimum,6); expect_equal(check$observed_maximum,86); expect_equal(check$below_count,0); expect_equal(check$above_count,0)
})

test_that("shared range validation distinguishes percent and fraction units", {
  rh <- get_variable_spec("agera5_relhum_min"); fraction <- rh; fraction$hard_valid_range <- c(0,1)
  expect_true(validate_variable_value_range(c(75,86),rh)$valid); expect_false(validate_variable_value_range(c(75,86),fraction)$valid); expect_equal(validate_variable_value_range(c(.75,.86),fraction)$valid,TRUE)
  expect_false(validate_variable_value_range(c(-.1,50),rh)$valid); expect_false(validate_variable_value_range(c(50,100.1),rh)$valid); expect_false(validate_variable_value_range(c(NA_real_,NA_real_),rh)$valid); expect_false(validate_variable_value_range(c(Inf,50),rh)$valid)
})
