test_that("coverage diagnostics exclude computational SpatRaster objects", {
  skip_if_not_installed("terra")
  make <- function() terra::rast(nrows=50,ncols=50,xmin=0,xmax=50,ymin=0,ymax=50,crs="EPSG:4326")
  source <- make(); template <- make(); bilinear <- make(); nearest <- make()
  terra::values(source) <- 1; terra::values(template) <- 1; terra::values(bilinear) <- 1; terra::values(nearest) <- 1; terra::values(bilinear)[1] <- NA
  result <- analyze_template_coverage(bilinear,nearest,source,template)
  expect_s4_class(result$raster,"SpatRaster"); expect_true(result$repaired); expect_equal(result$diagnostics$repair_count,1)
  expect_length(find_nonserializable_objects(result$diagnostics),0); expect_true(assert_json_serializable(result$diagnostics,"coverage diagnostics"))
  expect_error(assert_json_serializable(list(coverage_diagnostics=list(repaired_raster=result$raster)),"failure metadata"),"repaired_raster.*SpatRaster")
})

test_that("failure metadata preserves conditions and serializes diagnostics only", {
  skip_if_not_installed("terra")
  condition <- simpleError("later daily validation failed")
  diagnostics <- list(repair_applied=TRUE,repair_count=1L,repaired_cell_ids=1L)
  expect_equal(conditionMessage(condition),"later daily validation failed")
  expect_true(assert_json_serializable(list(coverage_diagnostics=diagnostics,error_class=class(condition),error_message=conditionMessage(condition)),"failure details"))
  expect_error(assert_json_serializable(list(coverage_diagnostics=list(raster=terra::rast(nrows=1,ncols=1))),"failure details"),"raster.*SpatRaster")
})
