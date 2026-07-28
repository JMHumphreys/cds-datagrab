test_that("all-NA rasters fail validation", { r<-terra::rast(nrows=2,ncols=2); terra::values(r)<-NA; expect_false(validate_raster_against_template(r,r)$valid) })
