test_that("incomplete named daily rasters are not observed", {
  skip_if_not_installed("terra"); td <- tempfile(); dir.create(td); t <- terra::rast(nrows=2,ncols=2); terra::values(t) <- 1; f <- file.path(td,"template.tif"); terra::writeRaster(t,f,overwrite=TRUE); o <- t; terra::values(o)[1] <- NA; terra::writeRaster(o,file.path(td,"mintemp_2026-07-01.tif"),overwrite=TRUE)
  i <- inventory_daily_products(td,"mintemp",f,TRUE,list(validation=list(minimum_celsius=-100,maximum_celsius=70)))
  row <- i[i$filename=="mintemp_2026-07-01.tif",]; expect_false(row$observed_valid); p <- plan_observed_update(i,"2026-07-01","2026-07-03",0); expect_equal(p$reason[1],"invalid_existing_output"); expect_equal(as.character(p$date),as.character(as.Date(c("2026-07-01","2026-07-02","2026-07-03"))))
})
