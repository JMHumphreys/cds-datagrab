test_that("matching weekly sidecar is reused", {
  skip_if_not_installed("terra")
  base <- tempfile("weekly-reuse-"); dir.create(base)
  on.exit(unlink(base, recursive=TRUE), add=TRUE)
  template <- terra::rast(nrows=1,ncols=1,xmin=0,xmax=1,ymin=0,ymax=1,crs="EPSG:4326"); terra::values(template)<-1
  template_path <- file.path(base,"template.tif"); terra::writeRaster(template,template_path,overwrite=TRUE)
  dates <- as.Date("2026-07-06") + 0:6
  paths <- vapply(seq_along(dates), function(i) { p<-file.path(base,format_daily_filename("soilmoist",dates[i])); r<-template; terra::values(r)<-i/10; terra::writeRaster(r,p,overwrite=TRUE); p }, character(1))
  inv <- data.frame(path=paths,date=dates,estimated=FALSE,observed_valid=TRUE)
  first <- aggregate_daily_to_weekly(base,base,"soilmoist",inventory=inv,template_path=template_path)
  second <- aggregate_daily_to_weekly(base,base,"soilmoist",inventory=inv,template_path=template_path)
  expect_length(first$written,1)
  expect_length(second$written,0)
  expect_length(second$reused,1)
  expect_true(file.exists(paste0(first$written[[1]],".json")))
})
