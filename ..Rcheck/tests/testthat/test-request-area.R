test_that("request extents combine, buffer, and align outward", {
  t <- c(north=40.5, west=-120, south=0, east=-40)
  b <- c(north=40, west=-125, south=-1, east=-35)
  u <- combine_request_extents(t,b,"template_bbox_union")
  expect_equal(as.numeric(u), c(40.5,-125,-1,-35))
  expect_equal(attr(u,"boundary_controllers"), c(north="template",west="bbox",south="bbox",east="bbox"))
  z <- buffer_geographic_extent(u,1)
  expect_equal(unname(z),c(41.5,-126,-2,-34))
  a <- align_extent_to_source_grid(z,.25)
  expect_equal(unname(a),c(41.5,-126,-2,-34))
  expect_equal(names(a),c("north","west","south","east"))
})

test_that("template footprint is based on non-NA cells", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows=4,ncols=4,xmin=0,xmax=4,ymin=0,ymax=4,crs="EPSG:3857")
  terra::values(r) <- c(NA,NA,NA,NA,NA,1,1,NA,NA,1,1,NA,NA,NA,NA,NA)
  f <- tempfile(fileext=".tif"); terra::writeRaster(r,f,overwrite=TRUE)
  x <- template_mask_footprint_lonlat(f)
  expect_equal(x$template_non_na_count,4)
  expect_true(grepl("4326|WGS 84|World Geodetic",terra::crs(x$geometry),ignore.case=TRUE))
})

test_that("request hash changes with buffer and area", {
  cfg <- list(cds=list(dataset_short_name="dataset",variable="2m_temperature",daily_statistic="daily_minimum",frequency="6_hourly"))
  a <- c(40,-125,0,-35)
  expect_false(identical(request_definition_hash(as.Date("2026-07-01"),a,cfg,1,TRUE),request_definition_hash(as.Date("2026-07-01"),a,cfg,2,TRUE)))
  expect_false(identical(request_definition_hash(as.Date("2026-07-01"),a,cfg,1,TRUE),request_definition_hash(as.Date("2026-07-01"),a+c(1,0,0,0),cfg,1,TRUE)))
})
