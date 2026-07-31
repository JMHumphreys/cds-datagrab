test_that("template coverage is cellwise and reports coordinates", {
  skip_if_not_installed("terra")
  t <- terra::rast(nrows=2,ncols=2); terra::values(t) <- c(1,1,NA,1)
  o <- t; terra::values(o) <- c(2,NA,NA,2)
  x <- validate_template_coverage(o,t,FALSE)
  expect_equal(x$template_non_na,3); expect_equal(x$missing_inside_count,1); expect_equal(x$outside_mask_count,0); expect_false(x$complete); expect_length(x$missing_cell_indices,1)
  expect_error(validate_template_coverage(o,t,TRUE),"missing_inside_count=1")
  expect_silent(validate_template_coverage(t,t,TRUE))
})

test_that("coverage diagnostics stay under the run diagnostics directory", {
  skip_if_not_installed("terra"); t <- terra::rast(nrows=2,ncols=2); terra::values(t) <- 1; o <- t; terra::values(o)[1] <- NA
  base <- file.path(getwd(),".test-coverage-run"); if(dir.exists(base)) unlink(base,recursive=TRUE); on.exit(unlink(base,recursive=TRUE),add=TRUE); d <- file.path(base,"run","diagnostics","coverage"); x <- validate_template_coverage(o,t,FALSE,d,"2026-07-01")
  expect_true(all(file.exists(x$coverage_diagnostic_paths))); expect_false(any(grepl("daily",x$coverage_diagnostic_paths)))
})

test_that("isolated bilinear NA is repaired with nearest-neighbor provenance", {
  skip_if_not_installed("terra")
  make <- function() terra::rast(nrows=50,ncols=50,xmin=0,xmax=50,ymin=0,ymax=50,crs="EPSG:4326")
  source <- make(); template <- make(); bilinear <- make(); nearest <- make(); terra::values(source) <- seq_len(2500); terra::values(template) <- seq_len(2500); terra::values(bilinear) <- seq_len(2500); terra::values(nearest) <- seq_len(2500); terra::values(bilinear)[1] <- NA
  x <- analyze_template_coverage(bilinear,nearest,source,template)
  expect_true(x$repair_applied); expect_equal(x$repair_count,1); expect_equal(x$repaired_cell_ids,1); expect_equal(x$repair_fraction,1/2500)
  expect_equal(x$details[[1]]$classification,"bilinear_interpolation_artifact"); expect_equal(x$details[[1]]$nearest_value,1); expect_equal(x$source_na_count,0)
  expect_equal(terra::values(x$raster,mat=FALSE)[1],1)
})

test_that("coverage classification rejects nodata and outside support", {
  skip_if_not_installed("terra")
  source <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326"); terra::values(source) <- 1
  template <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326"); terra::values(template) <- 1
  bilinear <- template; nearest <- template; terra::values(bilinear)[1] <- NA; terra::values(nearest)[1] <- NA
  nodata <- analyze_template_coverage(bilinear,nearest,source,template)
  expect_equal(nodata$details[[1]]$classification,"source_nodata"); expect_false(nodata$repair_applied)
  outside_template <- terra::rast(nrows=2,ncols=2,xmin=2,xmax=4,ymin=0,ymax=2,crs="EPSG:4326"); terra::values(outside_template) <- 1
  outside_bilinear <- outside_template; outside_nearest <- outside_template; terra::values(outside_bilinear)[1] <- NA; terra::values(outside_nearest)[1] <- NA
  outside <- analyze_template_coverage(outside_bilinear,outside_nearest,source,outside_template)
  expect_equal(outside$details[[1]]$classification,"outside_source_support"); expect_false(outside$repair_applied)
})

test_that("coverage repair limits and contiguous gaps remain hard failures", {
  skip_if_not_installed("terra")
  make <- function() terra::rast(nrows=10,ncols=10,xmin=0,xmax=10,ymin=0,ymax=10,crs="EPSG:4326")
  source <- make(); template <- make(); bilinear <- make(); nearest <- make(); terra::values(source) <- 1; terra::values(template) <- 1; terra::values(bilinear) <- 1; terra::values(nearest) <- 1
  terra::values(bilinear)[1:5] <- NA
  x <- analyze_template_coverage(bilinear,nearest,source,template)
  expect_false(x$repair_applied); expect_equal(x$unresolved_count,5); expect_false(x$eligible)
  single <- make(); single_b <- make(); single_n <- make(); terra::values(single) <- 1; terra::values(single_b) <- 1; terra::values(single_n) <- 1; terra::values(single_b)[1] <- NA
  y <- analyze_template_coverage(single_b,single_n,source,single,maximum_repair_count=0L)
  expect_false(y$repair_applied); expect_false(y$eligible)
  z <- analyze_template_coverage(single_b,single_n,source,single,maximum_repair_fraction=0.000001)
  expect_false(z$repair_applied); expect_false(z$eligible)
})

test_that("complete bilinear coverage does not invoke repair", {
  skip_if_not_installed("terra")
  make <- function() terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326")
  source <- make(); bilinear <- make(); nearest <- make(); template <- make(); terra::values(source) <- 7; terra::values(bilinear) <- 7; terra::values(nearest) <- 7; terra::values(template) <- 7
  x <- analyze_template_coverage(bilinear,nearest,source,template)
  expect_false(x$repair_applied); expect_equal(x$repair_count,0); expect_equal(x$unresolved_count,0); expect_equal(x$details,list())
  expect_equal(terra::values(x$raster,mat=FALSE),rep(7,4))
})

test_that("isolated land-mask mismatch uses local final-grid IDW", {
  skip_if_not_installed("terra")
  source <- terra::rast(nrows=3,ncols=3,xmin=0,xmax=3,ymin=0,ymax=3,crs="EPSG:4326"); terra::values(source) <- NA_real_
  make <- function() terra::rast(nrows=50,ncols=50,xmin=0,xmax=3,ymin=0,ymax=3,crs="EPSG:4326")
  template <- make(); bilinear <- make(); nearest <- make(); terra::values(template) <- 1; terra::values(bilinear) <- 50; terra::values(nearest) <- 50
  target <- 1250L; terra::values(bilinear)[target] <- NA; terra::values(nearest)[target] <- NA
  result <- analyze_template_coverage(bilinear,nearest,source,template)
  expect_equal(result$diagnostics$details[[1]]$classification,"isolated_land_mask_mismatch"); expect_true(result$repaired); expect_equal(result$diagnostics$repair_method,"local_final_grid_idw"); expect_true(is.finite(terra::values(result$raster,mat=FALSE)[target])); expect_equal(result$diagnostics$repair_count,1)
})
