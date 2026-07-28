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
