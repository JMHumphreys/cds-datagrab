test_that("bounded coverage repair fills connected projection gaps through size four", {
  skip_if_not_installed("terra")
  template <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:4326")
  terra::values(template) <- 1
  bilinear <- template
  nearest <- template
  source <- template
  terra::values(bilinear) <- 10
  terra::values(nearest) <- 20
  terra::values(source) <- 30
  missing_cells <- c(
    terra::cellFromRowCol(template, 2, 2),
    terra::cellFromRowCol(template, 2, 6), terra::cellFromRowCol(template, 2, 7),
    terra::cellFromRowCol(template, 5, 2), terra::cellFromRowCol(template, 5, 3), terra::cellFromRowCol(template, 6, 2),
    terra::cellFromRowCol(template, 8, 7), terra::cellFromRowCol(template, 8, 8), terra::cellFromRowCol(template, 9, 7), terra::cellFromRowCol(template, 9, 8)
  )
  terra::values(bilinear)[missing_cells] <- NA
  result <- analyze_template_coverage(bilinear, nearest, source, template, maximum_repair_count = 20L, maximum_repair_fraction = 1, maximum_component_size = 4L)
  expect_true(result$repair_applied)
  expect_equal(result$repair_count, 10)
  expect_equal(result$unresolved_count, 0)
  expect_equal(sort(result$diagnostics$component_sizes), c(1L, 2L, 3L, 4L))
  expect_equal(result$diagnostics$projection_created_nodata_count, 10)
  expect_equal(result$diagnostics$missing_inside_pre_repair_count, 10)
  expect_equal(result$diagnostics$missing_inside_post_repair_count, 0)
  expect_equal(terra::values(result$raster, mat = FALSE)[missing_cells], rep(20, 10))
})

test_that("coverage repair leaves a component larger than the configured bound unresolved", {
  skip_if_not_installed("terra")
  template <- terra::rast(nrows = 6, ncols = 6, xmin = 0, xmax = 6, ymin = 0, ymax = 6, crs = "EPSG:4326")
  terra::values(template) <- 1
  bilinear <- template; nearest <- template; source <- template
  terra::values(bilinear) <- 10; terra::values(nearest) <- 20; terra::values(source) <- 30
  cells <- terra::cellFromRowCol(template, 2, 2:6)
  terra::values(bilinear)[cells] <- NA
  result <- analyze_template_coverage(bilinear, nearest, source, template, maximum_repair_count = 100L, maximum_repair_fraction = 1, maximum_component_size = 4L)
  expect_false(result$repair_applied)
  expect_equal(result$repair_count, 0)
  expect_equal(result$unresolved_count, 5)
  expect_equal(result$diagnostics$component_sizes, 5L)
  expect_equal(result$diagnostics$unrepairable_count, 5)
})

test_that("coverage repair diagnostics include pre/post rasters", {
  skip_if_not_installed("terra")
  diagnostic_dir <- file.path(getwd(), ".test-era5land-coverage-diagnostics")
  if (dir.exists(diagnostic_dir)) unlink(diagnostic_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(diagnostic_dir, recursive = TRUE, force = TRUE), add = TRUE)
  template <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3, crs = "EPSG:4326")
  terra::values(template) <- 1
  bilinear <- template; nearest <- template; source <- template
  terra::values(bilinear) <- 10; terra::values(nearest) <- 20; terra::values(source) <- 30
  terra::values(bilinear)[5] <- NA
  result <- analyze_template_coverage(bilinear, nearest, source, template, maximum_repair_count = 4L, maximum_repair_fraction = 1, diagnostics_dir = diagnostic_dir, date = "2026-02-01", prefix = "era5land")
  expect_true(all(file.exists(result$coverage_diagnostic_paths)))
  expect_true(all(c("missing_inside_pre_repair", "repaired_cells", "missing_inside_post_repair", "outside_mask") %in% names(result$coverage_diagnostic_paths)))
})
