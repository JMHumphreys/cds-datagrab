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

test_that("same-cell missing nearest uses an original local donor", {
  skip_if_not_installed("terra")
  template <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 5, crs = "EPSG:4326")
  terra::values(template) <- 1
  bilinear <- template; nearest <- template; source <- template
  terra::values(bilinear) <- NA; terra::values(nearest) <- NA; terra::values(source) <- 1; terra::values(template) <- NA
  donor <- terra::cellFromRowCol(template, 2, 2); target <- terra::cellFromRowCol(template, 3, 3)
  terra::values(template)[target] <- 1; terra::values(bilinear)[donor] <- 42
  result <- analyze_template_coverage(bilinear, nearest, source, template, maximum_repair_count = 4L, maximum_repair_fraction = 1, maximum_donor_radius_cells = 2L)
  expect_true(result$repair_applied)
  expect_equal(terra::values(result$raster, mat = FALSE)[target], 42)
  expect_equal(result$details[[1]]$gap_type, "bilinear_missing_nearest_same_cell_missing_local_donor")
  expect_equal(result$details[[1]]$maximum_donor_distance_cells, 1)
})

test_that("local donor search expands to radius two but not beyond", {
  skip_if_not_installed("terra")
  make_case <- function(donor_row, donor_col) {
    template <- terra::rast(nrows = 7, ncols = 7, xmin = 0, xmax = 7, ymin = 0, ymax = 7, crs = "EPSG:4326")
    terra::values(template) <- 1
    bilinear <- template; nearest <- template; source <- template
    terra::values(bilinear) <- NA; terra::values(nearest) <- NA; terra::values(source) <- 1; terra::values(template) <- NA
    target <- terra::cellFromRowCol(template, 4, 4); donor <- terra::cellFromRowCol(template, donor_row, donor_col)
    terra::values(template)[target] <- 1; terra::values(bilinear)[donor] <- 7
    list(template = template, bilinear = bilinear, nearest = nearest, source = source, target = target)
  }
  radius_two <- make_case(2, 2)
  repaired <- analyze_template_coverage(radius_two$bilinear, radius_two$nearest, radius_two$source, radius_two$template, maximum_repair_count = 4L, maximum_repair_fraction = 1, maximum_donor_radius_cells = 2L)
  expect_true(repaired$repair_applied)
  expect_equal(repaired$details[[1]]$maximum_donor_distance_cells, 2)
  radius_three <- make_case(1, 1)
  rejected <- analyze_template_coverage(radius_three$bilinear, radius_three$nearest, radius_three$source, radius_three$template, maximum_repair_count = 4L, maximum_repair_fraction = 1, maximum_donor_radius_cells = 2L)
  expect_false(rejected$repair_applied)
  expect_equal(rejected$component_records[[1]]$repair_failure_reason, "no_valid_donor_within_radius")
  expect_equal(rejected$unresolved_count, 1)
})

test_that("component repair is atomic and never self-propagates", {
  skip_if_not_installed("terra")
  template <- terra::rast(nrows = 7, ncols = 7, xmin = 0, xmax = 7, ymin = 0, ymax = 7, crs = "EPSG:4326")
  terra::values(template) <- 1
  bilinear <- template; nearest <- template; source <- template
  terra::values(bilinear) <- NA; terra::values(nearest) <- NA; terra::values(source) <- 1; terra::values(template) <- NA
  targets <- terra::cellFromRowCol(template, 4:5, 4)
  terra::values(template)[targets] <- 1; donor <- terra::cellFromRowCol(template, 2, 2); terra::values(bilinear)[donor] <- 9
  result <- analyze_template_coverage(bilinear, nearest, source, template, maximum_repair_count = 4L, maximum_repair_fraction = 1, maximum_donor_radius_cells = 2L)
  expect_false(result$repair_applied)
  expect_equal(result$repair_count, 0)
  expect_equal(result$unresolved_count, 2)
  expect_true(result$component_records[[1]]$repair_attempted)
  expect_match(result$component_records[[1]]$repair_failure_reason, "no_valid_donor")
  expect_equal(sum(as.numeric(result$diagnostics$repaired_cell_ids %||% integer()) > 0), 0)
  expect_equal(result$repair_count, result$missing_inside_count - result$unresolved_count)
})

test_that("failed product results retain date diagnostics and original error fields", {
  process <- list(written = character(), reused = character(), date_results = list(
    list(date = "2026-02-01", status = "failed", output_path = "out.tif", pre_repair_missing_cells = 148L, component_count = 124L, repaired_cells = 0L, post_repair_missing_cells = 148L, outside_mask_cells = 0L, failure_stage = "coverage_repair", failure_message = "no valid donor within radius")),
    processing_failures = list(list(stage = "coverage_repair", condition_class = "simpleError", condition_message = "no valid donor within radius", condition_call = "stop(...)", traceback = "trace")))
  failure <- list(failure_stage = "coverage_repair", failure_message = "no valid donor within radius", condition_class = "simpleError", condition_call = "stop(...)", traceback = "trace")
  result <- cdsdatagrab:::era5land_product_result("era5land_tmean", as.Date("2026-02-01"), process, "failed", failure, "t2m.nc", "t2m")
  expect_equal(result$failed_dates, "2026-02-01")
  expect_equal(result$pre_repair_missing_cells, 148)
  expect_equal(result$post_repair_missing_cells, 148)
  expect_equal(result$failure_stage, "coverage_repair")
  expect_equal(result$failure_message, "no valid donor within radius")
  expect_equal(result$condition_call, "stop(...)")
  expect_equal(result$date_results[[1]]$failure_message, "no valid donor within radius")
})
