test_that("installed debug runner completes one cached ERA5-Land date", {
  skip_if_not_installed("terra")
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("sf")
  skip_if_not_installed("yaml")
  installed_roots <- .libPaths()[file.exists(file.path(.libPaths(), "cdsdatagrab", "DESCRIPTION"))]
  skip_if(!length(installed_roots), "requires an installed cdsdatagrab package library")

  base <- file.path(getwd(), ".test-era5land-debug-date-results")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)
  template <- terra::rast(nrows = 2, ncols = 2, xmin = -126.75, xmax = -125.25, ymin = 42.25, ymax = 43.25, crs = "EPSG:4326")
  terra::values(template) <- 1
  template_path <- file.path(base, "template.tif")
  terra::writeRaster(template, template_path, overwrite = TRUE)
  polygon <- sf::st_polygon(list(matrix(c(-126.5, 42.5, -125.5, 42.5, -125.5, 43, -126.5, 43, -126.5, 42.5), ncol = 2, byrow = TRUE)))
  bbox_path <- file.path(base, "bbox.gpkg")
  sf::st_write(sf::st_sf(id = 1L, geometry = sf::st_sfc(polygon, crs = 4326)), bbox_path, quiet = TRUE, delete_dsn = TRUE)
  support_mask <- template
  terra::values(support_mask)[1L] <- NA_real_
  support_path <- file.path(base, "support.tif")
  terra::writeRaster(support_mask, support_path, overwrite = TRUE)
  xy <- terra::xyFromCell(template, 1L)
  audit_path <- file.path(base, "audit.csv")
  utils::write.csv(data.frame(cell = 1L, longitude = xy[1], latitude = xy[2], reason = "test structural exclusion", affected_products = "all", nearest_finite_source_distance_km = 0, source_request_hash = "fixture", representative_date = "2026-02-01", support_decision_method = "test fixture", stringsAsFactors = FALSE), audit_path, row.names = FALSE)

  config_path <- file.path(base, "debug.yml")
  relative <- function(path) sub(paste0("^", gsub("\\\\", "/", package_root()), "/?"), "", gsub("\\\\", "/", path), ignore.case = TRUE)
  cfg <- list(
    project = list(dataset_id = "era5land_tmean", source_family_id = "era5land_daily_mean_utc06", profile = "smoke", output_prefix = "era5land"),
    spatial = list(template_path = relative(template_path), bbox_path = relative(bbox_path), request_extent_source = "template_bbox_union", api_bbox_buffer_degrees = 0, source_grid_degrees = 0.5, align_request_to_source_grid = TRUE, mask_to_template = TRUE, mask_to_boundary = FALSE, require_complete_template_coverage = TRUE, write_coverage_diagnostics = FALSE),
    paths = list(root = NULL),
    coverage = list(support_mask = relative(support_path), unsupported_cells_audit = relative(audit_path), local_target_radius_cells = 2L, source_buffer_max_km = 35),
    temporal = list(initial_start_date = "2026-02-01", observed_end = "2026-02-03", configured_start_date = "2026-02-01", configured_end_date = "2026-02-03", source_lag_days = 0L, overlap_days = 0L, future_end_date = "2026-02-03"),
    cds = list(dataset_short_name = "derived-era5-land-daily-statistics", variable = "2m_temperature", daily_statistic = "daily_mean", time_zone = "utc-06:00", frequency = "1_hourly", data_format = "netcdf", download_format = "unarchived", product_type = "reanalysis"),
    processing = list(source_units = "K", output_units = "degrees C", unit_conversion = "kelvin_to_celsius", resampling_method = "bilinear", overwrite_observed_overlap = TRUE, datatype = "FLT4S", naflag = -9999),
    weekly = list(aggregation = "mean", require_complete_week = TRUE, expected_days = 7L),
    future = list(daily_enabled = FALSE, weekly_enabled = FALSE),
    validation = list(minimum = -100, maximum = 70, physical_minimum = -100, physical_maximum = 70, require_exact_template_geometry = TRUE, hard_tolerance = 1e-6, coverage_max_repair_count = 256L, coverage_max_repair_fraction = 0.01, coverage_max_component_size = 4L, coverage_max_donor_radius_cells = 2L, coverage_max_donor_radius_km = 75, coverage_max_source_buffer_km = 35, coverage_donor_count = 8L)
  )
  yaml::write_yaml(cfg, config_path)

  configured_test_root <- Sys.getenv("CDS_DATAGRAB_TEST_EXTERNAL_ROOT", "")
  output_root <- if (nzchar(configured_test_root)) file.path(configured_test_root, paste0("debug-output-", as.integer(Sys.getpid()))) else file.path(tempdir(), paste0("cdsdatagrab-debug-output-", as.integer(Sys.getpid())))
  unlink(output_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(output_root, recursive = TRUE, force = TRUE), add = TRUE)
  output_root_available <- tryCatch({ dir.create(output_root, recursive = TRUE, showWarnings = FALSE); fs::dir_create(file.path(output_root, "probe"), recurse = TRUE); TRUE }, error = function(e) FALSE)
  skip_if(!output_root_available, "external output roots are not writable in this test environment")
  parsed <- read_pipeline_config(config_path)
  prepared <- validate_pipeline_config(resolve_config_paths(parsed, package_root(), output_root, FALSE))
  domain <- diagnose_spatial_domain(prepared$spatial$template_path, prepared$spatial$bbox_path, prepared)
  request <- build_era5land_daily_mean_requests(era5land_expected_dates(prepared, dry_run = FALSE), domain$final_cds_area, prepared, era5land_family_product_ids())[[1L]]
  archive <- file.path(base, "cached-month.zip")
  source(package_file("tests", "fixtures", "make_era5land_zip_fixture.R"))
  make_era5land_zip_fixture(archive)
  source_paths <- resolve_source_storage_paths(prepared, package_root(), output_root, create = TRUE)
  era5land_extract_archive(archive, source_paths$extracted_dir, request$request_hash, as.Date(request$raw_request_dates))

  script <- package_file("scripts", "debug_era5land_slice.R")
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  output <- system2(rscript, c(script, "--config", config_path, "--product", "era5land_tmean", "--date", "2026-02-01", "--output-root", output_root), env = c(paste0("R_LIBS=", installed_roots[[1L]]), paste0("R_LIBS_USER=", installed_roots[[1L]])), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(any(grepl("slice status=success", output, fixed = TRUE)), paste(output, collapse = "\n"))
  expect_false(any(grepl("date_results.*not found|object 'date_results'", output)), paste(output, collapse = "\n"))
  expect_true(file.exists(file.path(output_root, "data", "smoke", "era5land_tmean", "daily", "era5land_tmean_2026-02-01.tif")))
})
