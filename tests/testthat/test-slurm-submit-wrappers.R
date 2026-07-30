test_that("new submit wrappers use the shared sbatch path and isolated log patterns", {
  root <- package_root()
  generic <- paste(readLines(package_file("hpc", "submit_era5_variable.sh"), warn = FALSE), collapse = "\n")
  expect_match(generic, "sbatch --parsable")
  expect_match(generic, "DIRECT_EXECUTION")
  for (item in list(
    list(file = "submit_era5_lai_low.sh", job = "cds_lai_low", ext = "lai_low"),
    list(file = "submit_agera5_relhum_min.sh", job = "cds_relhum_min", ext = "relhum_min")
  )) {
    script <- paste(readLines(package_file("hpc", item$file), warn = FALSE), collapse = "\n")
    expect_match(script, "submit_era5_variable.sh")
    expect_match(script, "logs/slurm/\\$PROFILE")
    expect_match(script, item$job)
    expect_match(script, paste0("cds_", item$ext, "_%j\\.out"))
    expect_match(script, paste0("cds_", item$ext, "_%j\\.err"))
    expect_true(grepl("mkdir -p", script, fixed = TRUE))
  }
  expect_true(dir.exists(root))
})

test_that("AgERA5 request validation is shared by plan and execution", {
  cfg <- yaml::read_yaml(package_file("config", "agera5_relhum_min_smoke.yml"))
  req <- build_variable_requests(as.Date(c("2025-06-01", "2025-06-02", "2025-06-03")), c(42.8, -126, -1.1, -34), cfg)[[1]]
  expect_equal(req$dataset_short_name, "sis-agrometeorological-indicators")
  expect_equal(req$variable, "2m_relative_humidity_derived")
  expect_equal(req$daily_statistic, "24_hour_minimum")
  expect_equal(req$statistic, list("24_hour_minimum"))
  expect_equal(req$version, "2_0")
  expect_silent(validate_cds_request_structure(req))
  expect_error(validate_cds_request_structure(within(req, statistic <- list("unsupported"))), "Invalid AgERA5 request schema")
  expect_equal(names(build_cds_api_payload(req)), c("variable", "statistic", "year", "month", "day", "version", "area"))
})
