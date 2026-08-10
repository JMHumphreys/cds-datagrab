local_registry_paths <- function(label) {
  root <- tempfile(paste0("era5land-registry-", label, "-"), tmpdir = package_root())
  source_root <- file.path(root, "data", "smoke", "_sources", "era5land_daily_mean_utc06")
  paths <- list(root = root, source_root = source_root, requests_dir = file.path(source_root, "requests"), request_registry = file.path(source_root, "requests", "request_registry.csv"), raw_dir = file.path(source_root, "raw"), raw_quarantine_dir = file.path(source_root, "quarantine", "raw"), extracted_dir = file.path(source_root, "extracted"), root_marker = file.path(root, ".cds-datagrab-root"))
  dir.create(paths$raw_dir, recursive = TRUE); dir.create(paths$requests_dir, recursive = TRUE); writeLines("marker", paths$root_marker)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE), envir = testthat::teardown_env())
  paths
}

registry_request_fixture <- function() {
  cfg <- read_pipeline_config(package_file("config", "era5land_daily_mean_utc06_smoke.yml"))
  build_era5land_daily_mean_requests(as.Date("2026-02-01") + 0:2, c(43, -127, 42, -125), cfg)[[1L]]
}

make_registry_zip <- function(path, members = paste0("member", seq_len(8), ".nc")) {
  source_dir <- tempfile("registry-zip-members-", tmpdir = package_root()); dir.create(source_dir)
  withr::defer(unlink(source_dir, recursive = TRUE, force = TRUE), envir = testthat::teardown_env())
  files <- file.path(source_dir, members); invisible(lapply(files, function(file) writeLines("fixture", file)))
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(source_dir); utils::zip(path, members, flags = "-q")
  path
}

test_that("valid local archive is retrieved state and never staged", {
  paths <- local_registry_paths("local"); request <- registry_request_fixture(); archive <- file.path(paths$raw_dir, sub("[.]nc$", ".zip", request$target)); make_registry_zip(archive)
  cfg <- read_pipeline_config(package_file("config", "era5land_daily_mean_utc06_smoke.yml")); registry <- era5land_registry_reconcile(list(request), era5land_empty_registry(), paths, cfg, persist = TRUE)
  calls <- 0L
  staged <- era5land_stage_requests(list(request), registry, paths, cfg, stage_fun = function(...) { calls <<- calls + 1L; list(job_url = "https://cds/jobs/duplicate") })
  expect_equal(calls, 0L)
  expect_identical(staged$registry$request_status[[1L]], "retrieved")
  expect_equal(era5land_request_inventory(list(request), staged$registry, paths)$new_cds_requests_required, 0L)
})

test_that("staging persists a job immediately and restart does not duplicate it", {
  paths <- local_registry_paths("stage"); request <- registry_request_fixture(); cfg <- read_pipeline_config(package_file("config", "era5land_daily_mean_utc06_smoke.yml")); calls <- 0L
  stage <- function(...) { calls <<- calls + 1L; list(job_id = "job-123", job_url = "https://cds/jobs/job-123") }
  first <- era5land_stage_requests(list(request), era5land_empty_registry(), paths, cfg, stage_fun = stage)
  expect_equal(calls, 1L); expect_identical(first$registry$request_status[[1L]], "submitted"); expect_identical(first$registry$cds_job_id[[1L]], "job-123"); expect_true(file.exists(paths$request_registry))
  second <- era5land_stage_requests(list(request), era5land_read_request_registry(paths), paths, cfg, stage_fun = stage)
  expect_equal(calls, 1L); expect_identical(second$registry$cds_job_url[[1L]], "https://cds/jobs/job-123")
})

test_that("pending retrieval is represented as processing and completed retrieval finalizes atomically", {
  paths <- local_registry_paths("retrieve"); request <- registry_request_fixture(); cfg <- read_pipeline_config(package_file("config", "era5land_daily_mean_utc06_smoke.yml")); stage <- era5land_stage_requests(list(request), era5land_empty_registry(), paths, cfg, stage_fun = function(...) list(job_url = "https://cds/jobs/job-456"))
  pending <- era5land_retrieve_requests(list(request), stage$registry, paths, cfg, transfer_fun = function(...) stop("request is still processing"))
  expect_equal(pending$processing, 1L); expect_identical(pending$registry$request_status[[1L]], "processing")
  completed <- era5land_retrieve_requests(list(request), pending$registry, paths, cfg, transfer_fun = function(url, target) { make_registry_zip(target); target })
  expect_equal(completed$retrieved, 1L); expect_identical(completed$registry$request_status[[1L]], "retrieved"); expect_true(file.exists(completed$registry$local_raw_path[[1L]])); expect_true(grepl("[.]zip$", completed$registry$local_raw_path[[1L]]))
})

test_that("expired jobs retain provenance and partial archives remain retryable", {
  paths <- local_registry_paths("failure"); request <- registry_request_fixture(); cfg <- read_pipeline_config(package_file("config", "era5land_daily_mean_utc06_smoke.yml")); stage <- era5land_stage_requests(list(request), era5land_empty_registry(), paths, cfg, stage_fun = function(...) list(job_id = "job-expired", job_url = "https://cds/jobs/job-expired"))
  expired <- era5land_retrieve_requests(list(request), stage$registry, paths, cfg, transfer_fun = function(...) stop("404 request expired"))
  expect_identical(expired$registry$request_status[[1L]], "expired"); expect_identical(expired$registry$cds_job_id[[1L]], "job-expired")
  stage2 <- stage$registry; stage2$request_status <- "submitted"
  partial <- era5land_retrieve_requests(list(request), stage2, paths, cfg, transfer_fun = function(url, target) { make_registry_zip(target, "only.nc"); target })
  expect_equal(partial$processing, 1L); expect_false(identical(partial$registry$request_status[[1L]], "retrieved")); expect_true(is.na(partial$registry$local_raw_path[[1L]]) || !nzchar(partial$registry$local_raw_path[[1L]]))
})

test_that("current production-shaped inventory recognizes thirteen cached months", {
  paths <- local_registry_paths("production"); cfg <- read_pipeline_config(package_file("config", "era5land_daily_mean_utc06_production.yml")); dates <- era5land_expected_dates(cfg, dry_run = FALSE); requests <- build_era5land_daily_mean_requests(dates, c(43, -127, 42, -125), cfg)
  expect_equal(length(requests), 55L)
  for (request in requests[seq_len(13L)]) make_registry_zip(file.path(paths$raw_dir, sub("[.]nc$", ".zip", request$target)))
  registry <- era5land_registry_reconcile(requests, era5land_empty_registry(), paths, cfg, persist = FALSE); inventory <- era5land_request_inventory(requests, registry, paths)
  expect_equal(inventory$source_requests_planned, 55L); expect_equal(inventory$raw_archives_valid, 13L); expect_equal(inventory$new_cds_requests_required, 42L); expect_equal(inventory$registered_pending_cds_jobs, 0L)
})
