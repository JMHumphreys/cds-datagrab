era5land_family_product_ids <- function() .era5land_product_ids()

era5land_expected_dates <- function(config, start_date = NULL, end_date = NULL, dry_run = FALSE) {
  window <- resolve_pipeline_date_window(config, start_date, end_date, dry_run)
  safe_date_sequence(window$effective_start, window$effective_end)
}

era5land_request_for_date <- function(requests, date) {
  date <- as.Date(date)
  if (length(date) != 1L || is.na(date)) stop("Debug date must be a valid scalar date", call. = FALSE)
  matches <- vapply(requests, function(x) date %in% as.Date(x$raw_request_dates), logical(1))
  if (sum(matches) != 1L) stop("Expected exactly one configured family request containing debug date ", format(date), "; found ", sum(matches), call. = FALSE)
  requests[[which(matches)]]
}

era5land_validate_debug_date <- function(date, family_dates) {
  date <- as.Date(date)
  family_dates <- as.Date(family_dates)
  if (length(date) != 1L || is.na(date)) stop("Debug date must be a valid scalar date", call. = FALSE)
  if (!(date %in% family_dates)) stop("Debug date ", format(date), " is outside the configured ERA5-Land request period", call. = FALSE)
  invisible(date)
}

era5land_family_manifest <- function(run_dir, root, source_paths, request, cfg, products, status = "running") {
  m <- list(run_id = basename(run_dir), run_dir = run_dir, source_family_id = "era5land_daily_mean_utc06", profile = cfg$project$profile,
    resolved_output_root = root, output_root_source = source_paths$root_source, source_directory = source_paths$source_root,
    raw_directory = source_paths$raw_dir, requested_variables = request$requested_variables, product_ids = products,
    request_hash = request$request_hash, request_start = request$request_start, request_end = request$request_end,
    daily_statistic = request$daily_statistic, daily_time_zone = request$time_zone, daily_sampling_frequency = request$frequency,
    request_area = request$area, status = status, family_status = status,
    started_at = format(Sys.time(), tz = "UTC", usetz = TRUE), completed_at = NULL,
    product_count = length(products), successful_products = character(), failed_products = character(),
    requested_product_dates = as.vector(outer(products, as.character(request$raw_request_dates), paste, sep = "__")),
    successful_product_dates = character(), failed_product_dates = character(), raw_reused = FALSE,
    archive_reused = FALSE, extraction_reused = FALSE, CDS_contacted = FALSE,
    daily_outputs_written = 0L, daily_outputs_reused = 0L, pre_repair_missing_cells = 0L,
    repaired_cells = 0L, post_repair_missing_cells = 0L, outside_mask_cells = 0L,
    failure_stage = NULL, failure_message = NULL)
  jsonlite::write_json(m, file.path(run_dir, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  m
}

era5land_annotate_product_metadata <- function(directory, spec, request, member = NULL) {
  files <- if (dir.exists(directory)) list.files(directory, pattern = "[.]json$", full.names = TRUE) else character()
  for (f in files) {
    x <- tryCatch(jsonlite::read_json(f, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(x)) next
    x$source_family_id <- request$source_family_id; x$daily_time_zone <- request$time_zone
    x$daily_sampling_frequency <- request$frequency; x$daily_statistic <- request$daily_statistic
    x$metadata_notes <- spec$metadata_notes %||% NULL
    if (!is.null(member)) { x$source_member <- member$member_name; x$source_alias <- member$environmental_variable_alias; x$source_archive_path <- member$archive_path; x$source_map_rows <- 3L }
    jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, null = "null")
  }
  invisible(files)
}

era5land_member_date_map <- function(member, request, member_request) {
  dates <- normalize_date_vector(request$raw_request_dates, "raw_request_dates")
  data.frame(date = as.character(dates), selected_raw_source = member$extracted_path, source_path = member$extracted_path,
    request_hash = request$request_hash, raw_request_start = min(dates), raw_request_end = max(dates), decoded_source_start = min(dates),
    decoded_source_end = max(dates), mapping_reason = "era5land_archive_member", stringsAsFactors = FALSE)
}

era5land_sum_available <- function(x) {
  x <- as.numeric(x)
  if (!length(x) || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

era5land_condition_record <- function(e, stage = "process") {
  call <- tryCatch(conditionCall(e), error = function(err) NULL)
  list(failure_stage = stage, failure_message = conditionMessage(e), condition_class = class(e), condition_call = if (is.null(call)) NULL else paste(deparse(call), collapse = " "),
    traceback = substr(paste(vapply(sys.calls(), function(x) paste(deparse(x), collapse = " "), character(1)), collapse = " <- "), 1L, 8000L))
}

era5land_product_result <- function(product_id, expected_dates, process = NULL, status = "failed", failure = NULL, source_member = NULL, source_alias = NULL) {
  dates <- if (!is.null(process)) process$date_results %||% list() else list()
  if (!length(dates)) dates <- lapply(as.character(expected_dates), function(d) list(date = d, status = if (status == "success") "success" else "failed", output_path = NULL,
    pre_repair_missing_cells = NA_integer_, component_count = NA_integer_, repaired_cells = NA_integer_, post_repair_missing_cells = NA_integer_, outside_mask_cells = NA_integer_,
    failure_stage = if (status == "success") NULL else failure$failure_stage %||% "process", failure_message = if (status == "success") NULL else failure$failure_message %||% "not_available"))
  dates <- unname(dates)
  successful <- vapply(dates, function(x) identical(x$status, "success") || identical(x$status, "reused"), logical(1))
  failed <- !successful
  failure <- failure %||% list()
  list(product_id = product_id, status = status, source_member = source_member, source_alias = source_alias, requested_dates = as.character(expected_dates),
    successful_dates = as.character(vapply(dates[successful], `[[`, character(1), "date")), failed_dates = as.character(vapply(dates[failed], `[[`, character(1), "date")),
    daily_outputs_written = if (is.null(process)) 0L else length(process$written), daily_outputs_reused = if (is.null(process)) 0L else length(process$reused),
    pre_repair_missing_cells = if (is.null(process)) NA_real_ else era5land_sum_available(vapply(dates, function(x) x$pre_repair_missing_cells %||% NA_real_, numeric(1))),
    repaired_cells = if (is.null(process)) NA_real_ else era5land_sum_available(vapply(dates, function(x) x$repaired_cells %||% NA_real_, numeric(1))),
    post_repair_missing_cells = if (is.null(process)) NA_real_ else era5land_sum_available(vapply(dates, function(x) x$post_repair_missing_cells %||% NA_real_, numeric(1))),
    outside_mask_cells = if (is.null(process)) NA_real_ else era5land_sum_available(vapply(dates, function(x) x$outside_mask_cells %||% NA_real_, numeric(1))),
    failure_stage = if (status == "success") NULL else failure$failure_stage %||% "process", failure_message = if (status == "success") NULL else failure$failure_message %||% "not_available",
    condition_class = if (status == "success") NULL else failure$condition_class %||% NULL, condition_call = if (status == "success") NULL else failure$condition_call %||% NULL,
    traceback = if (status == "success") NULL else failure$traceback %||% NULL, date_results = dates)
}

run_era5land_daily_mean_family <- function(config_path = "config/era5land_daily_mean_utc06_smoke.yml", mode = c("plan", "download", "process", "aggregate", "full"), dry_run = TRUE,
                                           start_date = NULL, end_date = NULL, output_root = NULL, product_ids = .era5land_product_ids(), overwrite = FALSE, transfer_fun = NULL) {
  mode <- match.arg(mode); cfg <- read_pipeline_config(config_path); root <- resolve_project_root(dirname(config_path)); cfg <- resolve_config_paths(cfg, root, output_root, FALSE); cfg <- validate_pipeline_config(cfg)
  if (!identical(unname(as.character(cfg$project$source_family_id)), "era5land_daily_mean_utc06")) stop("Configuration is not an ERA5-Land daily-mean source-family configuration", call. = FALSE)
  if (!all(product_ids %in% .era5land_product_ids())) stop("Unknown ERA5-Land product selector", call. = FALSE)
  expected <- era5land_expected_dates(cfg, start_date, end_date, dry_run)
  source_paths <- resolve_source_storage_paths(cfg, root, output_root, create = TRUE)
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", substr(digest::digest(list(cfg, mode, expected, product_ids), algo = "xxhash32"), 1, 8))
  run_dir <- file.path(source_paths$runs_root, run_id); fs::dir_create(run_dir, recurse = TRUE)
  diag <- diagnose_spatial_domain(cfg$spatial$template_path, cfg$spatial$bbox_path, cfg)
  requests <- build_era5land_daily_mean_requests(expected, diag$final_cds_area, cfg, .era5land_product_ids()); req <- if (length(requests)) requests[[1L]] else NULL
  manifest <- if (!is.null(req)) era5land_family_manifest(run_dir, source_paths$root, source_paths, req, cfg, product_ids) else list(run_dir = run_dir)
  fail_family <- function(stage, message) {
    if (!is.null(req)) {
      manifest$status <- "failed"; manifest$family_status <- "failed"; manifest$failure_stage <- stage; manifest$failure_message <- message
      manifest$completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      jsonlite::write_json(manifest, file.path(run_dir, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
    }
    stop(message, call. = FALSE)
  }
  jsonlite::write_json(diag, file.path(run_dir, "spatial_diagnostics.json"), pretty = TRUE, auto_unbox = TRUE)
  write_cds_request_manifests(requests, run_dir)
  if (mode == "plan" || isTRUE(dry_run)) return(list(status = "planned", run_id = run_id, run_dir = run_dir, requests = requests, source_paths = source_paths, spatial_diagnostics = diag, products = product_ids))

  download_result <- if (mode %in% c("download", "full")) tryCatch(download_cds_requests(requests, paths = source_paths, run_dir = run_dir, dry_run = FALSE, overwrite = overwrite, config = cfg, run_id = run_id, transfer_fun = transfer_fun), error = function(e) fail_family("download", conditionMessage(e))) else data.frame()
  if (mode == "download") return(list(status = "downloaded", run_id = run_id, run_dir = run_dir, requests = requests, download = download_result, source_paths = source_paths, products = product_ids))
  raw_path <- if (nrow(download_result) && "final_raw_path" %in% names(download_result)) download_result$final_raw_path[[1L]] else NULL
  if (is.null(raw_path) || !file.exists(raw_path)) {
    info <- find_reusable_raw_artifact(req, source_paths); candidates <- c(info$candidates, info$partials)
    valid <- candidates[vapply(candidates, function(p) isTRUE(validate_downloaded_target(p, req)$valid), logical(1))]
     if (!length(valid)) fail_family("raw_validation", paste0("Shared ERA5-Land raw bundle is missing: ", file.path(source_paths$raw_dir, req$target)))
    finalized <- finalize_raw_artifact(valid[[1L]], req, source_paths, run_dir, info$partials); finalized$raw_reused <- TRUE; raw_path <- finalized$final_raw_path
  }
  inventory <- tryCatch(era5land_extract_archive(raw_path, source_paths$extracted_dir, req$request_hash, req$raw_request_dates, run_dir), error = function(e) fail_family("extraction", conditionMessage(e)))
  source_map <- attr(inventory, "source_map")
  if (is.null(source_map)) source_map <- utils::read.csv(file.path(source_paths$extracted_dir, req$request_hash, "source_map.csv"), stringsAsFactors = FALSE)
  finalization <- if (file.exists(file.path(run_dir, "raw_finalization.json"))) tryCatch(jsonlite::read_json(file.path(run_dir, "raw_finalization.json"), simplifyVector = TRUE), error = function(e) NULL) else NULL
  shared_source_diagnostic <- list(raw_artifact_path = raw_path, original_raw_extension = finalization$original_extension %||% tools::file_ext(raw_path), detected_container = detect_container_type(raw_path),
    extension_mismatch = !isTRUE(finalization$extension_content_match %||% identical(tolower(tools::file_ext(raw_path)), container_extension(detect_container_type(raw_path)))), request_hash = req$request_hash,
    archive_checksum = raw_checksum(raw_path), archive_member_count = nrow(inventory), source_map_rows = nrow(source_map), netcdf_member_count = sum(inventory$container_type %in% c("netcdf_classic", "netcdf4_hdf5")),
    member_inventory = inventory, aliases_found = inventory$environmental_variable_alias, units_found = inventory$source_units,
    dimensions_found = inventory$dimension_names, dates_found = inventory$decoded_dates, raw_reused = nrow(download_result) == 0L || any(download_result$status == "reused_existing"),
    archive_extracted = TRUE, extraction_reused = isTRUE(attr(inventory, "extraction_reused")), cds_contacted = nrow(download_result) > 0L && any(download_result$status %in% c("downloaded", "failed")))
  jsonlite::write_json(shared_source_diagnostic, file.path(run_dir, "source_diagnostic.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")

  results <- list(); failures <- list()
  for (id in product_ids) {
    spec <- get_variable_spec(id); member <- era5land_member_for_product(inventory, id); member_request <- req; member_request$target <- basename(member$extracted_path)
    member_map <- era5land_member_date_map(member, req, member_request)
    pcfg <- cfg; pcfg$project$dataset_id <- id; pcfg$cds$variable <- spec$cds_variable; pcfg$cds$daily_statistic <- spec$daily_statistic
    pcfg$paths <- list(root = source_paths$root); pcfg <- resolve_config_paths(pcfg, root, source_paths$root, FALSE)
    p <- resolve_storage_paths(pcfg, root, source_paths$root, create = TRUE); product_run <- file.path(p$runs_root, run_id); fs::dir_create(product_run, recurse = TRUE)
    lineage <- list(run_id = run_id, product_id = id, source_family_id = req$source_family_id, source_run_directory = run_dir, shared_raw_path = raw_path,
      shared_extracted_directory = dirname(member$extracted_path), source_member = member$member_name, source_alias = member$environmental_variable_alias,
      request_hash = req$request_hash, profile = cfg$project$profile, resolved_output_root = source_paths$root, data_directory = p$dataset_root,
      run_directory = product_run, slurm_log_directory = p$slurm_log_dir, daily_time_zone = req$time_zone, daily_sampling_frequency = req$frequency,
      daily_statistic = req$daily_statistic, weekly_statistic = spec$weekly_statistic,
      status = "running", started_at = format(Sys.time(), tz = "UTC", usetz = TRUE), completed_at = NULL,
      requested_dates = as.character(expected), successful_dates = character(), failed_dates = character(),
      daily_outputs_written = 0L, daily_outputs_reused = 0L, daily_outputs_replaced = 0L, pre_repair_missing_cells = 0L,
      repaired_cells = 0L, post_repair_missing_cells = 0L, outside_mask_cells = 0L,
      failure_stage = NULL, failure_message = NULL)
    jsonlite::write_json(lineage, file.path(product_run, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
    finalize_product_manifest <- function(x) {
      x$completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      jsonlite::write_json(x, file.path(product_run, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
      invisible(x)
    }
    pr <- NULL; product_result <- NULL
    tryCatch({
      pr <- process_downloaded_variable(member$extracted_path, p$daily_dir, cfg$spatial$template_path, cfg$spatial$bbox_path, pcfg, spec,
        overwrite_dates = if (overwrite) expected else NULL, expected_dates = expected, run_expected_dates = expected, request_manifest = list(member_request), date_source_map = member_map, run_dir = product_run)
      lineage$daily_outputs_written <- length(pr$written); lineage$daily_outputs_reused <- length(pr$reused); lineage$daily_outputs_replaced <- length(pr$replaced)
      lineage$pre_repair_missing_cells <- pr$coverage_summary$pre_repair_missing_cells %||% 0L
      lineage$repaired_cells <- pr$coverage_summary$repaired_cells %||% 0L
      lineage$post_repair_missing_cells <- pr$coverage_summary$post_repair_missing_cells %||% 0L
      lineage$outside_mask_cells <- pr$coverage_summary$outside_mask_cells %||% 0L
      failed_dates <- expected[vapply(expected, function(d) any(grepl(format(as.Date(d), "%Y-%m-%d"), pr$failed, fixed = TRUE)), logical(1))]
      if (length(pr$processing_failures) && any(vapply(pr$processing_failures, function(x) is.na(x$date) || !nzchar(x$date), logical(1)))) failed_dates <- expected
      lineage$failed_dates <- as.character(failed_dates); lineage$successful_dates <- as.character(setdiff(expected, failed_dates))
      if (length(pr$failed) || length(pr$processing_failures) || length(lineage$successful_dates) != length(expected)) {
        if (!length(lineage$failed_dates)) lineage$failed_dates <- as.character(expected)
        stop("Product/date processing incomplete for ", id, call. = FALSE)
      }
      era5land_annotate_product_metadata(p$daily_dir, spec, req, member); product_result <- era5land_product_result(id, expected, pr, "success", source_member = member$member_name, source_alias = member$environmental_variable_alias); wr <- c(product_result, list(request_hash = req$request_hash, process = pr))
      if (mode %in% c("aggregate", "full")) { inv <- inventory_daily_products(p$daily_dir, spec$daily_filename_prefix, cfg$spatial$template_path, TRUE, pcfg); wr$weekly <- aggregate_daily_to_weekly(p$daily_dir, p$weekly_dir, spec$weekly_filename_prefix, template_path = cfg$spatial$template_path, inventory = inv, variable_spec = spec, config = pcfg); era5land_annotate_product_metadata(p$weekly_dir, spec, req, member) }
      lineage <- modifyList(lineage, product_result); lineage$status <- "success"; lineage$process <- pr; results[[id]] <<- wr; finalize_product_manifest(lineage)
    }, error = function(e) {
      original <- if (!is.null(pr) && length(pr$processing_failures)) pr$processing_failures[[1L]] else era5land_condition_record(e, "process")
      if (!is.null(pr) && length(pr$processing_failures)) { original$failure_stage <- original$stage %||% original$processing_step %||% "process"; original$failure_message <- original$condition_message %||% original$error_message; original$condition_class <- original$condition_class %||% original$error_class; original$condition_call <- original$condition_call %||% NULL; original$traceback <- original$traceback %||% NULL }
      product_result <- era5land_product_result(id, expected, pr, "failed", original, source_member = member$member_name, source_alias = member$environmental_variable_alias)
      lineage <- modifyList(lineage, product_result); lineage$status <- "failed"; lineage$process <- pr; results[[id]] <<- c(product_result, list(request_hash = req$request_hash, process = pr)); failures[[id]] <<- product_result
      finalize_product_manifest(lineage)
    })
  }
  status <- if (length(failures)) if (length(results)) "partial_failure" else "failed" else "success"
  manifest$status <- status; manifest$family_status <- status; manifest$completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  if (length(failures)) { first_failure <- failures[[1L]]; manifest$failure_stage <- first_failure$failure_stage %||% "product_processing"; manifest$failure_message <- first_failure$failure_message %||% "not_available"; manifest$condition_class <- first_failure$condition_class %||% NULL; manifest$condition_call <- first_failure$condition_call %||% NULL; manifest$traceback <- first_failure$traceback %||% NULL }
  manifest$product_results <- unname(results); manifest$failures <- unname(failures)
  manifest$successful_products <- names(results)[vapply(results, function(x) identical(x$status, "success"), logical(1))]; manifest$failed_products <- names(results)[vapply(results, function(x) identical(x$status, "failed"), logical(1))]
  manifest$successful_product_dates <- unlist(lapply(results[manifest$successful_products], function(x) as.vector(outer(x$product_id, x$successful_dates %||% character(), paste, sep = "__"))), use.names = FALSE)
  manifest$failed_product_dates <- unlist(lapply(results[manifest$failed_products], function(x) as.vector(outer(x$product_id, x$failed_dates %||% character(), paste, sep = "__"))), use.names = FALSE)
  manifest$raw_reused <- isTRUE(shared_source_diagnostic$raw_reused); manifest$archive_reused <- isTRUE(shared_source_diagnostic$raw_reused)
  manifest$extraction_reused <- isTRUE(shared_source_diagnostic$extraction_reused); manifest$CDS_contacted <- isTRUE(shared_source_diagnostic$cds_contacted); manifest$cds_contacted <- manifest$CDS_contacted
  manifest$daily_outputs_written <- era5land_sum_available(vapply(results, function(x) x$daily_outputs_written %||% NA_real_, numeric(1)))
  manifest$daily_outputs_reused <- era5land_sum_available(vapply(results, function(x) x$daily_outputs_reused %||% NA_real_, numeric(1)))
  manifest$daily_outputs_replaced <- era5land_sum_available(vapply(results, function(x) if (!is.null(x$process)) length(x$process$replaced) else NA_real_, numeric(1)))
  manifest$pre_repair_missing_cells <- era5land_sum_available(vapply(results, function(x) x$pre_repair_missing_cells %||% NA_real_, numeric(1)))
  manifest$repaired_cells <- era5land_sum_available(vapply(results, function(x) x$repaired_cells %||% NA_real_, numeric(1)))
  manifest$post_repair_missing_cells <- era5land_sum_available(vapply(results, function(x) x$post_repair_missing_cells %||% NA_real_, numeric(1)))
  manifest$outside_mask_cells <- era5land_sum_available(vapply(results, function(x) x$outside_mask_cells %||% NA_real_, numeric(1)))
  jsonlite::write_json(manifest, file.path(run_dir, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  list(status = status, family_status = status, run_id = run_id, run_dir = run_dir, requests = requests, download = download_result, products = results, failures = failures, source_paths = source_paths, manifest = manifest, source_diagnostic = shared_source_diagnostic)
}
