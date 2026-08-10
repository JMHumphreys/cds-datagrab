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

era5land_family_manifest <- function(run_dir, root, source_paths, request, cfg, products, status = "running", execution_mode = NULL, workflow_mode = NULL, overwrite = FALSE, rebuild_all_weeks = FALSE) {
  m <- list(run_id = basename(run_dir), run_dir = run_dir, source_family_id = "era5land_daily_mean_utc06", profile = cfg$project$profile,
    resolved_output_root = root, output_root_source = source_paths$root_source, source_directory = source_paths$source_root,
    raw_directory = source_paths$raw_dir, requested_variables = request$requested_variables, product_ids = products,
    request_hash = request$request_hash, request_start = request$request_start, request_end = request$request_end,
    daily_statistic = request$daily_statistic, daily_time_zone = request$time_zone, daily_sampling_frequency = request$frequency,
    request_area = request$area, execution_mode = execution_mode %||% "execute", workflow_mode = workflow_mode %||% "full", overwrite = isTRUE(overwrite), rebuild_all_weeks = isTRUE(rebuild_all_weeks), status = status, family_status = status,
    started_at = format(Sys.time(), tz = "UTC", usetz = TRUE), completed_at = NULL,
    product_count = length(products), successful_products = character(), failed_products = character(),
    requested_product_dates = as.vector(outer(products, as.character(request$raw_request_dates), paste, sep = "__")),
    successful_product_dates = character(), failed_product_dates = character(), raw_reused = FALSE,
    archive_reused = FALSE, extraction_reused = FALSE, CDS_contacted = FALSE,
    daily_outputs_written = 0L, daily_outputs_reused = 0L, pre_repair_missing_cells = 0L,
    repaired_cells = 0L, post_repair_missing_cells = 0L, outside_mask_cells = 0L,
    failure_stage = NULL, failure_message = NULL)
  if (exists("era5land_support_provenance", mode = "function")) m <- modifyList(m, era5land_support_provenance(cfg))
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

era5land_first_available <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else x[[1L]]
}

era5land_condition_record <- function(e, stage = "process") {
  call <- tryCatch(conditionCall(e), error = function(err) NULL)
  list(failure_stage = stage, failure_message = conditionMessage(e), condition_class = class(e), condition_call = if (is.null(call)) NULL else paste(deparse(call), collapse = " "),
    traceback = substr(paste(vapply(sys.calls(), function(x) paste(deparse(x), collapse = " "), character(1)), collapse = " <- "), 1L, 8000L))
}

era5land_product_result <- function(product_id, expected_dates, process = NULL, status = "failed", failure = NULL, source_member = NULL, source_alias = NULL) {
  dates <- if (!is.null(process)) process$date_results %||% list() else list()
  if (!length(dates)) dates <- lapply(as.character(expected_dates), function(d) list(date = d, status = if (status == "success") "success" else "failed", output_path = NULL,
    pre_repair_missing_cells = NA_integer_, pre_repair_missing_supported_cells = NA_integer_, structurally_unsupported_cells = NA_integer_, component_count = NA_integer_, repaired_cells = NA_integer_, post_repair_missing_cells = NA_integer_, post_repair_unexpected_missing_cells = NA_integer_, outside_mask_cells = NA_integer_, outside_support_finite_cells = NA_integer_,
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
    master_template_cells = if (is.null(process)) NA_real_ else process$coverage_summary$master_template_cells %||% NA_real_,
    era5land_supported_cells = if (is.null(process)) NA_real_ else process$coverage_summary$era5land_supported_cells %||% NA_real_,
    structurally_unsupported_cells = if (is.null(process)) NA_real_ else process$coverage_summary$structurally_unsupported_cells %||% NA_real_,
    pre_repair_missing_supported_cells = if (is.null(process)) NA_real_ else process$coverage_summary$pre_repair_missing_supported_cells %||% NA_real_,
    repaired_supported_cells = if (is.null(process)) NA_real_ else process$coverage_summary$repaired_supported_cells %||% NA_real_,
    post_repair_unexpected_missing_cells = if (is.null(process)) NA_real_ else process$coverage_summary$post_repair_unexpected_missing_cells %||% NA_real_,
    outside_support_finite_cells = if (is.null(process)) NA_real_ else process$coverage_summary$outside_support_finite_cells %||% NA_real_,
    failure_stage = if (status == "success") NULL else failure$failure_stage %||% "process", failure_message = if (status == "success") NULL else failure$failure_message %||% "not_available",
    condition_class = if (status == "success") NULL else failure$condition_class %||% NULL, condition_call = if (status == "success") NULL else failure$condition_call %||% NULL,
    traceback = if (status == "success") NULL else failure$traceback %||% NULL, date_results = dates)
}

era5land_collect_product_execution <- function(results, failures, product_id, product_execution) {
  collection_error <- tryCatch({
    results[[product_id]] <- product_execution$result
    if (!is.null(product_execution$failure)) failures[[product_id]] <- product_execution$failure
    NULL
  }, error = function(e) e)
  if (is.null(collection_error)) return(list(results = results, failures = failures, collection_error = NULL))

  internal <- era5land_condition_record(collection_error, "result_collection")
  if (is.null(product_execution$failure)) {
    collection_failure <- product_execution$result
    if (is.null(collection_failure) || !is.list(collection_failure)) collection_failure <- list(product_id = product_id, status = "failed")
    collection_failure$status <- "failed"
    collection_failure$failure_stage <- internal$failure_stage
    collection_failure$failure_message <- internal$failure_message
    collection_failure$internal_error <- internal
  } else {
    collection_failure <- product_execution$failure
    collection_failure$internal_error <- internal
  }
  assignment_error <- tryCatch({
    failures[[product_id]] <- collection_failure
    NULL
  }, error = function(e) e)
  if (!is.null(assignment_error)) {
    internal$secondary_error <- era5land_condition_record(assignment_error, "result_collection")
  }
  list(results = results, failures = failures, collection_error = internal)
}

era5land_family_status <- function(results, failures, collection_errors = list()) {
  successful <- vapply(results, function(x) identical(x$status, "success"), logical(1))
  if (!length(failures) && !length(collection_errors)) "success" else if (any(successful)) "partial_failure" else "failed"
}

era5land_safe_failure_result <- function(product_id, expected_dates, process, original, source_member = NULL, source_alias = NULL) {
  tryCatch(
    era5land_product_result(product_id, expected_dates, process, "failed", original, source_member, source_alias),
    error = function(e) {
      internal <- era5land_condition_record(e, "result_collection")
      list(
        product_id = product_id,
        status = "failed",
        source_member = source_member,
        source_alias = source_alias,
        requested_dates = as.character(expected_dates),
        successful_dates = character(),
        failed_dates = as.character(expected_dates),
        failure_stage = original$failure_stage %||% "process",
        failure_message = original$failure_message %||% "not_available",
        original_condition = original,
        internal_error = internal
      )
    }
  )
}

run_era5land_daily_mean_family <- function(config_path = "config/era5land_daily_mean_utc06_smoke.yml", mode = c("plan", "download", "stage-requests", "retrieve-requests", "process", "aggregate", "execute", "full"), dry_run = TRUE,
                                           start_date = NULL, end_date = NULL, output_root = NULL, product_ids = .era5land_product_ids(), overwrite = FALSE, rebuild_all_weeks = FALSE, transfer_fun = NULL, stage_fun = NULL, status_fun = NULL, request_override = NULL, internal_call = FALSE) {
  mode <- match.arg(mode); cfg <- read_pipeline_config(config_path); attr(cfg, "config_path") <- normalizePath(config_path, winslash = "/", mustWork = FALSE); root <- resolve_project_root(dirname(config_path)); attr(cfg, "project_root") <- root; cfg <- resolve_config_paths(cfg, root, output_root, FALSE); cfg <- validate_pipeline_config(cfg); attr(cfg, "config_path") <- normalizePath(config_path, winslash = "/", mustWork = FALSE); attr(cfg, "project_root") <- root
  if (!identical(unname(as.character(cfg$project$source_family_id)), "era5land_daily_mean_utc06")) stop("Configuration is not an ERA5-Land daily-mean source-family configuration", call. = FALSE)
  if (!all(product_ids %in% .era5land_product_ids())) stop("Unknown ERA5-Land product selector", call. = FALSE)
  expected <- if (!is.null(request_override)) as.Date(request_override$raw_request_dates) else era5land_expected_dates(cfg, start_date, end_date, dry_run)
  source_paths <- resolve_source_storage_paths(cfg, root, output_root, create = TRUE)
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", substr(digest::digest(list(cfg, mode, expected, product_ids), algo = "xxhash32"), 1, 8))
  run_dir <- file.path(source_paths$runs_root, run_id); fs::dir_create(run_dir, recurse = TRUE)
  diag <- diagnose_spatial_domain(cfg$spatial$template_path, cfg$spatial$bbox_path, cfg)
  requests <- if (!is.null(request_override)) list(request_override) else build_era5land_daily_mean_requests(expected, diag$final_cds_area, cfg, .era5land_product_ids()); req <- if (length(requests)) requests[[1L]] else NULL
  manifest <- if (!is.null(req)) era5land_family_manifest(run_dir, source_paths$root, source_paths, req, cfg, product_ids, execution_mode = if (isTRUE(dry_run)) "dry-run" else "execute", workflow_mode = mode, overwrite = overwrite, rebuild_all_weeks = rebuild_all_weeks) else list(run_dir = run_dir)
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
  registry <- era5land_registry_reconcile(requests, era5land_read_request_registry(source_paths), source_paths, cfg, persist = FALSE)
  registry_inventory <- era5land_request_inventory(requests, registry, source_paths)
  if (mode == "plan" || isTRUE(dry_run)) return(list(status = "planned", run_id = run_id, run_dir = run_dir, requests = requests, source_paths = source_paths, spatial_diagnostics = diag, products = product_ids, request_registry = registry, request_inventory = registry_inventory))

  if (mode == "stage-requests") {
    staged <- tryCatch(era5land_stage_requests(requests, registry, source_paths, cfg, stage_fun = stage_fun), error = function(e) fail_family("stage_requests", conditionMessage(e)))
    inventory <- era5land_request_inventory(requests, staged$registry, source_paths)
    status <- if (length(staged$failures)) "stage_failed" else "staged"
    return(list(status = status, run_id = run_id, run_dir = run_dir, requests = requests, source_paths = source_paths, request_registry = staged$registry, request_inventory = inventory, staged = staged))
  }

  if (mode == "retrieve-requests") {
    era5land_write_request_registry(registry, source_paths)
    retrieved <- tryCatch(era5land_retrieve_requests(requests, registry, source_paths, cfg, transfer_fun = transfer_fun, status_fun = status_fun), error = function(e) fail_family("retrieve_requests", conditionMessage(e)))
    inventory <- era5land_request_inventory(requests, retrieved$registry, source_paths)
    status <- if (retrieved$failed > 0L) "retrieval_failed" else if (retrieved$processing > 0L || inventory$registered_pending_cds_jobs > 0L) "retrieval_pending" else "success"
    return(list(status = status, run_id = run_id, run_dir = run_dir, requests = requests, source_paths = source_paths, request_registry = retrieved$registry, request_inventory = inventory, retrieval = retrieved))
  }

  if (mode %in% c("execute", "full") && !internal_call) {
    staged <- tryCatch(era5land_stage_requests(requests, registry, source_paths, cfg, stage_fun = stage_fun), error = function(e) fail_family("stage_requests", conditionMessage(e)))
    retrieved <- tryCatch(era5land_retrieve_requests(requests, staged$registry, source_paths, cfg, transfer_fun = transfer_fun), error = function(e) fail_family("retrieve_requests", conditionMessage(e)))
    inventory <- era5land_request_inventory(requests, retrieved$registry, source_paths)
    if (length(staged$failures) || retrieved$failed > 0L) return(list(status = "execute_failed", run_id = run_id, run_dir = run_dir, requests = requests, source_paths = source_paths, request_registry = retrieved$registry, request_inventory = inventory, staged = staged, retrieval = retrieved))
    if (inventory$new_cds_requests_required > 0L || inventory$registered_pending_cds_jobs > 0L) return(list(status = "execute_pending", run_id = run_id, run_dir = run_dir, requests = requests, source_paths = source_paths, request_registry = retrieved$registry, request_inventory = inventory, staged = staged, retrieval = retrieved))
    return(run_era5land_daily_mean_family(config_path = config_path, mode = "process", dry_run = FALSE, start_date = start_date, end_date = end_date, output_root = output_root, product_ids = product_ids, overwrite = overwrite, rebuild_all_weeks = rebuild_all_weeks, transfer_fun = transfer_fun, stage_fun = stage_fun, status_fun = status_fun, internal_call = TRUE))
  }

  if (mode %in% c("process", "aggregate") && !internal_call && length(requests) > 1L) {
    results <- lapply(requests, function(request) run_era5land_daily_mean_family(config_path = config_path, mode = mode, dry_run = FALSE, output_root = output_root, product_ids = product_ids, overwrite = overwrite, rebuild_all_weeks = rebuild_all_weeks, transfer_fun = transfer_fun, stage_fun = stage_fun, status_fun = status_fun, request_override = request, internal_call = TRUE))
    statuses <- vapply(results, function(x) as.character(x$status %||% "failed"), character(1))
    status <- if (all(statuses == "success")) "success" else if (all(statuses %in% c("success", "planned"))) "partial_success" else "failed"
    return(list(status = status, family_status = status, run_id = run_id, run_dir = run_dir, requests = requests, request_registry = registry, request_inventory = registry_inventory, request_results = results, source_paths = source_paths, products = product_ids))
  }

  download_result <- if (mode %in% c("download", "full")) tryCatch(download_cds_requests(requests, paths = source_paths, run_dir = run_dir, dry_run = FALSE, overwrite = overwrite, config = cfg, run_id = run_id, transfer_fun = transfer_fun), error = function(e) fail_family("download", conditionMessage(e))) else data.frame()
  if (mode == "download") return(list(status = "downloaded", run_id = run_id, run_dir = run_dir, requests = requests, download = download_result, source_paths = source_paths, products = product_ids))
  raw_path <- if (nrow(download_result) && "final_raw_path" %in% names(download_result)) download_result$final_raw_path[[1L]] else NULL
  if (is.null(raw_path) || !isTRUE(era5land_validate_raw_archive(raw_path, req)$valid)) {
    raw_path <- era5land_local_archive(req, source_paths)
    if (is.null(raw_path)) fail_family("raw_validation", paste0("Shared ERA5-Land raw bundle is missing or invalid: ", file.path(source_paths$raw_dir, req$target)))
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

  results <- list(); failures <- list(); collection_errors <- list()
  safe_finalize_product_manifest <- function(x) {
    tryCatch({
      finalize_product_manifest(x)
      list(ok = TRUE, error = NULL)
    }, error = function(e) {
      list(ok = FALSE, error = e)
    })
  }
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
    if (exists("era5land_support_provenance", mode = "function")) lineage <- modifyList(lineage, era5land_support_provenance(cfg))
    jsonlite::write_json(lineage, file.path(product_run, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
    finalize_product_manifest <- function(x) {
      x$completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      jsonlite::write_json(x, file.path(product_run, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
      invisible(x)
    }
    pr <- NULL; product_result <- NULL
    product_execution <- tryCatch({
      pr <- process_downloaded_variable(member$extracted_path, p$daily_dir, cfg$spatial$template_path, cfg$spatial$bbox_path, pcfg, spec,
        overwrite_dates = if (overwrite) expected else NULL, expected_dates = expected, run_expected_dates = expected, request_manifest = list(member_request), date_source_map = member_map, run_dir = product_run)
      lineage$daily_outputs_written <- length(pr$written); lineage$daily_outputs_reused <- length(pr$reused); lineage$daily_outputs_replaced <- length(pr$replaced)
      lineage$master_template_cells <- pr$coverage_summary$master_template_cells %||% 0L
      lineage$era5land_supported_cells <- pr$coverage_summary$era5land_supported_cells %||% 0L
      lineage$structurally_unsupported_cells <- pr$coverage_summary$structurally_unsupported_cells %||% 0L
      lineage$pre_repair_missing_supported_cells <- pr$coverage_summary$pre_repair_missing_supported_cells %||% 0L
      lineage$repaired_supported_cells <- pr$coverage_summary$repaired_supported_cells %||% 0L
      lineage$post_repair_unexpected_missing_cells <- pr$coverage_summary$post_repair_unexpected_missing_cells %||% 0L
      lineage$outside_support_finite_cells <- pr$coverage_summary$outside_support_finite_cells %||% 0L
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
      if (mode %in% c("aggregate", "full")) { inv <- inventory_daily_products(p$daily_dir, spec$daily_filename_prefix, cfg$spatial$template_path, TRUE, pcfg); wr$weekly <- aggregate_daily_to_weekly(p$daily_dir, p$weekly_dir, spec$weekly_filename_prefix, template_path = cfg$spatial$template_path, rebuild_all = rebuild_all_weeks, inventory = inv, variable_spec = spec, config = pcfg); era5land_annotate_product_metadata(p$weekly_dir, spec, req, member) }
      lineage$coverage_metrics_source <- if (length(pr$written) || length(pr$replaced)) "recomputed" else if (length(pr$reused)) "existing_output_metadata" else "not_recomputed"
      wr$coverage_metrics_source <- lineage$coverage_metrics_source
      lineage <- modifyList(lineage, product_result); lineage$status <- "success"; lineage$process <- pr
      finalized <- safe_finalize_product_manifest(lineage)
      if (!isTRUE(finalized$ok)) {
        internal <- era5land_condition_record(finalized$error, "manifest_finalization")
        product_result$status <- "failed"; product_result$failure_stage <- internal$failure_stage; product_result$failure_message <- internal$failure_message; product_result$internal_error <- internal
        lineage$status <- "failed"; lineage$failure_stage <- internal$failure_stage; lineage$failure_message <- internal$failure_message
        list(result = c(product_result, list(request_hash = req$request_hash, process = pr, coverage_metrics_source = lineage$coverage_metrics_source)), failure = product_result, lineage = lineage)
      } else {
        list(result = wr, failure = NULL, lineage = lineage)
      }
    }, error = function(e) {
      original <- if (!is.null(pr) && length(pr$processing_failures)) pr$processing_failures[[1L]] else era5land_condition_record(e, "process")
      if (!is.null(pr) && length(pr$processing_failures)) { original$failure_stage <- original$stage %||% original$processing_step %||% "process"; original$failure_message <- original$condition_message %||% original$error_message; original$condition_class <- original$condition_class %||% original$error_class; original$condition_call <- original$condition_call %||% NULL; original$traceback <- original$traceback %||% NULL }
      product_result <- era5land_safe_failure_result(id, expected, pr, original, source_member = member$member_name, source_alias = member$environmental_variable_alias)
      lineage$coverage_metrics_source <- "not_recomputed"
      product_result$coverage_metrics_source <- lineage$coverage_metrics_source
      lineage <- modifyList(lineage, product_result); lineage$status <- "failed"; lineage$process <- pr
      finalized <- safe_finalize_product_manifest(lineage)
      if (!isTRUE(finalized$ok)) {
        internal <- era5land_condition_record(finalized$error, "manifest_finalization")
        product_result$manifest_finalization_error <- internal
        lineage$manifest_finalization_error <- internal
      }
      list(result = c(product_result, list(request_hash = req$request_hash, process = pr, coverage_metrics_source = lineage$coverage_metrics_source)), failure = product_result, lineage = lineage, original = original)
    })
    collected <- era5land_collect_product_execution(results, failures, id, product_execution)
    results <- collected$results; failures <- collected$failures
    if (!is.null(collected$collection_error)) collection_errors[[id]] <- collected$collection_error
  }
  status <- era5land_family_status(results, failures, collection_errors)
  manifest$status <- status; manifest$family_status <- status; manifest$completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  manifest$collection_errors <- unname(collection_errors)
  failure_records <- c(failures, collection_errors)
  if (length(failure_records)) { first_failure <- failure_records[[1L]]; manifest$failure_stage <- first_failure$failure_stage %||% "product_processing"; manifest$failure_message <- first_failure$failure_message %||% "not_available"; manifest$condition_class <- first_failure$condition_class %||% NULL; manifest$condition_call <- first_failure$condition_call %||% NULL; manifest$traceback <- first_failure$traceback %||% NULL }
  manifest$product_results <- unname(results); manifest$failures <- unname(failures)
  manifest$successful_products <- names(results)[vapply(results, function(x) identical(x$status, "success"), logical(1))]; manifest$failed_products <- names(results)[vapply(results, function(x) identical(x$status, "failed"), logical(1))]
  manifest$successful_product_dates <- unlist(lapply(results[manifest$successful_products], function(x) as.vector(outer(x$product_id, x$successful_dates %||% character(), paste, sep = "__"))), use.names = FALSE)
  manifest$failed_product_dates <- unlist(lapply(results[manifest$failed_products], function(x) as.vector(outer(x$product_id, x$failed_dates %||% character(), paste, sep = "__"))), use.names = FALSE)
  manifest$coverage_metrics_source <- unique(vapply(results, function(x) x$coverage_metrics_source %||% "not_recomputed", character(1)))
  manifest$raw_reused <- isTRUE(shared_source_diagnostic$raw_reused); manifest$archive_reused <- isTRUE(shared_source_diagnostic$raw_reused)
  manifest$extraction_reused <- isTRUE(shared_source_diagnostic$extraction_reused); manifest$CDS_contacted <- isTRUE(shared_source_diagnostic$cds_contacted); manifest$cds_contacted <- manifest$CDS_contacted
  manifest$daily_outputs_written <- era5land_sum_available(vapply(results, function(x) x$daily_outputs_written %||% NA_real_, numeric(1)))
  manifest$daily_outputs_reused <- era5land_sum_available(vapply(results, function(x) x$daily_outputs_reused %||% NA_real_, numeric(1)))
  manifest$daily_outputs_replaced <- era5land_sum_available(vapply(results, function(x) if (!is.null(x$process)) length(x$process$replaced) else NA_real_, numeric(1)))
  manifest$master_template_cells <- era5land_first_available(vapply(results, function(x) x$master_template_cells %||% NA_real_, numeric(1)))
  manifest$era5land_supported_cells <- era5land_first_available(vapply(results, function(x) x$era5land_supported_cells %||% NA_real_, numeric(1)))
  manifest$structurally_unsupported_cells <- era5land_first_available(vapply(results, function(x) x$structurally_unsupported_cells %||% NA_real_, numeric(1)))
  manifest$pre_repair_missing_supported_cells <- era5land_sum_available(vapply(results, function(x) x$pre_repair_missing_supported_cells %||% NA_real_, numeric(1)))
  manifest$repaired_supported_cells <- era5land_sum_available(vapply(results, function(x) x$repaired_supported_cells %||% NA_real_, numeric(1)))
  manifest$post_repair_unexpected_missing_cells <- era5land_sum_available(vapply(results, function(x) x$post_repair_unexpected_missing_cells %||% NA_real_, numeric(1)))
  manifest$outside_support_finite_cells <- era5land_sum_available(vapply(results, function(x) x$outside_support_finite_cells %||% NA_real_, numeric(1)))
  manifest$pre_repair_missing_cells <- era5land_sum_available(vapply(results, function(x) x$pre_repair_missing_cells %||% NA_real_, numeric(1)))
  manifest$repaired_cells <- era5land_sum_available(vapply(results, function(x) x$repaired_cells %||% NA_real_, numeric(1)))
  manifest$post_repair_missing_cells <- era5land_sum_available(vapply(results, function(x) x$post_repair_missing_cells %||% NA_real_, numeric(1)))
  manifest$outside_mask_cells <- era5land_sum_available(vapply(results, function(x) x$outside_mask_cells %||% NA_real_, numeric(1)))
  jsonlite::write_json(manifest, file.path(run_dir, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  list(status = status, family_status = status, run_id = run_id, run_dir = run_dir, requests = requests, download = download_result, products = results, failures = failures, source_paths = source_paths, manifest = manifest, source_diagnostic = shared_source_diagnostic)
}
