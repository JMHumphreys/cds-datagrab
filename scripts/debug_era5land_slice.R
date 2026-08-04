#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
value <- function(flag, default = NULL) { i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[[i + 1L]] }
config_path <- value("--config", "config/era5land_daily_mean_utc06_smoke.yml")
product_id <- value("--product")
date_text <- value("--date")
output_root <- value("--output-root")
if (is.null(product_id) || is.null(date_text)) stop("--product and --date are required", call. = FALSE)
date <- as.Date(date_text)
if (is.na(date)) stop("--date must be YYYY-MM-DD", call. = FALSE)

library(cdsdatagrab)
ids <- era5land_family_product_ids()
if (!product_id %in% ids) stop("Unknown ERA5-Land product: ", product_id, call. = FALSE)
cfg <- read_pipeline_config(config_path)
root <- resolve_project_root(dirname(config_path))
cfg <- resolve_config_paths(cfg, root, output_root, FALSE)
cfg <- validate_pipeline_config(cfg)
if (!identical(as.character(cfg$project$source_family_id), "era5land_daily_mean_utc06")) stop("Configuration is not an ERA5-Land family configuration", call. = FALSE)
source_paths <- resolve_source_storage_paths(cfg, root, output_root, create = FALSE)
diag <- diagnose_spatial_domain(cfg$spatial$template_path, cfg$spatial$bbox_path, cfg)
request <- build_era5land_daily_mean_requests(date, diag$final_cds_area, cfg, ids)[[1L]]
extracted_dir <- file.path(source_paths$extracted_dir, request$request_hash)
inventory_path <- file.path(extracted_dir, "member_inventory.csv")
if (!file.exists(inventory_path)) stop("No cached extraction for request hash ", request$request_hash, " at ", extracted_dir, call. = FALSE)
inventory <- utils::read.csv(inventory_path, stringsAsFactors = FALSE)
member <- cdsdatagrab:::era5land_member_for_product(inventory, product_id)
spec <- get_variable_spec(product_id)
member_request <- request; member_request$target <- basename(member$extracted_path)
member_map <- cdsdatagrab:::era5land_member_date_map(member, request, member_request)
pcfg <- cfg; pcfg$project$dataset_id <- product_id; pcfg$cds$variable <- spec$cds_variable; pcfg$cds$daily_statistic <- spec$daily_statistic; pcfg$paths <- list(root = source_paths$root)
pcfg <- resolve_config_paths(pcfg, root, source_paths$root, FALSE)
p <- resolve_storage_paths(pcfg, root, source_paths$root, create = TRUE)
run_id <- paste0("debug_era5land_slice_", product_id, "_", format(date, "%Y-%m-%d"), "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
run_dir <- file.path(p$runs_root, run_id); fs::dir_create(run_dir, recurse = TRUE)
cat(sprintf("debug slice: product=%s date=%s source_member=%s request_hash=%s CDS_contacted=false\n", product_id, date, member$member_name, request$request_hash))
result <- process_downloaded_variable(member$extracted_path, p$daily_dir, cfg$spatial$template_path, cfg$spatial$bbox_path, pcfg, spec,
  overwrite_dates = date, expected_dates = date, run_expected_dates = date, request_manifest = list(member_request), date_source_map = member_map, run_dir = run_dir)
if (length(result$coverage_diagnostics)) {
  for (record in result$coverage_diagnostics) {
    cat(sprintf("coverage: date=%s pre=%s components=%s repaired=%s post=%s outside=%s\n", record$date %||% date, record$missing_inside_pre_repair_count %||% NA, length(record$component_records %||% list()), record$repair_count %||% NA, record$missing_inside_post_repair_count %||% NA, record$outside_mask_count %||% NA))
    if (length(record$component_records)) print(utils::head(utils::read.csv(record$coverage_diagnostic_paths[["component_csv"]]), 20L))
  }
}
if (length(result$processing_failures)) {
  failure <- result$processing_failures[[1L]]
  cat(sprintf("slice failed: product=%s date=%s stage=%s class=%s message=%s\n", product_id, failure$date %||% date, failure$stage %||% failure$processing_step, paste(failure$condition_class %||% failure$error_class, collapse = ","), failure$condition_message %||% failure$error_message), file = stderr())
  quit(save = "no", status = 1L, runLast = FALSE)
}
cat(sprintf("slice status=success output=%s run_dir=%s\n", paste(result$written, collapse = ","), run_dir))
