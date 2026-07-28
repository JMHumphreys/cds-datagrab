`%||%` <- function(x, y) if (is.null(x)) y else x
resolve_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(p, "DESCRIPTION")) && dirname(p) != p) p <- dirname(p)
  if (!file.exists(file.path(p, "DESCRIPTION"))) stop("Could not locate project root from ", start)
  p
}
expand_config_environment <- function(config) {
  walk <- function(x) if (is.list(x)) lapply(x, walk) else if (is.character(x)) {
    vapply(x, function(z) { m <- regmatches(z, gregexpr("\\$\\{[A-Za-z_][A-Za-z0-9_]*\\}", z))[[1]]; for (a in m) z <- sub(a, Sys.getenv(substr(a, 3, nchar(a)-1), a), z, fixed=TRUE); z }, character(1))
  } else x
  walk(config)
}
read_pipeline_config <- function(path) {
  if (!requireNamespace("yaml", quietly=TRUE)) stop("Package 'yaml' is required")
  if (!file.exists(path)) stop("Configuration does not exist: ", path)
  cfg <- expand_config_environment(yaml::read_yaml(path)); cfg$config_path <- normalizePath(path, winslash="/", mustWork=TRUE); cfg
}
resolve_config_paths <- function(config, project_root) {
  root <- Sys.getenv("CDS_DATAGRAB_DATA_ROOT", unset="")
  root <- if (nzchar(root)) root else file.path(project_root, config$paths$local_data_root %||% "runtime")
  abs <- function(x) if (isAbsolute <- grepl("^(?:[A-Za-z]:|/|\\\\)", x)) normalizePath(x, winslash="/", mustWork=FALSE) else normalizePath(file.path(project_root, x), winslash="/", mustWork=FALSE)
  config$project_root <- normalizePath(project_root, winslash="/", mustWork=TRUE); config$paths$data_root <- abs(root)
  ov <- c(raw="CDS_DATAGRAB_RAW_DIR", daily="CDS_DATAGRAB_DAILY_DIR", weekly="CDS_DATAGRAB_WEEKLY_DIR", run="CDS_DATAGRAB_RUN_DIR")
  subs <- c(raw=unname(config$paths$raw_subdir), daily=unname(config$paths$daily_subdir), weekly=unname(config$paths$weekly_subdir), run=unname(config$paths$run_subdir))
  for (n in names(subs)) config$paths[[paste0(n, "_dir")]] <- { e <- Sys.getenv(unname(ov[n]), ""); if (nzchar(e)) abs(e) else file.path(config$paths$data_root, unname(subs[n])) }
  config$spatial$template_path <- abs(config$spatial$template_path); config$spatial$bbox_path <- abs(config$spatial$bbox_path); config
}
validate_pipeline_config <- function(config) {
  req <- c("project", "spatial", "paths", "temporal", "cds", "processing", "weekly", "future", "validation")
  miss <- req[!vapply(req, function(x) !is.null(config[[x]]), logical(1))]; if (length(miss)) stop("Missing configuration sections: ", paste(miss, collapse=", "))
  if (!file.exists(config$spatial$template_path)) stop("Template raster is missing: ", config$spatial$template_path)
  if (!file.exists(config$spatial$bbox_path)) stop("Bounding-box file is missing: ", config$spatial$bbox_path)
  if (config$weekly$aggregation != "min") stop("Only weekly aggregation='min' is supported")
  config$temporal$initial_start_date <- as.Date(config$temporal$initial_start_date)
  config$temporal$future_end_date <- as.Date(config$temporal$future_end_date)
  if (is.character(config$temporal$observed_end) && config$temporal$observed_end == "auto") config$temporal$observed_end <- Sys.Date() - as.integer(config$temporal$source_lag_days) else config$temporal$observed_end <- as.Date(config$temporal$observed_end)
  if (anyNA(c(config$temporal$initial_start_date, config$temporal$observed_end, config$temporal$future_end_date))) stop("Invalid configured date")
  if (config$cds$variable != "2m_temperature" || config$cds$daily_statistic != "daily_minimum") stop("Configuration must use ERA5 2m_temperature daily_minimum")
  if (!is.null(config$spatial$expected_crs) && requireNamespace("terra", quietly=TRUE)) { validate_template_crs(terra::rast(config$spatial$template_path), config$spatial$expected_crs, config$spatial$geometry_tolerance %||% 0.001); validate_template_geometry(terra::rast(config$spatial$template_path), list(rows=config$spatial$expected_dimensions$rows, columns=config$spatial$expected_dimensions$columns, layers=config$spatial$expected_dimensions$layers, resolution=config$spatial$expected_resolution, extent=config$spatial$expected_extent), config$spatial$geometry_tolerance %||% 0.001) }
  config
}
ensure_pipeline_directories <- function(config) { validate_pipeline_config(config); dirs <- unname(config$paths[c("raw_dir","daily_dir","weekly_dir","run_dir")]); fs::dir_create(dirs, recurse=TRUE); invisible(dirs) }
