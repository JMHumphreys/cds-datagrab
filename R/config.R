`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(p, "DESCRIPTION")) && dirname(p) != p) p <- dirname(p)
  if (!file.exists(file.path(p, "DESCRIPTION"))) stop("Could not locate project root from ", start)
  p
}

expand_config_environment <- function(config) {
  walk <- function(x) if (is.list(x)) lapply(x, walk) else if (is.character(x)) {
    vapply(x, function(z) {
      m <- regmatches(z, gregexpr("\\$\\{[A-Za-z_][A-Za-z0-9_]*\\}", z))[[1]]
      for (a in m) z <- sub(a, Sys.getenv(substr(a, 3, nchar(a) - 1), a), z, fixed = TRUE)
      z
    }, character(1))
  } else x
  walk(config)
}

read_pipeline_config <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required")
  if (!file.exists(path)) stop("Configuration does not exist: ", path)
  cfg <- expand_config_environment(yaml::read_yaml(path))
  cfg$config_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  cfg
}

.normal_path <- function(x, project_root) {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) stop("Output root must not be empty", call. = FALSE)
  if (grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", x)) stop("Output root must not contain path traversal components", call. = FALSE)
  if (!grepl("^(?:[A-Za-z]:|/|\\\\)", x)) x <- file.path(project_root, x)
  normalizePath(x, winslash = "/", mustWork = FALSE)
}
.descendant <- function(path, root) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  r <- normalizePath(root, winslash = "/", mustWork = FALSE)
  identical(tolower(p), tolower(r)) || startsWith(tolower(p), paste0(tolower(r), "/"))
}
.reject_root <- function(root, project_root) {
  if (!nzchar(root) || root %in% c("/", "\\")) stop("Output root is invalid", call. = FALSE)
  home <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  old <- "/project/disease_ecology/NWScrewworm"
  if (identical(tolower(root), tolower(home))) stop("User home directory cannot be the output root", call. = FALSE)
  if (identical(tolower(root), tolower(normalizePath(project_root, winslash = "/", mustWork = TRUE)))) stop("Repository root cannot be the output root", call. = FALSE)
  if (grepl("NWScrewworm", root, ignore.case=TRUE) || identical(tolower(root), tolower(old)) || startsWith(tolower(root), paste0(tolower(old), "/"))) stop("The old NWScrewworm data root is not permitted", call. = FALSE)
}

resolve_storage_paths <- function(config, project_root, output_root = NULL, create = FALSE) {
  profile <- as.character(config$project$profile %||% "")
  dataset <- as.character(config$project$dataset_id %||% "")
  if (length(profile) != 1L || !profile %in% c("smoke", "production")) stop("project.profile must be 'smoke' or 'production'", call. = FALSE)
  if (length(dataset) != 1L || !nzchar(dataset) || grepl("[/\\\\]", dataset) || grepl("\\.\\.", dataset)) stop("project.dataset_id is invalid", call. = FALSE)
  obsolete <- Sys.getenv("CDS_DATAGRAB_DATA_ROOT", "")
  if (nzchar(obsolete)) warning("CDS_DATAGRAB_DATA_ROOT is obsolete and is ignored.\nSet CDS_DATAGRAB_ROOT instead.", call. = FALSE)
  configured <- config$paths$root %||% ""
  env_root <- Sys.getenv("CDS_DATAGRAB_ROOT", "")
  chosen <- if (!is.null(output_root) && nzchar(output_root)) output_root else if (nzchar(env_root)) env_root else if (nzchar(configured)) configured else file.path("runtime", "cds-datagrab")
  root <- .normal_path(chosen, project_root); .reject_root(root, project_root)
  dataset_root <- file.path(root, "data", profile, dataset)
  run_root <- file.path(root, "runs", profile, dataset)
  p <- list(root = root, profile = profile, dataset_id = dataset, dataset_root = dataset_root,
            raw_dir = file.path(dataset_root, "raw"), raw_quarantine_dir = file.path(dataset_root, "quarantine", "raw"), quarantine_dir = file.path(dataset_root, "quarantine"), extracted_dir = file.path(dataset_root, "extracted"),
            daily_dir = file.path(dataset_root, "daily"), weekly_dir = file.path(dataset_root, "weekly"),
            temp_dir = file.path(dataset_root, "temp"), cache_dir = file.path(dataset_root, "cache"),
            runs_root = run_root, run_dir = NULL,
            pipeline_log_dir = file.path(root, "logs", "pipeline", profile, dataset),
            slurm_log_dir = file.path(root, "logs", "slurm", profile),
            root_marker = file.path(root, ".cds-datagrab-root"))
  path_names <- c("root", "dataset_root", "raw_dir", "raw_quarantine_dir", "quarantine_dir", "extracted_dir", "daily_dir", "weekly_dir", "temp_dir", "cache_dir", "runs_root", "pipeline_log_dir", "slurm_log_dir", "root_marker")
  all_paths <- unname(unlist(p[path_names], use.names = FALSE)); if (any(!vapply(all_paths, .descendant, logical(1), root = root))) stop("Resolved path escaped output root", call. = FALSE)
  if (create) {
    fs::dir_create(root, recurse = TRUE)
    if (file.exists(p$root_marker)) { marker <- tryCatch(jsonlite::read_json(p$root_marker, simplifyVector=TRUE), error=function(e)NULL); if (is.null(marker) || !identical(marker$application, "cds-datagrab")) stop("Existing storage root marker is not owned by cds-datagrab", call.=FALSE) } else jsonlite::write_json(list(application = "cds-datagrab", schema_version = 1L, created_utc = format(Sys.time(), tz = "UTC"), created_by = Sys.info()[["user"]]), p$root_marker, auto_unbox = TRUE, pretty = TRUE)
    fs::dir_create(unname(unlist(p[c("raw_dir", "raw_quarantine_dir", "quarantine_dir", "extracted_dir", "daily_dir", "weekly_dir", "temp_dir", "cache_dir", "runs_root", "pipeline_log_dir", "slurm_log_dir")], use.names=FALSE)), recurse = TRUE)
  }
  p
}

resolve_config_paths <- function(config, project_root, output_root = NULL, create = FALSE) {
  config$project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  config$paths <- config$paths %||% list(root = NULL)
  p <- resolve_storage_paths(config, project_root, output_root, create)
  for (n in names(p)) config$paths[[n]] <- p[[n]]
  config$spatial$template_path <- .normal_path(config$spatial$template_path, project_root)
  config$spatial$bbox_path <- .normal_path(config$spatial$bbox_path, project_root)
  config
}

validate_pipeline_config <- function(config) {
  req <- c("project", "spatial", "paths", "temporal", "cds", "processing", "weekly", "future", "validation")
  miss <- req[!vapply(req, function(x) !is.null(config[[x]]), logical(1))]
  if (length(miss)) stop("Missing configuration sections: ", paste(miss, collapse = ", "))
  if (!is.null(config$project$profile) && !config$project$profile %in% c("smoke", "production")) stop("project.profile must be smoke or production")
  if (!file.exists(config$spatial$template_path)) stop("Template raster is missing: ", config$spatial$template_path)
  if (!file.exists(config$spatial$bbox_path)) stop("Bounding-box file is missing: ", config$spatial$bbox_path)
  if (config$weekly$aggregation != "min") stop("Only weekly aggregation='min' is supported")
  config$temporal$initial_start_date <- as.Date(config$temporal$initial_start_date)
  config$temporal$future_end_date <- as.Date(config$temporal$future_end_date)
  if (is.character(config$temporal$observed_end) && config$temporal$observed_end == "auto") config$temporal$observed_end <- Sys.Date() - as.integer(config$temporal$source_lag_days) else config$temporal$observed_end <- as.Date(config$temporal$observed_end)
  if (anyNA(c(config$temporal$initial_start_date, config$temporal$observed_end, config$temporal$future_end_date))) stop("Invalid configured date")
  if (config$cds$variable != "2m_temperature" || config$cds$daily_statistic != "daily_minimum") stop("Configuration must use ERA5 2m_temperature daily_minimum")
  if (!is.null(config$spatial$expected_crs) && requireNamespace("terra", quietly = TRUE)) { validate_template_crs(terra::rast(config$spatial$template_path), config$spatial$expected_crs, config$spatial$geometry_tolerance %||% 0.001); validate_template_geometry(terra::rast(config$spatial$template_path), list(rows=config$spatial$expected_dimensions$rows, columns=config$spatial$expected_dimensions$columns, layers=config$spatial$expected_dimensions$layers, resolution=config$spatial$expected_resolution, extent=config$spatial$expected_extent), config$spatial$geometry_tolerance %||% 0.001) }
  config
}

ensure_pipeline_directories <- function(config, output_root = NULL) { validate_pipeline_config(config); resolve_storage_paths(config, config$project_root %||% resolve_project_root(), output_root, create = TRUE) }
