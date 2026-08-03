era5land_family_product_ids <- function() .era5land_product_ids()

era5land_family_date_source_map <- function(raw_path, request) {
  data.frame(date=as.character(request$raw_request_dates), selected_raw_source=raw_path, source_path=raw_path,
    request_hash=request$request_hash, raw_request_start=request$request_start, raw_request_end=request$request_end,
    decoded_source_start=NA_character_, decoded_source_end=NA_character_, mapping_reason="shared_source_family_bundle", stringsAsFactors=FALSE)
}

era5land_family_manifest <- function(run_dir, root, source_paths, request, cfg, products, status="running") {
  m <- list(run_id=basename(run_dir), run_dir=run_dir, source_family_id="era5land_daily_mean_utc06", profile=cfg$project$profile,
    resolved_output_root=root, output_root_source=source_paths$root_source, source_directory=source_paths$source_root,
    raw_directory=source_paths$raw_dir, requested_variables=request$requested_variables, product_ids=products,
    request_hash=request$request_hash, request_start=request$request_start, request_end=request$request_end,
    daily_statistic=request$daily_statistic, daily_time_zone=request$time_zone, daily_sampling_frequency=request$frequency,
    request_area=request$area, status=status)
  jsonlite::write_json(m,file.path(run_dir,"run_manifest.json"),pretty=TRUE,auto_unbox=TRUE,null="null"); m
}

era5land_annotate_product_metadata <- function(directory, spec, request) {
  files <- if (dir.exists(directory)) list.files(directory, pattern="\\.json$", full.names=TRUE) else character()
  for (f in files) { x <- tryCatch(jsonlite::read_json(f,simplifyVector=FALSE),error=function(e)NULL); if (is.null(x)) next; x$source_family_id <- request$source_family_id; x$daily_time_zone <- request$time_zone; x$daily_sampling_frequency <- request$frequency; x$daily_statistic <- request$daily_statistic; x$metadata_notes <- spec$metadata_notes %||% NULL; jsonlite::write_json(x,f,pretty=TRUE,auto_unbox=TRUE,null="null") }
  invisible(files)
}

run_era5land_daily_mean_family <- function(config_path="config/era5land_daily_mean_utc06_smoke.yml", mode=c("plan","download","process","aggregate","full"), dry_run=TRUE, start_date=NULL, end_date=NULL, output_root=NULL, product_ids=.era5land_product_ids(), overwrite=FALSE, transfer_fun=NULL) {
  mode <- match.arg(mode); cfg <- read_pipeline_config(config_path); root <- resolve_project_root(dirname(config_path)); cfg <- resolve_config_paths(cfg,root,output_root,FALSE); cfg <- validate_pipeline_config(cfg)
  if (!identical(unname(as.character(cfg$project$source_family_id)),"era5land_daily_mean_utc06")) stop("Configuration is not an ERA5-Land daily-mean source-family configuration",call.=FALSE)
  window <- resolve_pipeline_date_window(cfg,start_date,end_date,dry_run); expected <- safe_date_sequence(window$effective_start,window$effective_end)
  source_paths <- resolve_source_storage_paths(cfg,root,output_root,create=TRUE); run_id <- paste0(format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"),"_",substr(digest::digest(list(cfg,mode,expected,product_ids),algo="xxhash32"),1,8)); run_dir <- file.path(source_paths$runs_root,run_id); fs::dir_create(run_dir,recurse=TRUE)
  diag <- diagnose_spatial_domain(cfg$spatial$template_path,cfg$spatial$bbox_path,cfg)
  # Product selection controls fan-out only; the shared source request always contains all eight variables.
  requests <- build_era5land_daily_mean_requests(expected,diag$final_cds_area,cfg,.era5land_product_ids()); req <- if(length(requests)) requests[[1]] else NULL
  manifest <- if(!is.null(req)) era5land_family_manifest(run_dir,source_paths$root,source_paths,req,cfg,product_ids) else list(run_dir=run_dir)
  jsonlite::write_json(diag,file.path(run_dir,"spatial_diagnostics.json"),pretty=TRUE,auto_unbox=TRUE); write_cds_request_manifests(requests,run_dir)
  if (mode=="plan" || isTRUE(dry_run)) return(list(status="planned",run_id=run_id,run_dir=run_dir,requests=requests,source_paths=source_paths,spatial_diagnostics=diag,products=product_ids))
  download_result <- if (mode %in% c("download","full")) download_cds_requests(requests,paths=source_paths,run_dir=run_dir,dry_run=FALSE,overwrite=overwrite,config=cfg,run_id=run_id,transfer_fun=transfer_fun) else data.frame()
  if (mode=="download") return(list(status="downloaded",run_id=run_id,run_dir=run_dir,requests=requests,download=download_result,source_paths=source_paths,products=product_ids))
  raw_path <- file.path(source_paths$raw_dir,req$target); if(!file.exists(raw_path)) stop("Shared ERA5-Land raw bundle is missing: ",raw_path,call.=FALSE)
  md <- inspect_netcdf_file(raw_path); requested_aliases <- lapply(req$product_ids %||% product_ids, function(id) get_variable_spec(id)$netcdf_variable_names); present <- names(md$variables); resolved <- lapply(seq_along(requested_aliases), function(i) intersect(requested_aliases[[i]],present)); names(resolved) <- product_ids
  shared_source_diagnostic <- list(container_format=md$format,raw_file_size=as.numeric(file.info(raw_path)$size),requested_variables=req$requested_variables,variables_present=present,variables_absent=setdiff(unlist(requested_aliases),present),resolved_aliases=resolved,source_units=lapply(resolved,function(x)if(length(x))md$variables[[x[[1]]]]$units else NA_character_),coordinate_dimensions=md$dimensions,source_extent=list(longitude=md$coordinate_variables$longitude,latitude=md$coordinate_variables$latitude),source_resolution=list(longitude=if(length(md$coordinate_variables$longitude)>1)median(diff(md$coordinate_variables$longitude)) else NA_real_,latitude=if(length(md$coordinate_variables$latitude)>1)median(diff(md$coordinate_variables$latitude)) else NA_real_),decoded_dates=as.character(md$decoded_dates),requested_dates=req$raw_request_dates,date_mapping_status=if(identical(as.character(md$decoded_dates),as.character(as.Date(req$raw_request_dates))))"complete" else "mismatch",selected_reader="ncdf4",gdal_netcdf_capability=NA,ncdf4_capability=requireNamespace("ncdf4",quietly=TRUE))
  jsonlite::write_json(shared_source_diagnostic,file.path(run_dir,"source_diagnostic.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
  date_map <- era5land_family_date_source_map(raw_path,req); results <- list(); failures <- list()
  for (id in product_ids) {
    spec <- get_variable_spec(id); pcfg <- cfg; pcfg$project$dataset_id <- id; pcfg$cds$variable <- spec$cds_variable; pcfg$cds$daily_statistic <- spec$daily_statistic; pcfg$paths <- list(root=source_paths$root); pcfg <- resolve_config_paths(pcfg,root,source_paths$root,FALSE); p <- resolve_storage_paths(pcfg,root,source_paths$root,create=TRUE); product_run <- file.path(p$runs_root,run_id); fs::dir_create(product_run,recurse=TRUE)
    lineage <- list(run_id=run_id,product_id=id,source_family_id=req$source_family_id,source_run_directory=run_dir,shared_raw_path=raw_path,request_hash=req$request_hash,profile=cfg$project$profile,resolved_output_root=source_paths$root,data_directory=p$dataset_root,run_directory=product_run,slurm_log_directory=p$slurm_log_dir,daily_time_zone=req$time_zone,daily_sampling_frequency=req$frequency,daily_statistic=req$daily_statistic,weekly_statistic=spec$weekly_statistic)
    jsonlite::write_json(lineage,file.path(product_run,"run_manifest.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
    tryCatch({ pr <- process_downloaded_variable(raw_path,p$daily_dir,cfg$spatial$template_path,cfg$spatial$bbox_path,pcfg,spec,overwrite_dates=if(overwrite)expected else NULL,expected_dates=expected,run_expected_dates=expected,request_manifest=list(req),date_source_map=date_map,run_dir=product_run); era5land_annotate_product_metadata(p$daily_dir,spec,req); wr <- list(status="success",product_id=id,request_hash=req$request_hash,process=pr); if(mode %in% c("aggregate","full")){ inv <- inventory_daily_products(p$daily_dir,spec$daily_filename_prefix,cfg$spatial$template_path,TRUE,pcfg); wr$weekly <- aggregate_daily_to_weekly(p$daily_dir,p$weekly_dir,spec$weekly_filename_prefix,template_path=cfg$spatial$template_path,inventory=inv,variable_spec=spec,config=pcfg); era5land_annotate_product_metadata(p$weekly_dir,spec,req) }; results[[id]] <<- wr }, error=function(e) failures[[id]] <<- list(product_id=id,source_family=req$source_family_id,request_hash=req$request_hash,stage="process",failure_reason=conditionMessage(e)))
  }
  status <- if(length(failures)) if(length(results)) "partial_failure" else "failed" else "success"; manifest$status <- status; manifest$product_results <- results; manifest$failures <- failures; jsonlite::write_json(manifest,file.path(run_dir,"run_manifest.json"),pretty=TRUE,auto_unbox=TRUE,null="null"); list(status=status,run_id=run_id,run_dir=run_dir,requests=requests,download=download_result,products=results,failures=failures,source_paths=source_paths)
}
