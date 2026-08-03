#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE); get_arg <- function(name, default=NULL) { i <- match(name,args); if (is.na(i)) default else args[[i+1L]] }
root <- get_arg("--output-root"); profile <- get_arg("--profile", "production"); product <- get_arg("--product")
if (is.null(root) || !nzchar(root) || !dir.exists(root)) stop("--output-root must name an existing directory", call.=FALSE)
if (!profile %in% c("production","smoke")) stop("--profile must be production or smoke", call.=FALSE)
products <- if (is.null(product)) c("era5_mintemp","era5_soilmoist","era5_lai_low","agera5_relhum_min") else product
count <- function(path) if (!dir.exists(path)) 0L else length(list.files(path, recursive=TRUE, full.names=TRUE, include.dirs=FALSE))
for (id in products) {
  data <- file.path(root,"data",profile,id); run <- file.path(root,"runs",profile,id); log <- file.path(root,"logs","slurm",profile)
  daily <- list.files(file.path(data,"daily"), pattern="\\.tif$", full.names=FALSE); weekly <- list.files(file.path(data,"weekly"), pattern="\\.tif$", full.names=FALSE)
  dd <- sub(".*([0-9]{4}-[0-9]{2}-[0-9]{2}).*","\\1",daily); ww <- sub(".*([0-9]{4}-W[0-9]{2}).*","\\1",weekly)
  invalid <- c(daily[!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}\\.tif$",daily)], weekly[!grepl("[0-9]{4}-W[0-9]{2}\\.tif$",weekly)])
  cat(sprintf("product=%s\ndata=%s\nruns=%s\nlogs=%s\nraw_files=%d\nextracted_files=%d\ndaily_geotiffs=%d\nweekly_geotiffs=%d\nrun_directories=%d\nearly_daily=%s\nlatest_daily=%s\nearly_weekly=%s\nlatest_weekly=%s\ninvalid_filenames=%s\nduplicate_daily_dates=%s\nduplicate_weekly_identifiers=%s\nmissing_root_marker=%s\npath_safety=%s\n\n",id,data,run,log,count(file.path(data,"raw")),count(file.path(data,"extracted")),length(daily),length(weekly),if(dir.exists(run)) length(list.dirs(run,recursive=FALSE,full.names=TRUE)) else 0L,if(length(dd)) min(dd) else "none",if(length(dd)) max(dd) else "none",if(length(ww)) min(ww) else "none",if(length(ww)) max(ww) else "none",paste(invalid,collapse=";"),paste(unique(dd[duplicated(dd)]),collapse=";"),paste(unique(ww[duplicated(ww)]),collapse=";"),!file.exists(file.path(root,".cds-datagrab-root")),grepl("^/|^[A-Za-z]:",normalizePath(root,mustWork=TRUE))))
  outside <- list.files(file.path(root,"data"), recursive=TRUE, full.names=TRUE); outside <- outside[!grepl(paste0("[/\\\\]",profile,"[/\\\\]"),outside)]
  out_range <- dd[nzchar(dd) & (dd < "2022-01-01" | dd > "2026-12-31")]
  cat(sprintf("files_outside_requested_profile=%d\nout_of_config_range_products=%s\n\n", length(outside), paste(out_range, collapse=";")))
}
