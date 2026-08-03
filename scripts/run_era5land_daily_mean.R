#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
value <- function(flag, default=NULL) { i <- match(flag,args); if (is.na(i)) default else args[[i+1L]] }
config <- value("--config", "config/era5land_daily_mean_utc06_smoke.yml")
mode <- value("--mode", "plan"); dry_run <- value("--dry-run", "true")
start <- value("--start-date"); end <- value("--end-date"); root <- value("--output-root")
if (!mode %in% c("plan","download","process","aggregate","full")) stop("--mode must be plan, download, process, aggregate, or full",call.=FALSE)
library(cdsdatagrab)
products <- value("--products", paste(era5land_family_product_ids(), collapse=","))
ids <- strsplit(products,",",fixed=TRUE)[[1L]]
ans <- run_era5land_daily_mean_family(config_path=config, mode=mode, dry_run=tolower(dry_run) %in% c("true","1","yes"), start_date=start, end_date=end, output_root=root, product_ids=ids)
cat(sprintf("source family: era5land_daily_mean_utc06\nproducts: %s\nstatus: %s\nrun directory: %s\n",paste(ids,collapse=", "),ans$status,if(is.null(ans$run_dir)) "" else ans$run_dir))
