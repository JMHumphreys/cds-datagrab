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
field <- function(x, name, default = NULL) if(!is.null(x[[name]])) x[[name]] else default
manifest <- field(ans, "manifest", list())
source <- field(ans, "source_diagnostic", list())
cat(sprintf("source family: era5land_daily_mean_utc06\nproducts: %s\nstatus: %s\nfamily status: %s\nrun directory: %s\nraw reused: %s\narchive members: %s\nsource map rows: %s\nrequested product-dates: %s\ndaily outputs written: %s\ndaily outputs reused: %s\npre-repair missing cells: %s\nrepaired cells: %s\npost-repair missing cells: %s\nfailed products: %s\nfailed dates: %s\n",
  paste(ids,collapse=", "),ans$status,field(ans,"family_status",ans$status),if(is.null(ans$run_dir)) "" else ans$run_dir,
  field(source,"raw_reused",field(manifest,"raw_reused",NA)),field(source,"archive_member_count",NA),field(source,"source_map_rows",NA),
  length(field(manifest,"requested_product_dates",character())),field(manifest,"daily_outputs_written",0),field(manifest,"daily_outputs_reused",0),
  field(manifest,"pre_repair_missing_cells",0),field(manifest,"repaired_cells",0),field(manifest,"post_repair_missing_cells",0),
  paste(field(manifest,"failed_products",character()),collapse=","),paste(field(manifest,"failed_product_dates",character()),collapse=",")))
ok <- if(mode %in% c("plan","download") || tolower(dry_run) %in% c("true","1","yes")) ans$status %in% c("planned","downloaded") else identical(ans$status,"success")
if(!ok) {
  failed <- field(manifest,"failed_product_dates",character())
  first <- field(manifest,"product_results",list())
  failed_results <- if(length(first)) Filter(function(x) identical(field(x,"status"), "failed"), first) else list()
  first_failure <- if(length(failed_results)) failed_results[[1L]] else if(length(first)) first[[1L]] else manifest
  cat(sprintf("ERA5-Land processing failed:\nproduct=%s\ndate=%s\nstage=%s\nmessage=%s\nfailed_product_dates=%d\n",
    field(first_failure,"product_id",NA_character_), field(first_failure,"failed_dates",NA_character_)[1L], field(first_failure,"failure_stage",field(manifest,"failure_stage",NA_character_)),
    field(first_failure,"failure_message",field(manifest,"failure_message",ans$status)), length(failed)), file=stderr())
  quit(save="no",status=1L,runLast=FALSE)
}
