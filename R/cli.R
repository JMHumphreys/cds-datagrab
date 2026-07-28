build_pipeline_option_parser <- function() {
  if (!requireNamespace("optparse", quietly = TRUE)) stop("Package 'optparse' is required by scripts/run_pipeline.R.", call. = FALSE)
  optparse::OptionParser(option_list = list(
    optparse::make_option("--config", dest="config", type="character", default="config/era5_mintemp.yml"),
    optparse::make_option("--mode", dest="mode", type="character", default="plan"),
    optparse::make_option("--dry-run", dest="dry_run", action="store_true", default=FALSE, help="Plan operations without network requests or raster-output changes."),
    optparse::make_option("--execute", dest="execute", action="store_true", default=FALSE, help="Permit network requests and generated-data writes."),
    optparse::make_option("--observed-end", dest="observed_end", type="character"),
    optparse::make_option("--future-end", dest="future_end", type="character"),
    optparse::make_option("--output-root", dest="output_root", type="character"),
    optparse::make_option("--overwrite", dest="overwrite", action="store_true", default=FALSE),
    optparse::make_option("--rebuild-all-weeks", dest="rebuild_all_weeks", action="store_true", default=FALSE),
    optparse::make_option("--verbose", dest="verbose", action="store_true", default=FALSE)
  ))
}
parse_pipeline_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- optparse::parse_args(build_pipeline_option_parser(), args=args, positional_arguments=TRUE); x <- parsed$options
  if (length(parsed$args)) stop("Unexpected trailing arguments: ", paste(parsed$args, collapse=" "), call.=FALSE)
  for (n in c("dry_run","execute","overwrite","rebuild_all_weeks")) if (!is.logical(x[[n]]) || length(x[[n]])!=1L || is.na(x[[n]])) stop("--", n, " did not parse as a scalar logical.",call.=FALSE)
  if (!x$mode %in% c("diagnose","plan","download","process","aggregate","estimate","full")) stop("Invalid pipeline mode: ",x$mode,call.=FALSE)
  for (n in c("observed_end","future_end")) if (!is.null(x[[n]])) { z<-tryCatch(as.Date(x[[n]]),error=function(e)as.Date(NA)); if(is.na(z)) stop("Invalid date for --",n,call.=FALSE); x[[n]]<-as.character(z) }
  x
}
resolve_execution_choice <- function(parsed) { dry_run_requested <- isTRUE(parsed$dry_run); execute_requested <- isTRUE(parsed$execute); if (dry_run_requested && execute_requested) stop("Use only one of --dry-run or --execute.",call.=FALSE); list(dry_run=!execute_requested, execute=execute_requested, source=if(execute_requested) "--execute" else if(dry_run_requested) "--dry-run" else "default") }
