# aggregate daily values to weekly

library(terra)

aggregate_daily_to_weekly <- function(
    in_dir,
    out_dir = NULL,                    # Now defaults based on fun choice
    fun = "min",                      # Input function as string or object
    filename_prefix = "tmin_",
    pattern = "\\.tif$",
    overwrite = TRUE,
    scale_to_7days = FALSE             # Usually used with fun = "sum"
){
  library(terra)
  
  # 1. Setup Default Directory Name
  # If out_dir isn't provided, append the function name (e.g., _weekly_mean)
  fun_name <- if(is.character(fun)) fun else deparse(substitute(fun))
  if (is.null(out_dir)) {
    out_dir <- file.path(dirname(in_dir), paste0(basename(in_dir), "_weekly_", fun_name))
  }
  
  files <- list.files(in_dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No files found with 'pattern' in: ", in_dir)
  
  b <- basename(files)
  
  # 2. Date parsing (Handles YYYY-MM-DD or YYYYMMDD)
  dstr_dash <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", b)
  ok_dash   <- grepl("^\\d{4}-\\d{2}-\\d{2}$", dstr_dash)
  
  dstr_nod  <- sub(".*_(\\d{8})\\.tif$", "\\1", b)
  ok_nod    <- grepl("^\\d{8}$", dstr_nod)
  
  dstr <- ifelse(ok_dash, dstr_dash,
                 ifelse(ok_nod, paste0(substr(dstr_nod,1,4), "-", substr(dstr_nod,5,6), "-", substr(dstr_nod,7,8)),
                        NA_character_))
  
  ok <- !is.na(dstr)
  if (!all(ok)) {
    warning("Skipping nonconforming names: ", paste(b[!ok], collapse = ", "))
    files <- files[ok]; dstr <- dstr[ok]
  }
  
  dates <- as.Date(dstr)
  ord   <- order(dates)
  files <- files[ord]; dates <- dates[ord]
  
  # 3. ISO Week Grouping
  wk_lab <- strftime(dates, "%G-W%V")
  grp    <- as.integer(factor(wk_lab, levels = unique(wk_lab)))
  
  # 4. Processing
  r <- rast(files)
  
  # Calculate weekly statistic
  weekly_stat <- tapp(r, index = grp, fun = fun, na.rm = TRUE)
  
  # 5. Optional Scaling (typically for rainfall/accumulation sums)
  if (scale_to_7days) {
    # Count non-NA days per week
    pres_mask <- !is.na(r)
    n_days <- tapp(pres_mask, index = grp, fun = sum, na.rm = TRUE)
    weekly_stat <- weekly_stat * (7 / n_days)
  }
  
  # 6. Writing Outputs
  wk_levels <- levels(factor(wk_lab, levels = unique(wk_lab)))
  names(weekly_stat) <- wk_levels
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (i in seq_len(nlyr(weekly_stat))) {
    # Result filename example: relhum_2025-W32.tif
    out_file <- file.path(out_dir, paste0(filename_prefix, names(weekly_stat)[i], ".tif"))
    
    writeRaster(weekly_stat[[i]], out_file, overwrite = overwrite,
                wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW")))
  }
  
  message("Aggregation complete. Files saved to: ", out_dir)
  invisible(list(out_dir = out_dir, weeks = wk_levels))
}