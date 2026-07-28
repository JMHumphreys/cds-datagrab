# ffwc_diagnose: reads folder to give date range of files and indicate steps to target
# fill_future_weekly_grids: plug missing weeks until reaching target date
# fill_future_daily_grids: plug missing days (as above)

library(terra)
library(stringr)
library(ISOweek)
library(lubridate)

# available files
ffwc_diagnose <- function(source_dir, target_date) {
  target_date <- as.Date(target_date)
  all  <- list.files(source_dir, pattern="\\.tif$", full.names=TRUE)
  base <- basename(all)
  keep <- !grepl("_est\\.tif$", base, ignore.case=TRUE)
  files <- all[keep]; base <- base[keep]
  
  rxw <- "^(?:.*_)?(\\d{4})_wk_(\\d{2})\\.tif$"
  idx <- grepl(rxw, base)
  yr  <- as.integer(sub(rxw, "\\1", base[idx]))
  wk  <- sprintf("%02d", as.integer(sub(rxw, "\\2", base[idx])))
  rep <- ISOweek2date(paste0(yr, "-W", wk, "-1"))
  
  cat("# donors:", length(files), " weekly-matching:", sum(idx), "\n")
  cat("first 3:", paste(head(base[idx], 3), collapse=" "), "\n")
  cat("unique years:", paste(sort(unique(yr)), collapse=","), "\n")
  cat("NAs in rep:", sum(is.na(rep)), "\n")
  cat("min(rep):", as.character(min(rep, na.rm=TRUE)),
      " max(rep):", as.character(max(rep, na.rm=TRUE)), "\n")
  cat("target ISO:", paste0(format(target_date, "%G"), "-W", format(target_date, "%V")), "\n")
}

# fill missing future weeks
fill_future_weekly_grids <- function(source_dir, target_date, overwrite = FALSE, prefix = "") {
  stopifnot(dir.exists(source_dir))
  target_date <- as.Date(target_date)
  
  files <- list.files(source_dir, pattern = "\\.tif$", full.names = TRUE)
  stopifnot(length(files) > 0)
  
  file_info <- tibble::tibble(
    path  = files,
    fname = basename(files)
  ) |>
    dplyr::mutate(
      year  = as.integer(stringr::str_match(fname, "(\\d{4})-W(\\d{2})")[, 2]),
      week  = as.integer(stringr::str_match(fname, "(\\d{4})-W(\\d{2})")[, 3]),
      date  = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", year, week))
    ) |>
    dplyr::arrange(year, week)
  
  last_year <- max(file_info$year, na.rm = TRUE)
  last_week <- max(file_info$week[file_info$year == last_year], na.rm = TRUE)
  
  start_date <- ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", last_year, last_week)) + 7
  if (start_date > target_date) {
    message("Target date already covered. Nothing to do.")
    return(invisible(NULL))
  }
  
  donor_clim <- file_info |>
    dplyr::group_by(week) |>
    dplyr::summarise(
      stack = list(terra::rast(path)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean_rast = purrr::map(stack, ~ mean(.x, na.rm = TRUE))
    )
  
  cur_date <- start_date
  while (cur_date <= target_date) {
    iso_year <- lubridate::isoyear(cur_date)
    iso_week <- lubridate::isoweek(cur_date)
    
    clim_row <- donor_clim[donor_clim$week == iso_week, ]
    if (nrow(clim_row) == 1) {
      out_name <- sprintf("%s%04d-W%02d_est.tif", prefix, iso_year, iso_week)
      out_path <- file.path(source_dir, out_name)
      if (!file.exists(out_path) || overwrite) {
        terra::writeRaster(clim_row$mean_rast[[1]], out_path, overwrite = TRUE)
        message("Wrote: ", out_name)
      }
    }
    cur_date <- cur_date + 7
  }
  
  invisible(NULL)
}

# fill missing future days 
fill_future_daily_grids <- function(source_dir, target_date, overwrite = FALSE, prefix = "") {
  target_date <- as.Date(target_date)
  
  files <- list.files(source_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(files) == 0) stop("No .tif files found in source_dir")
  
  fnames <- basename(files)
  date_str <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", fnames)
  dates <- as.Date(date_str, format = "%Y-%m-%d")
  
  if (any(is.na(dates))) stop("Could not parse dates from filenames. Check naming format.")
  
  ord <- order(dates)
  files <- files[ord]
  dates <- dates[ord]
  
  last_date <- max(dates, na.rm = TRUE)
  if (last_date >= target_date) {
    message("No gap to fill. Last file date is already at or beyond target date.")
    return(invisible(NULL))
  }
  
  donors_df <- data.frame(file = files, date = dates)
  donors_df$month_day <- format(donors_df$date, "%m-%d")
  
  missing_dates <- seq(from = last_date + 1, to = target_date, by = "day")
  
  for (md in missing_dates) {
    md_str <- format(md, "%m-%d")
    
    donors <- donors_df[donors_df$month_day == md_str, ]
    if (nrow(donors) == 0) {
      warning("No donors found for ", md_str, "; skipping.")
      next
    }
    
    rasters <- lapply(donors$file, terra::rast)
    if (length(rasters) > 1) {
      rasters <- lapply(rasters, function(r) terra::resample(r, rasters[[1]], method = "bilinear"))
    }
    avg_rast <- Reduce("+", rasters) / length(rasters)
    
    out_name <- paste0(prefix, format(md, "%Y-%m-%d"), "_est.tif")
    out_path <- file.path(source_dir, out_name)
    
    if (!file.exists(out_path) || overwrite) {
      terra::writeRaster(avg_rast, out_path, overwrite = TRUE)
      message("Created: ", out_name)
    }
  }
  
  invisible(NULL)
}
