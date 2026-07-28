format_daily_filename <- function(prefix, date, estimated=FALSE) paste0(prefix, "_", format(as.Date(date), "%Y-%m-%d"), if (estimated) "_est" else "", ".tif")
format_weekly_filename <- function(prefix, iso_year, iso_week, estimated=FALSE) paste0(prefix, "_", sprintf("%04d-W%02d", as.integer(iso_year), as.integer(iso_week)), if (estimated) "_est" else "", ".tif")
parse_grid_filename <- function(path, expected_prefix=NULL) {
  f <- basename(path); ext <- tolower(tools::file_ext(f)); stem <- tools::file_path_sans_ext(f); p <- strsplit(stem, "_", fixed=TRUE)[[1]]; prefix <- if(length(p)>1) paste(p[-1], collapse="_") else NA_character_; est <- grepl("_est$", stem); valid <- FALSE; timestep <- NA_character_; date <- as.Date(NA); iy <- iw <- NA_integer_; msg <- "unparsed filename"
  if (length(p) >= 2 && (is.null(expected_prefix) || startsWith(stem, paste0(expected_prefix, "_")))) {
    prefix <- if (est) sub("_est$", "", sub("^[^_]+_", "", stem)) else sub("^[^_]+_", "", stem); prefix <- sub("_.*$", "", prefix)
    tok <- sub("^[^_]+_", "", stem); tok <- sub("_est$", "", tok)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", tok)) { d <- as.Date(tok); if(!is.na(d)) { timestep <- "daily"; date <- d; valid <- ext %in% c("tif","tiff"); msg <- if(valid) "ok" else "unsupported extension" } }
    if (grepl("^\\d{4}-W\\d{2}$", tok)) { iy <- as.integer(substr(tok,1,4)); iw <- as.integer(substr(tok,7,8)); if(iw>=1 && iw<=53) { timestep <- "weekly"; date <- ISOweek::ISOweek2date(paste0(tok, "-1")); valid <- ext %in% c("tif","tiff"); msg <- if(valid) "ok" else "unsupported extension" } }
  }
  data.frame(path=path, filename=f, prefix=if(is.null(expected_prefix)) ifelse(is.na(prefix), NA, sub("_.*$", "", stem)) else expected_prefix, timestep=timestep, date=date, iso_year=iy, iso_week=iw, estimated=est, extension=ext, valid=valid, parse_message=msg, stringsAsFactors=FALSE)
}
