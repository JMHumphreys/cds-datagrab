format_daily_filename <- function(prefix, date, estimated=FALSE) paste0(prefix, "_", format(as.Date(date), "%Y-%m-%d"), if (estimated) "_est" else "", ".tif")
format_weekly_filename <- function(prefix, iso_year, iso_week, estimated=FALSE) paste0(prefix, "_", sprintf("%04d-W%02d", as.integer(iso_year), as.integer(iso_week)), if (estimated) "_est" else "", ".tif")
parse_grid_filename <- function(path, expected_prefix=NULL) {
  f <- basename(path); ext <- tolower(tools::file_ext(f)); stem <- tools::file_path_sans_ext(f); escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x); prefix <- expected_prefix %||% NA_character_; est <- FALSE; valid <- FALSE; timestep <- NA_character_; date <- as.Date(NA); iy <- iw <- NA_integer_; msg <- "unparsed filename"
  if (!is.null(expected_prefix)) {
    daily <- regexec(paste0("^", escape_regex(expected_prefix), "_(est_)?(\\d{4}-\\d{2}-\\d{2})(_est)?\\.(tif|tiff)$"), f, perl=TRUE); m <- regmatches(f,daily)[[1]]
    weekly <- regexec(paste0("^", escape_regex(expected_prefix), "_(\\d{4}-W\\d{2})(_est)?\\.(tif|tiff)$"), f, perl=TRUE); w <- regmatches(f,weekly)[[1]]
    if (length(m)) { d<-tryCatch(as.Date(m[3], format="%Y-%m-%d"), error=function(e)as.Date(NA)); if(!is.na(d)){date<-d;timestep<-"daily";est<-nzchar(m[2])||nzchar(m[4]);valid<-TRUE;msg<-"ok"} }
    if (!length(m) && length(w)) { iy<-as.integer(substr(w[2],1,4));iw<-as.integer(substr(w[2],7,8));if(iw>=1&&iw<=53){date<-ISOweek::ISOweek2date(paste0(w[2],"-1"));timestep<-"weekly";est<-nzchar(w[3]);valid<-TRUE;msg<-"ok"} }
    if (!valid && grepl(paste0("^",escape_regex(expected_prefix),"_"),stem,perl=TRUE)) msg <- "unparsed filename"
  }
  if (is.null(expected_prefix)) msg <- "expected_prefix is required"
  data.frame(path=path, filename=f, prefix=if(is.null(expected_prefix)) ifelse(is.na(prefix), NA, sub("_.*$", "", stem)) else expected_prefix, timestep=timestep, date=date, iso_year=iy, iso_week=iw, estimated=est, extension=ext, valid=valid, parse_message=msg, stringsAsFactors=FALSE)
}
