# rename files to have consistent format for date extraction and/or add prefix

rename_tif_files <- function(dir_path, prefix = "relhum") {

  files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
  
  for (f in files) {

    fname <- basename(f)
    
    match <- regexpr("(\\d{4})[-_]?(?:wk[_-]?|W)(\\d{1,2})", fname, perl = TRUE)
    
    if (match != -1) {
      year <- regmatches(fname, regexpr("\\d{4}", fname))
      week <- regmatches(fname, regexpr("(?:(?<=wk[_-])|(?<=W))\\d{1,2}", fname, perl = TRUE))
      
      week <- sprintf("%02d", as.integer(week))
      
      new_name <- paste0(prefix, year, "_wk_", week, ".tif")
      new_path <- file.path(dir_path, new_name)
      
      file.rename(f, new_path)
    } else {
  
      already_match <- regexpr("^\\d{4}_wk_\\d{2}\\.tif$", fname, perl = TRUE)
      if (already_match != -1 && prefix != "") {
        new_name <- paste0(prefix, fname)
        new_path <- file.path(dir_path, new_name)
        file.rename(f, new_path)
      }
    }
  }
}

