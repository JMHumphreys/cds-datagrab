variable_spec_hash <- function(spec) {
  x <- spec[c("id","short_name","dataset_short_name","cds_variable","netcdf_variable_names","source_units","output_units","daily_statistic","time_zone","frequency","unit_conversion","spatial_interpolation","weekly_statistic","hard_valid_range","soft_warning_range","soil_layer")]
  digest::digest(x, algo="xxhash32")
}

.variable_spec_mintemp <- function() {
  x <- list(id="era5_mintemp", short_name="mintemp", long_name="2 metre temperature", dataset_short_name="derived-era5-single-levels-daily-statistics", cds_variable="2m_temperature", netcdf_variable_names=c("t2m","2m_temperature"), source_units="K", output_units="degrees C", soil_layer=NULL, daily_statistic="daily_minimum", time_zone="utc+00:00", frequency="6_hourly", unit_conversion="kelvin_to_celsius", spatial_interpolation="bilinear", weekly_statistic="minimum", hard_valid_range=c(-100,70), soft_warning_range=NULL, daily_filename_prefix="mintemp", weekly_filename_prefix="mintemp")
  x$variable_spec_hash <- variable_spec_hash(x); x
}

.variable_spec_soilmoist <- function() {
  x <- list(id="era5_soilmoist", short_name="soilmoist", long_name="Volumetric soil water layer 1", dataset_short_name="derived-era5-single-levels-daily-statistics", cds_variable="volumetric_soil_water_layer_1", netcdf_variable_names=c("swvl1","volumetric_soil_water_layer_1"), source_units="m3 m-3", output_units="m3 m-3", soil_layer=list(top_cm=0, bottom_cm=7), daily_statistic="daily_mean", time_zone="utc+00:00", frequency="6_hourly", unit_conversion="identity", spatial_interpolation="bilinear", weekly_statistic="mean", hard_valid_range=c(0,1), soft_warning_range=c(0.01,0.80), daily_filename_prefix="soilmoist", weekly_filename_prefix="soilmoist")
  x$variable_spec_hash <- variable_spec_hash(x); x
}

get_variable_spec <- function(variable_id, config=NULL) {
  id <- as.character(variable_id %||% if(!is.null(config)) config$project$dataset_id else "")
  if(id %in% c("era5_mintemp","mintemp","2m_temperature","t2m")) return(.variable_spec_mintemp())
  if(id %in% c("era5_soilmoist","soilmoist","volumetric_soil_water_layer_1")) return(.variable_spec_soilmoist())
  stop("Unknown variable_id: ", id, call.=FALSE)
}

resolve_netcdf_variable <- function(nc_metadata, variable_spec) {
  candidates <- intersect(variable_spec$netcdf_variable_names, names(nc_metadata$variables))
  if(!length(candidates)) stop("No NetCDF variable matched aliases: ", paste(variable_spec$netcdf_variable_names, collapse=", "), call.=FALSE)
  if(length(candidates)>1L) {
    a <- lapply(candidates, function(n) nc_metadata$variables[[n]]$data_signature %||% nc_metadata$variables[[n]]$dimensions)
    if(!identical(a[[1]], a[[2]])) stop("NetCDF variable aliases are ambiguous and refer to different data", call.=FALSE)
  }
  candidates[[1]]
}

normalize_source_units <- function(units) {
  if (length(units) != 1L || is.na(units) || !nzchar(trimws(units))) return(NA_character_)
  x <- trimws(as.character(units))
  x <- chartr("⁰¹²³⁴⁵⁶⁷⁸⁹⁻", "0123456789-", x)
  x <- gsub("[×·⋅]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("\\s*[/]\\s*", "/", x)
  x <- gsub("\\s*[\\^]\\s*", "^", x)
  x <- gsub("\\*\\*", "^", x)
  x <- tolower(trimws(x))
  if (x %in% c("1", "1/1", "m3/m3", "m^3/m^3", "m3 m-3", "m^3 m^-3", "m^3 m-3", "m3 m^ -3")) return("m3 m-3")
  if (x %in% c("k", "kelvin")) return("K")
  x
}

convert_source_units <- function(x, variable_spec, source_units=NULL) {
  if (variable_spec$id == "era5_soilmoist" && !is.null(source_units)) {
    u <- normalize_source_units(source_units)
    if (!identical(u, "m3 m-3")) stop("Unsupported soil-moisture source units: ", source_units, call.=FALSE)
  }
  if(variable_spec$unit_conversion == "kelvin_to_celsius") return(x - 273.15)
  if(variable_spec$unit_conversion == "identity") return(x)
  stop("Unsupported unit conversion: ", variable_spec$unit_conversion, call.=FALSE)
}

validate_variable_values <- function(x, variable_spec, tolerance=1e-6) {
  z <- as.numeric(x); finite <- is.finite(z)
  hard <- variable_spec$hard_valid_range
  if(variable_spec$id == "era5_soilmoist") {
    z[finite & z < hard[1] & z >= hard[1]-tolerance] <- hard[1]
    z[finite & z > hard[2] & z <= hard[2]+tolerance] <- hard[2]
  }
  hard_valid <- !any(finite & (z < hard[1] | z > hard[2]))
  warning_range <- variable_spec$soft_warning_range
  soft_warning <- !is.null(warning_range) && any(finite & (z < warning_range[1] | z > warning_range[2]))
  if(!hard_valid) stop("Values outside hard valid range [", hard[1], ", ", hard[2], "]", call.=FALSE)
  qs <- if(any(finite)) stats::quantile(z[finite], probs=c(0,.25,.5,.75,1), names=FALSE, na.rm=TRUE) else rep(NA_real_,5)
  list(values=z, minimum=if(any(finite))min(z[finite]) else NA_real_, maximum=if(any(finite))max(z[finite]) else NA_real_, mean=if(any(finite))mean(z[finite]) else NA_real_, standard_deviation=if(sum(finite)>1)stats::sd(z[finite]) else NA_real_, quantiles=qs, hard_range_valid=hard_valid, soft_range_warning=soft_warning, non_na_count=sum(finite))
}

aggregate_daily_values <- function(rasters, variable_spec) {
  if(variable_spec$daily_statistic == "daily_minimum") return(terra::app(terra::rast(rasters), fun=min, na.rm=TRUE))
  if(variable_spec$daily_statistic == "daily_mean") return(terra::app(terra::rast(rasters), fun=mean, na.rm=FALSE))
  stop("Unsupported daily statistic: ", variable_spec$daily_statistic, call.=FALSE)
}

aggregate_weekly_values <- function(rasters, variable_spec) {
  if(variable_spec$weekly_statistic == "minimum") return(terra::app(terra::rast(rasters), fun=min, na.rm=TRUE))
  if(variable_spec$weekly_statistic == "mean") return(terra::app(terra::rast(rasters), fun=mean, na.rm=FALSE))
  stop("Unsupported weekly statistic: ", variable_spec$weekly_statistic, call.=FALSE)
}

daily_output_filename <- function(variable_spec, date, estimated=FALSE) format_daily_filename(variable_spec$daily_filename_prefix, date, estimated)
weekly_output_filename <- function(variable_spec, iso_year, iso_week, estimated=FALSE) format_weekly_filename(variable_spec$weekly_filename_prefix, iso_year, iso_week, estimated)
