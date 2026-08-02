make_lai_low_fixture <- function(path, dates = as.Date("2025-06-01") + 0:2) {
  lon <- ncdf4::ncdim_def("longitude", "degrees_east", c(-126, -125.75, -125.5))
  lat <- ncdf4::ncdim_def("latitude", "degrees_north", c(42.75, 42.5, 42.25))
  tim <- ncdf4::ncdim_def("valid_time", "hours since 2025-06-01 00:00:00", as.numeric(dates - dates[1]) * 24)
  v <- ncdf4::ncvar_def("lai_lv", "m**2 m**-2", list(lon, lat, tim), missval = -9999, prec = "double")
  nc <- ncdf4::nc_create(path, list(v), force_v4 = TRUE); on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncatt_put(nc, "lai_lv", "long_name", "Leaf area index, low vegetation")
  vals <- array(0, dim = c(3, 3, length(dates)))
  for (k in seq_along(dates)) vals[, , k] <- outer(1:3, 1:3, function(i, j) 0.25 * i + 0.75 * j + k / 10)
  ncdf4::ncvar_put(nc, v, vals); invisible(path)
}

make_agera5_relhum_min_fixture <- function(path, dates = as.Date("2025-06-01") + 0:2) {
  lon <- ncdf4::ncdim_def("longitude", "degrees_east", seq(-126, -125.8, by = .1))
  lat <- ncdf4::ncdim_def("latitude", "degrees_north", seq(42.8, 42.6, by = -.1))
  tim <- ncdf4::ncdim_def("valid_time", "days since 2025-06-01 00:00:00", as.numeric(dates - dates[1]))
  v <- ncdf4::ncvar_def("relative_humidity_2m_min", "percent", list(lon, lat, tim), missval = -9999, prec = "double")
  nc <- ncdf4::nc_create(path, list(v), force_v4 = TRUE); on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncatt_put(nc, "relative_humidity_2m_min", "long_name", "2 m relative humidity derived, 24-hour minimum")
  ncdf4::ncatt_put(nc, "relative_humidity_2m_min", "daily_statistic", "24_hour_minimum")
  ncdf4::ncatt_put(nc, "relative_humidity_2m_min", "time_basis", "local_time")
  vals <- array(0, dim = c(3, 3, length(dates)))
  for (k in seq_along(dates)) vals[, , k] <- outer(1:3, 1:3, function(i, j) 35 + 2 * i + 3 * j + k)
  ncdf4::ncvar_put(nc, v, vals); invisible(path)
}
