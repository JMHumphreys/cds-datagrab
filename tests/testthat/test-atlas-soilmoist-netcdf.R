make_atlas_soilmoist_fixture <- function(path) {
  lon <- ncdf4::ncdim_def("longitude", "degrees_east", seq(-126, -34, by=.25))
  lat <- ncdf4::ncdim_def("latitude", "degrees_north", seq(42.75, -1.25, by=-.25))
  tim <- ncdf4::ncdim_def("valid_time", "days since 2026-07-01 00:00:00", 0:2)
  v <- ncdf4::ncvar_def("swvl1", "m**3 m**-3", list(lon, lat, tim), missval=-9999, prec="double")
  nc <- ncdf4::nc_create(path, list(v), force_v4=TRUE); on.exit(ncdf4::nc_close(nc), add=TRUE)
  vals <- array(0, dim=c(length(lon$vals), length(lat$vals), 3L))
  for (k in seq_len(3)) vals[,,k] <- outer(seq_along(lon$vals), seq_along(lat$vals), function(i,j) .0005*i + .0001*j + .05*k)
  ncdf4::ncvar_put(nc, v, vals); invisible(path)
}

test_that("Atlas soilmoist dates normalize to canonical ISO keys", {
  expected <- as.Date("2026-07-01") + 0:2
  forms <- list(expected, as.character(expected), as.numeric(expected), factor(as.character(expected)), structure(expected, names=letters[1:3]), list(expected))
  keys <- lapply(forms, function(x) format(normalize_date_vector(x, "expected_dates"), "%Y-%m-%d"))
  expect_true(all(vapply(keys, identical, logical(1), keys[[1]])))
  expect_equal(keys[[1]], c("2026-07-01", "2026-07-02", "2026-07-03"))
})

test_that("exact Atlas NetCDF structure reads without date or unit failure", {
  skip_if_not_installed("ncdf4"); skip_if_not_installed("terra")
  f <- tempfile(fileext=".nc"); make_atlas_soilmoist_fixture(f)
  x <- read_era5_daily_with_ncdf4(f, expected_dates=as.numeric(as.Date("2026-07-01") + 0:2), variable_spec=get_variable_spec("era5_soilmoist"))
  expect_equal(x$selected_netcdf_variable, "swvl1")
  expect_equal(x$data_variable_dimensions, c("longitude", "latitude", "valid_time"))
  expect_equal(x$dimension_lengths, c(369,177,3))
  expect_equal(x$time_coordinate_name, "valid_time")
  expect_equal(x$time_coordinate_calendar_effective, "standard")
  expect_equal(x$time_coordinate_calendar_original, NULL)
  expect_equal(x$source_units_original, "m**3 m**-3")
  expect_equal(x$source_units_normalized, "m3 m-3")
  expect_equal(format(x$decoded_dates, "%Y-%m-%d"), c("2026-07-01","2026-07-02","2026-07-03"))
  expect_equal(x$latitude_direction, "descending")
  expect_equal(x$longitude_direction, "ascending")
  expect_equal(as.numeric(terra::values(x$rasters[[1]])[1]), .0005 + .0001 + .05, tolerance=1e-8)
})

test_that("raw reader accepts a valid partial-month processing subset", {
  skip_if_not_installed("ncdf4"); skip_if_not_installed("terra")
  f <- tempfile(fileext=".nc"); make_atlas_soilmoist_fixture(f)
  req <- as.Date("2026-07-01") + 0:2
  x <- read_daily_netcdf(f, get_variable_spec("era5_soilmoist"), req, req[2:3])
  expect_equal(format(x$decoded_dates, "%Y-%m-%d"), c("2026-07-01","2026-07-02","2026-07-03"))
  expect_equal(format(x$dates, "%Y-%m-%d"), c("2026-07-02","2026-07-03"))
  expect_length(x$rasters, 2)
  expect_error(read_daily_netcdf(f, get_variable_spec("era5_soilmoist"), req, as.Date("2026-07-04")), "absent")
})

test_that("twelve monthly request assignments cover a non-leap production year", {
  months <- sprintf("%02d", 1:12); ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  reqs <- lapply(seq_along(months), function(i) list(target=paste0("era5_soilmoist_daily_2022-",months[i],"_hash",i,".nc"), request_hash=paste0("hash",i), year="2022", month=months[i], day=sprintf("%02d", seq_len(ndays[i]))))
  paths <- file.path(tempdir(), vapply(reqs, `[[`, character(1), "target")); rows <- lapply(seq_along(reqs), function(i) data.frame(date=build_request_dates(reqs[[i]]), selected_raw_source=paths[i], request_hash=reqs[[i]]$request_hash, excluded_sources="", selection_reason="test", stringsAsFactors=FALSE)); map <- do.call(rbind, rows)
  expect_true(validate_date_source_map(map, paths, reqs)); expect_length(map$date, 365); expect_equal(length(unique(format(map$date, "%Y-%m-%d"))), 365); expect_equal(length(unique(map$selected_raw_source)), 12)
  bad <- map; bad$date[1] <- as.Date("2022-02-01"); expect_error(validate_date_source_map(bad, paths, reqs), "exactly one|absent")
  dup <- rbind(map, map[1,]); expect_error(validate_date_source_map(dup, paths, reqs), "exactly one")
})

test_that("soilmoist source and output range rules are distinct", {
  sm <- get_variable_spec("era5_soilmoist")
  cfg <- list(validation=list(physical_minimum=0, physical_maximum=1, clamp_tolerance=.01, soft_warning_minimum=.01, soft_warning_maximum=.80))
  expect_equal(validate_source_values(c(-0.00239669531583786, .611285507678986), sm, -.05, 1.05)$source_minimum, -0.00239669531583786)
  expect_equal(normalize_output_values(c(-0.0023967, .5, 1.002), sm, cfg)$values, c(0,.5,1))
  expect_error(normalize_output_values(-.02, sm, cfg), "physical tolerance")
  expect_error(normalize_output_values(1.02, sm, cfg), "physical tolerance")
  expect_equal(normalize_output_values(c(0,.5,1), sm, cfg)$values, c(0,.5,1))
})
