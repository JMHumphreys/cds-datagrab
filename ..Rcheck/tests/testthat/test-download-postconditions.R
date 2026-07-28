make_test_download_nc <- function(path) {
  lon <- ncdf4::ncdim_def("longitude","degrees_east",c(-1,0)); lat <- ncdf4::ncdim_def("latitude","degrees_north",c(0,1)); tm <- ncdf4::ncdim_def("valid_time","hours since 2026-07-01 00:00:00",0); v <- ncdf4::ncvar_def("t2m","K",list(lon,lat,tm),-9999,prec="double")
  n <- ncdf4::nc_create(path,list(v),force_v4=TRUE); ncdf4::ncvar_put(n,v,array(273.15,dim=c(2,2,1))); ncdf4::ncatt_put(n,"t2m","units","K"); ncdf4::nc_close(n); path
}

test_that("download target resolves under the active raw directory", {
  base <- file.path(getwd(),".test-download-root"); if(dir.exists(base)) unlink(base,recursive=TRUE); on.exit(unlink(base,recursive=TRUE),add=TRUE)
  cfg <- list(project=list(profile="smoke",dataset_id="era5_mintemp"),paths=list(root=NULL)); p <- resolve_storage_paths(cfg,getwd(),base,create=TRUE); req <- list(target="era5_mintemp_daily_2026-07_2a5e54f4.nc"); expect_true(.descendant(resolve_download_target(req,p),p$raw_dir)); expect_error(resolve_download_target(list(target="../bad.nc"),p),"filename only|path traversal")
})

test_that("valid mocked transfer is atomically validated and reusable", {
  skip_if_not_installed("ncdf4"); base <- file.path(getwd(),".test-download-root-valid"); if(dir.exists(base)) unlink(base,recursive=TRUE); on.exit(unlink(base,recursive=TRUE),add=TRUE); p <- resolve_storage_paths(list(project=list(profile="smoke",dataset_id="era5_mintemp"),paths=list(root=NULL)),getwd(),base,create=TRUE); req <- list(dataset_short_name="derived-era5-single-levels-daily-statistics",product_type="reanalysis",variable="2m_temperature",daily_statistic="daily_minimum",time_zone="utc+00:00",frequency="6_hourly",data_format="netcdf",download_format="unarchived",year="2026",month="07",day="01",area=c(42.75,-126,-1.25,-34),target="era5_mintemp_daily_2026-07_2a5e54f4.nc")
  writer <- function(r,target) { make_test_download_nc(target); invisible(NULL) }; x <- download_cds_requests(list(req),paths=p,dry_run=FALSE,transfer_fun=writer); expect_true(x$valid); expect_true(file.exists(file.path(p$raw_dir,req$target))); expect_equal(validate_downloaded_target(file.path(p$raw_dir,req$target),req)$format,"netcdf4_hdf5"); y <- download_cds_requests(list(req),paths=p,dry_run=FALSE,transfer_fun=function(...) stop("must not run")); expect_equal(y$status,"reused_existing")
})

test_that("missing, zero-byte, and HTML transfers fail at download postcondition", {
  base <- file.path(getwd(),".test-download-root-fail"); if(dir.exists(base)) unlink(base,recursive=TRUE); on.exit(unlink(base,recursive=TRUE),add=TRUE); p <- resolve_storage_paths(list(project=list(profile="smoke",dataset_id="era5_mintemp"),paths=list(root=NULL)),getwd(),base,create=TRUE); req <- list(dataset_short_name="derived-era5-single-levels-daily-statistics",product_type="reanalysis",variable="2m_temperature",daily_statistic="daily_minimum",time_zone="utc+00:00",frequency="6_hourly",data_format="netcdf",download_format="unarchived",year="2026",month="07",day="01",area=c(42.75,-126,-1.25,-34),target="bad.nc")
  expect_error(download_cds_requests(list(req),paths=p,dry_run=FALSE,transfer_fun=function(...) NULL),"failed_stage=download"); expect_error(download_cds_requests(list(req),paths=p,dry_run=FALSE,overwrite=TRUE,transfer_fun=function(r,target) { file.create(target); NULL }),"failed_stage=download")
})

test_that("missing targets have unknown request match", {
  x <- validate_downloaded_target(file.path(getwd(),"definitely-missing-target.nc"),list(target="definitely-missing-target.nc")); expect_true(is.na(x$request_match)); expect_false(x$valid)
})
