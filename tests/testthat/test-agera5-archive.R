test_that("AgERA5 ZIP fixtures extract and map one NetCDF member per date", {
  skip_if_not_installed("ncdf4")
  base <- test_external_root("agera5-archive"); src <- file.path(base,"members"); dir.create(src,recursive=TRUE)
  make_member <- function(date) {
    lon<-ncdf4::ncdim_def("lon","degrees_east",c(-126,-125.9)); lat<-ncdf4::ncdim_def("lat","degrees_north",c(42.8,42.7)); tm<-ncdf4::ncdim_def("time",paste0("days since 1900-01-01 00:00:00"),as.numeric(as.Date(date)-as.Date("1900-01-01"))); v<-ncdf4::ncvar_def("Derived_Relative_Humidity_2m_Min_24h","%",list(lon,lat,tm),-9999,prec="double"); p<-file.path(src,paste0("rh_",gsub("-","",date),".nc")); n<-ncdf4::nc_create(p,list(v),force_v4=TRUE); ncdf4::ncvar_put(n,v,array(50,dim=c(2,2,1))); ncdf4::nc_close(n); p
  }
  members <- vapply(c("2025-06-01","2025-06-02","2025-06-03"),make_member,character(1)); zip_path<-file.path(base,"response.zip"); old<-getwd(); setwd(src); on.exit(setwd(old),add=TRUE); utils::zip(zip_path,basename(members),flags="-q"); setwd(old)
  manifest<-extract_agera5_archive(zip_path,file.path(base,"extracted"),"fixturehash",get_variable_spec("agera5_relhum_min")); expect_equal(nrow(manifest),3); expect_true(all(manifest$detected_format%in%c("netcdf4_hdf5","netcdf_classic"))); chosen<-select_agera5_archive_members(manifest,as.Date(c("2025-06-01","2025-06-02","2025-06-03"))); expect_equal(chosen$date_from_filename,c("2025-06-01","2025-06-02","2025-06-03")); expect_equal(length(unique(chosen$extracted_path)),3)
})

test_that("unsafe archive member names are rejected", {
  skip_if_not_installed("ncdf4")
  expect_error(archive_member_safe("../escape.nc"), NA)
  expect_false(archive_member_safe("../escape.nc"))
  expect_false(archive_member_safe("/absolute.nc"))
})
