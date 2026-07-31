test_that("AgERA5 ZIP fixtures extract and map one NetCDF member per date", {
  skip_if_not_installed("ncdf4")
  base <- test_external_root("agera5-archive"); src <- file.path(base,"members"); dir.create(src,recursive=TRUE)
  make_member <- function(date) {
    lon<-ncdf4::ncdim_def("lon","degrees_east",c(-126,-125.9)); lat<-ncdf4::ncdim_def("lat","degrees_north",c(42.8,42.7)); tm<-ncdf4::ncdim_def("time",paste0("days since 1900-01-01 00:00:00"),as.numeric(as.Date(date)-as.Date("1900-01-01"))); v<-ncdf4::ncvar_def("Derived_Relative_Humidity_2m_Min_24h","%",list(lon,lat,tm),-9999,prec="double"); p<-file.path(src,paste0("rh_",gsub("-","",date),".nc")); n<-ncdf4::nc_create(p,list(v),force_v4=TRUE); ncdf4::ncvar_put(n,v,array(50,dim=c(2,2,1))); ncdf4::nc_close(n); p
  }
  members <- vapply(c("2025-06-01","2025-06-02","2025-06-03"),make_member,character(1)); zip_path<-file.path(base,"response.zip"); old<-getwd(); setwd(src); on.exit(setwd(old),add=TRUE); utils::zip(zip_path,basename(members),flags="-q"); setwd(old)
  manifest<-extract_agera5_archive(zip_path,file.path(base,"extracted"),"fixturehash",get_variable_spec("agera5_relhum_min")); expect_equal(nrow(manifest),3); expect_true(all(manifest$detected_format%in%c("netcdf4_hdf5","netcdf_classic"))); expect_true(all(c("archive_manifest_schema_version","adapter_id","adapter_inspection_version","dataset_id","variable_spec_hash","variable_alias_hash","selection_rules_hash","created_with_package_version","selected_netcdf_variable")%in%names(manifest))); expect_true(all(manifest$selected_netcdf_variable=="Derived_Relative_Humidity_2m_Min_24h")); chosen<-select_agera5_archive_members(manifest,as.Date(c("2025-06-01","2025-06-02","2025-06-03"))); expect_equal(chosen$date_from_filename,c("2025-06-01","2025-06-02","2025-06-03")); expect_equal(length(unique(chosen$extracted_path)),3)
  extracted_before<-manifest$extracted_path; legacy<-manifest[,c("archive_path","archive_checksum","request_hash","member_name","member_relative_path","extracted_path","member_size","member_checksum","detected_format","date_from_filename","date_from_content","variable_candidate","selected","selection_reason","validation_message")]; legacy$variable_candidate<-FALSE; legacy$selected_netcdf_variable<-NULL; utils::write.csv(legacy,file.path(dirname(extracted_before[[1]]),"archive_manifest.csv"),row.names=FALSE)
  migrated<-extract_agera5_archive(zip_path,file.path(base,"extracted"),"fixturehash",get_variable_spec("agera5_relhum_min")); expect_true(isTRUE(attr(migrated,"extraction_reused"))); expect_false(isTRUE(attr(migrated,"manifest_reused"))); expect_equal(attr(migrated,"manifest_rebuild_reason"),"adapter fingerprint changed"); expect_equal(migrated$extracted_path,extracted_before); expect_true(all(migrated$variable_candidate)); expect_true(all(migrated$selected_netcdf_variable=="Derived_Relative_Humidity_2m_Min_24h")); expect_equal(select_agera5_archive_members(migrated,as.Date(c("2025-06-01","2025-06-02","2025-06-03")))$date_from_filename,c("2025-06-01","2025-06-02","2025-06-03"))
})

test_that("unsafe archive member names are rejected", {
  skip_if_not_installed("ncdf4")
  expect_error(archive_member_safe("../escape.nc"), NA)
  expect_false(archive_member_safe("../escape.nc"))
  expect_false(archive_member_safe("/absolute.nc"))
})
