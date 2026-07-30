test_that("CDS payload is a positive allowlist of dataset fields", {
  r <- list(dataset_short_name="derived-era5-single-levels-daily-statistics",product_type="reanalysis",variable="2m_temperature",year="2026",month="07",day=c("01","02","03"),daily_statistic="daily_minimum",time_zone="utc+00:00",frequency="6_hourly",data_format="netcdf",download_format="unarchived",area=c(42.75,-126,-1.25,-34),source_grid_alignment=TRUE,buffer_degrees=1,source_grid_degrees=.25,request_extent_source="template_bbox_union",request_hash="2a5e54f4",target="era5_mintemp_daily_2026-07_2a5e54f4.nc",resolved_target_path="C:/raw/test.nc",sentinel_internal_field="must-not-leak",new_pipeline_metadata="example")
  p <- build_cds_api_payload(r); expect_named(p,cds_api_fields); expect_false(any(cds_internal_fields %in% names(p))); expect_equal(p$area,c(42.75,-126,-1.25,-34)); expect_equal(p$day,c("01","02","03")); expect_true(all(c("request_hash","target","source_grid_alignment") %in% names(r)))
})

test_that("malformed payloads fail before transfer", {
  r <- list(product_type="reanalysis",variable="2m_temperature",year="2026",month="07",day="01",daily_statistic="daily_minimum",time_zone="utc+00:00",frequency="6_hourly",data_format="netcdf",download_format="unarchived",area=c(42.75,-126,-1.25,-34))
  expect_error(validate_cds_api_payload(c(r,request_hash="leak")),"Unexpected fields"); r$area <- c(1,2,3,4); expect_error(build_cds_api_payload(r),"area")
})

test_that("captured transfer receives sanitized payload only", {
  captured <- NULL; transfer <- function(dataset, payload, target) { captured <<- list(dataset=dataset,payload=payload,target=target); NULL }
  r <- list(dataset_short_name="derived-era5-single-levels-daily-statistics",product_type="reanalysis",variable="2m_temperature",year="2026",month="07",day=c("01","02","03"),daily_statistic="daily_minimum",time_zone="utc+00:00",frequency="6_hourly",data_format="netcdf",download_format="unarchived",area=c(42.75,-126,-1.25,-34),source_grid_alignment=TRUE,buffer_degrees=1,request_hash="2a5e54f4",target="test.nc",sentinel_internal_field="must-not-leak")
  base <- file.path("D:/tmp",".test-payload-root"); if(dir.exists(base)) unlink(base,recursive=TRUE); on.exit(unlink(base,recursive=TRUE),add=TRUE); p <- resolve_storage_paths(list(project=list(profile="smoke",dataset_id="era5_mintemp"),paths=list(root=NULL)),getwd(),base,create=TRUE); expect_error(download_cds_requests(list(r),paths=p,dry_run=FALSE,transfer_fun=transfer),"failed_stage=download"); expect_equal(captured$dataset,r$dataset_short_name); expect_named(captured$payload,cds_api_fields); expect_false(any(c("source_grid_alignment","buffer_degrees","request_hash","target","sentinel_internal_field") %in% names(captured$payload)))
})
