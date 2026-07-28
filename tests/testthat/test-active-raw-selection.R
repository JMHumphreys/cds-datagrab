test_that("active raw selection excludes superseded hashes", {
  base <- file.path(getwd(),".test-raw-select"); if(dir.exists(base))unlink(base,recursive=TRUE);dir.create(base);on.exit(unlink(base,recursive=TRUE),add=TRUE); mk<-function(h) {p<-file.path(base,paste0("era5_mintemp_daily_2026-07_",h,".nc")); file.create(p);p}; old<-mk("be2a7361"); new<-mk("2a5e54f4"); r<-list(dataset_short_name="derived-era5-single-levels-daily-statistics",product_type="reanalysis",variable="2m_temperature",year="2026",month="07",day=c("01","02","03"),daily_statistic="daily_minimum",time_zone="utc+00:00",frequency="6_hourly",data_format="netcdf",download_format="unarchived",area=c(42.75,-126,-1.25,-34),request_hash="2a5e54f4",target=basename(new)); x<-select_active_raw_inputs(list(r),NULL,c(old,new)); expect_equal(x$active,character()); expect_true(old %in% x$superseded); expect_true(new %in% x$invalid)
})

test_that("date source mapping rejects duplicate active sources", {
  r1<-list(target="a.nc",request_hash="2a5e54f4",year="2026",month="07",day="01"); r2<-list(target="b.nc",request_hash="be2a7361",year="2026",month="07",day="01"); expect_error(map_dates_to_active_raw_sources(c("a.nc","b.nc"),list(r1,r2)),"same date")
})
