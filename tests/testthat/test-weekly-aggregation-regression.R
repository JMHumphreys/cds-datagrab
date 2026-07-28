test_that("three dates are an incomplete ISO week and a nonfatal no-op", {
  x<-assess_iso_week_completeness(as.Date(c("2026-07-01","2026-07-02","2026-07-03"))); expect_equal(nrow(x),1); expect_false(x$complete); expect_equal(x$available_count,3); expect_equal(length(x$missing_dates[[1]]),4)
  inv<-data.frame(path=paste0("x",1:3),date=as.Date(c("2026-07-01","2026-07-02","2026-07-03")),estimated=FALSE,observed_valid=TRUE); z<-aggregate_daily_to_weekly(tempdir(),tempdir(),inventory=inv,template_path="unused"); expect_equal(z$status,"success_noop"); expect_length(z$written,0); expect_equal(nrow(z$incomplete_weeks),1)
})

test_that("ISO weeks spanning years are assigned to the correct week", {
  x<-assess_iso_week_completeness(as.Date(c("2020-12-28","2020-12-29","2021-01-03"))); expect_equal(x$week_id,"2020-W53"); expect_equal(x$week_start,as.Date("2020-12-28")); expect_equal(x$week_end,as.Date("2021-01-03"))
})

test_that("week assessment handles reverse, unsorted, empty, and multi-week dates", {
  d<-as.Date(c("2026-07-03","2026-07-01","2026-07-02")); expect_equal(assess_iso_week_completeness(rev(d))$week_start,as.Date("2026-06-29")); expect_equal(nrow(assess_iso_week_completeness(c(d,as.Date("2026-07-06")))),2); expect_equal(nrow(assess_iso_week_completeness(as.Date(character()))),0); expect_equal(iso_week_start(as.Date("2026-07-01")),as.Date("2026-06-29")); expect_error(assess_iso_week_completeness(as.Date(NA)),"invalid")
})

test_that("complete July 6-12 week matches list-column dates and computes cellwise minima", {
  skip_if_not_installed("terra"); base<-file.path(getwd(),".test-complete-week");if(dir.exists(base))unlink(base,recursive=TRUE);dir.create(base);on.exit(unlink(base,recursive=TRUE),add=TRUE);t<-terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326");terra::values(t)<-1;tp<-file.path(base,"template.tif");terra::writeRaster(t,tp,overwrite=TRUE);ds<-as.Date("2026-07-06")+0:6;paths<-vapply(seq_along(ds),function(i){r<-t;terra::values(r)<-c(i,10-i,20-i,30-i);p<-file.path(base,format_daily_filename("mintemp",ds[i]));terra::writeRaster(r,p,overwrite=TRUE);p},character(1));inv<-data.frame(path=paths,date=ds,estimated=FALSE,observed_valid=TRUE);z<-aggregate_daily_to_weekly(base,base,inventory=inv,template_path=tp,overwrite=TRUE);expect_equal(z$status,"success");expect_length(z$written,1);expect_equal(basename(z$written),"mintemp_2026-W28.tif");out<-terra::rast(z$written);expect_equal(as.numeric(terra::values(out)),c(1,3,13,23));expect_equal(unname(z$match_counts[["2026-W28"]]),rep(1L,7))
})
