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
