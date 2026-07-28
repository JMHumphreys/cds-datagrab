test_that("affected ISO weeks are identified", { expect_equal(identify_affected_iso_weeks(as.Date("2026-01-01")),"2026-W01") })
