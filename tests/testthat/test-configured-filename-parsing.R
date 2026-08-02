test_that("configured prefixes with underscores parse observed and estimated files", {
  cases <- list(
    c("mintemp_2026-07-01.tif", "mintemp", "2026-07-01", FALSE),
    c("soilmoist_2025-01-01.tif", "soilmoist", "2025-01-01", FALSE),
    c("lai_low_2025-06-01.tif", "lai_low", "2025-06-01", FALSE),
    c("relhum_min_2025-06-01.tif", "relhum_min", "2025-06-01", FALSE),
    c("lai_low_est_2027-01-01.tif", "lai_low", "2027-01-01", TRUE),
    c("relhum_min_est_2027-01-01.tif", "relhum_min", "2027-01-01", TRUE)
  )
  for (x in cases) {
    p <- parse_grid_filename(x[[1]], x[[2]])
    expect_true(p$valid); expect_equal(as.character(p$date), x[[3]]); expect_identical(p$estimated, as.logical(x[[4]]))
  }
  expect_false(parse_grid_filename("lai_low_2025-02-30.tif", "lai_low")$valid)
  expect_false(parse_grid_filename("other_2025-06-01.tif", "lai_low")$valid)
  expect_false(parse_grid_filename("lai_low_2025-06-01.nc", "lai_low")$valid)
  expect_false(parse_grid_filename("lai_low__2025-06-01.tif", "lai_low")$valid)
  expect_true(parse_grid_filename("a.b_2025-06-01.tif", "a.b")$valid)
})

