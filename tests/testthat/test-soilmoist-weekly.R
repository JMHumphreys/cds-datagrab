test_that("soil moisture weekly aggregation dispatches to arithmetic mean", {
  sm <- get_variable_spec("era5_soilmoist")
  expect_equal(sm$weekly_statistic, "mean")
  z <- aggregate_weekly_values(list(terra::rast(matrix(1,1,1)), terra::rast(matrix(3,1,1))), sm)
  expect_equal(as.numeric(terra::values(z)), 2)
})
