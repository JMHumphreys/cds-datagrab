test_that("release documentation references existing workflow entry points", {
  refs <- c(
    "hpc/install_cdsdatagrab_atlas.sh", "hpc/preflight_cdsdatagrab.R",
    "hpc/submit_product_year.sh", "hpc/submit_era5_mintemp.sh",
    "hpc/submit_era5_soilmoist.sh", "hpc/submit_era5_lai_low.sh",
    "hpc/submit_agera5_relhum_min.sh", "config/era5_mintemp_production.yml",
    "config/era5_soilmoist_production.yml", "config/era5_lai_low_production.yml",
    "config/agera5_relhum_min_production.yml", "docs/operator_runbook.md",
    "docs/output_schema.md", "docs/production_validation_summary.md"
  )
  expect_true(all(file.exists(package_file(refs))))
  readme <- paste(readLines(package_file("README.md"), warn=FALSE), collapse="\n")
  expect_match(readme, "2022-01-01")
  expect_match(readme, "2026-12-31")
  expect_match(readme, "ecmwfr_PAT")
  expect_false(grepl("ecmwfr_PAT[=:][[:space:]]*[^`[:space:]]+", readme))
})
