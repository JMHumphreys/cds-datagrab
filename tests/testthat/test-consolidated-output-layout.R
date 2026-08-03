test_that("environment root precedence and canonical composition are stable", {
  root <- test_external_root("resolution"); old <- Sys.getenv(c("CDS_DATAGRAB_ROOT","CDS_DATAGRAB_PRODUCTION_ROOT","CDS_DATAGRAB_SMOKE_ROOT"), unset=""); on.exit(do.call(Sys.setenv, as.list(old)), add=TRUE)
  Sys.setenv(CDS_DATAGRAB_ROOT=file.path(root,"explicit"), CDS_DATAGRAB_PRODUCTION_ROOT=file.path(root,"production"), CDS_DATAGRAB_SMOKE_ROOT=file.path(root,"smoke"))
  cfg <- list(project=list(profile="production",dataset_id="era5_mintemp"),paths=list(root=NULL)); p <- resolve_storage_paths(cfg, package_root()); expect_match(p$root, "explicit", fixed=TRUE); expect_match(p$dataset_root, "data/production/era5_mintemp", fixed=TRUE); expect_match(p$runs_root, "runs/production/era5_mintemp", fixed=TRUE); expect_match(p$slurm_log_dir, "logs/slurm/production", fixed=TRUE)
  Sys.unsetenv("CDS_DATAGRAB_ROOT"); p <- resolve_storage_paths(cfg, package_root()); expect_match(p$root, "production", fixed=TRUE)
  cfg$project$profile <- "smoke"; Sys.unsetenv("CDS_DATAGRAB_PRODUCTION_ROOT"); p <- resolve_storage_paths(cfg, package_root()); expect_match(p$root, "smoke", fixed=TRUE)
})

test_that("documentation and audit entry points cover the consolidated contract", {
  files <- c("README.md","docs/operator_runbook.md","docs/output_schema.md","docs/consolidated_output_migration.md","docs/adding_products.md","scripts/audit_output_layout.R","hpc/init_output_root.sh")
  expect_true(all(file.exists(package_file(files)))); x <- paste(unlist(lapply(package_file(files), readLines, warn=FALSE)), collapse="\n"); expect_true(all(vapply(c("era5_mintemp","era5_soilmoist","era5_lai_low","agera5_relhum_min"), grepl, logical(1), x=x, fixed=TRUE))); expect_true(all(vapply(c("cds-datagrab-output","cds-datagrab-smoke-output"), grepl, logical(1), x=x, fixed=TRUE)))
})

test_that("explicit legacy roots warn without becoming implicit defaults", {
  cfg <- list(project=list(profile="production",dataset_id="era5_mintemp"),paths=list(root=NULL))
  expect_warning(resolve_storage_paths(cfg, package_root(), "/project/disease_ecology/cds-datagrab-production-output"), "retired product-specific")
})
