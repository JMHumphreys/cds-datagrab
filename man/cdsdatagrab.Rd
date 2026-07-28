\name{cdsdatagrab}
\alias{cdsdatagrab}
\alias{read_pipeline_config}\alias{resolve_project_root}\alias{expand_config_environment}\alias{resolve_config_paths}\alias{validate_pipeline_config}\alias{ensure_pipeline_directories}
\alias{format_daily_filename}\alias{format_weekly_filename}\alias{parse_grid_filename}
\alias{inventory_grid_directory}\alias{summarize_grid_inventory}\alias{find_inventory_conflicts}
\alias{expected_daily_dates}\alias{find_missing_dates}\alias{compress_dates_to_intervals}\alias{split_dates_by_year_month}\alias{plan_observed_update}
\alias{bbox_to_cds_area}\alias{diagnose_spatial_domain}\alias{validate_template_crs}\alias{template_fingerprint}\alias{validate_template_geometry}
\alias{build_era5_mintemp_requests}\alias{validate_cds_request_structure}\alias{write_cds_request_manifests}\alias{download_cds_requests}
\alias{normalize_downloaded_file}\alias{discover_netcdf_files}\alias{inspect_netcdf_file}\alias{extract_daily_layers}\alias{standardize_daily_raster}\alias{process_downloaded_mintemp}
\alias{validate_raster_against_template}\alias{validate_daily_output}\alias{validate_weekly_output}\alias{identify_affected_iso_weeks}\alias{aggregate_daily_to_weekly}
\alias{build_daily_climatology}\alias{build_weekly_climatology}\alias{fill_future_daily_analogs}\alias{fill_future_weekly_analogs}
\alias{initialize_run_manifest}\alias{record_manifest_event}\alias{finalize_run_manifest}\alias{run_era5_mintemp_pipeline}
\title{Configuration-driven ERA5 minimum-temperature pipeline}
\description{Reusable functions for inventory, planning, spatial standardization, validation, aggregation, and audit manifests.}
\keyword{internal}
