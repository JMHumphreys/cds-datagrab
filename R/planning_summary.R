write_production_planning_summary <- function(run_dir, cfg, planned_dates, requests, spatial_diagnostics) {
  dates <- sort(unique(as.Date(planned_dates$date %||% planned_dates)))
  start <- as.Date(cfg$temporal$initial_start_date)
  end <- as.Date(cfg$temporal$observed_end)
  all_dates <- safe_date_sequence(start, end)
  weeks <- assess_iso_week_completeness(all_dates)
  complete <- weeks$week_id[weeks$complete]
  incomplete <- weeks$week_id[!weeks$complete]
  area <- spatial_diagnostics$final_cds_area %||% numeric()
  sha <- spatial_diagnostics$template_file_sha256 %||% NA_character_
  summary <- list(
    profile=cfg$project$profile, initial_start_date=format(start, "%Y-%m-%d"), observed_end=format(end, "%Y-%m-%d"),
    planned_date_count=length(dates), monthly_request_count=length(requests), first_planned_date=if(length(dates))format(min(dates), "%Y-%m-%d") else NULL,
    last_planned_date=if(length(dates))format(max(dates), "%Y-%m-%d") else NULL, complete_iso_week_count=length(complete),
    complete_iso_weeks=complete, incomplete_boundary_weeks=incomplete, request_area=area, template_sha256=sha,
    estimated_raw_file_count=length(requests), boundary_note="2022-01-01 and 2022-01-02 form an incomplete ISO week; the first complete weekly raster is 2022-W01."
  )
  jsonlite::write_json(summary, file.path(run_dir, "production_planning_summary.json"), pretty=TRUE, auto_unbox=TRUE, null="null")
  lines <- c("ERA5 minimum-temperature production planning summary", paste0("initial_start_date: ", summary$initial_start_date), paste0("observed_end: ", summary$observed_end), paste0("planned_dates: ", summary$planned_date_count), paste0("monthly_requests: ", summary$monthly_request_count), paste0("first_planned_date: ", summary$first_planned_date %||% "null"), paste0("last_planned_date: ", summary$last_planned_date %||% "null"), paste0("complete_iso_weeks: ", summary$complete_iso_week_count), paste0("incomplete_boundary_weeks: ", paste(incomplete, collapse=", ")), paste0("request_area: ", paste(area, collapse=",")), paste0("template_sha256: ", sha), paste0("estimated_raw_files: ", summary$estimated_raw_file_count), summary$boundary_note)
  writeLines(lines, file.path(run_dir, "production_planning_summary.txt"))
  summary
}
