# Keep deterministic request validation identical between planning and execution.
.build_variable_requests_unvalidated <- build_variable_requests
build_variable_requests <- function(...) {
  requests <- .build_variable_requests_unvalidated(...)
  if (length(requests)) {
    lapply(requests, function(request) {
      if (identical(request$adapter, "agera5")) {
        request$dataset_version <- request$dataset_version %||% "2_0"
        request$statistic <- list(request$daily_statistic)
        request$version <- request$dataset_version
        request$scientific_data_format <- "netcdf4"
        request$transport_format <- "portal-resolved"
        request$raw_container_format <- "unresolved"
        request$format <- NULL
      }
      validate_cds_request_structure(request)
      validate_cds_api_payload(build_cds_api_payload(request))
      request
    })
  } else requests
}
