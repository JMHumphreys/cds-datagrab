# Keep deterministic request validation identical between planning and execution.
.build_variable_requests_unvalidated <- build_variable_requests
build_variable_requests <- function(...) {
  requests <- .build_variable_requests_unvalidated(...)
  if (length(requests)) {
    lapply(requests, function(request) {
      validate_cds_request_structure(request)
      validate_cds_api_payload(build_cds_api_payload(request))
      request
    })
  } else requests
}

