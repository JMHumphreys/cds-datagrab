package_root <- function() fs::path_abs(testthat::test_path("..", ".."))
package_file <- function(...) fs::path(package_root(), ...)
test_external_root <- function(label) {
  root <- fs::path(tempdir(), paste0("cds-datagrab-test-", label, "-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  withr::defer(if (fs::dir_exists(root)) fs::dir_delete(root), envir = testthat::teardown_env())
  root
}
