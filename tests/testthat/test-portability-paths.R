test_that("committed runtime inputs do not embed drive-letter paths", {
  roots <- c("R", "config", "docs", "hpc", "scripts", "tests", "README.md")
  files <- unlist(lapply(roots, function(x) {
    path <- package_file(x)
    if (file.exists(path)) path else list.files(path, recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)
  files <- files[!dir.exists(files)]
  text <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  expect_false(any(grepl("[A-Za-z]:[/\\\\]", text)), info = "committed paths must be package-relative or external at runtime")
})

