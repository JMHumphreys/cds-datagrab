#!/usr/bin/env Rscript
# Read-only cleanup audit. No files are removed unless an explicit future
# implementation adds a validated --execute path.
args <- commandArgs(trailingOnly = TRUE)
root <- Sys.getenv("CDS_DATAGRAB_ROOT", "")
if (!nzchar(root)) stop("Set CDS_DATAGRAB_ROOT to audit an external output root.", call. = FALSE)
root <- normalizePath(root, winslash = "/", mustWork = FALSE)
if (identical(root, "/") || !dir.exists(root)) stop("Refusing missing or root cleanup target.", call. = FALSE)
files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
rows <- data.frame(path = files, finding = character(length(files)), stringsAsFactors = FALSE)
rows$finding[grepl("/\\.partial(/|$)|\\.part$", gsub("\\\\", "/", files))] <- "stale_partial_candidate"
rows$finding[grepl("\\.zip$", files, ignore.case = TRUE) & grepl("\\.nc$|\\.netcdf$", files, ignore.case = TRUE)] <- "archive_wrong_extension_candidate"
rows <- rows[nzchar(rows$finding), , drop = FALSE]
if (length(args) && identical(args[[1]], "--execute")) stop("Destructive cleanup is intentionally disabled in this audit script.", call. = FALSE)
print(rows, row.names = FALSE)
