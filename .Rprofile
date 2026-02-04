source("renv/activate.R")
# Project .Rprofile
# - Works whether direnv is available or not
# - Provides robust directory helpers

get_project_dir <- function() {
  # Prefer direnv/env var
  proj <- Sys.getenv("PROJECT_DIR")
  if (nzchar(proj)) return(proj)

  # Fallback: use current working directory (RStudio project root)
  getwd()
}

get_out_dir <- function() {
  out_dir <- Sys.getenv("OUT_DIR")
  if (!nzchar(out_dir)) {
    out_dir <- file.path(get_project_dir(), "outputs")
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_dir
}

get_data_dir <- function() {
  data_dir <- Sys.getenv("DATA_DIR")
  if (!nzchar(data_dir)) {
    data_dir <- file.path(get_project_dir(), "data")
  }
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  data_dir
}

get_report_dir <- function() {
  rep_dir <- Sys.getenv("REPORT_DIR")
  if (!nzchar(rep_dir)) {
    rep_dir <- file.path(get_project_dir(), "reports")
  }
  dir.create(rep_dir, showWarnings = FALSE, recursive = TRUE)
  rep_dir
}
