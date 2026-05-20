# 00_setup.R --------------------------------------------------------------
# Load packages and define project paths. Sourced first by main.R.

required_pkgs <- c(
  "here",       # project-relative paths
  "readr",      # fast CSV reader
  "dplyr",      # data wrangling
  "tidyr",      # reshaping
  "stringr",    # string helpers
  "lubridate",  # dates
  "janitor",    # clean_names, tabyl
  "purrr",      # functional helpers
  "tibble",     # tibbles
  "gtsummary",  # publication tables
  "gt",         # save gt tables
  "ggplot2",    # plots
  "scales"      # axis formatters
)

missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

suppressPackageStartupMessages({
  invisible(lapply(required_pkgs, library, character.only = TRUE))
})

# Paths -------------------------------------------------------------------
# The Rproj sits in Aggregator/analysis/, so the raw wave folders are one
# level up. here::here() resolves to the project root regardless of OS.

paths <- list(
  baseline = here::here("..", "Baseline", "data", "Aggregator Baseline.csv"),
  midline  = here::here("..", "Midline",  "data", "Aggregator Midline.csv"),
  endline  = here::here("..", "Endline",  "data", "Aggregator endline survey.csv"),
  out_tab  = here::here("output", "tables"),
  out_fig  = here::here("output", "figures")
)

dir.create(paths$out_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$out_fig, recursive = TRUE, showWarnings = FALSE)

stopifnot(
  "Baseline CSV not found" = file.exists(paths$baseline),
  "Midline CSV not found"  = file.exists(paths$midline),
  "Endline CSV not found"  = file.exists(paths$endline)
)
