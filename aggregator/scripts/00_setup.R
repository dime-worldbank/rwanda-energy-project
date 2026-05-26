
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
dropbox <- 'C:/Users/wb614406/Dropbox'

aggregator_path <- file.path(dropbox, "Rwanda Feeder Roads", "Trucker Experiment", "SCTO_backup", "Aggregator" )
paths <- list(
  baseline = file.path(aggregator_path, "Baseline", "data", "Aggregator Baseline.csv"),
  midline  = file.path(aggregator_path, "Midline",  "data", "Aggregator Midline.csv"),
  endline  = file.path(aggregator_path, "Endline",  "data", "Aggregator endline survey.csv"),
  out_tab  = file.path(aggregator_path, "outputs", "tables"),
  out_fig  = file.path(aggregator_path, "outputs", "figures")
)

dir.create(paths$out_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$out_fig, recursive = TRUE, showWarnings = FALSE)

stopifnot(
  "Baseline CSV not found" = file.exists(paths$baseline),
  "Midline CSV not found"  = file.exists(paths$midline),
  "Endline CSV not found"  = file.exists(paths$endline)
)
