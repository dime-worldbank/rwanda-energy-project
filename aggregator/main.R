# main.R ------------------------------------------------------------------
# Entry point. Run from the analysis/ directory (e.g. via aggregator.Rproj)
# with `source("main.R")` — reproduces every table and figure under
# output/ from a fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# `here` is needed before 00_setup.R can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("aggregator", "scripts", "00_setup.R"))         # packages + paths
source(here::here("aggregator", "scripts", "01_load.R"))          # waves_raw
source(here::here("aggregator", "scripts", "02_clean.R"))         # waves, waves_long, common_cols
source(here::here("aggregator", "scripts", "03_descriptives.R"))  # tables, figures
source(here::here("aggregator", "scripts", "04_export.R"))        # write to output/

invisible(NULL)
