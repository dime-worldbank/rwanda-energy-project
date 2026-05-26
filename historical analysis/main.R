# main.R ------------------------------------------------------------------
# Entry point. Run from the historical analysis/ directory (e.g. via historical analysis.Rproj)
# with source("main.R") — reproduces every analysis under scripts/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("historical analysis", "scripts", "establishment_census_cleaning.R"))
source(here::here("historical analysis", "scripts", "establishment_census_analysis.R"))
source(here::here("historical analysis", "scripts", "establishment check.R"))
source(here::here("historical analysis", "scripts", "establishment_icc.R"))
source(here::here("historical analysis", "scripts", "es_usage.R"))
source(here::here("historical analysis", "scripts", "check grid data.R"))
source(here::here("historical analysis", "scripts", "analysis w usage.R"))
source(here::here("historical analysis", "scripts", "diff in diff.R"))
source(here::here("historical analysis", "scripts", "ec_2023.R"))
source(here::here("historical analysis", "scripts", "cto.R"))

invisible(NULL)
