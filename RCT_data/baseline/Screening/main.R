# main.R ------------------------------------------------------------------
# Entry point. Run from the Screening/ directory (e.g. via Screening.Rproj)
# with source("main.R") — reproduces every analysis under this directory 
# from a fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("RCT_data", "baseline", "Screening", "0.main.R"))
source(here::here("RCT_data", "baseline", "Screening", "1.screening_constr.R"))
source(here::here("RCT_data", "baseline", "Screening", "2.screening_admin.R"))
source(here::here("RCT_data", "baseline", "Screening", "3.screening_enum.R"))
source(here::here("RCT_data", "baseline", "Screening", "4.backcheck.R"))

invisible(NULL)
