# main.R ------------------------------------------------------------------
# Entry point. Run from the google earth/ directory (e.g. via google earth.Rproj)
# with source("main.R") — reproduces every analysis under script/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("google earth", "script", "rgee.R"))
source(here::here("google earth", "script", "open building.R"))

invisible(NULL)
