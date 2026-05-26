# main.R ------------------------------------------------------------------
# Entry point. Run from the ntl/ directory (e.g. via ntl.Rproj)
# with source("main.R") — reproduces every analysis under script/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("ntl", "script", "LRCC-DVNL.R"))
source(here::here("ntl", "script", "ntl_presentation.R"))
source(here::here("ntl", "script", "viirs.R"))
source(here::here("ntl", "script", "viirs_win_pixel.R"))

invisible(NULL)
