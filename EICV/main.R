# main.R ------------------------------------------------------------------
# Entry point. Run from the EICV/ directory (e.g. via EICV.Rproj)
# with source("main.R") — reproduces every analysis under scripts/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("EICV", "scripts", "EICV5.R"))
source(here::here("EICV", "scripts", "EICV7.R"))

invisible(NULL)
