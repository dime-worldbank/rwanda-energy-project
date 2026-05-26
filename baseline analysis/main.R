# main.R ------------------------------------------------------------------
# Entry point. Run from the baseline analysis/ directory (e.g. via baseline analysis.Rproj)
# with source("main.R") — reproduces every analysis under script/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("baseline analysis", "script", "old log.R"))

invisible(NULL)
