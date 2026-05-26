# main.R ------------------------------------------------------------------
# Entry point. Run from the March mission/ directory (e.g. via March mission.Rproj)
# with source("main.R") — reproduces every analysis under scripts/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("March mission", "scripts", "graph.R"))
source(here::here("March mission", "scripts", "november mission graph.R"))

invisible(NULL)
