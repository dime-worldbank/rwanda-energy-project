# main.R ------------------------------------------------------------------
# Entry point. Run from the presentation graphs/ directory (e.g. via mission graphs.Rproj)
# with source("main.R") — reproduces every analysis under scripts/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("presentation graphs", "scripts", "afe.R"))
source(here::here("presentation graphs", "scripts", "ntl.R"))
source(here::here("presentation graphs", "scripts", "october presentation.R"))
source(here::here("presentation graphs", "scripts", "september 2024.R"))
source(here::here("presentation graphs", "scripts", "dissemination 2024.R"))

invisible(NULL)
