# main.R ------------------------------------------------------------------
# Entry point. Run from the historical expansion/ directory
# with source("main.R") — reproduces every analysis under script/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("historical expansion", "script", "0. Main dataset.R"))
source(here::here("historical expansion", "script", "0.1 Unified FE Dataset.R"))
source(here::here("historical expansion", "script", "0.2 Run new FE regressions.R"))
source(here::here("historical expansion", "script", "0.5 descriptives and plots.R"))
source(here::here("historical expansion", "script", "1. first stage.R"))
source(here::here("historical expansion", "script", "2.1 EARP.R"))
source(here::here("historical expansion", "script", "2.2 EARP new FE.R"))
source(here::here("historical expansion", "script", "2.3 EARP balance table.R"))

invisible(NULL)
