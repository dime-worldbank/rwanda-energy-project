# main.R ------------------------------------------------------------------
# Entry point. Run from the intervention/ directory (e.g. via intervention.Rproj)
# with source("main.R") — reproduces every analysis under script/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("intervention", "script", "0. Treatment and scope.R"))
source(here::here("intervention", "script", "1. epc readyboard updated.R"))
source(here::here("intervention", "script", "1.5. scope verification.R"))
source(here::here("intervention", "script", "2. distance to lv.R"))
source(here::here("intervention", "script", "2. name match customer.R"))
source(here::here("intervention", "script", "3. gps match customer.R"))
source(here::here("intervention", "script", "4. Installed readyboard.R"))
source(here::here("intervention", "script", "deployment final check.R"))

invisible(NULL)
