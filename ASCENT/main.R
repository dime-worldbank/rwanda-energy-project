# main.R ------------------------------------------------------------------
# Entry point. Run from the ASCENT/ directory (e.g. via Ascent.Rproj)
# with source("main.R") — reproduces every analysis under script/ from a 
# fresh R session.

# Quiet operational logs but keep package startup hidden in the modules.
options(readr.show_progress = FALSE)

# here is needed before setup can resolve project paths.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

source(here::here("ASCENT", "script", "clean cooking.R"))
source(here::here("ASCENT", "script", "restaurant.R"))
source(here::here("ASCENT", "script", "restaurant filter.R"))
source(here::here("ASCENT", "script", "nrestaurant.R"))
source(here::here("ASCENT", "script", "Sector sheet.R"))

invisible(NULL)
