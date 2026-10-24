#----------------------------------------------------------------------#
#### ASCENT phone survey: clean cooking beneficiary list            ####
#----------------------------------------------------------------------#

# Run from the repo root

#--------------------------------------
# 0. Setup
#--------------------------------------

library(tidyverse)

source("PATHS.R")

# Make sure the ASCENT paths loaded
stopifnot(exists("DROPBOX_ASCENT"), dir.exists(DROPBOX_ASCENT))
stopifnot(exists("DATA_ASCENT_PRELOAD"), dir.exists(DATA_ASCENT_PRELOAD))

#--------------------------------------
# 1. Load raw beneficiary list
#--------------------------------------

beneficiary_raw <- read_csv(
  file.path(DATA_ASCENT_PRELOAD, "clean-cooking-beneficiaries-list-raw.csv")
)

glimpse(beneficiary_raw)
