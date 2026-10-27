# Create a deidentified copy of the HFC SurveyCTO export and preload files.
# This is the pre-step before running 1.hfc_main.R. It preserves hh_id but strips
# identifiable fields such as names, phone numbers, GPS, and media paths.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")

HFC_MODE <- "test" # or "live"
stopifnot(HFC_MODE %in% c("test", "live"))

FORM_ID <- "REP_readyboard_test"
ENUM_FILE <- if (HFC_MODE == "test") "test_enumerators.csv" else "enumerators.csv"

raw_data_dir <- if (HFC_MODE == "test") {
  file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")
} else {
  file.path(DROPBOX_EAQIP, "datawork/HFC_midline/data")
}
raw_preload_dir <- if (HFC_MODE == "test") {
  file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data/preload")
} else {
  file.path(DROPBOX_EAQIP, "questionnaires/02-midline/02-CTO-preload")
}

deid_dir <- if (HFC_MODE == "test") {
  file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/deidentified")
} else {
  file.path(DROPBOX_EAQIP, "datawork/HFC_midline/deidentified")
}
preload_dir <- file.path(deid_dir, "preload")

IDENTIFYING_COLS <- c(
  "first_name", "last_name", "hh_head_name", "deviceid", "devicephonenum",
  "pull_phonenumber", "phonenumber", "phonenumber_revise",
  "pull_second_phonenumber", "second_phonenumber", "second_phonenumber_revise",
  "meter_number", "meter_photo", "readyboard_photo", "shs_photo",
  "audio", "text_audit", "location", "obs_gps", "location-Accuracy", "obs_gps-Accuracy",
  "coordinate.Latitude", "coordinate.Longitude", "coordinate.Altitude", "coordinate.Accuracy",
  "nid", "NID"
)

export_path <- file.path(raw_data_dir, paste0(FORM_ID, "_WIDE.csv"))
raw <- read_csv(export_path, col_types = cols(.default = col_guess()), show_col_types = FALSE, guess_max = 10000)
raw_deid <- raw |> select(-any_of(IDENTIFYING_COLS))

dir.create(preload_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(raw_deid, file.path(deid_dir, paste0(FORM_ID, "_WIDE.csv")), na = "")

read_preload <- function(file) {
  path <- file.path(raw_preload_dir, file)
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE,
           name_repair = "unique_quiet") |>
    select(-any_of(IDENTIFYING_COLS)) |>
    select(-starts_with("..."))
}

walk(c("contact_info.csv", "shs_eligible.csv", "scope_villages.csv", ENUM_FILE), \(file) {
  x <- read_preload(file)
  write_csv(x, file.path(preload_dir, file), na = "")
})

message(sprintf("Wrote deidentified HFC inputs to %s", deid_dir))
