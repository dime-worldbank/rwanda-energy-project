# ==============================================================================
# 1.hfc_main.R
# Midline high-frequency checks (HFC): runner. Sets the run mode, all paths and
# config constants, sources scripts 2-9 in order, and exports the outputs.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/1.hfc_main.R
#
# Test mode reads the synthetic data (RCT_data/midline/HFC/test/README.md) and
# never writes to the Google Sheet.
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

t_main <- Sys.time()
message("[1.hfc_main] start: ", format(t_main, "%Y-%m-%d %H:%M:%S"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
})
options(scipen = 999)

if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")


# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "test"                               # "test" or "live"
stopifnot(HFC_MODE %in% c("test", "live"))

# Paths (only DROPBOX_EAQIP from PATHS.R; scripts 2-9 never build paths)
form_path <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx")
if (HFC_MODE == "test") {
  data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")
  preload_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data/preload")
  media_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data/media")
  output_dir  <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/output")
  audio_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/output/audio_check")
} else {
  data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/data")
  preload_dir <- file.path(DROPBOX_EAQIP, "questionnaires/02-midline/02-CTO-preload")
  media_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/data/media")
  output_dir  <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/output")
  audio_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/output/audio_check")
}
sheet_url <- NA_character_                       # TBD: Google Sheet HFC_REP_26 URL (live only)
GS4_EMAIL <- NA_character_                       # TBD: account for googlesheets4::gs4_auth() (live only)
script_dir <- "RCT_data/midline/HFC/scripts"

FORM_ID     <- "REP_readyboard_test"             # TBD: production form id
ENUM_FILE   <- "test_enumerators.csv"            # TBD: rename to production file before launch
TZ          <- "Africa/Kigali"                   # TBD: confirm export timezone on a real midline export

if (HFC_MODE == "test") {
  # Test values assumed by the synthetic answer key (test/README.md)
  FIELDWORK_START <- as.Date("2026-10-12")
  CURRENT_FORMDEF <- as.numeric(read_excel(form_path, "settings", col_types = "text")$version[1])
  DURATION_MIN    <- 15
  DURATION_MAX    <- 90
} else {
  FIELDWORK_START <- as.Date(NA)                 # TBD (date)
  CURRENT_FORMDEF <- NA_real_                    # TBD (numeric, yymmddHHMM)
  DURATION_MIN    <- NA_real_                    # TBD from pilot (minutes)
  DURATION_MAX    <- NA_real_                    # TBD from pilot (minutes)
}
if (anyNA(c(FIELDWORK_START, CURRENT_FORMDEF, DURATION_MIN, DURATION_MAX))) {
  stop("Config constants still TBD for HFC_MODE = '", HFC_MODE, "'.")
}

OUTLIER_SD             <- 3                      # +-3 SD on log(1 + x)
CONSENT_TARGET         <- 0.95                   # enumerator consent rate target (among located)
SHORT_SURVEYS_ENUM_MIN <- 3                      # TBD: "many short surveys" = at least this many per enumerator
GPS_ACCURACY_MAX       <- 8                      # TBD: metres; information column only
AUDIO_SAMPLE_N         <- 20                     # TBD: audio audits per day

SPECIAL_CODES <- c(-77, -88)                     # set to NA in continuous variables only

# Scripts, in run order
HFC_SCRIPTS <- c("2.hfc_construct.R", "3.hfc_admin.R", "4.hfc_outliers.R", "5.hfc_logic.R",
                 "6.hfc_enum.R", "7.hfc_stats.R", "8.hfc_audio.R", "9.hfc_shs.R")


# ---- 3. Helper functions -----------------------------------------------------

# Every check adds its flagged rows here (names match expected_flags.csv `check`).
# `check` is one name for all rows, or NULL to use the df's own `check` column.
hfc_flags <- tibble(KEY = character(), hh_id = character(), enumerator = character(),
                    check = character(), detail = character())
add_flags <- function(df, check = NULL, detail = NA_character_) {
  rows <- df |>
    transmute(KEY = if ("KEY" %in% names(df)) as.character(KEY) else NA_character_,
              hh_id = if ("hh_id" %in% names(df)) as.character(hh_id) else NA_character_,
              enumerator = if ("enumerator" %in% names(df)) as.character(enumerator) else NA_character_,
              check = if (is.null(check)) df$check else check, detail = as.character(detail))
  hfc_flags <<- bind_rows(hfc_flags, rows)
  invisible(nrow(rows))
}

# Every output object to export (one sheet / tab each)
hfc_outputs <- list()


# ---- 4. Run scripts ----------------------------------------------------------

for (i in seq_along(HFC_SCRIPTS)) {
  t_script <- Sys.time()
  message(sprintf("[%d/%d] %s", i, length(HFC_SCRIPTS), HFC_SCRIPTS[i]))
  source(file.path(script_dir, HFC_SCRIPTS[i]))
  message(sprintf("      done in %.1fs", as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
}


# ---- 5. Export ---------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
run_date <- format(Sys.Date(), "%Y%m%d")

hfc_outputs$run_info <- tibble(
  mode = HFC_MODE, run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), form_id = FORM_ID,
  current_formdef = CURRENT_FORMDEF, fieldwork_start = format(FIELDWORK_START),
  scripts_run = paste(HFC_SCRIPTS, collapse = ", "), submissions = nrow(hfc_constr)
)

write_xlsx(hfc_constr, file.path(output_dir, paste0("hfc_constr_", run_date, ".xlsx")))
write_xlsx(hfc_outputs, file.path(output_dir, paste0("hfc_outputs_", run_date, ".xlsx")))
write_csv(hfc_flags, file.path(output_dir, paste0("hfc_flags_", run_date, ".csv")), na = "")

if (HFC_MODE == "live") {
  if (is.na(sheet_url) || is.na(GS4_EMAIL)) stop("Set sheet_url and GS4_EMAIL before a live run.")
  googlesheets4::gs4_auth(email = GS4_EMAIL)
  hfc_sheet <- googlesheets4::gs4_get(sheet_url)
  iwalk(hfc_outputs, \(df, tab) googlesheets4::sheet_write(df, ss = hfc_sheet, sheet = tab))
}

message(sprintf("\n%d submissions, %d flags; outputs in %s", nrow(hfc_constr), nrow(hfc_flags), output_dir))
message(sprintf("[1.hfc_main] done in %.1fs", as.numeric(difftime(Sys.time(), t_main, units = "secs"))))
