# ==============================================================================
# hfc_config.R
# Clean cooking usage survey HFC: run mode, form id and every path, in one place.
# Sourced by 0.hfc_deidentify.R, 1.hfc_main.R, 11.hfc_backcheck.R and the test
# scripts, so the mode and paths can never differ between them.
#
# Change HFC_MODE here (or set it before sourcing, as the test runner does).
# All paths start from SHARED_ASCENT_CC in PATHS.R (the shared OneDrive folder
# "ASCENT_clean_cooking"); nothing here is user-specific.
#
# Author: Tanay (using Claude) ?? Last modified: 2026-11-04
# ==============================================================================

HFC_MODE_DEFAULT <- "test"                           # "test", "pilot" or "live": change this line to switch
# Overrides, in order: HFC_MODE set before sourcing, then the HFC_MODE environment variable (test/run_test.R)
if (!exists("HFC_MODE")) HFC_MODE <- Sys.getenv("HFC_MODE", HFC_MODE_DEFAULT)
stopifnot(HFC_MODE %in% c("test", "pilot", "live"))

if (is.na(SHARED_ASCENT_CC) || !dir.exists(SHARED_ASCENT_CC)) {
  stop("SHARED_ASCENT_CC is not set or not found for user '", Sys.getenv("USERNAME"),
       "'. Set it in PATHS.R (the shared OneDrive folder ASCENT_clean_cooking).", call. = FALSE)
}

FORM_ID <- "LPG_usage_survey"                        # settings$form_id of the SurveyCTO form

HFC_ROOT   <- file.path(SHARED_ASCENT_CC, "datawork/HFC")
FORM_DIR   <- file.path(SHARED_ASCENT_CC, "questionnaires")
SHARE_ROOT <- file.path(SHARED_ASCENT_CC, "share")   # dashboards shared with the survey firm
SCRIPT_DIR <- "ASCENT/usage_survey/HFC/scripts"

# Exactly one active form in questionnaires/ (older versions go to questionnaires/archive)
find_active_form <- function() {
  f <- list.files(FORM_DIR, pattern = "^clean_cooking_questionnaire_[0-9]{6}\\.xlsx$", full.names = TRUE)
  if (length(f) != 1) {
    stop("Expected exactly one clean_cooking_questionnaire_YYMMDD.xlsx in:\n", FORM_DIR,
         "\nFound: ", length(f), ". Move older versions to questionnaires/archive.", call. = FALSE)
  }
  f
}

MODE_CONFIG <- switch(
  HFC_MODE,
  test = list(
    raw_data_dir     = file.path(HFC_ROOT, "test/synthetic_data"),
    raw_preload_dir  = file.path(HFC_ROOT, "test/synthetic_data/preload"),
    deid_dir         = file.path(HFC_ROOT, "test/deidentified"),
    output_dir       = file.path(HFC_ROOT, "test/output"),
    share_dir        = file.path(SHARE_ROOT, "test"),
    enum_file        = "test_enumerators.csv",
    versioned_export = FALSE,                       # synthetic export keeps the plain _WIDE.csv name
    fieldwork_start  = as.Date("2026-11-09")        # static: the synthetic fieldwork week
  ),
  pilot = list(
    raw_data_dir     = file.path(HFC_ROOT, "pilot/data"),
    raw_preload_dir  = file.path(HFC_ROOT, "pilot/preload"),
    deid_dir         = file.path(HFC_ROOT, "pilot/deidentified"),
    output_dir       = file.path(HFC_ROOT, "pilot/output"),
    share_dir        = file.path(SHARE_ROOT, "pilot"),
    enum_file        = "test_enumerators.csv",
    versioned_export = TRUE,
    fieldwork_start  = as.Date(NA)                  # TBD: first pilot day
  ),
  live = list(
    raw_data_dir     = file.path(HFC_ROOT, "data"),
    raw_preload_dir  = file.path(HFC_ROOT, "preload"),
    deid_dir         = file.path(HFC_ROOT, "deidentified"),
    output_dir       = file.path(HFC_ROOT, "output"),
    share_dir        = file.path(SHARE_ROOT, "main"),
    enum_file        = "test_enumerators.csv",      # TBD: production enumerator list (rename in the form too)
    versioned_export = TRUE,
    fieldwork_start  = as.Date(NA)                  # TBD: first day of live data collection
  )
)

form_path        <- find_active_form()
raw_data_dir     <- MODE_CONFIG$raw_data_dir
raw_preload_dir  <- MODE_CONFIG$raw_preload_dir
deid_dir         <- MODE_CONFIG$deid_dir
data_dir         <- deid_dir                        # what scripts 2-10 read
preload_dir      <- file.path(deid_dir, "preload")
output_dir       <- MODE_CONFIG$output_dir
archive_dir      <- file.path(output_dir, "archive")
checks_dir       <- file.path(output_dir, "checks")
share_dir        <- MODE_CONFIG$share_dir
share_archive_dir <- file.path(share_dir, "archive")
ENUM_FILE        <- MODE_CONFIG$enum_file
FIELDWORK_START  <- MODE_CONFIG$fieldwork_start

# Raw preload files used by the form (search() / pulldata()); raw = identified
PRELOAD_RAW <- c(enumerators = ENUM_FILE,
                 villages    = "scope_villages.csv",
                 households  = "household_head.csv",
                 stoves      = "stove_type.csv")   # from ASCENT/usage_survey/preload/3.preload.R

# Raw SurveyCTO export: plain name in test, FORM_ID_WIDE_YYMMDD.csv otherwise (one active file)
find_raw_export <- function() {
  if (!MODE_CONFIG$versioned_export) return(file.path(raw_data_dir, paste0(FORM_ID, "_WIDE.csv")))
  f <- list.files(raw_data_dir, pattern = paste0("^", FORM_ID, "_WIDE_[0-9]{6}\\.csv$"), full.names = TRUE)
  if (length(f) != 1) {
    stop("Expected exactly one ", FORM_ID, "_WIDE_YYMMDD.csv in:\n", raw_data_dir,
         "\nFound: ", length(f), ". Move older exports to an archive folder.", call. = FALSE)
  }
  f
}


