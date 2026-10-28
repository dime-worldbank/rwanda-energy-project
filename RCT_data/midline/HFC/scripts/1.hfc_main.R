# ==============================================================================
# 1.hfc_main.R
# Midline high-frequency checks (HFC): runner. Sets the run mode, all paths and
# config constants, sources scripts 2-10 in order, and exports the outputs to
# Dropbox (output_dir). The dashboard is also copied to the shared OneDrive
# folder share_dir (HFC_26/test or HFC_26/main); the previous one there goes to
# share_dir/archive.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/1.hfc_main.R
#
# Test mode reads the synthetic data (RCT_data/midline/HFC/test/README.md).
#
# Author: Yeji (using Claude) · Last modified: 2026-10-22
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
source("RCT_data/midline/HFC/scripts/hfc_privacy.R")


# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "test"                               # "test" or "live"
stopifnot(HFC_MODE %in% c("test", "live"))

FORM_ID     <- "REP_readyboard_test"             # TBD: production form id

# Deidentified HFC inputs are written by 0.hfc_deidentify.R and read here.
form_path <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx")
if (HFC_MODE == "test") {
  data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/deidentified")
  preload_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/deidentified/preload")
  media_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/deidentified/media")
  output_dir  <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/output")
  archive_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/output/archive")
  checks_dir  <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/output/checks")
} else {
  data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/deidentified")
  preload_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/deidentified/preload")
  media_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/deidentified/media")
  output_dir  <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/output")
  archive_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/output/archive")
  checks_dir  <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/output/checks")
}
if (!all(file.exists(c(file.path(data_dir, paste0(FORM_ID, "_WIDE.csv")),
                      file.path(preload_dir, "scope_villages.csv"),
                      file.path(preload_dir, "household_frame.csv"),
                      file.path(preload_dir, "shs_eligible.csv"),
                      file.path(preload_dir, "enumerators.csv"))))) {
  stop("Deidentified HFC inputs missing. Run RCT_data/midline/HFC/scripts/0.hfc_deidentify.R first.")
}
CONSTR_FILE <- file.path(output_dir, "hfc_constr.xlsx")             # current version only; old ones -> archive_dir
DASH_FILE   <- file.path(output_dir, "HFC_midline_dashboard.xlsx")  # current version only; old ones -> archive_dir
SHARE_FOLDER      <- c(test = "test_deidentified", live = "main_deidentified")
share_dir         <- file.path(ONEDRIVE_HFC_ML, SHARE_FOLDER[[HFC_MODE]])
share_archive_dir <- file.path(share_dir, "archive")
script_dir <- "RCT_data/midline/HFC/scripts"

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
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

OUTLIER_SD             <- 3                      # +-3 SD on log(1 + x)
LOCATE_TARGET          <- 0.90                   # data quality targets (AGENTS.md)
CONSENT_TARGET         <- 0.95                   # consent rate target (among located), also per enumerator
COMPLETION_TARGET      <- 0.90
LOGIC_TARGET           <- 0.05                   # share of submissions with a logic issue
SHORT_SURVEYS_ENUM_MIN <- 3                      # TBD: "many short surveys" = at least this many per enumerator

SPECIAL_CODES <- c(-77, -88)                     # set to NA in continuous variables only
ARMS          <- c("C", "T1", "T2", "T3")
RB_ARMS       <- c("T1", "T3")                   # arms offered the readyboard
SHS_ARMS      <- c("T2", "T3")                   # arms offered the SHS nudge

# Scripts, in run order. Audio is disabled for deidentified HFCs.
HFC_SCRIPTS <- c("2.hfc_construct.R", "3.hfc_admin.R", "4.hfc_outliers.R", "5.hfc_logic.R",
                 "6.hfc_enum.R", "7.hfc_stats.R", "9.hfc_shs.R", "10.hfc_dashboard.R")


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

# Move the previous version of an output into archive_dir, stamped with its last-modified time
archive_old <- function(path) {
  if (!file.exists(path)) return(invisible(TRUE))
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(archive_dir, sub("\\.xlsx$", paste0("_", format(file.mtime(path), "%Y%m%d_%H%M"), ".xlsx"),
                                     basename(path)))
  ok <- file.rename(path, dest)
  if (!ok) warning("Could not archive ", basename(path), " (open in Excel?).")
  invisible(ok)
}

# Archive the old file, then write the new one; if the file is open in Excel, write a time-stamped copy
save_current <- function(path, write_fun) {
  archive_old(path)
  tryCatch({ write_fun(path); path }, error = \(e) {
    alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%H%M"), ".xlsx"), path)
    warning("Could not write ", basename(path), " (open in Excel?); saved ", basename(alt), " instead.")
    write_fun(alt)
    alt
  })
}

# Copy the dashboard to share_dir, moving the previous one to share_archive_dir (stamped like
# archive_old()). On Windows this runs in cmd: once the HFC packages are loaded, the R process
# itself cannot write into the OneDrive folder ("No such file or directory").
share_dashboard <- function(src) {
  dest <- file.path(share_dir, basename(DASH_FILE))
  old  <- file.path(share_archive_dir, sub("\\.xlsx$", paste0("_", format(file.mtime(dest), "%Y%m%d_%H%M"), ".xlsx"),
                                           basename(dest)))
  if (.Platform$OS.type == "windows") {
    cmd <- \(...) system2("cmd", c("/c", ...), stdout = FALSE, stderr = FALSE) == 0
    win <- \(p) shQuote(normalizePath(p, mustWork = FALSE), "cmd")
    if (!dir.exists(share_archive_dir)) cmd("mkdir", win(share_archive_dir))
    moved <- !file.exists(dest) || cmd("move", "/Y", win(dest), win(old))
    ok <- moved && cmd("copy", "/Y", win(src), win(dest))
  } else {
    dir.create(share_archive_dir, recursive = TRUE, showWarnings = FALSE)
    moved <- !file.exists(dest) || file.rename(dest, old)
    ok <- moved && file.copy(src, dest)
  }
  if (!ok) warning("Could not update the dashboard in ", share_dir, " (open in Excel?).")
  invisible(ok)
}

# Run metadata (exported as `run_info`; also shown on the dashboard)
make_run_info <- function() {
  tibble(mode = HFC_MODE, run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), form_id = FORM_ID,
         current_formdef = CURRENT_FORMDEF, fieldwork_start = format(FIELDWORK_START),
         first_submission = format(min(hfc_constr$submissiondate)),
         last_submission = format(max(hfc_constr$submissiondate)),
         scripts_run = paste(HFC_SCRIPTS, collapse = ", "), submissions = nrow(hfc_constr))
}


# ---- 4. Run scripts ----------------------------------------------------------

for (i in seq_along(HFC_SCRIPTS)) {
  t_script <- Sys.time()
  message(sprintf("[%d/%d] %s", i, length(HFC_SCRIPTS), HFC_SCRIPTS[i]))
  source(file.path(script_dir, HFC_SCRIPTS[i]))
  message(sprintf("      done in %.1fs", as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
}


# ---- 5. Export ---------------------------------------------------------------

# The dashboard workbook (all output tabs) is written by 10.hfc_dashboard.R
hfc_outputs$run_info <- make_run_info()

saved_constr <- save_current(CONSTR_FILE, \(p) write_xlsx(hfc_constr, p))

# All flags in one file, for the test comparison (test/compare_expected_flags.R)
dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(hfc_flags, file.path(checks_dir, "hfc_flags.csv"), na = "")

# Current dashboard to the shared OneDrive folder; the previous one there -> share_archive_dir
share_dashboard(saved_dash)

message(sprintf("\n%d submissions, %d flags; outputs in %s; dashboard shared in %s", nrow(hfc_constr), nrow(hfc_flags),
                output_dir, share_dir))
message(sprintf("[1.hfc_main] done in %.1fs", as.numeric(difftime(Sys.time(), t_main, units = "secs"))))
