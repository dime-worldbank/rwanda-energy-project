# ==============================================================================
# 1.hfc_main.R
# Clean cooking usage survey HFC: runner. Reads the mode and paths from
# hfc_config.R, sets the check constants, sources scripts 2-10 in order, and
# writes the outputs to output_dir (OneDrive, core team). The dashboard is also
# copied to share_dir (shared with the survey firm); the previous one there goes
# to share_dir/archive.
#
# Run 0.hfc_deidentify.R first. Then, from the repo root:
#   Rscript ASCENT/usage_survey/HFC/scripts/1.hfc_main.R
#
# Same structure as RCT_data/midline/HFC (EAQIP midline). Differences: strata are
# the preload stove types (no treatment arms), logic flags are split into
# "inconsistent" (count toward the logic target) and "verify" (plausible but
# worth a back-check call), and 9.hfc_usage.R previews the report parameters.
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
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
source("ASCENT/usage_survey/HFC/scripts/hfc_config.R")
source("ASCENT/usage_survey/HFC/scripts/hfc_privacy.R")
message("      mode: ", HFC_MODE, " | form: ", basename(form_path))


# ---- 2. Config / constants ---------------------------------------------------

TZ        <- "Africa/Kigali"
EXPORT_TZ <- Sys.timezone()      # SurveyCTO export times are in the downloading computer's time zone

DURATION_MIN <- 10               # TBD from pilot (minutes)
DURATION_MAX <- 60               # TBD from pilot (minutes)
UPLOAD_DELAY_DAYS <- 2           # flag uploads this many days or more after the survey ended

OUTLIER_SD <- 3

LOCATE_TARGET     <- 0.90
CONSENT_TARGET    <- 0.95
COMPLETION_TARGET <- 0.90
LOGIC_TARGET      <- 0.05        # share of submissions with an "inconsistent" logic flag

SHORT_SURVEYS_ENUM_MIN <- 3
SPECIAL_CODES <- c(-77, -88)

# Strata: preload stove type (choice list cookingmethods)
STOVE_TYPES <- c("1" = "LPG", "2" = "PAYG LPG", "3" = "EPC")
TRADITIONAL_STOVES <- c("4", "5", "6")           # charcoal, firewood, briquettes/pellets

# Midpoints (% of cooking) for the cookshare choices
SHARE_MID <- c("1" = 5, "2" = 15, "3" = 25, "4" = 50, "5" = 75, "6" = 95, "7" = 100)
SHARE_SUM_RANGE <- c(60, 140)    # sum of shares across stoves used in the last 7 days
COOK_HOURS_MAX <- 70             # weekly cooking hours of one member worth verifying
CHILD_COOK_AGE <- 10             # TBD: a cooking member younger than this is worth verifying

if (is.na(FIELDWORK_START) || anyNA(c(DURATION_MIN, DURATION_MAX))) {
  stop("Config constants still TBD for HFC_MODE = '", HFC_MODE, "' (FIELDWORK_START in hfc_config.R).")
}

required_inputs <- c(file.path(data_dir, paste0(FORM_ID, "_WIDE.csv")),
                     file.path(preload_dir, c("household_frame.csv", "scope_villages.csv", "enumerators.csv")))
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("Deidentified inputs missing:\n", paste0("  - ", missing_inputs, collapse = "\n"),
       "\nRun ASCENT/usage_survey/HFC/scripts/0.hfc_deidentify.R first.")
}

FORM_VERSION_TEXT <- read_excel(form_path, "settings", col_types = "text")$version[1]
CURRENT_FORMDEF <- suppressWarnings(as.numeric(FORM_VERSION_TEXT))
if (is.na(CURRENT_FORMDEF)) {
  warning("Form version '", FORM_VERSION_TEXT, "' in ", basename(form_path), " is not numeric. SurveyCTO needs a ",
          "numeric version (e.g. 2611041500): fix the settings sheet before deploying.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
CONSTR_FILE <- file.path(output_dir, "hfc_constr.xlsx")
DASH_FILE   <- file.path(output_dir, "HFC_cleancooking_dashboard.xlsx")

HFC_SCRIPTS <- c("2.hfc_construct.R", "3.hfc_admin.R", "4.hfc_outliers.R", "5.hfc_logic.R",
                 "6.hfc_enum.R", "7.hfc_stats.R", "9.hfc_usage.R", "10.hfc_dashboard.R")


# ---- 3. Helper functions -----------------------------------------------------

# Every check adds its flagged rows here (names match expected_flags.csv `check`)
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

# Archive the old file, then write the new one; if it is open in Excel, write a time-stamped copy
save_current <- function(path, write_fun) {
  archive_old(path)
  tryCatch({ write_fun(path); path }, error = \(e) {
    alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%H%M"), ".xlsx"), path)
    warning("Could not write ", basename(path), " (open in Excel?); saved ", basename(alt), " instead.")
    write_fun(alt)
    alt
  })
}

# Copy the dashboard to share_dir, moving the previous one to share_archive_dir. On Windows this
# runs in cmd (as in the midline HFC: the R process may not be able to write there directly).
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

make_run_info <- function() {
  tibble(mode = HFC_MODE, run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), form_id = FORM_ID,
         form_file = basename(form_path), current_formdef = CURRENT_FORMDEF,
         fieldwork_start = format(FIELDWORK_START),
         first_submission = format(min(hfc_constr$submissiondate)),
         last_submission = format(max(hfc_constr$submissiondate)),
         scripts_run = paste(HFC_SCRIPTS, collapse = ", "), submissions = nrow(hfc_constr),
         user = Sys.getenv("USERNAME"))
}


# ---- 4. Run scripts ----------------------------------------------------------

for (i in seq_along(HFC_SCRIPTS)) {
  t_script <- Sys.time()
  message(sprintf("[%d/%d] %s", i, length(HFC_SCRIPTS), HFC_SCRIPTS[i]))
  source(file.path(SCRIPT_DIR, HFC_SCRIPTS[i]))
  message(sprintf("      done in %.1fs", as.numeric(difftime(Sys.time(), t_script, units = "secs"))))
}


# ---- 5. Export ---------------------------------------------------------------

saved_constr <- save_current(CONSTR_FILE, \(p) write_xlsx(hfc_constr, p))

dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(hfc_flags, file.path(checks_dir, "hfc_flags.csv"), na = "")

# Usage parameters also as CSV: the input for the report pipeline
write_csv(hfc_outputs$usage_params, file.path(output_dir, "usage_params.csv"), na = "")

share_dashboard(saved_dash)

message(sprintf("\n%d submissions, %d flags; outputs in %s; dashboard shared in %s",
                nrow(hfc_constr), nrow(hfc_flags), output_dir, share_dir))
message(sprintf("[1.hfc_main] done in %.1fs", as.numeric(difftime(Sys.time(), t_main, units = "secs"))))
