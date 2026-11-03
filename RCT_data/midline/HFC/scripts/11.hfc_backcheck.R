# ==============================================================================
# 11.hfc_backcheck.R
# Midline HFC: back-check call list. Picks one flagged survey per enumerator per
# survey day (Rwanda time) from the latest HFC run (checks/hfc_flags.csv) and adds the respondent's name,
# household head and phone numbers from the raw export, for a back-checker to
# call. Run it after 1.hfc_main.R, with the same HFC_MODE.
#
# The list contains names and phone numbers: it reads the raw export (like
# 0.hfc_deidentify.R) and is kept out of 1.hfc_main.R so that scripts 2-10 only
# ever see deidentified data. Do not copy the list to the folder shared with the
# survey firm.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/11.hfc_backcheck.R
#
# Author: Tanay (using Claude) · Last modified: 2026-11-01
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
})
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")


# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "live"                     # same as 1.hfc_main.R: "pilot" or "live"
stopifnot(HFC_MODE %in% c("pilot", "live"))

FORM_ID <- "REP_readyboard_test"

TZ        <- "Africa/Kigali"
EXPORT_TZ <- Sys.timezone()            # time zone of the export (see 1.hfc_main.R)

MODE_DIR <- switch(HFC_MODE,
                   pilot = file.path(SHARED_EAQIP, "datawork/HFC_midline/pilot"),
                   live  = file.path(SHARED_EAQIP, "datawork/HFC_midline"))

raw_data_dir <- file.path(MODE_DIR, "data")
preload_dir  <- file.path(MODE_DIR, "deidentified/preload")
checks_dir   <- file.path(MODE_DIR, "output/checks")

# Which flagged survey to back-check per enumerator: lowest priority number first,
# then the survey with most flags, then at random (seeded, so a re-run gives the same list)
FLAG_PRIORITY <- c(observe_vs_meter_confirm = 1, duplicate = 2, outlier = 3, duration_long = 4,
                   start_end_date = 5, end_submission_date = 5, duration_short = 9)
OTHER_PRIORITY <- 7
SEED <- 20261101


# ---- 3. Inputs ---------------------------------------------------------------

active_exports <- list.files(raw_data_dir, pattern = paste0("^", FORM_ID, "_WIDE_[0-9]{6}\\.csv$"), full.names = TRUE)
if (length(active_exports) != 1) {
  stop("Expected exactly one ", FORM_ID, "_WIDE_YYMMDD.csv in:\n", raw_data_dir,
       "\nFound: ", length(active_exports))
}
message("      source export: ", basename(active_exports))

flags_path <- file.path(checks_dir, "hfc_flags.csv")
if (!file.exists(flags_path)) stop("Run 1.hfc_main.R first (", flags_path, " not found).")

read_chr <- \(path) read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)

raw      <- read_chr(active_exports)
flags    <- read_chr(flags_path) |> filter(!is.na(KEY), KEY != "")   # drops enumerator-level flags
enums    <- read_chr(file.path(preload_dir, "enumerators.csv"))
villages <- read_chr(file.path(preload_dir, "scope_villages.csv"))


# ---- 4. Pick one flagged survey per enumerator per day -----------------------

# Contact details for every flagged survey; survey day is the start date in Rwanda time.
# Phone numbers corrected during the survey take precedence over the preload numbers.
contacts <- raw |>
  filter(KEY %in% flags$KEY) |>
  transmute(KEY, hh_id, village,
            start = with_tz(mdy_hms(starttime, tz = EXPORT_TZ), TZ),
            survey_date = as_date(start),
            survey_min = round(as.numeric(survey_duration) / 60, 1),
            respondent_name = str_squish(paste(coalesce(first_name, ""), coalesce(last_name, ""))),
            hh_head_name,
            primary_phone = coalesce(phonenumber_revise, phonenumber, pull_phonenumber),
            secondary_phone = coalesce(second_phonenumber_revise, second_phonenumber, pull_second_phonenumber))

per_survey <- flags |>
  mutate(flag = str_remove(check, "^[^:]+: "),
         priority = case_when(str_starts(flag, "outlier") ~ FLAG_PRIORITY[["outlier"]],
                              flag %in% names(FLAG_PRIORITY) ~ unname(FLAG_PRIORITY[flag]),
                              .default = OTHER_PRIORITY)) |>
  group_by(KEY, enumerator) |>
  summarise(flags = paste(unique(flag), collapse = "; "),
            flag_details = paste(unique(na.omit(detail)), collapse = "; "),
            n_flags = n_distinct(flag), priority = min(priority), .groups = "drop") |>
  left_join(contacts, by = "KEY")

# Surveys with no phone number at all cannot be called back: skip them, so the next
# flagged survey of that enumerator and day is picked instead
set.seed(SEED)
picked <- per_survey |>
  mutate(tiebreak = runif(n())) |>
  filter(!is.na(primary_phone) | !is.na(secondary_phone)) |>
  arrange(survey_date, enumerator, priority, desc(n_flags), tiebreak) |>
  slice(1, .by = c(survey_date, enumerator))

# Enumerator-days whose flagged surveys all lack a phone number
no_phone_days <- anti_join(distinct(per_survey, survey_date, enumerator), picked, by = c("survey_date", "enumerator"))


# ---- 5. Write ----------------------------------------------------------------

backcheck <- picked |>
  left_join(enums, by = c("enumerator" = "enumeratorid_key")) |>
  left_join(villages, by = c("village" = "villageid_key")) |>
  transmute(enumerator_id = as.integer(enumerator), enumerator_name = enumerator_key, hh_id,
            survey_date = format(start, "%Y-%m-%d"), survey_start_rwanda = format(start, "%H:%M"), survey_min,
            district = district_key, sector = sector_key, cell = cell_key, village = village_key,
            respondent_name, hh_head_name, primary_phone, secondary_phone, flags, flag_details, KEY,
            call_date = "", reached = "", respondent_confirms_interview = "", notes = "") |>
  arrange(survey_date, enumerator_id)

wb <- createWorkbook()
addWorksheet(wb, "backcheck")
writeData(wb, "backcheck", backcheck, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7"))
# Keep the leading 0 of phone numbers and IDs in Excel
addStyle(wb, "backcheck", createStyle(numFmt = "@"), rows = seq_len(nrow(backcheck)) + 1,
         cols = match(c("hh_id", "primary_phone", "secondary_phone"), names(backcheck)), gridExpand = TRUE)
setColWidths(wb, "backcheck", seq_along(backcheck), "auto")
freezePane(wb, "backcheck", firstRow = TRUE)

out_path <- file.path(checks_dir, paste0("backcheck_list_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
saveWorkbook(wb, out_path, overwrite = TRUE)

message(sprintf("[11.hfc_backcheck] %d surveys (one per enumerator per day, %d days); %d with only a secondary phone -> %s",
                nrow(backcheck), n_distinct(backcheck$survey_date), sum(is.na(backcheck$primary_phone)), out_path))
if (nrow(no_phone_days) > 0) {
  warning(nrow(no_phone_days), " enumerator-day(s) left out: no flagged survey with a phone number (",
          paste0("enumerator ", no_phone_days$enumerator, " on ", no_phone_days$survey_date, collapse = ", "), ").")
}
