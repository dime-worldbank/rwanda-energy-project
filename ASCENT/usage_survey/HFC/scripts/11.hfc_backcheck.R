# ==============================================================================
# 11.hfc_backcheck.R
# Clean cooking HFC: back-check call list. Picks one flagged survey per
# enumerator per survey day (Rwanda time) from the latest HFC run
# (checks/hfc_flags.csv). "Verify" flags come first: they are plausible answers
# (e.g. program stove never used) that a call should confirm.
#
# The form collects no names or phone numbers, so contact details come from the
# raw (identified) preload household file. The list therefore contains names and
# phone numbers: it is kept out of 1.hfc_main.R so that scripts 2-10 only see
# deidentified data. Do not copy it to the folder shared with the survey firm.
#
# Run from the repo root, after 1.hfc_main.R (same HFC_MODE, set in hfc_config.R):
#   Rscript ASCENT/usage_survey/HFC/scripts/11.hfc_backcheck.R
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
})
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
source("ASCENT/usage_survey/HFC/scripts/hfc_config.R")


# ---- 2. Config / constants ---------------------------------------------------

TZ        <- "Africa/Kigali"
EXPORT_TZ <- Sys.timezone()

# Contact columns in the raw preload household file (PRELOAD_RAW[["households"]]). TBD: confirm
# the column names on the real beneficiary list.
CONTACT_COLS <- c(hh_head_name = "hh_head_name", primary_phone = "phone_clean", secondary_phone = "phone2_clean")

# Lowest number first; then the survey with most flags; then at random (seeded)
FLAG_PRIORITY <- c(program_stove_not_used_30d = 1, program_stove_not_confirmed = 1, program_stove_not_observed = 2,
                   used_but_not_working = 2, duplicate = 3, outlier = 4, child_cook = 5,
                   duration_short = 9)
OTHER_PRIORITY <- 7
SEED <- 20261104


# ---- 3. Inputs ---------------------------------------------------------------

read_chr <- \(path) read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)

flags_path <- file.path(checks_dir, "hfc_flags.csv")
if (!file.exists(flags_path)) stop("Run 1.hfc_main.R first (", flags_path, " not found).")

raw      <- read_chr(find_raw_export())
flags    <- read_chr(flags_path) |> filter(!is.na(KEY), KEY != "")   # drops enumerator-level flags
enums    <- read_chr(file.path(preload_dir, "enumerators.csv"))
villages <- read_chr(file.path(preload_dir, "scope_villages.csv"))
contacts_pre <- read_chr(file.path(raw_preload_dir, PRELOAD_RAW[["households"]]))

miss <- setdiff(unname(CONTACT_COLS), names(contacts_pre))
if (length(miss) > 0) stop(PRELOAD_RAW[["households"]], " is missing contact columns: ", paste(miss, collapse = ", "),
                           ". Update CONTACT_COLS.")
contacts_pre <- contacts_pre |> select(householdid_key, all_of(CONTACT_COLS))


# ---- 4. Pick one flagged survey per enumerator per day -----------------------

surveys <- raw |>
  filter(KEY %in% flags$KEY) |>
  transmute(KEY, hh_id, village,
            start = with_tz(mdy_hms(starttime, tz = EXPORT_TZ), TZ),
            survey_date = as_date(start),
            survey_min = round(as.numeric(survey_duration) / 60, 1)) |>
  left_join(contacts_pre, by = c("hh_id" = "householdid_key"))

per_survey <- flags |>
  mutate(flag = str_remove(check, "^[^:]+: "),
         priority = case_when(str_starts(flag, "outlier") ~ FLAG_PRIORITY[["outlier"]],
                              flag %in% names(FLAG_PRIORITY) ~ unname(FLAG_PRIORITY[flag]),
                              .default = OTHER_PRIORITY)) |>
  group_by(KEY, enumerator) |>
  summarise(flags = paste(unique(flag), collapse = "; "),
            flag_details = paste(unique(na.omit(detail)), collapse = "; "),
            n_flags = n_distinct(flag), priority = min(priority), .groups = "drop") |>
  left_join(surveys, by = "KEY")

set.seed(SEED)
picked <- per_survey |>
  mutate(tiebreak = runif(n())) |>
  filter(!is.na(primary_phone) | !is.na(secondary_phone)) |>
  arrange(survey_date, enumerator, priority, desc(n_flags), tiebreak) |>
  slice(1, .by = c(survey_date, enumerator))

no_phone_days <- anti_join(distinct(per_survey, survey_date, enumerator), picked, by = c("survey_date", "enumerator"))


# ---- 5. Write ----------------------------------------------------------------

backcheck <- picked |>
  left_join(enums, by = c("enumerator" = "enumeratorid_key")) |>
  left_join(villages, by = c("village" = "villageid_key")) |>
  transmute(enumerator_id = enumerator, enumerator_name = enumerator_key, hh_id,
            survey_date = format(start, "%Y-%m-%d"), survey_start_rwanda = format(start, "%H:%M"), survey_min,
            district = district_key, sector = sector_key, cell = cell_key, village = village_key,
            hh_head_name, primary_phone, secondary_phone, flags, flag_details, KEY,
            call_date = "", reached = "", respondent_confirms_interview = "", answers_confirmed = "", notes = "") |>
  arrange(survey_date, suppressWarnings(as.numeric(enumerator_id)))

wb <- createWorkbook()
addWorksheet(wb, "backcheck")
writeData(wb, "backcheck", backcheck, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7"))
addStyle(wb, "backcheck", createStyle(numFmt = "@"), rows = seq_len(nrow(backcheck)) + 1,
         cols = match(c("hh_id", "primary_phone", "secondary_phone"), names(backcheck)), gridExpand = TRUE)
setColWidths(wb, "backcheck", seq_along(backcheck), "auto")
freezePane(wb, "backcheck", firstRow = TRUE)

dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(checks_dir, paste0("backcheck_list_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
saveWorkbook(wb, out_path, overwrite = TRUE)

message(sprintf("[11.hfc_backcheck] %d surveys (one per enumerator per day, %d days) -> %s",
                nrow(backcheck), n_distinct(backcheck$survey_date), out_path))
if (nrow(no_phone_days) > 0) {
  warning(nrow(no_phone_days), " enumerator-day(s) left out: no flagged survey with a phone number.")
}
