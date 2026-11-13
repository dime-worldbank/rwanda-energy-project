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
TODAY     <- Sys.Date()

# Contact columns in the raw preload household file (PRELOAD_RAW[["households"]]). TBD: confirm
# the column names on the real beneficiary list.
CONTACT_COLS <- c(hh_head_name = "hh_head_name", primary_phone = "phone_clean", secondary_phone = "phone2_clean")

# Lowest number first; then the survey with most flags; then at random (seeded)
FLAG_PRIORITY <- c(program_stove_not_used_30d = 1, program_stove_not_confirmed = 1, program_stove_not_observed = 2,
                   used_but_not_working = 2, duplicate = 3, outlier = 4, child_cook = 5,
                   duration_short = 9)
OTHER_PRIORITY <- 7
SEED <- 20261104
HARD_HIGH_AT <- 10
HARD_N_HIGH  <- 5
HARD_N       <- 2
hard_sheet   <- paste0("hardcheck_", format(TODAY, "%Y%m%d"))
out_path     <- file.path(checks_dir, "backcheck_list.xlsx")

VERIFY_FLAGS <- c(
  "program_stove_not_used_30d",
  "program_stove_not_confirmed",
  "program_stove_not_observed",
  "used_but_not_working",
  "child_cook",
  "cook_hours_high",
  "firewood_not_source"
)


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

# Households that already appear in earlier backcheck/hardcheck sheets.
already_checked <- character()
if (file.exists(out_path)) {
  wb_prev <- loadWorkbook(out_path)
  prev_sheets <- names(wb_prev)[str_detect(names(wb_prev), "^(backcheck|hardcheck)_[0-9]{8}$")]
  if (length(prev_sheets) > 0) {
    already_checked <- map(prev_sheets, \(s) {
      dat <- readWorkbook(wb_prev, sheet = s)
      if (!("hh_id" %in% names(dat))) return(character())
      as.character(na.omit(dat$hh_id))
    }) |>
      unlist() |>
      unique()
  }
}
message("Already listed households excluded: ", length(already_checked))


# ---- 4. Build hard-check pool -------------------------------------------------

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
hard_pool <- per_survey |>
  mutate(is_hard = !str_split(flags, ";\\s*") |> map_lgl(\(x) any(x %in% VERIFY_FLAGS))) |>
  filter(is_hard) |>
  mutate(tiebreak = runif(n())) |>
  filter(!is.na(primary_phone) | !is.na(secondary_phone)) |>
  filter(!is.na(hh_id), !(hh_id %in% already_checked))

enum_quota <- hard_pool |>
  count(enumerator, name = "hard_checks_total") |>
  mutate(hard_quota = if_else(hard_checks_total >= HARD_HIGH_AT, HARD_N_HIGH, HARD_N))

picked <- hard_pool |>
  left_join(enum_quota, by = "enumerator") |>
  arrange(enumerator, desc(n_flags), priority, tiebreak) |>
  distinct(hh_id, .keep_all = TRUE) |>
  filter(row_number() <= first(hard_quota), .by = enumerator)


# ---- 5. Write ----------------------------------------------------------------

backcheck <- picked |>
  left_join(enums, by = c("enumerator" = "enumeratorid_key")) |>
  left_join(villages, by = c("village" = "villageid_key")) |>
  transmute(enumerator_id = enumerator, enumerator_name = enumerator_key, hh_id,
            survey_date = format(start, "%Y-%m-%d"), survey_start_rwanda = format(start, "%H:%M"), survey_min,
            district = district_key, sector = sector_key, cell = cell_key, village = village_key,
            hh_head_name, primary_phone, secondary_phone, hard_checks_total, hard_quota, flags, comment = flag_details, KEY,
            call_date = "", reached = "", respondent_confirms_interview = "", answers_confirmed = "", notes = "") |>
  arrange(survey_date, suppressWarnings(as.numeric(enumerator_id)))

dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)

if (file.exists(out_path)) {
  wb <- loadWorkbook(out_path)
} else {
  wb <- createWorkbook()
}

if (hard_sheet %in% names(wb)) removeWorksheet(wb, hard_sheet)
addWorksheet(wb, hard_sheet)
writeData(wb, hard_sheet, backcheck, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7"))
addStyle(wb, hard_sheet, createStyle(numFmt = "@"), rows = seq_len(nrow(backcheck)) + 1,
         cols = match(c("hh_id", "primary_phone", "secondary_phone"), names(backcheck)), gridExpand = TRUE)
setColWidths(wb, hard_sheet, seq_along(backcheck), "auto")
freezePane(wb, hard_sheet, firstRow = TRUE)
saveWorkbook(wb, out_path, overwrite = TRUE)

message(sprintf("[11.hfc_backcheck] added %s: %d hard-check surveys (%d enumerators) -> %s",
                hard_sheet, nrow(backcheck), n_distinct(backcheck$enumerator_id), out_path))
