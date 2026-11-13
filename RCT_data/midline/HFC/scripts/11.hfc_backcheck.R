# ==============================================================================
# 11.hfc_backcheck.R
# Midline HFC: cumulative back-check workbook (checks/backcheck_list.xlsx).
# hard_checks and not_located reproduce the dashboard rows with contact and GPS details.
# gps_issues is recalculated from the current midline and baseline exports each run.
# enumerator and summary show counts and overlaps by submission KEY.
# Prior daily-list keys are marked in the pool and excluded from today's quota-based sample.
# Each daily list is saved separately in checks/backcheck_daily_list, without overwriting existing lists.
# Before each save the previous workbook is copied to checks/!archive (as backcheck_list_YYYYMMDD_HHMM.xlsx).
#
# Reads the raw export (names, phones, GPS) like 0.hfc_deidentify.R, so it is kept out of
# 1.hfc_main.R: scripts 2-10 only ever see deidentified data. Do not copy the workbook to
# the folder shared with the survey firm. Run it after 1.hfc_main.R, with the same HFC_MODE.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/11.hfc_backcheck.R
#
# Author: Tanay, Yeji (using Claude) · Last modified: 2026-11-09
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
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

# Same fieldwork start as MODE_CONFIG in 1.hfc_main.R
FIELDWORK_START <- switch(HFC_MODE, pilot = as.Date("2026-10-27"), live = as.Date("2026-10-31"))

MODE_DIR <- switch(HFC_MODE,
                   pilot = file.path(SHARED_EAQIP, "datawork/HFC_midline/pilot"),
                   live  = file.path(SHARED_EAQIP, "datawork/HFC_midline"))

raw_data_dir <- file.path(MODE_DIR, "data")
preload_dir  <- file.path(MODE_DIR, "deidentified/preload")
raw_preload_dir <- switch(HFC_MODE,
                          pilot = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload/pilot"),
                          live = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload"))
checks_dir   <- file.path(MODE_DIR, "output/checks")

# Baseline exports with the household GPS (coordinate-Latitude / -Longitude)
BASELINE_EXPORTS <- c(first  = file.path(DATA_CTO, "baseline-first-round", "REP_baseline_test_WIDE.csv"),
                      second = file.path(DATA_CTO, "baseline-second-round", "REP_baseline_test_WIDE.csv"))

# GPS issue: midline GPS further than GPS_MAX_KM + midline accuracy + baseline accuracy from every
# baseline point. Accuracy is the phone's own error estimate (m); a missing accuracy counts as 0.
GPS_MAX_KM     <- 1
DAILY_SIZE     <- 100L
DAILY_QUOTAS   <- c("hard check" = 40L, "no_locate" = 40L, "GPS" = 20L)
out_path    <- file.path(checks_dir, "backcheck_list.xlsx")
archive_dir <- file.path(checks_dir, "!archive")
daily_dir   <- file.path(checks_dir, "backcheck_daily_list")
daily_date  <- lubridate::today(tzone = TZ)
daily_path  <- file.path(daily_dir, paste0("backcheck_list_", format(daily_date, "%y%m%d"), ".xlsx"))
SUMMARY_SHEETS <- c("hard_checks", "not_located", "gps_issues", "enumerator", "summary")


# ---- 3. Helper functions -----------------------------------------------------

read_chr <- \(path, ...) read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE, ...)

# Export timestamps (Korean AM/PM markers or English) in EXPORT_TZ, as Rwanda time (as in 2.hfc_construct.R)
parse_export_datetime <- function(x) {
  x <- str_replace_all(str_squish(x), c("오전" = "AM", "오후" = "PM"))
  with_tz(parse_date_time(x, orders = c("ymd p IMS", "mdy IMS p", "ymd HMS"), locale = "C",
                          tz = EXPORT_TZ, quiet = TRUE), TZ)
}

# Phone number as 07XXXXXXXX, or NA if it is not a valid Rwandan mobile number
clean_phone <- function(x) {
  d <- str_remove_all(coalesce(x, ""), "[^0-9]")
  d <- str_replace(d, "^250", "0")
  d <- if_else(str_detect(d, "^7[0-9]{8}$"), paste0("0", d), d)
  if_else(str_detect(d, "^07[0-9]{8}$"), d, NA_character_)
}

valid_phone <- function(pulled, confirm, revised) {
  if_else(str_detect(coalesce(pulled, ""), "^[0-9]+$") &
            !is.na(clean_phone(pulled)) & confirm %in% "1",
          clean_phone(pulled), revised)
}

has_phone <- function(primary, secondary) {
  nzchar(str_trim(coalesce(primary, ""))) | nzchar(str_trim(coalesce(secondary, "")))
}

# Great-circle distance in km
haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}

unreadable <- \(what) \(e) stop("Cannot read ", what, " (", conditionMessage(e), "). Close it in Excel, ",
                                "check its access permissions, and re-run.")


# ---- 4. Inputs ---------------------------------------------------------------

active_exports <- list.files(raw_data_dir, pattern = paste0("^", FORM_ID, "_WIDE_[0-9]{6}\\.csv$"), full.names = TRUE)
if (length(active_exports) != 1) {
  stop("Expected exactly one ", FORM_ID, "_WIDE_YYMMDD.csv in:\n", raw_data_dir, "\nFound: ", length(active_exports))
}
message("      source export: ", basename(active_exports))

if (file.exists(daily_path)) stop("Today's daily backcheck list already exists; it was not overwritten.")
if (sum(DAILY_QUOTAS) != DAILY_SIZE) stop("Daily sampling quotas must add up to DAILY_SIZE.")
daily_files <- list.files(daily_dir, pattern = "^backcheck_list_[0-9]{6}\\.xlsx$", full.names = TRUE)
prior_keys <- bind_rows(tibble(KEY = character(), backcheck_date = as.Date(character())), map_dfr(daily_files, function(path) {
  date_text <- str_match(basename(path), "^backcheck_list_([0-9]{6})\\.xlsx$")[, 2]
  date <- as.Date(date_text, format = "%y%m%d")
  if (is.na(date) || format(date, "%y%m%d") != date_text) stop("Invalid daily-list date: ", basename(path))
  tabs <- tryCatch(excel_sheets(path), error = unreadable(basename(path)))
  entries <- map(tabs, function(tab) {
    data <- tryCatch(read_excel(path, sheet = tab, col_types = "text"), error = unreadable(basename(path)))
    if (!"KEY" %in% names(data)) return(NULL)
    tibble(KEY = data$KEY)
  })
  if (all(map_lgl(entries, is.null))) stop("No KEY column in ", basename(path))
  bind_rows(entries) |> filter(!is.na(KEY), KEY != "") |> mutate(backcheck_date = date)
})) |>
  arrange(backcheck_date) |>
  distinct(KEY, .keep_all = TRUE) |>
  mutate(backcheck_date = format(backcheck_date, "%y%m%d"))

dashboard_path <- file.path(MODE_DIR, "output", "HFC_midline_dashboard.xlsx")
if (!file.exists(dashboard_path)) stop("Run 1.hfc_main.R first (", dashboard_path, " not found).")
dashboard_hard <- read_excel(dashboard_path, sheet = "hard_checks")
dashboard_not_located <- read_excel(dashboard_path, sheet = "not_located")

raw      <- read_chr(active_exports)
enums    <- read_chr(file.path(preload_dir, "enumerators.csv"))
villages <- read_chr(file.path(preload_dir, "scope_villages.csv"))
households <- read_chr(file.path(preload_dir, "household_frame.csv"))
contact_phones <- read_chr(file.path(raw_preload_dir, "contact_info.csv"),
                           col_select = c(householdid_key, phone_clean, phone2_clean)) |>
  transmute(hh_id = householdid_key, preload_primary = phone_clean, preload_secondary = phone2_clean)
if (anyNA(contact_phones$hh_id) || anyDuplicated(contact_phones$hh_id)) stop("Missing or duplicate household IDs in the phone preload.")
if (!"comment" %in% names(raw)) raw$comment <- NA_character_

# Baseline GPS points (both rounds; a household can have several)
missing_bl <- BASELINE_EXPORTS[!file.exists(BASELINE_EXPORTS)]
if (length(missing_bl) > 0) stop("Baseline export(s) not found:\n", paste(missing_bl, collapse = "\n"))
baseline_gps <- imap(BASELINE_EXPORTS, \(p, round) {
  read_chr(p, col_select = c(hh_id, `coordinate-Latitude`, `coordinate-Longitude`, `coordinate-Accuracy`)) |>
    transmute(hh_id, baseline_round = round,
              bl_lat = as.numeric(`coordinate-Latitude`), bl_lon = as.numeric(`coordinate-Longitude`),
              bl_accuracy_m = as.numeric(`coordinate-Accuracy`))
}) |>
  list_rbind() |>
  filter(!is.na(hh_id), !is.na(bl_lat), !is.na(bl_lon), bl_lat != 0, bl_lon != 0)

# ---- 5. Submissions ------------------------------------------------------------

# One row per midline submission in fieldwork, with names, valid phones and location
subs <- raw |>
  mutate(start = parse_export_datetime(starttime), survey_date = as_date(start),
         submission_date = as_date(parse_export_datetime(SubmissionDate), tz = TZ)) |>
  filter(!is.na(survey_date), survey_date >= FIELDWORK_START) |>
  transmute(KEY, hh_id, enumerator, village, start, survey_date, submission_date,
            survey_min = round(as.numeric(survey_duration) / 60, 1),
            hh_head_name, comment,
            primary_phone = valid_phone(pull_phonenumber, confirm_phonenumber, phonenumber_revise),
            secondary_phone = valid_phone(pull_second_phonenumber, confirm_second_phonenumber,
                                          second_phonenumber_revise),
            ml_lat = as.numeric(`obs_gps-Latitude`), ml_lon = as.numeric(`obs_gps-Longitude`),
            ml_accuracy_m = as.numeric(`obs_gps-Accuracy`)) |>
  left_join(select(enums, enumerator = enumeratorid_key, enumerator_name = enumerator_key), by = "enumerator") |>
  left_join(select(villages, village = villageid_key, district = district_key, sector = sector_key,
                   cell = cell_key, village_name = village_key), by = "village") |>
  left_join(select(households, hh_id = householdid_key, treat), by = "hh_id", relationship = "many-to-one")

if (anyNA(subs$KEY) || anyDuplicated(subs$KEY)) stop("Missing or duplicate submission KEYs in the raw export.")
if (any(!dashboard_hard$KEY %in% subs$KEY) || any(!dashboard_not_located$KEY %in% subs$KEY)) {
  stop("Dashboard submissions do not match the current raw export. Re-run the HFC before the backcheck.")
}


# ---- 6. GPS: midline vs. closest baseline point --------------------------------

gps_compare <- subs |>
  filter(!is.na(ml_lat), !is.na(ml_lon), ml_lat != 0, ml_lon != 0) |>
  select(KEY, hh_id, ml_lat, ml_lon, ml_accuracy_m) |>
  inner_join(baseline_gps, by = "hh_id", relationship = "many-to-many") |>
  mutate(distance_km = round(haversine_km(ml_lat, ml_lon, bl_lat, bl_lon), 2),
         threshold_km = round(GPS_MAX_KM + (coalesce(ml_accuracy_m, 0) + coalesce(bl_accuracy_m, 0)) / 1000, 2),
         excess_km = distance_km - threshold_km) |>
  # The baseline point that fits best (smallest distance beyond its threshold)
  slice_min(excess_km, n = 1, with_ties = FALSE, by = KEY)

gps_issues <- gps_compare |>
  filter(excess_km > 0) |>
  select(KEY, distance_km, threshold_km, baseline_round, bl_lat, bl_lon, bl_accuracy_m) |>
  inner_join(subs, by = "KEY")

n_no_ml_gps <- sum(is.na(subs$ml_lat) | subs$ml_lat %in% 0)
n_no_bl_gps <- sum(!subs$KEY %in% gps_compare$KEY) - n_no_ml_gps
message(sprintf("      GPS: %d submissions compared, %d more than %g km + accuracy from baseline; not compared: %d no midline GPS, %d no baseline GPS",
                nrow(gps_compare), nrow(gps_issues), GPS_MAX_KM, n_no_ml_gps, n_no_bl_gps))


# ---- 7. Back-check tables -----------------------------------------------------

contacts <- subs |> select(KEY, hh_head_name, primary_phone, secondary_phone)
hard_rows <- dashboard_hard |>
  left_join(contacts, by = "KEY", relationship = "many-to-one") |>
  left_join(subs |> select(KEY, sector_key = sector, cell_key = cell), by = "KEY", relationship = "many-to-one") |>
  mutate(submissiondate = as.Date(submissiondate), gps_issue = as.integer(KEY %in% gps_issues$KEY)) |>
  select(KEY, submissiondate, district_key, sector_key, cell_key, village_key, treat, hh_id, hh_head_name,
         enumerator, enumerator_key, primary_phone, secondary_phone, issue, gps_issue, comment)
not_located_rows <- dashboard_not_located |>
  left_join(contacts, by = "KEY", relationship = "many-to-one") |>
  left_join(contact_phones, by = "hh_id", relationship = "many-to-one") |>
  mutate(submissiondate = as.Date(attempt_date),
         primary_phone = preload_primary, secondary_phone = preload_secondary) |>
  select(KEY, submissiondate, district_key, sector_key, cell_key, village_key, treat, hh_id, hh_head_name,
         enumerator, enumerator_key, primary_phone, secondary_phone, nolocate, nolocate_label, nolocate_other)
gps_rows <- gps_issues |>
  arrange(desc(distance_km)) |>
  transmute(KEY, submission_date, district_key = district, sector_key = sector, cell_key = cell,
            village_key = village_name, treat, hh_id, hh_head_name, enumerator, enumerator_key = enumerator_name,
            primary_phone, secondary_phone, distance_km, comment)

enum_counts <- subs |>
  distinct(enumerator, enumerator_name) |>
  left_join(count(dashboard_hard, enumerator, name = "n_hard_checks"), by = "enumerator") |>
  left_join(count(dashboard_not_located, enumerator, name = "n_not_located"), by = "enumerator") |>
  left_join(count(gps_issues, enumerator, name = "n_gps_issues"), by = "enumerator") |>
  mutate(across(c(n_hard_checks, n_not_located, n_gps_issues), \(x) coalesce(x, 0L))) |>
  arrange(as.numeric(enumerator))

summarise_issues <- function(hard, not_located, gps, population) {
  tibble(
    population = population,
    issue = c("hard_checks", "not_located", "gps_issues"),
    total = c(nrow(hard), nrow(not_located), nrow(gps)),
    with_gps_issue = c(sum(hard$KEY %in% gps$KEY), sum(not_located$KEY %in% gps$KEY), NA_integer_),
    without_gps_issue = c(sum(!hard$KEY %in% gps$KEY), sum(!not_located$KEY %in% gps$KEY), NA_integer_),
    gps_not_compared = c(sum(!hard$KEY %in% gps_compare$KEY), sum(!not_located$KEY %in% gps_compare$KEY), NA_integer_),
    from_hard_checks = c(NA_integer_, NA_integer_, sum(gps$KEY %in% hard$KEY)),
    from_not_located_only = c(NA_integer_, NA_integer_, sum(gps$KEY %in% not_located$KEY & !gps$KEY %in% hard$KEY)),
    standalone = c(NA_integer_, NA_integer_, sum(!gps$KEY %in% hard$KEY & !gps$KEY %in% not_located$KEY))
  )
}
summary_rows <- bind_rows(
  summarise_issues(hard_rows, not_located_rows, gps_rows, "all"),
  summarise_issues(filter(hard_rows, has_phone(primary_phone, secondary_phone)),
                   filter(not_located_rows, has_phone(primary_phone, secondary_phone)),
                   filter(gps_rows, has_phone(primary_phone, secondary_phone)), "has_phone")
)

mark_backchecked <- function(data) {
  data |>
    left_join(prior_keys, by = "KEY", relationship = "many-to-one") |>
    mutate(backchecked = if_else(is.na(backcheck_date), "NO", "YES"), `backcheck-date` = backcheck_date) |>
    select(-backcheck_date)
}
hard_rows <- mark_backchecked(hard_rows)
not_located_rows <- mark_backchecked(not_located_rows)
gps_rows <- mark_backchecked(gps_rows)

daily_pools <- list(
  "hard check" = hard_rows |>
    mutate(issue = paste(unique(issue), collapse = "; "), .by = KEY) |>
    distinct(KEY, .keep_all = TRUE),
  "no_locate" = not_located_rows |> distinct(KEY, .keep_all = TRUE),
  "GPS" = gps_rows |> distinct(KEY, .keep_all = TRUE)
) |>
  imap(\(data, source) data |>
         filter(backchecked == "NO", has_phone(primary_phone, secondary_phone)) |>
         mutate(source = source))

key_sources <- bind_rows(map(daily_pools, \(data) select(data, KEY))) |>
  count(KEY, name = "n_sources")
set.seed(as.integer(format(daily_date, "%Y%m%d")))
candidate_keys <- map(daily_pools, \(data) {
  data |>
    left_join(key_sources, by = "KEY") |>
    mutate(random_order = runif(n())) |>
    arrange(n_sources, random_order) |>
    pull(KEY)
})
quota_sources <- rep(names(DAILY_QUOTAS), DAILY_QUOTAS)
quota_keys <- rep(NA_character_, length(quota_sources))
assign_quota <- function(slot, seen) {
  for (key in candidate_keys[[quota_sources[slot]]]) {
    if (exists(key, envir = seen, inherits = FALSE)) next
    assign(key, TRUE, envir = seen)
    owner <- match(key, quota_keys, nomatch = 0L)
    if (owner == 0L || assign_quota(owner, seen)) {
      quota_keys[slot] <<- key
      return(TRUE)
    }
  }
  FALSE
}
for (slot in seq_along(quota_sources)) assign_quota(slot, new.env(parent = emptyenv()))
daily_sample <- imap_dfr(daily_pools, \(data, source) {
  selected_keys <- quota_keys[quota_sources == source]
  data |> filter(KEY %in% .env$selected_keys)
})
remaining <- bind_rows(daily_pools) |>
  filter(!KEY %in% daily_sample$KEY) |>
  mutate(random_order = runif(n())) |>
  arrange(random_order) |>
  distinct(KEY, .keep_all = TRUE)
take <- min(DAILY_SIZE - nrow(daily_sample), nrow(remaining))
daily_sample <- bind_rows(daily_sample, remaining |> slice_head(n = take) |> select(-random_order)) |>
  relocate(source, .after = KEY) |>
  mutate(call_date = NA_character_, reachied = NA_character_,
         respondent_confirms_interview = NA_character_, notes = NA_character_)

sheets <- list(
  hard_checks = hard_rows,
  not_located = not_located_rows,
  gps_issues = gps_rows,
  enumerator = enum_counts,
  summary = summary_rows
)

# ---- 8. Write ----------------------------------------------------------------

if (file.exists(out_path)) {
  wb <- tryCatch(loadWorkbook(out_path), error = unreadable(basename(out_path)))
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  backup <- file.path(archive_dir, paste0("backcheck_list_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  if (!file.copy(out_path, backup)) stop("Could not archive the previous backcheck workbook; it was not changed.")
} else {
  wb <- createWorkbook()
}

head_style <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
for (nm in names(sheets)) {
  df <- sheets[[nm]]
  if ("comment" %in% names(df)) df <- df |> mutate(comment = str_replace_all(comment, "\r\n", "\n"))
  if (nm %in% names(wb)) removeWorksheet(wb, nm)
  addWorksheet(wb, nm)
  writeData(wb, nm, df, headerStyle = head_style)
  # Keep the leading 0 of phone numbers and IDs in Excel
  txt <- match(intersect(c("hh_id", "primary_phone", "secondary_phone"), names(df)), names(df))
  if (length(txt) > 0 && nrow(df) > 0) {
    addStyle(wb, nm, createStyle(numFmt = "@"), rows = seq_len(nrow(df)) + 1, cols = txt, gridExpand = TRUE)
  }
  setColWidths(wb, nm, seq_along(df), "auto")
  freezePane(wb, nm, firstRow = TRUE)
}

for (nm in intersect(c("hard_checks_all", "enumerators", "gps_issues_all"), names(wb))) removeWorksheet(wb, nm)
sheet_order <- c(SUMMARY_SHEETS, setdiff(names(wb), SUMMARY_SHEETS))
worksheetOrder(wb) <- match(sheet_order, names(wb))
activeSheet(wb) <- "summary"

dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
tryCatch(saveWorkbook(wb, out_path, overwrite = TRUE),
         error = \(e) stop("Cannot save ", basename(out_path), " (", conditionMessage(e), "). Close it in Excel and re-run."))

dir.create(daily_dir, recursive = TRUE, showWarnings = FALSE)
daily_wb <- createWorkbook()
addWorksheet(daily_wb, "backcheck")
daily_export <- daily_sample |> select(-backchecked, -`backcheck-date`)
if ("comment" %in% names(daily_export)) {
  daily_export <- daily_export |> mutate(comment = str_replace_all(comment, "\r\n", "\n"))
}
writeData(daily_wb, "backcheck", daily_export, headerStyle = head_style, withFilter = TRUE)
txt <- match(intersect(c("hh_id", "primary_phone", "secondary_phone"), names(daily_export)), names(daily_export))
if (length(txt) > 0 && nrow(daily_export) > 0) {
  addStyle(daily_wb, "backcheck", createStyle(numFmt = "@"), rows = seq_len(nrow(daily_export)) + 1,
           cols = txt, gridExpand = TRUE)
}
setColWidths(daily_wb, "backcheck", seq_along(daily_export), "auto")
freezePane(daily_wb, "backcheck", firstRow = TRUE)
tryCatch(saveWorkbook(daily_wb, daily_path, overwrite = FALSE),
         error = \(e) stop("Cannot save the daily backcheck list (", conditionMessage(e), ")."))

message(sprintf("[11.hfc_backcheck] %d hard checks, %d not located, %d GPS issues -> %s",
                nrow(hard_rows), nrow(not_located_rows), nrow(gps_rows), out_path))
message(sprintf("      daily list: %d of %d submissions -> %s", nrow(daily_sample), DAILY_SIZE, daily_path))
if (nrow(daily_sample) < DAILY_SIZE) warning("Fewer eligible submissions remain than the requested daily sample size.")
