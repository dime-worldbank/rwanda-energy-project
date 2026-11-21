# ==============================================================================
# 11b.hfc_backcheck_daily.R
# Midline HFC: daily back-check list, sampled from the master list (checks/backcheck_list.xlsx).
# Pool = master rows with backchecked == "NO" and at least one phone number, taken from
# hard_checks, not_located and gps_issues, sampled by quota.
# The list is saved in checks/backcheck_daily_list (never overwriting an existing list), then
# backchecked / backcheck-date are stamped for the sampled KEYs in the master.
# Before each save the master is copied to checks/!archive (as backcheck_list_YYYYMMDD_HHMMSS.xlsx).
#
# Reads only the master workbook (names and phones included): do not copy outputs to the
# folder shared with the survey firm. Run it after 11.hfc_backcheck.R, with the same HFC_MODE.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/11b.hfc_backcheck_daily.R
#
# Author: Tanay, Yeji (using Claude) · Last modified: 2026-11-19
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

HFC_MODE <- "live"                     # same as 1.hfc_main.R: "pilot", "live", or "mop-up"
stopifnot(HFC_MODE %in% c("pilot", "live", "mop-up"))

TZ <- "Africa/Kigali"

MODE_DIR <- switch(HFC_MODE,
                   pilot = file.path(SHARED_EAQIP, "datawork/HFC_midline/pilot"),
       live  = file.path(SHARED_EAQIP, "datawork/HFC_midline"),
       `mop-up` = file.path(SHARED_EAQIP, "datawork/HFC_midline"))
DROPBOX_MODE_DIR <- switch(HFC_MODE,
                           pilot = file.path(DROPBOX_EAQIP, "datawork/HFC_midline/pilot"),
         live  = file.path(DROPBOX_EAQIP, "datawork/HFC_midline"),
         `mop-up` = file.path(DROPBOX_EAQIP, "datawork/HFC_midline"))
DROPBOX_CHECKS_DIR <- file.path(DROPBOX_MODE_DIR, "output/checks")

output_dir <- switch(HFC_MODE,
                     pilot = file.path(MODE_DIR, "output"),
                     live = file.path(MODE_DIR, "output"),
                     `mop-up` = file.path(MODE_DIR, "mop-up-output"))
checks_dir  <- file.path(output_dir, "checks")
out_path    <- file.path(checks_dir, "backcheck_list.xlsx")
archive_dir <- file.path(checks_dir, "!archive")
daily_dir   <- file.path(checks_dir, "backcheck_daily_list")
daily_date  <- lubridate::today(tzone = TZ)
daily_path  <- file.path(daily_dir, paste0("backcheck_list_", format(daily_date, "%y%m%d"), ".xlsx"))

DAILY_SIZE   <- 100L
DAILY_QUOTAS <- c("hard check" = 40L, "no_locate" = 40L, "GPS" = 20L)
POOL_SHEETS  <- c("hard check" = "hard_checks", "no_locate" = "not_located", "GPS" = "gps_issues")
TEXT_COLS    <- c("KEY", "hh_id", "primary_phone", "secondary_phone", "district_id", "sector_id",
                  "cell_id", "village_id", "enumerator", "backchecked", "backcheck-date")


# ---- 3. Helper functions -----------------------------------------------------

unreadable <- \(what) \(e) stop("Cannot read ", what, " (", conditionMessage(e), "). Close it in Excel, ",
                                "check its access permissions, and re-run.")

has_phone <- function(primary, secondary) {
  nzchar(str_trim(coalesce(primary, ""))) | nzchar(str_trim(coalesce(secondary, "")))
}

copy_to_shared <- function(source, destination) {
  status <- system2(
    "cmd",
    c("/c", "copy", "/Y", shQuote(normalizePath(source, mustWork = TRUE), type = "cmd"),
      shQuote(normalizePath(destination, mustWork = FALSE), type = "cmd")),
    stdout = FALSE,
    stderr = FALSE
  )
  status == 0
}

ensure_shared_dir <- function(path) {
  if (dir.exists(path)) return(TRUE)
  status <- system2("cmd", c("/c", "mkdir", shQuote(normalizePath(path, mustWork = FALSE), type = "cmd")),
                    stdout = FALSE, stderr = FALSE)
  status == 0
}

read_master_sheet <- function(tab) {
  cols <- names(read_excel(out_path, sheet = tab, n_max = 0))
  read_excel(out_path, sheet = tab, col_types = if_else(cols %in% TEXT_COLS, "text", "guess")) |>
    mutate(across(where(is.POSIXct), as_date))
}


# ---- 4. Inputs: master list --------------------------------------------------

if (sum(DAILY_QUOTAS) != DAILY_SIZE) stop("Daily sampling quotas must add up to DAILY_SIZE.")
if (!file.exists(out_path)) stop("Run 11.hfc_backcheck.R first (", out_path, " not found).")

master_tabs <- tryCatch(excel_sheets(out_path), error = unreadable(basename(out_path)))
if (!all(POOL_SHEETS %in% master_tabs)) stop("The master workbook is missing one of: ", paste(POOL_SHEETS, collapse = ", "))
master <- map(set_names(POOL_SHEETS), \(tab) tryCatch(read_master_sheet(tab), error = unreadable(basename(out_path))))
if (!all(map_lgl(master, \(d) all(c("KEY", "backchecked", "backcheck-date") %in% names(d))))) {
  stop("The master workbook has no backchecked / backcheck-date columns; re-run 11.hfc_backcheck.R.")
}

# A KEY back-checked on any sheet counts as done on all of them
done_keys <- master |> map(\(d) filter(d, backchecked == "YES")) |> list_rbind() |> pull(KEY) |> unique()

daily_pools <- imap(POOL_SHEETS, \(tab, source) {
  data <- master[[tab]]
  if (source == "hard check") {
    data <- data |> mutate(issue = paste(unique(issue), collapse = "; "), .by = KEY)
  }
  data |>
    distinct(KEY, .keep_all = TRUE) |>
    filter(!KEY %in% done_keys, has_phone(primary_phone, secondary_phone)) |>
    mutate(source = source)
})
message(sprintf("      pool: %s", paste(names(daily_pools), map_int(daily_pools, nrow), sep = " = ", collapse = ", ")))


# ---- 5. Daily sample ---------------------------------------------------------

daily_list_exists <- file.exists(daily_path)
if (daily_list_exists) {
  message("      today's daily list exists; reusing it and re-stamping the master without resampling.")
  daily_sheets <- tryCatch(excel_sheets(daily_path), error = unreadable(basename(daily_path)))
  if (length(daily_sheets) != 1L) stop("Expected one sheet in today's daily list: ", basename(daily_path))
  daily_sample <- tryCatch(read_excel(daily_path, sheet = daily_sheets[[1]], col_types = "text"),
                           error = unreadable(basename(daily_path)))
  if (!"KEY" %in% names(daily_sample)) stop("Today's daily list has no KEY column.")
} else {
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
  # Augmenting-path matching so KEYs eligible for several sources do not block other quotas
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
}


# ---- 6. Write daily list -----------------------------------------------------

head_style <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
if (!daily_list_exists) {
  if (!ensure_shared_dir(daily_dir)) stop("Could not create the shared daily-list directory.")
  dir.create(DROPBOX_CHECKS_DIR, recursive = TRUE, showWarnings = FALSE)
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
  daily_candidate <- tempfile(pattern = paste0("backcheck_list_", format(daily_date, "%y%m%d"), "_"),
                              tmpdir = DROPBOX_CHECKS_DIR, fileext = ".xlsx")
  tryCatch(saveWorkbook(daily_wb, daily_candidate, overwrite = TRUE),
           error = \(e) stop("Cannot build the daily backcheck list in Dropbox (", conditionMessage(e), ")."))
  if (!copy_to_shared(daily_candidate, daily_path)) {
    stop("Daily list candidate is at ", daily_candidate, "; could not copy it to the shared folder.")
  }
  unlink(daily_candidate)
}


# ---- 7. Stamp the master -----------------------------------------------------

stamp_date <- format(daily_date, "%y%m%d")
sampled_keys <- as.character(daily_sample$KEY)

wb <- tryCatch(loadWorkbook(out_path), error = unreadable(basename(out_path)))
for (tab in POOL_SHEETS) {
  d <- master[[tab]] |>
    mutate(`backcheck-date` = if_else(KEY %in% sampled_keys & is.na(`backcheck-date`), stamp_date, `backcheck-date`),
           backchecked = if_else(is.na(`backcheck-date`), "NO", "YES"))
  # Only the two status columns are rewritten, so the sheet's formatting is kept
  for (col in c("backchecked", "backcheck-date")) {
    writeData(wb, tab, d[col], startCol = match(col, names(d)), startRow = 2, colNames = FALSE)
  }
}

dir.create(DROPBOX_CHECKS_DIR, recursive = TRUE, showWarnings = FALSE)
pool_candidate <- tempfile(pattern = "backcheck_pool_", tmpdir = DROPBOX_CHECKS_DIR, fileext = ".xlsx")
tryCatch(saveWorkbook(wb, pool_candidate, overwrite = TRUE),
         error = \(e) stop("Daily list was saved, but the master workbook could not be built (", conditionMessage(e), ")."))

if (!ensure_shared_dir(archive_dir)) stop("Daily list was saved, but the master archive directory is unavailable.")
backup <- file.path(archive_dir, paste0("backcheck_list_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
if (!copy_to_shared(out_path, backup)) {
  stop("Daily list was saved, but the master could not be archived. Candidate remains at ", pool_candidate)
}
if (!copy_to_shared(pool_candidate, out_path)) {
  stop("Daily list was saved, but the master could not be updated. Candidate remains at ", pool_candidate)
}
unlink(pool_candidate)

message(sprintf("[11b.hfc_backcheck_daily] daily list: %d of %d submissions -> %s",
                nrow(daily_sample), DAILY_SIZE, daily_path))
message(sprintf("      master stamped with backcheck-date %s -> %s", stamp_date, out_path))
if (nrow(daily_sample) < DAILY_SIZE) warning("Fewer eligible submissions remain than the requested daily sample size.")
