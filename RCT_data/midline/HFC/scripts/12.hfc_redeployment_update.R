# ==============================================================================
# 12.hfc_redeployment_update.R
# Fill missing administrative IDs in redeployment_list.xlsx from scope_villages.
#
# Loads the HFC preloads and the redeployment workbook. Existing nonblank IDs and
# all other columns are preserved. The original workbook is archived before the
# updated candidate is copied back to OneDrive.
#
# Run from the repo root. On Windows, set TEMP, TMP, and TMPDIR to an approved
# Dropbox or OneDrive directory before starting R, because openxlsx uses R's
# temporary directory while saving workbooks.
#
#   Rscript RCT_data/midline/HFC/scripts/12.hfc_redeployment_update.R
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(openxlsx)
})

if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
t_start <- Sys.time()


# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "live"                     # "pilot" or "live"
stopifnot(HFC_MODE %in% c("pilot", "live"))

MODE_DIR <- switch(
  HFC_MODE,
  pilot = file.path(SHARED_EAQIP, "datawork/HFC_midline/pilot"),
  live = file.path(SHARED_EAQIP, "datawork/HFC_midline")
)

DEID_PRELOAD_DIR <- file.path(MODE_DIR, "deidentified/preload")
RAW_PRELOAD_DIR <- switch(
  HFC_MODE,
  pilot = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload/pilot"),
  live = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload")
)
CHECKS_DIR <- file.path(MODE_DIR, "output/checks")
REDEPLOYMENT_FILE <- file.path(CHECKS_DIR, "redeployment_list.xlsx")
REDEPLOYMENT_SOURCE <- Sys.getenv("HFC_REDEPLOYMENT_SOURCE", REDEPLOYMENT_FILE)
ARCHIVE_DIR <- file.path(CHECKS_DIR, "!archive")
DRY_RUN <- tolower(Sys.getenv("HFC_REDEPLOYMENT_DRY_RUN", "false")) %in% c("true", "1", "yes")

PRELOAD_PATHS <- c(
  enumerators = file.path(DEID_PRELOAD_DIR, "enumerators.csv"),
  household_frame = file.path(DEID_PRELOAD_DIR, "household_frame.csv"),
  shs_eligible = file.path(DEID_PRELOAD_DIR, "shs_eligible.csv"),
  scope_villages = file.path(RAW_PRELOAD_DIR, "scope_villages.csv")
)
ADMIN_NAMES <- c("district", "sector", "cell", "village")
ADMIN_KEYS <- c("district_id", "sector_id", "cell_id", "village_id")
SCOPE_NAME_COLS <- c("district_key", "sector_key", "cell_key", "village_key")
SCOPE_ID_COLS <- c("districtid_key", "sectorid_key", "cellid_key", "villageid_key")


# ---- 3. Helper functions -----------------------------------------------------

read_preload <- function(path) {
  if (!file.exists(path)) stop("Required preload not found: ", path)
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
}

normalize_name <- function(x) {
  str_to_lower(str_squish(coalesce(as.character(x), "")))
}

location_key <- function(district, sector, cell, village) {
  str_c(normalize_name(district), normalize_name(sector), normalize_name(cell),
        normalize_name(village), sep = "\r")
}

copy_windows <- function(source, destination) {
  status <- system2(
    "cmd",
    c("/c", "copy", "/Y",
      shQuote(normalizePath(source, mustWork = TRUE), type = "cmd"),
      shQuote(normalizePath(destination, mustWork = FALSE), type = "cmd")),
    stdout = FALSE,
    stderr = FALSE
  )
  status == 0
}

unreadable <- function(path, error) {
  stop("Cannot open ", basename(path), " (", conditionMessage(error), "). Close it in Excel and retry.")
}


# ---- 4. Inputs: preloads and redeployment workbook ---------------------------

if (is.na(SHARED_EAQIP) || !dir.exists(SHARED_EAQIP)) {
  stop("SHARED_EAQIP is not set or is not accessible.")
}
missing_preloads <- PRELOAD_PATHS[!file.exists(PRELOAD_PATHS)]
if (length(missing_preloads) > 0) {
  stop("Required HFC preloads are missing:\n", paste(missing_preloads, collapse = "\n"))
}
if (!file.exists(REDEPLOYMENT_SOURCE)) stop("Redeployment source workbook not found: ", REDEPLOYMENT_SOURCE)

preloads <- imap(PRELOAD_PATHS, \(path, name) {
  message("      preload: ", name)
  read_preload(path)
})

scope <- preloads$scope_villages
missing_scope_cols <- setdiff(c(SCOPE_NAME_COLS, SCOPE_ID_COLS), names(scope))
if (length(missing_scope_cols) > 0) {
  stop("scope_villages is missing columns: ", paste(missing_scope_cols, collapse = ", "))
}

scope_map <- scope |>
  transmute(
    district_key = normalize_name(district_key),
    sector_key = normalize_name(sector_key),
    cell_key = normalize_name(cell_key),
    village_key = normalize_name(village_key),
    district_id = districtid_key,
    sector_id = sectorid_key,
    cell_id = cellid_key,
    village_id = villageid_key
  ) |>
  mutate(location_key = location_key(district_key, sector_key, cell_key, village_key))

if (any(vapply(scope_map[SCOPE_NAME_COLS], \(x) any(is.na(x) | str_trim(x) == ""), logical(1))) ||
    any(vapply(scope_map[ADMIN_KEYS], \(x) any(is.na(x) | str_trim(x) == ""), logical(1))) ||
    anyDuplicated(scope_map$location_key)) {
  stop("Scope preload has incomplete or non-unique village names/IDs.")
}

approved_roots <- normalizePath(c(DROPBOX_EAQIP, DROPBOX_ASCENT, SHARED_EAQIP), winslash = "/", mustWork = TRUE) |>
  str_to_lower()
temp_path <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE) |> str_to_lower()
if (!any(temp_path == approved_roots | str_starts(temp_path, paste0(approved_roots, "/")))) {
  stop("R tempdir is outside approved Dropbox/OneDrive storage. Set TEMP, TMP, and TMPDIR before starting R.")
}

source_workbook <- REDEPLOYMENT_SOURCE
wb <- tryCatch(loadWorkbook(source_workbook), error = identity)
if (inherits(wb, "error")) {
  archived_files <- list.files(ARCHIVE_DIR, pattern = "^redeployment_list_.*\\.xlsx$",
                               full.names = TRUE)
  archived_files <- archived_files[order(file.info(archived_files)$mtime, decreasing = TRUE)]
  for (candidate_path in archived_files) {
    candidate_wb <- tryCatch(loadWorkbook(candidate_path), error = \(e) NULL)
    if (!is.null(candidate_wb)) {
      source_workbook <- candidate_path
      wb <- candidate_wb
      message("      using archived source workbook: ", basename(source_workbook))
      break
    }
  }
  if (inherits(wb, "error")) {
    stop("Cannot open the redeployment workbook or any archived copy. Close it in Excel and retry.")
  }
}
sheet_names <- names(wb)
updated_sheets <- 0L
filled_counts <- setNames(integer(length(ADMIN_KEYS)), ADMIN_KEYS)


# ---- 5. Checks: match names and fill missing IDs -----------------------------

for (sheet in sheet_names) {
  data <- read.xlsx(wb, sheet = sheet, check.names = FALSE,
                    skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  if (!all(ADMIN_NAMES %in% names(data))) next

  row_has_data <- apply(data, 1, \(row) any(!is.na(row) & str_trim(as.character(row)) != ""))
  if (!any(row_has_data)) next

  names_for_match <- map(data[ADMIN_NAMES], normalize_name)
  incomplete_names <- row_has_data & Reduce(`|`, lapply(names_for_match, `==`, ""))
  if (any(incomplete_names)) {
    stop("Sheet '", sheet, "' has ", sum(incomplete_names),
         " nonblank row(s) with incomplete district/sector/cell/village names.")
  }

  match_key <- do.call(location_key, names_for_match)
  scope_index <- match(match_key, scope_map$location_key)
  unmatched <- row_has_data & is.na(scope_index)
  if (any(unmatched)) {
    stop("Sheet '", sheet, "' has ", sum(unmatched),
         " row(s) with no matching scope_villages location; workbook was not changed.")
  }

  original_cols <- names(data)
  for (id_col in ADMIN_KEYS) {
    if (!id_col %in% names(data)) data[[id_col]] <- NA_character_
    current <- as.character(data[[id_col]])
    expected <- scope_map[[id_col]][scope_index]
    present <- row_has_data & !is.na(current) & str_trim(current) != ""
    conflict <- present & current != expected
    if (any(conflict)) {
      stop("Sheet '", sheet, "' has ", sum(conflict), " conflicting values in ", id_col,
           "; workbook was not changed.")
    }

    missing_id <- row_has_data & !present
    current[missing_id] <- expected[missing_id]
    if (any(row_has_data & (is.na(current) | str_trim(current) == ""))) {
      stop("Could not fill all ", id_col, " values on sheet '", sheet, "'.")
    }
    data[[id_col]] <- current
    filled_counts[id_col] <- filled_counts[id_col] + sum(missing_id)
  }

  writeData(wb, sheet, data, startRow = 1, startCol = 1, colNames = TRUE, keepNA = FALSE)
  id_cols <- match(ADMIN_KEYS, names(data))
  if (nrow(data) > 0) {
    addStyle(wb, sheet, createStyle(numFmt = "@"), rows = seq_len(nrow(data)) + 1L,
             cols = id_cols, gridExpand = TRUE, stack = TRUE)
  }
  updated_sheets <- updated_sheets + 1L
}

if (updated_sheets == 0L) stop("No worksheet with district, sector, cell, and village columns was found.")

if (DRY_RUN) {
  message(sprintf("Dry run: matched %d sheet(s); IDs to fill: %s; source: %s; no workbook written.",
                  updated_sheets,
                  paste(paste0(names(filled_counts), "=", filled_counts), collapse = ", "),
                  basename(source_workbook)))
} else {

# ---- 6. Export: candidate, archive, and replace -----------------------------

candidate <- tempfile(pattern = "redeployment_list_updated_", tmpdir = tempdir(), fileext = ".xlsx")
saveWorkbook(wb, candidate, overwrite = TRUE)
check_wb <- loadWorkbook(candidate)
for (sheet in sheet_names) {
  data <- tryCatch(read.xlsx(check_wb, sheet = sheet, check.names = FALSE), error = \(e) NULL)
  if (is.null(data) || !all(ADMIN_NAMES %in% names(data))) next
  if (!all(ADMIN_KEYS %in% names(data))) stop("Candidate workbook is missing admin ID columns on sheet '", sheet, "'.")
}

if (file.exists(REDEPLOYMENT_FILE)) {
  dir.create(ARCHIVE_DIR, recursive = TRUE, showWarnings = FALSE)
  backup_path <- file.path(ARCHIVE_DIR, paste0("redeployment_list_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  if (.Platform$OS.type == "windows") {
    archived <- copy_windows(REDEPLOYMENT_FILE, backup_path)
  } else {
    archived <- file.copy(REDEPLOYMENT_FILE, backup_path)
  }
  if (!archived) stop("Could not archive the original redeployment workbook; it was not changed.")
}

if (.Platform$OS.type == "windows") {
  saved <- copy_windows(candidate, REDEPLOYMENT_FILE)
} else {
  saved <- file.copy(candidate, REDEPLOYMENT_FILE, overwrite = TRUE)
}
if (!saved) stop("Could not replace the redeployment workbook. The original is archived; candidate remains at: ", candidate)

message(sprintf("Updated %d sheet(s); IDs filled: %s; done in %.1fs",
                updated_sheets,
                paste(paste0(names(filled_counts), "=", filled_counts), collapse = ", "),
                as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
}
