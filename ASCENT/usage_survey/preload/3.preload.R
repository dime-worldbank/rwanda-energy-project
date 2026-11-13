# ==============================================================================
# 3.preload.R
# Build raw preload attachments for clean cooking form from sampled households.
#
# Inputs:
#   - DATA_ASCENT_PRELOAD/usage_survey_sample.csv (live/main+replacement)
#   - DATA_ASCENT_PRELOAD/usage_survey_pilot_sample.csv (pilot)
#
# Outputs (each mode writes all four):
#   - test_enumerators.csv
#   - scope_villages.csv
#   - household_head.csv
#   - stove_type.csv
#
# Destinations:
#   - Dropbox: DATA_ASCENT_PRELOAD/<main|pilot>/
#   - OneDrive: SHARED_ASCENT_CC/questionnaires/preload/<main|pilot>/
#   - Live only: DATA_ASCENT_PRELOAD/ (root compatibility copy)
#
# Run from repo root:
#   Rscript ASCENT/usage_survey/preload/3.preload.R
# Optional:
#   PRELOAD_MODE=pilot Rscript ASCENT/usage_survey/preload/3.preload.R
# ==============================================================================

suppressPackageStartupMessages(library(tidyverse))
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")

if (!exists("DATA_ASCENT_PRELOAD") || !dir.exists(DATA_ASCENT_PRELOAD)) {
  stop("DATA_ASCENT_PRELOAD is not set or does not exist.")
}
if (!exists("SHARED_ASCENT_CC") || is.na(SHARED_ASCENT_CC) || !dir.exists(SHARED_ASCENT_CC)) {
  stop("SHARED_ASCENT_CC is not set or not found. Set it in PATHS.R.")
}

PRELOAD_MODE <- Sys.getenv("PRELOAD_MODE", "live")
if (!(PRELOAD_MODE %in% c("live", "pilot"))) {
  stop("PRELOAD_MODE must be 'live' or 'pilot'.")
}
MODE_DIR <- if_else(PRELOAD_MODE == "live", "main", "pilot")

IN_SAMPLE <- if (PRELOAD_MODE == "live") {
  file.path(DATA_ASCENT_PRELOAD, "usage_survey_sample.csv")
} else {
  file.path(DATA_ASCENT_PRELOAD, "usage_survey_pilot_sample.csv")
}

if (!file.exists(IN_SAMPLE)) stop("Required input not found: ", IN_SAMPLE)

sample_df <- read_csv(IN_SAMPLE, col_types = cols(.default = col_character()), show_col_types = FALSE)
if (PRELOAD_MODE == "live") {
  sample_df <- sample_df |> filter(sample_status %in% c("main", "replacement"))
}

required_cols <- c("householdid_key", "household_id", "stove_type", "stove_type_code",
                   "product_name", "districtid_key", "sectorid_key", "cellid_key",
                   "villageid_key", "district", "sector", "cell", "village",
                   "beneficiary_first_name", "beneficiary_last_name", "phone_number")

if (length(setdiff(required_cols, names(sample_df))) > 0) {
  enrich_path <- file.path(DATA_ASCENT_PRELOAD, "beneficiaries-list-intermediate.csv")
  if (!file.exists(enrich_path)) {
    stop("Sample input is missing columns and enrichment file not found: ", enrich_path)
  }
  enrich <- read_csv(enrich_path, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
    select(householdid_key, household_id, districtid_key, sectorid_key, cellid_key, villageid_key,
           district, sector = sector_key, cell, village,
           beneficiary_first_name, beneficiary_last_name, phone_number, product_name)
  sample_df <- sample_df |>
    left_join(enrich, by = "householdid_key", suffix = c("", "_enrich"))
  for (nm in required_cols) {
    alt <- paste0(nm, "_enrich")
    if (alt %in% names(sample_df)) {
      sample_df[[nm]] <- coalesce(sample_df[[nm]], sample_df[[alt]])
    }
  }
  sample_df <- sample_df |> select(-ends_with("_enrich"))
}

missing_cols <- setdiff(required_cols, names(sample_df))
if (length(missing_cols) > 0) {
  stop("Sample input is missing columns after enrichment: ", paste(missing_cols, collapse = ", "))
}
required_nonmissing <- c("householdid_key", "stove_type", "stove_type_code", "product_name",
                         "districtid_key", "sectorid_key", "cellid_key", "villageid_key",
                         "district", "sector", "cell", "village")
if (any(sample_df[required_nonmissing] |> is.na() |> as.matrix())) {
  stop("Sample input has missing key preload values after enrichment.")
}

if (anyDuplicated(sample_df$householdid_key)) {
  stop("Sample input has duplicate householdid_key values.")
}

stove_code_map <- c(LPG = "1", LPG_PAYG = "2", EPC = "3")
bad_stove <- setdiff(unique(sample_df$stove_type), names(stove_code_map))
if (length(bad_stove) > 0) {
  stop("Unexpected stove_type values: ", paste(bad_stove, collapse = ", "))
}

sample_df <- sample_df |>
  mutate(stove_type_code = coalesce(stove_type_code, unname(stove_code_map[stove_type])))

product_code_map <- c(
  "ALY-K3 Pressure cooker" = "P001",
  "EBMS E Smart Pressure cooker 6L" = "P002",
  "Panda EPC" = "P003",
  "Teka Neza Pressure Cooker" = "P004",
  "Tembo Pressure Cooker" = "P005",
  "Ubushyuhe EPC" = "P006",
  "Alliup No 1 LPG stove" = "P007",
  "Alliup No 1 Metal LPG Stove" = "P008",
  "Alpha 1" = "P009",
  "BBoxx 12 Kg" = "P010",
  "BURNER & VALVE TIANLONG" = "P011",
  "EMGAS LPG Stove" = "P012",
  "EMS LPG Stove" = "P013",
  "Hello Gas II" = "P014",
  "Hello Gas III" = "P015",
  "Hisha Eco stove 1" = "P016",
  "Lemi Cookstove 12kg" = "P017",
  "Love Stove" = "P018",
  "Love Stove 10kg" = "P019",
  "Love Stove 12 kg" = "P020",
  "Neseltec S1578" = "P021",
  "Super Power Gas Cooker (Double)" = "P022",
  "Super Power Gas Cooker_Single" = "P023",
  "Tekera rimwe" = "P024",
  "Terimbere LPG Stove" = "P025",
  "Umunota (Induction cooker)" = "P026"
)
unknown_products <- setdiff(unique(sample_df$product_name), names(product_code_map))
if (length(unknown_products) > 0) {
  stop("Unknown product_name values; add to product_code_map: ", paste(unknown_products, collapse = ", "))
}

scope_villages <- sample_df |>
  distinct(districtid_key, sectorid_key, cellid_key, villageid_key,
           district_key = district, sector_key = sector, cell_key = cell, village_key = village) |>
  mutate(district = district_key,
         sector = sector_key,
         cell = cell_key,
         village = village_key) |>
  select(districtid_key, district_key, district,
         sectorid_key, sector_key, sector,
         cellid_key, cell_key, cell,
         villageid_key, village_key, village) |>
  arrange(district_key, sector_key, cell_key, village_key)

household_head <- sample_df |>
  transmute(householdid_key,
            household_id,
            villageid_key,
            village,
            hh_head_name = str_squish(str_c(coalesce(beneficiary_first_name, ""), coalesce(beneficiary_last_name, ""), sep = " ")),
            phone_clean = phone_number,
            phone2_clean = NA_character_)

if (any(household_head$hh_head_name == "")) {
  stop("Some sampled rows have empty hh_head_name after combining beneficiary first/last names.")
}

stove_type <- sample_df |>
  transmute(householdid_key,
            stove_type,
            stove_type_code,
            product_name,
            product_name_code = unname(product_code_map[product_name]))

if (!all(stove_type$stove_type_code %in% c("1", "2", "3"))) {
  stop("stove_type_code must be 1, 2 or 3.")
}

# One test enumerator per district in scope, keeping district fields for search() filters.
enumerators <- scope_villages |>
  distinct(districtid_key, district_key) |>
  arrange(districtid_key) |>
  transmute(districtid_key,
            district_key,
            enumeratorid_key = districtid_key,
            enumerator_key = str_c("Test Enumerator - ", district_key))

out <- list(
  "test_enumerators.csv" = enumerators,
  "scope_villages.csv" = scope_villages,
  "household_head.csv" = household_head,
  "stove_type.csv" = stove_type
)

write_bundle <- function(dir_path) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  iwalk(out, \(x, f) write_csv(x, file.path(dir_path, f), na = ""))
}

DROPBOX_MODE_DIR <- file.path(DATA_ASCENT_PRELOAD, MODE_DIR)
ONEDRIVE_MODE_DIR <- file.path(SHARED_ASCENT_CC, "questionnaires", "preload", MODE_DIR)
write_bundle(DROPBOX_MODE_DIR)
write_bundle(ONEDRIVE_MODE_DIR)

if (PRELOAD_MODE == "live") {
  write_bundle(DATA_ASCENT_PRELOAD)
}

message("Mode: ", PRELOAD_MODE)
message("Written to Dropbox mode folder: ", DROPBOX_MODE_DIR)
message("Written to OneDrive mode folder: ", ONEDRIVE_MODE_DIR)
if (PRELOAD_MODE == "live") message("Written compatibility copy to: ", DATA_ASCENT_PRELOAD)
message("Rows - enumerators: ", nrow(enumerators),
        " | villages: ", nrow(scope_villages),
        " | households: ", nrow(household_head),
        " | stove_type: ", nrow(stove_type))






