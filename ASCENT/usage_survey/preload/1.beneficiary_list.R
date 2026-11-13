#----------------------------------------------------------------------#
#### ASCENT phone survey: clean cooking beneficiary list            ####
### Purpose: Clean the raw beneficiary list 
### Categorize stoves, match official locations, and create new household IDs.
### For the evidence and decision to remake IDs, see beneficiary_cleaning_reasoning.rmd.
#----------------------------------------------------------------------#

# Run from the repo root

#--------------------------------------
# 0. Setup
#--------------------------------------

library(tidyverse)   # includes readr; janitor is called as janitor::

source("PATHS.R")

# Make sure the ASCENT paths loaded
stopifnot(exists("DROPBOX_ASCENT"), dir.exists(DROPBOX_ASCENT))
stopifnot(exists("DATA_ASCENT_PRELOAD"), dir.exists(DATA_ASCENT_PRELOAD))

# Official village list (all villages in Rwanda, with codes), used for the EAQIP baseline
stopifnot(exists("DROPBOX_EAQIP"), dir.exists(DROPBOX_EAQIP))
ADMIN_FILE <- file.path(DROPBOX_EAQIP, "questionnaires/01-baseline/vills.csv")

# Some source fields contain Windows-1252 bytes alongside valid UTF-8 text.
repair_text <- function(x) {
  invalid <- !is.na(x) & !validUTF8(x)
  converted <- iconv(x[invalid], from = "windows-1252", to = "UTF-8")
  if (anyNA(converted)) stop("Unable to decode some input text as Windows-1252.")
  x[invalid] <- converted
  x
}

#--------------------------------------
# 1. Load raw beneficiary list
#--------------------------------------

beneficiary_raw <- read_csv(
  file.path(DATA_ASCENT_PRELOAD, "clean-cooking-beneficiaries-list-raw.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  mutate(across(where(is.character), repair_text))

#--------------------------------------
# 2. Clean variable names
#--------------------------------------

beneficiary <- beneficiary_raw |>
  janitor::clean_names()

if (nrow(readr::problems(beneficiary_raw)) > 0) {
  stop("The beneficiary list has CSV parsing problems; inspect the source before continuing.")
}

parse_purchase_date <- function(x) {
  x <- str_squish(x)
  x <- na_if(x, "")

  out <- rep(as.Date(NA), length(x))
  idx <- !is.na(x)

  if (any(idx)) {
    out[idx] <- dplyr::case_when(
      str_detect(x[idx], "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$") ~ mdy(x[idx]),
      str_detect(x[idx], "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$") ~ dmy(x[idx]),
      str_detect(x[idx], "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") ~ ymd(x[idx]),
      TRUE ~ as.Date(NA)
    )
  }

  out
}

beneficiary <- beneficiary |>
  mutate(
    date_of_purchase = parse_purchase_date(date_of_purchase_confirmation_in_et),
    purchase_year_month = format(date_of_purchase, "%Y-%m"),
    paid_by_user = amount_to_be_paid_by_end_user,
    source_row = row_number()
  )

# For preload we need the stove type, whether they are LPG or EPC, and their date of purchase

#--------------------------------------
# 3. EPC and LPG categorization 
# Review -- check categorization 
#--------------------------------------

beneficiary <- beneficiary |>
  mutate(
    stove_type = case_when(
      product_name %in% c(
        "ALY-K3 Pressure cooker",
        "EBMS E Smart Pressure cooker 6L",
        "Panda EPC",
        "Teka Neza Pressure Cooker",
        "Tembo Pressure Cooker",
        "Ubushyuhe EPC"
      ) ~ "EPC",

      product_name %in% c(
        "Alliup No 1 LPG stove",
        "Alliup No 1 Metal LPG Stove",
        "Alpha 1",
        "BURNER & VALVE TIANLONG",
        "EMGAS LPG Stove",
        "EMS LPG Stove",
        "Hello Gas II",
        "Hello Gas III",
        "Hisha Eco stove 1",
        "Lemi Cookstove 12kg",
        "Love Stove",
        "Love Stove 10kg",
        "Love Stove 12 kg",
        "Neseltec S1578",
        "Super Power Gas Cooker (Double)",
        "Super Power Gas Cooker_Single",
        "Tekera rimwe",
        "Terimbere LPG Stove"
      ) ~ "LPG",

      product_name == "BBoxx 12 Kg" ~ "LPG_PAYG",

      TRUE ~ "Other"
    )
  )

#--------------------------------------
# 4. Match official administrative codes
#--------------------------------------
# The source does not contain sector names; exact district + cell + village
# matches must identify a single official village before assigning a new ID.
admin <- read_csv(ADMIN_FILE, col_types = cols(.default = col_character()),
                  show_col_types = FALSE) |>
  mutate(across(where(is.character), repair_text)) |>
  mutate(n_official = n(), .by = c(district_key, cell_key, village_key))

if (nrow(readr::problems(admin)) > 0) {
  stop("The official village list has CSV parsing problems; inspect the source before continuing.")
}
if (anyNA(admin$villageid_key) ||
    any(!str_detect(admin$villageid_key, "^[0-9]{8}$")) ||
    anyDuplicated(admin$villageid_key)) {
  stop("Official village IDs must be unique eight-digit codes.")
}
if (anyNA(admin[c("provinceid_key", "province_key", "districtid_key", "sectorid_key",
                  "sector_key", "cellid_key")]) ||
    any(str_sub(admin$villageid_key, 1, 2) != admin$districtid_key) ||
    any(str_sub(admin$villageid_key, 1, 4) != admin$sectorid_key) ||
    any(str_sub(admin$villageid_key, 1, 6) != admin$cellid_key)) {
  stop("Official administrative codes or sector names are missing or inconsistent.")
}

admin_unique <- admin |>
  filter(n_official == 1) |>
  select(district = district_key, cell = cell_key, village = village_key,
         provinceid_key, province_key, districtid_key, sectorid_key, sector_key, cellid_key, villageid_key)
admin_ambiguous <- admin |>
  filter(n_official > 1) |>
  distinct(district = district_key, cell = cell_key, village = village_key) |>
  mutate(ambiguous = TRUE)

beneficiary <- beneficiary |>
  left_join(admin_unique, by = c("district", "cell", "village")) |>
  left_join(admin_ambiguous, by = c("district", "cell", "village")) |>
  mutate(admin_match = case_when(!is.na(villageid_key) ~ "unique",
                                 ambiguous %in% TRUE ~ "ambiguous",
                                 .default = "none"),
         sector = sector_key) |>
  select(-ambiguous)

print(count(beneficiary, admin_match, name = "households"))

# New spellings in a future beneficiary list must not slip through silently
if (any(beneficiary$admin_match == "none")) {
  stop(sum(beneficiary$admin_match == "none"), " household(s) whose district + cell + village is not in ",
       basename(ADMIN_FILE), ". Check the spelling before continuing.")
}
message("Excluding ", sum(beneficiary$admin_match == "ambiguous"),
        " household(s) with ambiguous locations.")

beneficiary <- beneficiary |>
  filter(admin_match == "unique")

#--------------------------------------
# 5. Create new household IDs
#--------------------------------------
# The original household_id is retained for traceability. The new ID is the
# official village code followed by a four-digit sequence in beneficiary-name
# order. Source row breaks ties; additions or reordered ties can change IDs.
# Freeze the mapping before drawing or fielding the survey.
beneficiary <- beneficiary |>
  mutate(name_sort = str_squish(str_to_lower(str_c(
    coalesce(beneficiary_first_name, ""), coalesce(beneficiary_last_name, ""), sep = " "
  ))))

if (any(beneficiary$name_sort == "")) {
  stop(sum(beneficiary$name_sort == ""), " household(s) have no beneficiary name.")
}
village_counts <- count(beneficiary, villageid_key, name = "households")
if (any(village_counts$households > 9999)) {
  stop("At least one village has more than 9,999 households; four digits are insufficient.")
}
name_ties <- beneficiary |>
  count(villageid_key, name_sort) |>
  filter(n > 1)
message("Name ties within villages: ", nrow(name_ties), " group(s), ",
        sum(name_ties$n), " household(s); source row is the tie-breaker.")

beneficiary <- beneficiary |>
  arrange(villageid_key, name_sort, source_row, .locale = "en") |>
  mutate(householdid_key = paste0(villageid_key, sprintf("%04d", row_number())),
         .by = villageid_key) |>
  select(-name_sort, -source_row)

stopifnot(!anyDuplicated(beneficiary$householdid_key),
          all(str_detect(beneficiary$householdid_key, "^[0-9]{12}$")),
          all(str_sub(beneficiary$householdid_key, 1, 8) == beneficiary$villageid_key))

# Save output

write_csv(
  beneficiary,
  file.path(DATA_ASCENT_PRELOAD, "beneficiaries-list-intermediate.csv")
)
