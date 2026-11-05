# ==============================================================================
# 2.sampling.R
# ASCENT clean cooking usage survey: sample draw and form preload.
#
# Design (agreed 2026-11-05, Yeji):
#   - Frame: beneficiaries with a household ID, stove type LPG or EPC (the
#     "Other" product, Umunota induction cooker, is out of scope). One row per
#     phone number: when several beneficiaries share a phone, one is kept at
#     random, so no phone number is called for two sampled households.
#   - Strata: stove type (LPG, EPC) x year the stove was provided (2025, 2026;
#     year of date_of_purchase). PAYG LPG is not separated (no reliable flag).
#   - Equal allocation: N_MAIN / 4 main households + N_REPLACE / 4 replacements
#     per stratum (250 + 75).
#   - Implicit geographic stratification: within a stratum, the frame is sorted
#     by district, cell, village (random order within village) and drawn by
#     systematic sampling, so the sample spreads over the country like the frame.
#   - The 325 drawn per stratum are then randomly split into 250 main and 75
#     replacements; replacement_order sets the order replacements are used in,
#     within the same stratum.
#   - Design weight (main sample) = stratum frame size / main sample size: the
#     number of frame households one sampled household stands for. Needed only
#     for estimates that pool strata. Recompute on completed interviews after
#     fieldwork (replacements included).
#
# Input : DATA_ASCENT_PRELOAD/beneficiaries-list-intermediate.csv (1.beneficiary_list.R)
# Output: DATA_ASCENT_PRELOAD/usage_survey_sample.csv          IDENTIFIED (names, phones): Dropbox only
#         DATA_ASCENT_PRELOAD/usage_survey_sample_summary.csv  counts by stratum, no personal data
#         DATA_ASCENT_PRELOAD/usage_survey_preload.csv         form preload (pulldata): householdid_key,
#                                                              stove_type (cookingmethods code), stove_year
#
# Run from the repo root:
#   Rscript ASCENT/usage_survey/preload/2.sampling.R
#
# Author: Yeji (using Claude) · Last modified: 2026-11-05
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
stopifnot(exists("DATA_ASCENT_PRELOAD"), dir.exists(DATA_ASCENT_PRELOAD))
t_start <- Sys.time()


# ---- 2. Config / constants ---------------------------------------------------

SEED      <- 20261105          # fixed: the same frame always gives the same sample
N_MAIN    <- 1000              # first-round sample (all strata)
N_REPLACE <- 300               # replacements (all strata)
STOVES    <- c("LPG", "EPC")
YEARS     <- c(2025, 2026)     # year the stove was provided; equal allocation across stove x year
STOVE_CODE <- c(LPG = "1", EPC = "3")   # form choice list cookingmethods (1 LPG, 2 PAYG LPG, 3 EPC)
OVERWRITE <- FALSE             # TRUE only to redraw on purpose: the sample must not change once fielded

IN_FILE     <- file.path(DATA_ASCENT_PRELOAD, "beneficiaries-list-intermediate.csv")
OUT_SAMPLE  <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_sample.csv")
OUT_SUMMARY <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_sample_summary.csv")
OUT_PRELOAD <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_preload.csv")

N_STRATA    <- length(STOVES) * length(YEARS)
n_main_h    <- N_MAIN / N_STRATA
n_replace_h <- N_REPLACE / N_STRATA
stopifnot(n_main_h == round(n_main_h), n_replace_h == round(n_replace_h))


# ---- 3. Helper functions -----------------------------------------------------

# Systematic sample of n positions out of N (random start, interval N / n)
systematic <- function(N, n) {
  k <- N / n
  floor(runif(1, 0, k) + k * (0:(n - 1))) + 1
}


# ---- 4. Data loading ---------------------------------------------------------

if (file.exists(OUT_SAMPLE) && !OVERWRITE) {
  stop("Sample already exists:\n", OUT_SAMPLE,
       "\nSet OVERWRITE <- TRUE only if you intend to redraw it.")
}

beneficiary <- read_csv(IN_FILE, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
  mutate(stove_year = year(dmy(date_of_purchase)))

if (any(is.na(beneficiary$stove_year))) stop(sum(is.na(beneficiary$stove_year)), " row(s) with an unreadable date_of_purchase.")
message(sprintf("[1/4] input: %d rows", nrow(beneficiary)))


# ---- 5. Frame ----------------------------------------------------------------

set.seed(SEED)

in_scope <- beneficiary |>
  filter(!is.na(householdid_key), stove_type %in% STOVES)

other_years <- setdiff(unique(in_scope$stove_year), YEARS)
if (length(other_years) > 0) stop("Stove years outside YEARS: ", paste(other_years, collapse = ", "), ". Update YEARS.")

frame <- in_scope |>
  slice_sample(prop = 1) |>                      # random order, so the phone kept below is random
  distinct(phone_number, .keep_all = TRUE) |>
  mutate(stratum = paste(stove_type, stove_year, sep = "_"))

message(sprintf("[2/4] frame: %d in scope (ID + LPG/EPC); %d after one row per phone (%d dropped)",
                nrow(in_scope), nrow(frame), nrow(in_scope) - nrow(frame)))

frame_n <- count(frame, stratum, name = "frame_n")
short <- frame_n |> filter(frame_n < n_main_h + n_replace_h)
if (nrow(frame_n) != N_STRATA || nrow(short) > 0) stop("Stratum missing or smaller than the draw: ", paste(short$stratum, collapse = ", "))


# ---- 6. Draw -----------------------------------------------------------------

sample_df <- frame |>
  mutate(u = runif(n())) |>
  arrange(stratum, district, cell, village, u) |>
  group_by(stratum) |>
  group_modify(\(d, g) {
    drawn <- d[systematic(nrow(d), n_main_h + n_replace_h), ]
    drawn |>
      slice_sample(prop = 1) |>                  # random split into main and replacements
      mutate(sample_status = if_else(row_number() <= n_main_h, "main", "replacement"),
             replacement_order = if_else(sample_status == "replacement", row_number() - n_main_h, NA_integer_))
  }) |>
  ungroup() |>
  left_join(frame_n, by = "stratum") |>
  mutate(stove_type_code = unname(STOVE_CODE[stove_type]),
         design_weight = if_else(sample_status == "main", frame_n / n_main_h, NA_real_)) |>
  select(householdid_key, stratum, stove_type, stove_type_code, stove_year, sample_status, replacement_order,
         district, cell, village, product_name, product_company_supplier, date_of_purchase,
         beneficiary_first_name, beneficiary_last_name, phone_number, frame_n, design_weight) |>
  arrange(stratum, sample_status, replacement_order, district, cell, village)

message(sprintf("[3/4] drawn: %d households (%d main, %d replacements) in %d strata",
                nrow(sample_df), sum(sample_df$sample_status == "main"),
                sum(sample_df$sample_status == "replacement"), N_STRATA))


# ---- 7. Checks ---------------------------------------------------------------

stopifnot(
  !anyDuplicated(sample_df$householdid_key),
  !anyDuplicated(sample_df$phone_number),
  all(count(sample_df, stratum, sample_status)$n %in% c(n_main_h, n_replace_h))
)

# Geographic spread: share of each district in the frame vs in the main sample (aggregates only)
spread <- frame |> count(stratum, district, name = "frame") |>
  left_join(sample_df |> filter(sample_status == "main") |> count(stratum, district, name = "main"),
            by = c("stratum", "district")) |>
  mutate(main = coalesce(main, 0L),
         frame_pct = round(100 * frame / sum(frame), 1), main_pct = round(100 * main / sum(main), 1),
         .by = stratum)
message(sprintf("      district share, max |frame - main| gap: %.1f pp", max(abs(spread$frame_pct - spread$main_pct))))

summary_df <- sample_df |>
  count(stratum, stove_type, stove_type_code, stove_year, frame_n, sample_status) |>
  pivot_wider(names_from = sample_status, values_from = n) |>
  mutate(design_weight_main = round(frame_n / main, 2),
         districts_in_main = map_int(stratum, \(s) n_distinct(sample_df$district[sample_df$stratum == s & sample_df$sample_status == "main"])),
         seed = SEED, drawn_on = format(Sys.Date()))
print(summary_df)

# Form preload: what pulldata() needs, for every household that may be interviewed (main + replacements)
preload <- sample_df |>
  transmute(householdid_key, stove_type = stove_type_code, stove_year)


# ---- 8. Export ---------------------------------------------------------------

write_csv(sample_df, OUT_SAMPLE, na = "")
write_csv(summary_df, OUT_SUMMARY, na = "")
write_csv(preload, OUT_PRELOAD, na = "")
message(sprintf("[4/4] written: %s, %s, %s (%.1fs)", basename(OUT_SAMPLE), basename(OUT_SUMMARY),
                basename(OUT_PRELOAD), as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
