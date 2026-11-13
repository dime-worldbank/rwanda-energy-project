# ==============================================================================
# 2.sampling.R
# ASCENT clean cooking usage survey: sample draw outputs.
#
# Design:
#   - Seven groups: stove type crossed with purchase period; empty combinations are excluded.
#   - Allocation is proportional to group size times its relative sampling weight.
#   - Each draw is without replacement; replacement and pilot draws use the remaining pool.
#   - Sampling outputs are written identically to Dropbox and the shared OneDrive preload folder.
#
# Input : DATA_ASCENT_PRELOAD/beneficiaries-list-intermediate.csv
# Output: DATA_ASCENT_PRELOAD/usage_survey_sample.csv
#         DATA_ASCENT_PRELOAD/usage_survey_sample_summary.csv
#         DATA_ASCENT_PRELOAD/usage_survey_pilot_sample.csv
#         DATA_ASCENT_PRELOAD/usage_survey_pilot_sample_summary.csv
#         DATA_ASCENT_PRELOAD/usage_survey_strata_distribution.csv
#
# Run from repo root:
#   Rscript ASCENT/usage_survey/preload/2.sampling.R
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
stopifnot(exists("DATA_ASCENT_PRELOAD"), dir.exists(DATA_ASCENT_PRELOAD))
if (is.na(SHARED_ASCENT_CC) || !dir.exists(SHARED_ASCENT_CC)) {
  stop("SHARED_ASCENT_CC is not set or not accessible; both output copies are required.")
}
t_start <- Sys.time()


# ---- 2. Config / constants ---------------------------------------------------

SEED            <- 20261105
STOVES          <- c("LPG", "LPG_PAYG", "EPC")
STRATA          <- c("2025Q1Q2", "2025Q3Q4", "2026")
N_MAIN          <- 1000L
N_REPLACE       <- 300L
N_PILOT         <- 200L
N_PILOT_REPLACE <- 60L
STOVE_CODE      <- c(LPG = "1", LPG_PAYG = "2", EPC = "3")
GROUP_WEIGHTS   <- c(
  "2025Q1Q2|LPG" = 3,
  "2025Q3Q4|LPG" = 1,
  "2025Q3Q4|LPG_PAYG" = 8,
  "2025Q3Q4|EPC" = 1.5,
  "2026|LPG" = 1,
  "2026|LPG_PAYG" = 8,
  "2026|EPC" = 1.5
)
OVERWRITE       <- TRUE

IN_FILE            <- file.path(DATA_ASCENT_PRELOAD, "beneficiaries-list-intermediate.csv")
OUT_SAMPLE         <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_sample.csv")
OUT_SUMMARY        <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_sample_summary.csv")
OUT_PILOT          <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_pilot_sample.csv")
OUT_PILOT_SUMMARY  <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_pilot_sample_summary.csv")
OUT_STRATA_DIST    <- file.path(DATA_ASCENT_PRELOAD, "usage_survey_strata_distribution.csv")
ONEDRIVE_PRELOAD_DIR <- file.path(SHARED_ASCENT_CC, "questionnaires", "preload")
OUTPUT_FILES <- c(OUT_SAMPLE, OUT_SUMMARY, OUT_PILOT, OUT_PILOT_SUMMARY, OUT_STRATA_DIST)
OUTPUT_NAMES <- basename(OUTPUT_FILES)
stopifnot(sum(GROUP_WEIGHTS) > 0, length(GROUP_WEIGHTS) == 7)


# ---- 3. Helper functions -----------------------------------------------------

systematic <- function(N, n) {
  k <- N / n
  floor(runif(1, 0, k) + k * (0:(n - 1))) + 1
}

proportional_allocation <- function(total_n, counts) {
  counts <- as.integer(counts)
  counts[is.na(counts)] <- 0L
  if (sum(counts) == 0) return(rep(0L, length(counts)))
  raw <- total_n * counts / sum(counts)
  base <- floor(raw)
  remainder <- total_n - sum(base)
  if (remainder > 0) {
    ord <- order(raw - base, decreasing = TRUE)
    base[ord[seq_len(remainder)]] <- base[ord[seq_len(remainder)]] + 1L
  }
  as.integer(base)
}

weighted_allocation <- function(total_n, pool_n, weights) {
  if (total_n > sum(pool_n)) {
    stop("Requested ", total_n, " but only ", sum(pool_n), " eligible beneficiaries remain.")
  }

  weighted_targets <- numeric(length(pool_n))
  remaining <- total_n
  active <- which(pool_n > 0 & weights > 0)
  while (remaining > 0 && length(active) > 0) {
    proposed <- remaining * pool_n[active] * weights[active] /
      sum(pool_n[active] * weights[active])
    capped <- proposed >= pool_n[active]
    if (any(capped)) {
      capped_groups <- active[capped]
      weighted_targets[capped_groups] <- pool_n[capped_groups]
      remaining <- total_n - sum(weighted_targets)
      active <- active[!capped]
    } else {
      weighted_targets[active] <- proposed
      remaining <- 0
    }
  }
  if (remaining > 0) stop("The requested sample cannot be allocated across the weighted groups.")

  allocation <- floor(weighted_targets)
  remainder <- total_n - sum(allocation)
  fractional <- weighted_targets - allocation
  while (remainder > 0) {
    eligible <- which(allocation < pool_n & weights > 0)
    if (length(eligible) == 0) stop("Insufficient group capacity to complete the allocation.")
    next_group <- eligible[which.max(fractional[eligible])]
    allocation[next_group] <- allocation[next_group] + 1L
    fractional[next_group] <- -Inf
    remainder <- remainder - 1L
  }

  as.integer(allocation)
}

build_group_targets <- function(df, total_n) {
  groups <- expand_grid(stratum = STRATA, stove_type = STOVES) |>
    mutate(group_key = paste(stratum, stove_type, sep = "|")) |>
    left_join(df |> count(stratum, stove_type, name = "pool_n"), by = c("stratum", "stove_type")) |>
    mutate(pool_n = coalesce(pool_n, 0L),
           weight = coalesce(unname(GROUP_WEIGHTS[group_key]), 0))

  groups$group_target_n <- weighted_allocation(total_n, groups$pool_n, groups$weight)
  groups |> select(-group_key, -weight)
}

sample_grouped <- function(df, total_n, targets = build_group_targets(df, total_n)) {
  available <- df |> count(stratum, stove_type, name = "available_n")
  checked_targets <- targets |>
    left_join(available, by = c("stratum", "stove_type")) |>
    mutate(available_n = coalesce(available_n, 0L))
  if (any(checked_targets$group_target_n > checked_targets$available_n)) {
    stop("A weighted group target exceeds the remaining non-overlap pool.")
  }

  drawn <- df |>
    mutate(u = runif(n())) |>
    arrange(stratum, stove_type, district, cell, village, u) |>
    group_by(stratum, stove_type) |>
    group_map(function(d, g, ...) {
      take <- targets$group_target_n[targets$stratum == g$stratum & targets$stove_type == g$stove_type]
      if (length(take) == 0 || take == 0) return(tibble())
      d |> slice_sample(n = min(take, nrow(d)))
    }, .keep = TRUE) |>
    bind_rows()

  stopifnot(nrow(drawn) == total_n)
  drawn
}


# ---- 4. Data loading ---------------------------------------------------------

if (!OVERWRITE && any(file.exists(c(OUT_SAMPLE, OUT_PILOT)))) {
  stop("Sample output already exists.\n",
       "Set OVERWRITE <- TRUE only if you intend to redraw it.\n",
       "Checked files: ", paste(c(OUT_SAMPLE, OUT_PILOT), collapse = ", "))
}

parse_year_from_purchase <- function(x) {
  x <- str_squish(x)
  x <- na_if(x, "")
  out <- rep(NA_integer_, length(x))
  idx <- !is.na(x)
  if (any(idx)) {
    out[idx] <- suppressWarnings(dplyr::case_when(
      str_detect(x[idx], "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") ~ year(ymd(x[idx])),
      str_detect(x[idx], "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$") ~ year(dmy(x[idx])),
      str_detect(x[idx], "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$") ~ year(mdy(x[idx])),
      TRUE ~ NA_integer_
    ))
  }
  out
}

beneficiary <- read_csv(IN_FILE, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
  mutate(stove_year = parse_year_from_purchase(date_of_purchase))

if (any(is.na(beneficiary$stove_year))) {
  stop(sum(is.na(beneficiary$stove_year)), " row(s) with an unreadable date_of_purchase.")
}
message(sprintf("[1/5] input: %d rows", nrow(beneficiary)))


# ---- 5. Frame ----------------------------------------------------------------

set.seed(SEED)

in_scope <- beneficiary |>
  filter(!is.na(householdid_key), stove_type %in% STOVES) |>
  mutate(
    purchase_month = month(date_of_purchase),
    stratum = case_when(
      stove_year == 2025 & purchase_month <= 6 ~ "2025Q1Q2",
      stove_year == 2025 & purchase_month > 6 ~ "2025Q3Q4",
      stove_year == 2026 ~ "2026",
      TRUE ~ NA_character_
    )
  )

if (any(is.na(in_scope$stratum))) {
  stop("The new 3-period stratum definition is missing rows: ",
       sum(is.na(in_scope$stratum)), ". Check purchase dates.")
}

frame <- in_scope |>
  slice_sample(prop = 1) |>
  distinct(phone_number, .keep_all = TRUE)

message(sprintf("[2/5] frame: %d in scope; %d after one row per phone (%d dropped)",
               nrow(in_scope), nrow(frame), nrow(in_scope) - nrow(frame)))

frame_n <- count(frame, stratum, stove_type, name = "frame_n")
frame_groups <- paste(frame_n$stratum, frame_n$stove_type, sep = "|")
missing_groups <- setdiff(names(GROUP_WEIGHTS), frame_groups[frame_n$frame_n > 0])
if (length(missing_groups) > 0) {
  stop("Frame is missing a required weighted group: ", paste(missing_groups, collapse = ", "))
}


# ---- 6. Main + replacement draw ----------------------------------------------

main_targets <- build_group_targets(frame, N_MAIN)
main_draw <- sample_grouped(frame, N_MAIN, main_targets)
main_draw <- main_draw |>
  mutate(sample_status = "main",
         replacement_order = NA_integer_,
         design_weight = NA_real_)

replace_pool <- anti_join(frame, main_draw |> select(householdid_key), by = "householdid_key")
if (nrow(replace_pool) < N_REPLACE) {
  stop("Replacement pool is smaller than needed: requested ", N_REPLACE, ", available ", nrow(replace_pool), ".")
}

replace_targets <- build_group_targets(frame, N_REPLACE)
replace_draw <- sample_grouped(replace_pool, N_REPLACE, replace_targets)
replace_draw <- replace_draw |>
  mutate(sample_status = "replacement",
         replacement_order = row_number(),
         design_weight = NA_real_)

sample_df <- bind_rows(main_draw, replace_draw) |>
  left_join(frame_n, by = c("stratum", "stove_type")) |>
  group_by(stratum, stove_type) |>
  mutate(main_n = sum(sample_status == "main"),
         design_weight = if_else(sample_status == "main", frame_n / main_n, NA_real_)) |>
  ungroup() |>
  mutate(stove_type_code = unname(STOVE_CODE[stove_type])) |>
  select(householdid_key, household_id, stratum, stove_type, stove_type_code, stove_year,
         sample_status, replacement_order,
         provinceid_key, province_key, districtid_key, district, sectorid_key, sector,
         cellid_key, cell, villageid_key, village,
         product_name, product_company_supplier, date_of_purchase,
         beneficiary_first_name, beneficiary_last_name, phone_number,
         frame_n, design_weight) |>
  arrange(stratum, sample_status, replacement_order, district, cell, village)

message(sprintf("[3/5] main+replacement: %d households (%d main, %d replacements)",
               nrow(sample_df), sum(sample_df$sample_status == "main"),
               sum(sample_df$sample_status == "replacement")))

stopifnot(
  !anyDuplicated(sample_df$householdid_key),
  !anyDuplicated(sample_df$phone_number),
  sum(sample_df$sample_status == "main") == N_MAIN,
  sum(sample_df$sample_status == "replacement") == N_REPLACE,
  all(count(main_draw, stratum, stove_type) |> left_join(main_targets, by = c("stratum", "stove_type")) |> pull(n) ==
        count(main_draw, stratum, stove_type) |> left_join(main_targets, by = c("stratum", "stove_type")) |> pull(group_target_n)),
  all(count(replace_draw, stratum, stove_type) |> left_join(replace_targets, by = c("stratum", "stove_type")) |> pull(n) ==
        count(replace_draw, stratum, stove_type) |> left_join(replace_targets, by = c("stratum", "stove_type")) |> pull(group_target_n))
)

GROUP_GRID <- expand_grid(stratum = STRATA, stove_type = STOVES) |>
  mutate(stove_year = if_else(stratum == "2026", 2026L, 2025L),
         stove_type_code = unname(STOVE_CODE[stove_type]),
         relative_weight = coalesce(unname(GROUP_WEIGHTS[paste(stratum, stove_type, sep = "|")]), 0))

summary_df <- GROUP_GRID |>
  left_join(sample_df |>
  count(stratum, stove_type, stove_type_code, stove_year, sample_status) |>
  pivot_wider(names_from = sample_status, values_from = n),
  by = c("stratum", "stove_type", "stove_type_code", "stove_year")) |>
  mutate(main = coalesce(main, 0L), replacement = coalesce(replacement, 0L)) |>
  mutate(seed = SEED, drawn_on = format(Sys.Date()))
print(summary_df)


# ---- 7. Pilot draw (non-overlap) ---------------------------------------------

pilot_pool <- anti_join(frame, sample_df |> select(householdid_key), by = "householdid_key")
if (nrow(pilot_pool) < N_PILOT + N_PILOT_REPLACE) {
  stop("Pilot draw requested ", N_PILOT + N_PILOT_REPLACE,
       " but only ", nrow(pilot_pool),
       " households remain outside main+replacement.")
}

set.seed(SEED + 1)
pilot_targets <- build_group_targets(pilot_pool, N_PILOT)
pilot_df <- sample_grouped(pilot_pool, N_PILOT, pilot_targets)
pilot_df <- pilot_df |>
  mutate(stove_type_code = unname(STOVE_CODE[stove_type]),
         sample_status = "pilot",
         replacement_order = NA_integer_,
         design_weight = NA_real_) |>
  select(householdid_key, household_id, stratum, stove_type, stove_type_code, stove_year,
         sample_status, replacement_order,
         provinceid_key, province_key, districtid_key, district, sectorid_key, sector,
         cellid_key, cell, villageid_key, village,
         product_name, product_company_supplier, date_of_purchase,
         beneficiary_first_name, beneficiary_last_name, phone_number,
         design_weight) |>
  arrange(stratum, district, cell, village)

pilot_replace_pool <- anti_join(pilot_pool, pilot_df |> select(householdid_key), by = "householdid_key")
if (nrow(pilot_replace_pool) < N_PILOT_REPLACE) {
  stop("Pilot replacement requested ", N_PILOT_REPLACE,
       " but only ", nrow(pilot_replace_pool),
       " households remain after the pilot sample.")
}

pilot_replace_targets <- build_group_targets(pilot_pool, N_PILOT_REPLACE)
pilot_replace_df <- sample_grouped(pilot_replace_pool, N_PILOT_REPLACE, pilot_replace_targets)
pilot_replace_df <- pilot_replace_df |>
  mutate(stove_type_code = unname(STOVE_CODE[stove_type]),
         sample_status = "pilot_replacement",
         replacement_order = NA_integer_,
         design_weight = NA_real_) |>
  select(householdid_key, household_id, stratum, stove_type, stove_type_code, stove_year,
         sample_status, replacement_order,
         provinceid_key, province_key, districtid_key, district, sectorid_key, sector,
         cellid_key, cell, villageid_key, village,
         product_name, product_company_supplier, date_of_purchase,
         beneficiary_first_name, beneficiary_last_name, phone_number,
         design_weight) |>
  arrange(stratum, district, cell, village)

if (nrow(pilot_df) != N_PILOT || nrow(pilot_replace_df) != N_PILOT_REPLACE) {
  stop("Pilot draw size mismatch: requested pilot=", N_PILOT, ", pilot_replacement=", N_PILOT_REPLACE,
       " but produced pilot=", nrow(pilot_df), ", pilot_replacement=", nrow(pilot_replace_df))
}

stopifnot(!anyDuplicated(pilot_df$householdid_key),
         !anyDuplicated(pilot_replace_df$householdid_key),
         !anyDuplicated(pilot_df$phone_number),
         !anyDuplicated(pilot_replace_df$phone_number),
         nrow(inner_join(pilot_df |> select(householdid_key),
                         sample_df |> select(householdid_key),
                         by = "householdid_key")) == 0,
         nrow(inner_join(pilot_replace_df |> select(householdid_key),
                         bind_rows(sample_df, pilot_df) |> select(householdid_key),
                         by = "householdid_key")) == 0)

pilot_summary_df <- GROUP_GRID |>
  left_join(pilot_df |>
  count(stratum, stove_type, stove_type_code, stove_year, name = "pilot_n") |>
    left_join(pilot_replace_df |> count(stratum, stove_type, stove_type_code, stove_year, name = "pilot_replacement_n"),
              by = c("stratum", "stove_type", "stove_type_code", "stove_year")),
    by = c("stratum", "stove_type", "stove_type_code", "stove_year")) |>
  mutate(pilot_n = coalesce(pilot_n, 0L),
         pilot_replacement_n = coalesce(pilot_replacement_n, 0L),
         seed = SEED + 1,
         drawn_on = format(Sys.Date())) |>
  arrange(stratum)
print(pilot_summary_df)
message(sprintf("[4/5] pilot: %d households and pilot_replacement: %d households from non-overlap pool of %d",
                 nrow(pilot_df), nrow(pilot_replace_df), nrow(pilot_pool)))

# Actual distribution for all nine stove-by-period combinations across frame and draws.
strata_distribution_df <- GROUP_GRID |>
  left_join(frame_n, by = c("stratum", "stove_type")) |>
  left_join(sample_df |> count(stratum, stove_type, sample_status) |>
             pivot_wider(names_from = sample_status, values_from = n),
           by = c("stratum", "stove_type")) |>
  left_join(pilot_pool |> count(stratum, stove_type, name = "pilot_pool_n"), by = c("stratum", "stove_type")) |>
  left_join(pilot_df |> count(stratum, stove_type, name = "pilot_n"), by = c("stratum", "stove_type")) |>
  left_join(pilot_replace_df |> count(stratum, stove_type, name = "pilot_replacement_n"), by = c("stratum", "stove_type")) |>
  mutate(
    frame_n = coalesce(frame_n, 0L),
    main = coalesce(main, 0L),
    replacement = coalesce(replacement, 0L),
    pilot_pool_n = coalesce(pilot_pool_n, 0L),
    pilot_n = coalesce(pilot_n, 0L),
    pilot_replacement_n = coalesce(pilot_replacement_n, 0L),
    frame_share_pct = round(100 * frame_n / sum(frame_n), 2),
    main_share_pct = round(100 * main / sum(main), 2),
    replacement_share_pct = round(100 * replacement / sum(replacement), 2),
    pilot_pool_share_pct = round(100 * pilot_pool_n / sum(pilot_pool_n), 2),
    pilot_share_pct = round(100 * pilot_n / sum(pilot_n), 2),
    pilot_replacement_share_pct = round(100 * pilot_replacement_n / sum(pilot_replacement_n), 2),
    seed_main_replace = SEED,
    seed_pilot = SEED + 1,
    drawn_on = format(Sys.Date())
  ) |>
  arrange(stratum)


# ---- 8. Export ----------------------------------------------------------------

pilot_all_df <- bind_rows(pilot_df, pilot_replace_df)
stopifnot(
  nrow(summary_df) == length(STRATA) * length(STOVES),
  nrow(pilot_summary_df) == length(STRATA) * length(STOVES),
  nrow(strata_distribution_df) == length(STRATA) * length(STOVES),
  sum(summary_df$main) == N_MAIN,
  sum(summary_df$replacement) == N_REPLACE,
  sum(pilot_summary_df$pilot_n) == N_PILOT,
  sum(pilot_summary_df$pilot_replacement_n) == N_PILOT_REPLACE
)
output_data <- list(sample_df, summary_df, pilot_all_df, pilot_summary_df, strata_distribution_df)
names(output_data) <- OUTPUT_NAMES

write_outputs <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  iwalk(output_data, \(data, name) write_csv(data, file.path(directory, name), na = ""))
}

write_outputs(DATA_ASCENT_PRELOAD)
write_outputs(ONEDRIVE_PRELOAD_DIR)

message(sprintf("[5/5] mirrored to Dropbox and OneDrive: %s, %s, %s, %s, %s (%.1fs)",
                basename(OUT_SAMPLE), basename(OUT_SUMMARY),
                basename(OUT_PILOT), basename(OUT_PILOT_SUMMARY),
                basename(OUT_STRATA_DIST),
                as.numeric(difftime(Sys.time(), t_start, units = "secs"))))


