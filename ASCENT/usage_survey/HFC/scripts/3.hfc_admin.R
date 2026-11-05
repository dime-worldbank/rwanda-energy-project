# ==============================================================================
# 3.hfc_admin.R
# Clean cooking HFC: fieldwork progress by village, district and stove type
# (preload stratum), and the list of households not located (the form records
# no reason for not locating a household).
#
# Counts are households (a household counts once, whatever its number of
# submissions), except `submissions`. hh_attempted = hh_complete + hh_not_complete.
# Percentages are numbers 0-100, named <numerator>_of_<denominator>_pct.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R.
# Outputs: village_data, district_data, stove_data, not_located
#
# Author: Tanay (using Claude) · Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

pct <- function(n, d) round(if_else(d > 0, 100 * n / d, NA_real_), 1)

hh_status <- function(df) {
  df |>
    summarise(n_sub = n(), is_located = any(located), is_consented = any(consented),
              is_complete = any(complete), .by = hh_id)
}

progress_by <- function(frame, status, by) {
  frame |>
    left_join(status, by = "hh_id") |>
    mutate(across(starts_with("is_"), \(x) coalesce(x, FALSE))) |>
    summarise(target = n(),
              submissions = sum(n_sub, na.rm = TRUE),
              hh_attempted = sum(!is.na(n_sub)),
              hh_located = sum(is_located),
              hh_consented = sum(is_consented),
              hh_complete = sum(is_complete),
              .by = all_of(by)) |>
    mutate(hh_not_complete = hh_attempted - hh_complete,
           attempted_of_target_pct = pct(hh_attempted, target),
           located_of_attempted_pct = pct(hh_located, hh_attempted),
           consented_of_located_pct = pct(hh_consented, hh_located),
           complete_of_attempted_pct = pct(hh_complete, hh_attempted),
           complete_of_target_pct = pct(hh_complete, target),
           .after = hh_complete)
}

by_day <- function(by) {
  hfc_constr |>
    count(across(all_of(by)), submissiondate) |>
    arrange(submissiondate) |>
    mutate(submissiondate = format(submissiondate)) |>
    pivot_wider(names_from = submissiondate, values_from = n, values_fill = 0)
}


# ---- 2. Data loading ---------------------------------------------------------

admin_names <- read_preload("scope_villages.csv") |>
  select(villageid_key, districtid_key, district_key, sector_key, cell_key, village_key)

hh_frame <- read_preload("household_frame.csv") |>
  distinct(hh_id = householdid_key, villageid_key, stove_type) |>
  mutate(stove_group = unname(STOVE_TYPES[stove_type])) |>
  left_join(admin_names, by = "villageid_key")

status <- hh_status(hfc_constr)

not_in_frame <- setdiff(status$hh_id, hh_frame$hh_id)
if (length(not_in_frame) > 0) warning(length(not_in_frame), " submitted hh_id not in the preload (excluded from progress).")


# ---- 3. Checks: progress -----------------------------------------------------

village_data <- progress_by(hh_frame, status, c("districtid_key", "district_key", "sector_key", "cell_key",
                                                 "villageid_key", "village_key")) |>
  left_join(by_day("village") |> rename(villageid_key = village), by = "villageid_key") |>
  arrange(district_key, village_key)

district_data <- progress_by(hh_frame, status, c("districtid_key", "district_key")) |>
  left_join(by_day("district") |> rename(districtid_key = district), by = "districtid_key") |>
  arrange(district_key)

stove_data <- progress_by(hh_frame, status, "stove_group") |>
  left_join(by_day("stove_group"), by = "stove_group") |>
  arrange(match(stove_group, STOVE_TYPES))


# ---- 4. Checks: households not located ---------------------------------------

not_located <- hfc_constr |>
  filter(!any(located), .by = hh_id) |>
  arrange(hh_id, desc(starttime)) |>
  summarise(n_attempts = n(), last_attempt = first(submissiondate), last_enumerator = first(enumerator_key),
            KEY = first(KEY), .by = hh_id) |>
  left_join(hh_frame |> select(hh_id, stove_group, district_key, sector_key, cell_key, village_key), by = "hh_id") |>
  select(hh_id, district_key, sector_key, cell_key, village_key, stove_group,
         n_attempts, last_attempt, last_enumerator, KEY) |>
  arrange(district_key, village_key, hh_id)


# ---- 5. Export ---------------------------------------------------------------

hfc_outputs$village_data  <- village_data
hfc_outputs$district_data <- district_data
hfc_outputs$stove_data    <- stove_data
hfc_outputs$not_located   <- not_located

message(sprintf("      %d villages, %d of %d target households complete (%.1f%%); %d households not located",
                nrow(village_data), sum(village_data$hh_complete), sum(village_data$target),
                100 * sum(village_data$hh_complete) / sum(village_data$target), nrow(not_located)))
