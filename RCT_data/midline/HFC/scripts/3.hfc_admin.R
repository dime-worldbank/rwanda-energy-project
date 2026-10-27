# ==============================================================================
# 3.hfc_admin.R
# Midline HFC: fieldwork progress by village, district and treatment arm, and the
# list of households not located.
#
# Counts are households (a household counts once, whatever its number of
# submissions), except `submissions`. hh_attempted = hh_complete + hh_not_complete.
# Percentages are numbers 0-100, named <numerator>_of_<denominator>_pct.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, read_preload()).
# Outputs: village_data, district_data, treatment_data, not_located
#
# Author: Yeji (using Claude) · Last modified: 2026-10-22
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

pct <- function(n, d) round(if_else(d > 0, 100 * n / d, NA_real_), 1)

# Household-level status (is_* so the summaries below never mask these columns)
hh_status <- function(df) {
  df |>
    summarise(n_sub = n(),
              is_located = any(located), is_consented = any(consented), is_complete = any(complete),
              is_nudge_eligible = any(nudge_eligible),
              is_willing_yes = any(shs_willing %in% 1), is_willing_no = any(shs_willing %in% 0),
              is_willing_notsure = any(shs_willing %in% -88),
              is_data_consent = any(shs_data_consent %in% 1),
              .by = hh_id)
}

# Progress table for one grouping (village, district or arm)
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

# Submissions per day, wide (one column per submission date)
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

# Target households: everyone in the preload, with village and arm
hh_frame <- read_preload("contact_info.csv") |>
  distinct(hh_id = householdid_key, villageid_key, treat) |>
  left_join(admin_names, by = "villageid_key")

hh_contacts <- read_preload("contact_info.csv") |>
  select(hh_id = householdid_key)

nolocate_labels <- read_excel(form_path, "choices", col_types = "text", .name_repair = "unique_quiet") |>
  filter(list_name == "nolocate") |>
  transmute(nolocate = as.numeric(value), nolocate_label = `label:English`)

status <- hh_status(hfc_constr)

not_in_frame <- setdiff(status$hh_id, hh_frame$hh_id)
if (length(not_in_frame) > 0) warning(length(not_in_frame), " submitted hh_id not in the preload (excluded from progress).")


# ---- 3. Checks: progress -----------------------------------------------------

village_data <- progress_by(hh_frame, status, c("districtid_key", "district_key", "sector_key", "cell_key",
                                                 "villageid_key", "village_key", "treat")) |>
  left_join(by_day("village") |> rename(villageid_key = village), by = "villageid_key") |>
  arrange(district_key, village_key)

district_data <- progress_by(hh_frame, status, c("districtid_key", "district_key")) |>
  left_join(by_day("district") |> rename(districtid_key = district), by = "districtid_key") |>
  arrange(district_key)

# By arm, with the SHS nudge (T2/T3 only; "agreed" = shs_willing == 1)
nudge_by_arm <- hh_frame |>
  left_join(status, by = "hh_id") |>
  mutate(across(starts_with("is_"), \(x) coalesce(x, FALSE))) |>
  summarise(nudge_eligible = sum(is_nudge_eligible), nudge_agreed = sum(is_willing_yes),
            nudge_refused = sum(is_willing_no), nudge_notsure = sum(is_willing_notsure),
            nudge_data_consent = sum(is_data_consent), .by = treat) |>
  mutate(nudge_agreed_of_eligible_pct = pct(nudge_agreed, nudge_eligible),
         nudge_data_consent_of_agreed_pct = pct(nudge_data_consent, nudge_agreed),
         across(starts_with("nudge_"), \(x) if_else(treat %in% SHS_ARMS, x, NA_real_)))

treatment_data <- progress_by(hh_frame, status, "treat") |>
  left_join(nudge_by_arm, by = "treat") |>
  left_join(by_day("treat"), by = "treat") |>
  arrange(match(treat, ARMS))


# ---- 4. Checks: households not located ---------------------------------------

# Households with no located submission (mop-up list), latest attempt first
not_located <- hfc_constr |>
  filter(!any(located), .by = hh_id) |>
  arrange(hh_id, desc(starttime)) |>
  summarise(n_attempts = n(), last_attempt = first(submissiondate), last_enumerator = first(enumerator_key),
            nolocate = first(nolocate), KEY = first(KEY), .by = hh_id) |>
  left_join(nolocate_labels, by = "nolocate") |>
  left_join(hh_frame |> select(hh_id, treat, district_key, sector_key, cell_key, village_key), by = "hh_id") |>
  select(hh_id, district_key, sector_key, cell_key, village_key, treat,
         n_attempts, last_attempt, last_enumerator,
         nolocate, nolocate_label, KEY) |>
  arrange(district_key, village_key, hh_id)


# ---- 5. Export ---------------------------------------------------------------

hfc_outputs$village_data   <- village_data
hfc_outputs$district_data  <- district_data
hfc_outputs$treatment_data <- treatment_data
hfc_outputs$not_located    <- not_located

message(sprintf("      %d villages, %d of %d target households complete (%.1f%%); %d households not located",
                nrow(village_data), sum(village_data$hh_complete), sum(village_data$target),
                100 * sum(village_data$hh_complete) / sum(village_data$target), nrow(not_located)))
