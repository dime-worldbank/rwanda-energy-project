# ==============================================================================
# 3.hfc_admin.R
# Midline HFC: fieldwork progress by village, district and treatment arm:
# attempts, households attempted / located / consented / complete, progress vs.
# target (preload households), completion rate, submissions by day.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, read_preload()).
# Outputs: village_data, district_data, treatment_data
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

# Household-level status: a household counts once, whatever its number of submissions
hh_status <- function(df) {
  df |>
    summarise(attempts = n(), located = any(located), consented = any(consented),
              complete = any(complete), .by = hh_id)
}

# Progress table for one grouping (village, district or arm)
progress_by <- function(frame, status, by) {
  frame |>
    left_join(status, by = "hh_id") |>
    mutate(across(c(located, consented, complete), \(x) coalesce(x, FALSE))) |>
    summarise(target = n(),
              hh_attempted = sum(!is.na(attempts)),     # before `attempts` is summed below
              attempts = sum(attempts, na.rm = TRUE),
              hh_located = sum(located),
              hh_consented = sum(consented),
              hh_complete = sum(complete),
              .by = all_of(by)) |>
    mutate(locate_rate = hh_located / hh_attempted,
           consent_rate = hh_consented / hh_located,
           completion_rate = hh_complete / hh_attempted,
           progress = hh_complete / target,
           across(c(locate_rate, consent_rate, completion_rate, progress), \(x) round(if_else(is.nan(x), NA_real_, x), 3)))
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

# Target households: everyone in the preload, with village and arm
hh_frame <- read_preload("contact_info.csv") |>
  distinct(hh_id = householdid_key, villageid_key, treat) |>
  left_join(read_preload("scope_villages.csv") |>
              select(villageid_key, districtid_key, district_key, sector_key, cell_key, village_key),
            by = "villageid_key")

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

treatment_data <- progress_by(hh_frame, status, "treat") |>
  left_join(by_day("treat"), by = "treat") |>
  arrange(treat)


# ---- 4. Export ---------------------------------------------------------------

hfc_outputs$village_data   <- village_data
hfc_outputs$district_data  <- district_data
hfc_outputs$treatment_data <- treatment_data

message(sprintf("      %d villages, %d of %d target households complete (%.1f%%)",
                nrow(village_data), sum(village_data$hh_complete), sum(village_data$target),
                100 * sum(village_data$hh_complete) / sum(village_data$target)))
