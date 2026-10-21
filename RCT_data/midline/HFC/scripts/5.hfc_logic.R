# ==============================================================================
# 5.hfc_logic.R
# Midline HFC: logic / consistency checks, one row per submission x check.
# Programming checks (form version, dates, durations), treatment compliance,
# grid and SHS consistency, preload mismatch, phone changes (information only).
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr).
# Outputs: logic_data, logic_summary
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

SHS_NUMBER_MAX <- 3                    # more SHS than this in one household is flagged
RB_ARMS        <- c("T1", "T3")        # arms offered the readyboard
SHS_ARMS       <- c("T2", "T3")        # arms offered the SHS nudge
LOGIC_TARGET   <- 0.05                 # share of submissions with a logic error (target <5%)


# ---- 2. Helper functions -----------------------------------------------------

# Rows of hfc_constr meeting `cond` (NA counts as not flagged), in the logic_data layout
flag_rows <- function(check, issue, cond, type = "error") {
  hfc_constr |>
    filter({{ cond }}) |>
    transmute(KEY, hh_id, enumerator, enumerator_key, district_key, village_key, submissiondate,
              treat, check = paste0("5.hfc_logic: ", check), issue, type)
}


# ---- 3. Checks: logic ----------------------------------------------------------

logic_data <- bind_rows(
  # Survey programming (bug 5 fixed: formdef_version compared as a number)
  flag_rows("old_formdef", "Enumerator used an old form version",
            as.numeric(formdef_version) < CURRENT_FORMDEF),
  flag_rows("start_end_date", "End date differs from start date", startdate != enddate),
  flag_rows("end_submission_date", "Submission date differs from end date", enddate != submissiondate),
  flag_rows("negative_module_duration", "Negative module duration (stamp_*_2 < stamp_*_1)", !is.na(negative_mods)),
  flag_rows("duration_short", sprintf("Consented survey shorter than %g min", DURATION_MIN),
            consented & survey_min < DURATION_MIN),
  flag_rows("duration_long", sprintf("Consented survey longer than %g min", DURATION_MAX),
            consented & survey_min > DURATION_MAX),

  # Treatment compliance
  flag_rows("readyboard_in_C_T2", "Readyboard reported in a C or T2 village",
            readyboard_confirm == 1 & !treat %in% RB_ARMS),
  flag_rows("nudge_outside_T2T3", "SHS nudge answered outside T2/T3",
            (!is.na(nosolar_confirm) | !is.na(shs_willing)) & !treat %in% SHS_ARMS),

  # Grid consistency
  flag_rows("usage_without_meter", "Grid electricity use reported without a meter",
            electricity_usage_confirm == 1 & meter_confirm == 0),
  flag_rows("observe_vs_meter_confirm", "Observed grid connection disagrees with meter_confirm",
            consented & observe_electricity_connection != meter_confirm),

  # SHS consistency
  flag_rows("obs_shs_without_shs_confirm", "SHS observed but none reported", obs_shs == 1 & shs_confirm == 0),
  flag_rows("shs_number_gt3", sprintf("More than %d SHS reported", SHS_NUMBER_MAX), shs_number > SHS_NUMBER_MAX),

  # Preload mismatch
  flag_rows("preload_shs_eligible_not_T2T3", "Household in shs_eligible.csv but not in T2/T3",
            preload_shs_eligible & !treat %in% SHS_ARMS),

  # Information only (not an error)
  flag_rows("phone_changed", "Phone number changed from preload",
            confirm_phonenumber %in% 0 | confirm_second_phonenumber %in% 0, type = "info")
) |>
  arrange(check, submissiondate)

add_flags(logic_data, detail = logic_data$issue)

logic_summary <- logic_data |>
  count(check, issue, type, name = "n") |>
  mutate(share_of_submissions = round(n / nrow(hfc_constr), 3)) |>
  bind_rows(tibble(
    check = "5.hfc_logic: any_error", issue = sprintf("Submissions with at least one logic error (target <%.0f%%)", 100 * LOGIC_TARGET),
    type = "summary", n = n_distinct(logic_data$KEY[logic_data$type == "error"]),
    share_of_submissions = round(n_distinct(logic_data$KEY[logic_data$type == "error"]) / nrow(hfc_constr), 3)
  ))


# ---- 4. Export ---------------------------------------------------------------

hfc_outputs$logic_data    <- logic_data
hfc_outputs$logic_summary <- logic_summary

message(sprintf("      %d logic flags on %d submissions (%s with an error)",
                nrow(logic_data), n_distinct(logic_data$KEY),
                scales::percent(tail(logic_summary$share_of_submissions, 1), accuracy = 0.1)))
