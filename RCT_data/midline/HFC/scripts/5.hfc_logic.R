# ==============================================================================
# 5.hfc_logic.R
# Midline HFC: logic / consistency checks, one row per submission x check.
# Dates and durations, treatment compliance, grid and SHS consistency.
# Summary by date. (Logic issues per enumerator: n_logic_issue in enumerator_check.)
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr).
# Outputs: logic_data, logic_summary (issue x date)
#
# Author: Yeji (using Claude) · Last modified: 2026-10-22
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

# Rows of hfc_constr meeting `cond` (NA counts as not flagged), in the logic_data layout
flag_rows <- function(check, issue, cond) {
  hfc_constr |>
    filter({{ cond }}) |>
    transmute(KEY, hh_id, enumerator, enumerator_key, district_key, village_key, submissiondate,
              treat, check = paste0("5.hfc_logic: ", check), issue)
}


# ---- 2. Checks: logic ----------------------------------------------------------

logic_data <- bind_rows(
  # Dates and durations
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
  flag_rows("obs_shs_without_shs_confirm", "SHS observed but none reported", obs_shs == 1 & shs_confirm == 0)
) |>
  arrange(check, submissiondate)

add_flags(logic_data, detail = logic_data$issue)


# ---- 3. Summaries ------------------------------------------------------------

n_issue_sub <- n_distinct(logic_data$KEY)

# Issue x submission date (+ total and share of all submissions)
logic_summary <- logic_data |>
  count(check, issue, submissiondate) |>
  arrange(submissiondate) |>
  mutate(submissiondate = format(submissiondate)) |>
  pivot_wider(names_from = submissiondate, values_from = n, values_fill = 0) |>
  mutate(total = rowSums(across(-c(check, issue))),
         pct_of_submissions = round(100 * total / nrow(hfc_constr), 1), .after = issue) |>
  add_row(check = "5.hfc_logic: any_issue",
          issue = sprintf("Submissions with at least one logic issue (target <%.0f%%)", 100 * LOGIC_TARGET),
          total = n_issue_sub, pct_of_submissions = round(100 * n_issue_sub / nrow(hfc_constr), 1))


# ---- 4. Export ---------------------------------------------------------------

hfc_outputs$logic_data    <- logic_data
hfc_outputs$logic_summary <- logic_summary

message(sprintf("      %d logic issues on %d submissions (%.1f%% of submissions)",
                nrow(logic_data), n_issue_sub, 100 * n_issue_sub / nrow(hfc_constr)))
