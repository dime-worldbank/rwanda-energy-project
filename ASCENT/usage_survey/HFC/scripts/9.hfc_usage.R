# ==============================================================================
# 9.hfc_usage.R
# Clean cooking HFC: running preview of the report's key usage parameters, by
# preload stove type and overall, on consented households (first consented
# submission per household). Mean, n, standard error and 95% CI (normal
# approximation, unweighted).
#
# PRELIMINARY, for monitoring only: no sampling weights, no cleaning beyond the
# HFC. The report numbers come from the analysis scripts, which start from the
# same definitions (listed in USAGE_PARAMS below).
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R.
# Outputs: usage_params (also written to output_dir/usage_params.csv by 1.hfc_main.R)
#
# Author: Tanay (using Claude) · Last modified: 2026-11-05 (Yeji: new form; definitions in README.md "Decision log")
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

# parameter, definition (as shown), base (who is counted), value (per household)
USAGE_PARAMS <- tribble(
  ~parameter,            ~definition,                                                       ~base,
  "confirmed_stove",     "% confirming the stove type on the beneficiary list",             "consented",
  "used_7d",             "% that used the program stove in the last 7 days",                "consented",
  "days_used_7d",        "Days the program stove was used in the last 7 days (0 if not)",   "consented",
  "events_7d",           "Uses of the program stove in the last 7 days (0 if not)",          "consented",
  "share_of_cooking",    "% of cooking done on the program stove, last 7 days (midpoints)", "consented",
  "primary_stove",       "% where the program stove has the highest cooking share among stoves used, last 7 days (ties count)", "consented",
  "op_days_year",        "Operational days per year, annualised from the last 7 days: days used x 365/7 (no seasonality)", "consented",
  "stacking_traditional","% using a traditional stove (charcoal, firewood, briquettes) in the last 7 days", "consented",
  "observed_usable",     "% where the program stove was observed and usable",               "stoves observed"
)
PCT_PARAMS <- c("confirmed_stove", "used_7d", "primary_stove", "stacking_traditional", "observed_usable")


# ---- 2. Household values -------------------------------------------------------

hh_values <- hfc_constr |>
  filter(consented) |>
  arrange(hh_id, starttime) |>
  distinct(hh_id, .keep_all = TRUE) |>
  transmute(hh_id, stove_group,
            confirmed_stove      = ascent_confirm %in% "1",
            used_7d              = coalesce(prog_days_7d, 0) > 0,
            days_used_7d         = coalesce(prog_days_7d, 0),
            events_7d            = coalesce(prog_events_7d, 0),
            share_of_cooking     = coalesce(prog_share, 0),
            primary_stove        = prog_top_share,
            op_days_year         = op_days_year,
            stacking_traditional = trad_used_7d,
            observed_usable      = if_else(observe_stoves %in% "1", program_stove_seen %in% "1", NA))


# ---- 3. Parameters -------------------------------------------------------------

usage_params <- bind_rows(hh_values, hh_values |> mutate(stove_group = "All")) |>
  filter(!is.na(stove_group)) |>
  pivot_longer(all_of(USAGE_PARAMS$parameter), names_to = "parameter", values_to = "x",
               values_transform = as.numeric) |>
  filter(!is.na(x)) |>
  mutate(x = if_else(parameter %in% PCT_PARAMS, 100 * x, x)) |>
  summarise(n = n(), mean = mean(x), se = sd(x) / sqrt(n()), .by = c(parameter, stove_group)) |>
  mutate(ci_low = mean - 1.96 * se, ci_high = mean + 1.96 * se,
         ci_low = if_else(parameter %in% PCT_PARAMS, pmax(ci_low, 0), pmax(ci_low, 0)),
         ci_high = if_else(parameter %in% PCT_PARAMS, pmin(ci_high, 100), ci_high),
         across(c(mean, se, ci_low, ci_high), \(v) round(v, 1))) |>
  left_join(USAGE_PARAMS, by = "parameter") |>
  select(parameter, definition, base, stove_group, n, mean, se, ci_low, ci_high) |>
  arrange(match(parameter, USAGE_PARAMS$parameter), match(stove_group, c(unname(STOVE_TYPES), "All")))


# ---- 4. Export ---------------------------------------------------------------

hfc_outputs$usage_params <- usage_params

all_row <- \(p) usage_params |> filter(parameter == p, stove_group == "All")
message(sprintf("      %d households; used in last 7 days %.1f%%, operational days/year %.0f (preliminary)",
                nrow(hh_values), all_row("used_7d")$mean, all_row("op_days_year")$mean))
