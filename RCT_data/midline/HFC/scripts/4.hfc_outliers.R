# ==============================================================================
# 4.hfc_outliers.R
# Midline HFC: outliers in amounts and counts, +-OUTLIER_SD standard deviations
# on log(1 + x). Repeat fields (F2_4) are pooled over all instances.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, repeat_fields).
# Special codes are already NA in continuous variables (script 2).
# Outputs: outlier_data
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

OUTLIER_VARS <- c(
  "mt_1",            # meter connection, total paid (Rwf)
  "rb_1",            # readyboard, total paid (Rwf)
  "iw_1",            # internal wiring, total paid (Rwf)
  "eu_1",            # electricity, last payment (Rwf)
  "eu_6",            # electricity, usual payment (Rwf)
  "shs_1",           # SHS, total paid (Rwf)
  "shs_6",           # SHS, monthly partial payment (Rwf)
  "F2_4",            # phone charging, monthly fee (Rwf; repeat)
  "hh_member_count", # household size
  "F1_1"             # number of phones / tablets
)


# ---- 2. Checks: outliers -----------------------------------------------------

outlier_data <- map_dfr(OUTLIER_VARS, \(x) {
  cols <- if (x %in% repeat_fields) str_subset(names(hfc_constr), paste0("^", x, "_\\d+$")) else x
  long <- hfc_constr |>
    select(KEY, hh_id, enumerator, enumerator_key, village_key, submissiondate, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "column", values_to = "value") |>
    filter(!is.na(value))
  log_v <- log1p(long$value)
  m <- mean(log_v); s <- sd(log_v)
  long |>
    mutate(var = x,
           instance = if (x %in% repeat_fields) as.integer(str_extract(column, "\\d+$")) else NA_integer_,
           z = round((log1p(value) - m) / s, 2),
           low_limit = round(expm1(m - OUTLIER_SD * s)),
           high_limit = round(expm1(m + OUTLIER_SD * s)),
           median = round(expm1(m))) |>
    filter(abs(z) > OUTLIER_SD)
}) |>
  select(KEY, hh_id, enumerator, enumerator_key, village_key, submissiondate,
         var, instance, value, median, low_limit, high_limit, z) |>
  arrange(var, desc(abs(z)))

add_flags(outlier_data |> distinct(KEY, hh_id, enumerator, var) |> mutate(check = paste0("4.hfc_outliers: outlier_", var)),
          detail = NA_character_)


# ---- 3. Export ---------------------------------------------------------------

hfc_outputs$outlier_data <- outlier_data

message(sprintf("      %d outlier values in %d variables", nrow(outlier_data), n_distinct(outlier_data$var)))
