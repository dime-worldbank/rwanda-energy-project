# ==============================================================================
# 4.hfc_outliers.R
# Clean cooking HFC: outliers, +-OUTLIER_SD standard deviations on log(1 + x).
# Repeat fields are pooled over all instances. Fuel quantities and spending are
# compared only within the same fuel (and, for quantities, the same unit):
# 2 kg of LPG and 2 bundles of firewood are not comparable.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, rep_cols()).
# Special codes are already NA in continuous variables (script 2).
# `instance` is the repeat suffix: "2" (stove 2, member 2) or "1_2" (stove 1, fuel 2).
# Outputs: outlier_data
#
# Author: Tanay (using Claude) · Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

# Pooled over all submissions (and repeat instances)
OUTLIER_VARS <- c(
  "A1_1",                # household size
  "cooking_events_7d",   # uses of a stove in the last 7 days (stove loop)
  "cooking_hours_7d",    # weekly cooking hours of one member (cooking_time_loop)
  "firewood_hours_7d"    # weekly firewood collection hours of one member (firewood_time_loop)
)
# Within fuel (and unit): fuel loop (nested in the stove loop)
FUEL_OUTLIER_VARS <- c(fuel_qty_7d = "fuel_unit", fuel_spend_7d = NA)
MIN_GROUP_N <- 10         # skip fuel / unit groups with fewer values


# ---- 2. Helper functions -----------------------------------------------------

long_of <- function(x) {
  cols <- rep_cols(names(hfc_constr), x)
  hfc_constr |>
    select(KEY, hh_id, enumerator, enumerator_key, village_key, submissiondate, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "column", values_to = "value") |>
    mutate(var = x, instance = na_if(str_remove(column, paste0("^", x, "_?")), "")) |>
    filter(!is.na(value))
}

flag_z <- function(long, group_cols = character()) {
  long |>
    mutate(lv = log1p(value)) |>
    mutate(n_group = n(), m = mean(lv), s = sd(lv), .by = all_of(c("var", group_cols))) |>
    filter(n_group >= if (length(group_cols)) MIN_GROUP_N else 2, s > 0) |>
    mutate(z = round((lv - m) / s, 2),
           low_limit = round(expm1(m - OUTLIER_SD * s)),
           high_limit = round(expm1(m + OUTLIER_SD * s)),
           median = round(expm1(m))) |>
    filter(abs(z) > OUTLIER_SD)
}


# ---- 3. Checks: outliers -----------------------------------------------------

pooled <- map_dfr(OUTLIER_VARS, \(x) flag_z(long_of(x))) |> mutate(group = NA_character_)

fuel_group <- function(x, unit_field) {
  ft <- long_of("fuel_type") |> transmute(KEY, instance, fuel = as.character(value))
  d <- long_of(x) |> left_join(ft, by = c("KEY", "instance"))
  if (!is.na(unit_field)) {
    d <- d |> left_join(long_of(unit_field) |> transmute(KEY, instance, unit = as.character(value)),
                        by = c("KEY", "instance"))
  } else d$unit <- "any"
  flag_z(d, c("fuel", "unit")) |> mutate(group = paste0("fuel=", fuel, ", unit=", unit))
}
by_fuel <- imap_dfr(FUEL_OUTLIER_VARS, \(u, x) fuel_group(x, u))

outlier_data <- bind_rows(pooled, by_fuel) |>
  select(KEY, hh_id, enumerator, enumerator_key, village_key, submissiondate,
         var, instance, group, value, median, low_limit, high_limit, z) |>
  arrange(var, desc(abs(z)))

add_flags(outlier_data |> distinct(KEY, hh_id, enumerator, var) |> mutate(check = paste0("4.hfc_outliers: outlier_", var)),
          detail = NA_character_)


# ---- 4. Export ---------------------------------------------------------------

hfc_outputs$outlier_data <- outlier_data

message(sprintf("      %d outlier values in %d variables", nrow(outlier_data), n_distinct(outlier_data$var)))
