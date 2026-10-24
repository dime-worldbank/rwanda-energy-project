# ==============================================================================
# 7.hfc_stats.R
# Midline HFC: descriptive statistics and energy access, overall and by arm.
# - desc_stats_data: key variables among consented submissions. Yes/no variables
#   show % yes (mean_or_pct); amounts and counts show mean, sd, min, median, max.
#   `asked_if` is the form relevance: the base for each variable.
# - desc_cat_data: frequencies of every static-choice field, -77 / -88 kept.
# - energy_access: % of consented households with meter, readyboard, wiring,
#   grid use, SHS, none.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, form_survey,
# repeat_fields, FOLLOWUP).
# Outputs: desc_stats_data, desc_cat_data, energy_access
#
# Author: Yeji (using Claude) · Last modified: 2026-10-22
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

# Follow-up names (X_confirm_working) are read from the form in script 2
DESC_VARS <- c(
  "meter_confirm", FOLLOWUP[["meter"]], "meter_payment", "mt_1",
  "readyboard_confirm", FOLLOWUP[["readyboard"]], "readyboard_payment", "rb_1",
  "additional_wire_confirm", FOLLOWUP[["wiring"]], "additional_wire_payment", "iw_1",
  "electricity_usage_confirm", "electricity_usage_payment", "eu_1",
  "shs_confirm", FOLLOWUP[["shs"]], "shs_number", "shs_payment", "shs_1",
  "F1_0", "F1_1",
  "hh_member_count", "survey_min"
)
CAT_EXCLUDE <- c("enumerator", "district", "sector", "cell", "village", "hh_id")  # dynamic lists


# ---- 2. Helper functions -----------------------------------------------------

# Long values of one field, pooling repeat instances
field_long <- function(x, data = hfc_constr) {
  cols <- if (x %in% repeat_fields) str_subset(names(data), paste0("^", x, "_\\d+$")) else x
  data |>
    select(KEY, treat, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "column", values_to = "value", values_transform = as.character) |>
    filter(!is.na(value), value != "") |>
    mutate(var = x)
}

pct <- function(x) round(100 * mean(x), 1)


# ---- 3. Data loading ---------------------------------------------------------

form_choices <- read_excel(form_path, "choices", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(list_name), !is.na(value)) |>
  transmute(list_name = str_squish(list_name), code = str_squish(value), label = `label:English`)

# asked_if = the field's relevance AND its enclosing groups' relevance, without the
# general located / consented conditions (every variable here is among consented)
GENERAL_CONDITIONS <- c("${locate} = 1", "${consent} = 1")
group_stack <- character()
asked_if <- character(nrow(form_survey))
for (i in seq_len(nrow(form_survey))) {
  rel <- str_replace_all(str_squish(coalesce(form_survey$relevance[i], "")), "\\s*=\\s*", " = ")
  if (form_survey$type[i] %in% c("begin group", "begin repeat")) group_stack <- c(group_stack, rel)
  if (form_survey$type[i] %in% c("end group", "end repeat")) group_stack <- head(group_stack, -1)
  conds <- setdiff(c(group_stack, rel), c("", GENERAL_CONDITIONS))
  asked_if[i] <- if (length(conds) == 0) "consented" else paste(unique(conds), collapse = " and ")
}
form_fields <- form_survey |>
  transmute(var = name, base_type = word(type, 1), list_name = word(type, 2), asked_if = asked_if)

cat_fields <- form_fields |>
  filter(base_type %in% c("select_one", "select_multiple"), !var %in% CAT_EXCLUDE) |>
  transmute(var, multiple = base_type == "select_multiple", list_name)

consented_sub <- hfc_constr |> filter(consented)

missing_vars <- setdiff(DESC_VARS, names(hfc_constr))
if (length(missing_vars) > 0) warning("Descriptive variables not in the export: ", paste(missing_vars, collapse = ", "))
desc_vars <- intersect(DESC_VARS, names(hfc_constr))


# ---- 4. Checks: descriptives ---------------------------------------------------

## 4.1 Key variables (consented submissions; yes/no -> % yes)
desc_long <- map_dfr(desc_vars, \(x) field_long(x, consented_sub)) |>
  mutate(value = as.numeric(value)) |>
  left_join(form_fields |> select(var, list_name, asked_if), by = "var") |>
  mutate(binary = list_name %in% "yesno")

desc_stats_data <- desc_long |>
  summarise(type = if_else(first(binary), "% yes", "value"),
            asked_if = first(asked_if),
            n = n(),
            mean_or_pct = if (first(binary)) pct(value == 1) else round(mean(value), 1),
            sd = if (first(binary)) NA_real_ else round(sd(value), 1),
            min = if (first(binary)) NA_real_ else min(value),
            median = if (first(binary)) NA_real_ else round(median(value), 1),
            max = if (first(binary)) NA_real_ else max(value),
            .by = var) |>
  left_join(desc_long |>
              summarise(arm_value = if (first(binary)) pct(value == 1) else round(mean(value), 1),
                        .by = c(var, treat)) |>
              arrange(match(treat, ARMS)) |>
              pivot_wider(names_from = treat, values_from = arm_value),
            by = "var") |>
  mutate(asked_if = if_else(var == "survey_min", "consented", asked_if)) |>
  arrange(match(var, desc_vars))

## 4.2 Categorical frequencies (all submissions; select_multiple split; -77 / -88 kept)
desc_cat_data <- cat_fields |>
  pmap_dfr(\(var, multiple, list_name) {
    if (!var %in% names(hfc_constr) && !var %in% repeat_fields) return(NULL)
    d <- field_long(var)
    if (multiple) d <- d |> separate_longer_delim(value, " ")
    d |> mutate(list_name = list_name)
  }) |>
  rename(code = value) |>
  count(var, list_name, code, treat) |>
  pivot_wider(names_from = treat, values_from = n, values_fill = 0, names_prefix = "n_") |>
  mutate(n = rowSums(across(starts_with("n_"))), pct = round(100 * n / sum(n), 1), .by = var, .after = code) |>
  left_join(form_choices, by = c("list_name", "code")) |>
  relocate(label, .after = code) |>
  arrange(match(var, cat_fields$var), as.numeric(code) < 0, as.numeric(code))

## 4.3 Energy access (consented households, first consented submission per household)
access_hh <- consented_sub |>
  arrange(hh_id, starttime) |>
  distinct(hh_id, .keep_all = TRUE) |>
  transmute(treat,
            meter = meter_confirm %in% 1,
            readyboard = readyboard_confirm %in% 1,
            internal_wiring = additional_wire_confirm %in% 1,
            grid_use = electricity_usage_confirm %in% 1,
            shs = shs_confirm %in% 1,
            meter_and_shs = meter & shs,
            none = !meter & !shs)

energy_access <- bind_rows(access_hh, access_hh |> mutate(treat = "All")) |>
  summarise(households = n(), across(meter:none, pct, .names = "{.col}_pct"), .by = treat) |>
  arrange(match(treat, c(ARMS, "All")))


# ---- 5. Export ---------------------------------------------------------------

hfc_outputs$desc_stats_data <- desc_stats_data
hfc_outputs$desc_cat_data   <- desc_cat_data
hfc_outputs$energy_access   <- energy_access

message(sprintf("      %d key variables, %d categorical variables (%d with -77, %d with -88); energy access for %d households",
                nrow(desc_stats_data), n_distinct(desc_cat_data$var),
                n_distinct(desc_cat_data$var[desc_cat_data$code == "-77"]),
                n_distinct(desc_cat_data$var[desc_cat_data$code == "-88"]),
                nrow(access_hh)))
