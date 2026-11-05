# ==============================================================================
# 7.hfc_stats.R
# Clean cooking HFC: descriptive statistics, overall and by stove type.
# - desc_stats_data: key variables among consented submissions. Yes/no variables
#   show % yes (mean_or_pct); numbers show mean, sd, min, median, max.
#   `asked_if` is the form relevance: the base for each variable.
# - desc_cat_data: frequencies of every static-choice field, -77 / -88 kept.
# - stove_use: % of consented households using each stove type (last 30 days,
#   and last 7 days), by preload stove type: the stacking picture.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, form_survey,
# rep_cols() and stoves_long built there).
# Outputs: desc_stats_data, desc_cat_data, stove_use
#
# Author: Tanay (using Claude) · Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

DESC_VARS <- c(
  "ascent_confirm",
  "A1_1",
  "observe_stoves",
  "survey_min"
)
CAT_EXCLUDE <- c("enumerator", "district", "sector", "cell", "village", "hh_id")  # dynamic lists
GROUPS <- unname(STOVE_TYPES)


# ---- 2. Helper functions -----------------------------------------------------

field_long <- function(x, data = hfc_constr) {
  cols <- rep_cols(names(data), x)
  if (length(cols) == 0) return(NULL)   # field not in the export (e.g. repeat never answered yet)
  data |>
    select(KEY, stove_group, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "column", values_to = "value", values_transform = as.character) |>
    filter(!is.na(value), value != "") |>
    mutate(var = x)
}

pct1 <- function(x) round(100 * mean(x), 1)


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
            mean_or_pct = if (first(binary)) pct1(value == 1) else round(mean(value), 1),
            sd = if (first(binary)) NA_real_ else round(sd(value), 1),
            min = if (first(binary)) NA_real_ else min(value),
            median = if (first(binary)) NA_real_ else round(median(value), 1),
            max = if (first(binary)) NA_real_ else max(value),
            .by = var) |>
  left_join(desc_long |>
              summarise(group_value = if (first(binary)) pct1(value == 1) else round(mean(value), 1),
                        .by = c(var, stove_group)) |>
              filter(!is.na(stove_group)) |>
              arrange(match(stove_group, GROUPS)) |>
              pivot_wider(names_from = stove_group, values_from = group_value),
            by = "var") |>
  mutate(asked_if = if_else(var == "survey_min", "consented", asked_if)) |>
  arrange(match(var, desc_vars))

## 4.2 Categorical frequencies (all submissions; select_multiple split; -77 / -88 kept)
desc_cat_data <- cat_fields |>
  pmap_dfr(\(var, multiple, list_name) {
    d <- field_long(var)
    if (is.null(d)) return(NULL)
    if (multiple) d <- d |> separate_longer_delim(value, " ")
    d |> mutate(list_name = list_name)
  }) |>
  rename(code = value) |>
  mutate(stove_group = coalesce(stove_group, "unknown")) |>
  count(var, list_name, code, stove_group) |>
  pivot_wider(names_from = stove_group, values_from = n, values_fill = 0, names_prefix = "n_") |>
  mutate(n = rowSums(across(starts_with("n_"))), pct = round(100 * n / sum(n), 1), .by = var, .after = code) |>
  left_join(form_choices, by = c("list_name", "code")) |>
  relocate(label, .after = code) |>
  arrange(match(var, cat_fields$var), suppressWarnings(as.numeric(code)) < 0, suppressWarnings(as.numeric(code)))

## 4.3 Stove use (consented households, first consented submission per household)
first_sub <- hfc_constr |>
  filter(consented) |>
  arrange(hh_id, starttime) |>
  distinct(hh_id, .keep_all = TRUE)

stove_labels <- form_choices |> filter(list_name == "cookingmethods") |> select(stove = code, stove_label = label)
use_long <- stoves_long |>
  inner_join(first_sub |> select(KEY, hh_id, stove_group), by = "KEY") |>
  transmute(hh_id, stove_group, stove = stove_type, days)

households <- bind_rows(first_sub, first_sub |> mutate(stove_group = "All")) |> count(stove_group, name = "households")
stove_use <- bind_rows(use_long, use_long |> mutate(stove_group = "All")) |>
  summarise(used_30d = n_distinct(hh_id), used_7d = n_distinct(hh_id[coalesce(days, 0) > 0]),
            .by = c(stove_group, stove)) |>
  left_join(households, by = "stove_group") |>
  mutate(used_30d_pct = round(100 * used_30d / households, 1), used_7d_pct = round(100 * used_7d / households, 1)) |>
  left_join(stove_labels, by = "stove") |>
  select(stove_group, households, stove, stove_label, used_30d, used_30d_pct, used_7d, used_7d_pct) |>
  arrange(match(stove_group, c(GROUPS, "All")), suppressWarnings(as.numeric(stove)))


# ---- 5. Export ---------------------------------------------------------------

hfc_outputs$desc_stats_data <- desc_stats_data
hfc_outputs$desc_cat_data   <- desc_cat_data
hfc_outputs$stove_use       <- stove_use

message(sprintf("      %d key variables, %d categorical variables (%d with -77); stove use for %d households",
                nrow(desc_stats_data), n_distinct(desc_cat_data$var),
                n_distinct(desc_cat_data$var[desc_cat_data$code == "-77"]), nrow(first_sub)))
