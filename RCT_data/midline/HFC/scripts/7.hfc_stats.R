# ==============================================================================
# 7.hfc_stats.R
# Midline HFC: descriptive statistics, overall and by treatment arm.
# Continuous: amounts and counts (special codes already NA). Categorical:
# frequencies of every static-choice field, keeping -77 / -88 as categories.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, form_survey,
# repeat_fields).
# Outputs: desc_stats_data, desc_cat_data
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

DESC_VARS <- c(
  "hh_member_count", "F1_1",                     # household size, phones
  "mt_1", "mt_6", "rb_1", "rb_6", "iw_1", "iw_6", # grid connection payments (Rwf)
  "eu_1", "eu_6",                                # electricity payments (Rwf)
  "shs_number", "shs_1", "shs_6",                # SHS
  "F2_4",                                        # phone charging fee (Rwf; repeat)
  "survey_min"                                   # survey duration (minutes)
)
CAT_EXCLUDE <- c("enumerator", "district", "sector", "cell", "village", "hh_id")  # dynamic lists


# ---- 2. Helper functions -----------------------------------------------------

# Long values of one field, pooling repeat instances
field_long <- function(x) {
  cols <- if (x %in% repeat_fields) str_subset(names(hfc_constr), paste0("^", x, "_\\d+$")) else x
  hfc_constr |>
    select(KEY, treat, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "column", values_to = "value", values_transform = as.character) |>
    filter(!is.na(value), value != "") |>
    mutate(var = x)
}


# ---- 3. Data loading ---------------------------------------------------------

form_choices <- read_excel(form_path, "choices", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(list_name), !is.na(value)) |>
  transmute(list_name = str_squish(list_name), code = str_squish(value), label = `label:English`)

cat_fields <- form_survey |>
  filter(word(type, 1) %in% c("select_one", "select_multiple"), !name %in% CAT_EXCLUDE) |>
  transmute(var = name, multiple = word(type, 1) == "select_multiple", list_name = word(type, 2))


# ---- 4. Checks: descriptives ---------------------------------------------------

## 4.1 Continuous, overall and arm means
cont_long <- map_dfr(DESC_VARS, field_long) |> mutate(value = as.numeric(value))

desc_stats_data <- cont_long |>
  summarise(n = n(), mean = mean(value), sd = sd(value), min = min(value),
            median = median(value), max = max(value), .by = var) |>
  left_join(cont_long |>
              summarise(mean = mean(value), .by = c(var, treat)) |>
              arrange(treat) |>
              pivot_wider(names_from = treat, values_from = mean, names_prefix = "mean_"),
            by = "var") |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(match(var, DESC_VARS))

## 4.2 Categorical frequencies (select_multiple split into choices; -77 / -88 kept)
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
  mutate(n = rowSums(across(starts_with("n_"))), share = round(n / sum(n), 3), .by = var, .after = code) |>
  left_join(form_choices, by = c("list_name", "code")) |>
  relocate(label, .after = code) |>
  arrange(match(var, cat_fields$var), as.numeric(code) < 0, as.numeric(code))


# ---- 5. Export ---------------------------------------------------------------

hfc_outputs$desc_stats_data <- desc_stats_data
hfc_outputs$desc_cat_data   <- desc_cat_data

message(sprintf("      %d continuous variables, %d categorical variables (%d with -77, %d with -88)",
                nrow(desc_stats_data), n_distinct(desc_cat_data$var),
                n_distinct(desc_cat_data$var[desc_cat_data$code == "-77"]),
                n_distinct(desc_cat_data$var[desc_cat_data$code == "-88"])))
