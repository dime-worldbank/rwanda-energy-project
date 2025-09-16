##############
#Author: Xiaoming Zhang
#Date: 9.2.2025
#Purpose: Analysis
#############


pacman::p_load(knitr, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, stringr, broom, clubSandwich, rlang)

getwd()

library(haven)

# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

hfc_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/data"
)


#0. balance table function----

make_balance_table <- function(
    df,
    treat_var,                  # e.g., "treatment" (factor; first level = baseline)
    vars,                       # character vector of outcome vars
    id_var,                     # e.g., "villageid_key" (used only to keep IDs; no clustering counts here)
    cluster       = FALSE,      # TRUE to use clustered SEs
    cluster_var   = NULL,       # required if cluster = TRUE
    var_labels    = NULL,       # named vector: c(var = "Nice label")
    binary_as_pct = TRUE,       # show binary means as percents
    hc_type       = "HC2"       # robust type for non-clustered
){
  stopifnot(is.character(treat_var), is.character(vars), is.character(id_var))
  if (cluster && is.null(cluster_var)) stop("If cluster = TRUE, provide cluster_var.")
  
  # deps
  require(dplyr); require(tidyr); require(purrr); require(stringr)
  require(broom); require(sandwich); suppressMessages(require(clubSandwich))
  
  # helper
  starify <- function(p){
    dplyr::case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.10 ~ "*",
      TRUE ~ ""
    )
  }
  is_binary <- function(x){
    ux <- unique(na.omit(x))
    all(ux %in% c(0,1)) && length(ux) >= 1
  }
  
  # ensure treatment is a factor; first level = baseline
  if (!is.factor(df[[treat_var]])) {
    df[[treat_var]] <- factor(df[[treat_var]])
  }
  arms <- levels(df[[treat_var]])
  base_arm <- arms[1]
  other_arms <- arms[-1]
  
  out <- map_dfr(vars, function(v){
    d <- df %>%
      select(all_of(c(id_var, treat_var, v, if (cluster) cluster_var))) %>%
      filter(!is.na(.data[[v]]), !is.na(.data[[treat_var]]))
    if (nrow(d) == 0L) {
      # empty row with expected columns later
      tibble(variable = v)
    } else {
      # group stats by arm
      stats <- d %>%
        group_by(.data[[treat_var]]) %>%
        summarise(
          mean = mean(.data[[v]], na.rm = TRUE),
          sd   = sd(.data[[v]], na.rm = TRUE),
          n    = sum(!is.na(.data[[v]])),
          .groups = "drop"
        ) %>%
        mutate(
          mean_fmt = if (binary_as_pct && is_binary(d[[v]])) {
            sprintf("%.1f%% (%.1f)", mean*100, sd*100)
          } else {
            sprintf("%.2f (%.2f)", mean, sd)
          }
        ) %>%
        select(all_of(treat_var), mean_fmt, n) %>%
        tidyr::pivot_wider(
          names_from = all_of(treat_var),
          values_from = c(mean_fmt, n),
          names_sep = "_"
        )
      
      # regression with robust vcov
      fml <- as.formula(paste0(v, " ~ ", treat_var))
      fit <- lm(fml, data = d)
      V <- if (cluster) {
        clubSandwich::vcovCR(fit, cluster = d[[cluster_var]], type = "CR2")
      } else {
        sandwich::vcovHC(fit, type = hc_type)
      }
      tidy_est <- broom::tidy(fit, vcov = V)
      
      # pick only treatment diffs vs baseline (terms start with treat_var)
      est_sub <- tidy_est %>%
        filter(str_detect(term, paste0("^", treat_var))) %>%
        mutate(
          arm = str_remove(term, paste0("^", treat_var)),
          arm = ifelse(arm == "", base_arm, arm),   # safety
          arm = str_replace(arm, "^(?=[^A-Za-z0-9])", ""),  # drop leading separators if any
          arm = str_replace(arm, "^[[:punct:]]", "")
        ) %>%
        # R names are typically like treatmentT1; the arm suffix is e.g. "T1"
        mutate(
          diff_se_star = sprintf("%.2f (%.2f)%s", estimate, std.error, starify(p.value))
        ) %>%
        select(arm, diff_se_star, p.value) %>%
        tidyr::pivot_wider(
          names_from = arm,
          values_from = c(diff_se_star, p.value),
          names_sep = "_"
        )
      
      # assemble row
      row <- tibble(variable = v)
      
      # attach means/sds and Ns for each arm with stable column names
      for (a in arms) {
        row[[paste0("mean_", a, "(sd)")]] <- stats[[paste0("mean_fmt_", a)]] %||% NA_character_
        row[[paste0("N_", a)]]            <- stats[[paste0("n_", a)]] %||% NA_integer_
      }
      # attach diffs for each non-baseline arm
      for (a in other_arms) {
        row[[paste0("diff_", a, "–", base_arm, " (se)")]] <- est_sub[[paste0("diff_se_star_", a)]] %||% NA_character_
        row[[paste0("p_", a)]]                              <- est_sub[[paste0("p.value_", a)]] %||% NA_real_
      }
      row
    }
  })
  
  # add labels and order columns nicely
  if (!is.null(var_labels)) {
    out <- out %>% mutate(label = var_labels[variable] %||% variable) %>%
      relocate(variable, label)
  }
  
  # order columns: means by arm, diffs by arm, p-values, Ns
  mean_cols <- paste0("mean_", arms, "(sd)")
  n_cols    <- paste0("N_", arms)
  diff_cols <- paste0("diff_", other_arms, "–", base_arm, " (se)")
  p_cols    <- paste0("p_", other_arms)
  
  keep_order <- c(
    "variable",
    if (!is.null(var_labels)) "label" else NULL,
    mean_cols,
    diff_cols,
    p_cols,
    n_cols
  )
  # keep only columns that exist
  keep_order <- keep_order[keep_order %in% names(out)]
  out <- out %>% select(all_of(keep_order))
  out
}


bal_hh <- make_balance_table(
  df            = hfc_bal_df,          # your HH-level dataframe
  treat_var     = "treatment",
  vars          = desc_stats_variables,
  id_var        = "hh_id",
  cluster       = TRUE,
  cluster_var   = "village",     # <- you supply this
  binary_as_pct = TRUE
)



#1. Selected descriptives-----

desc_stats_variables <- c(
  "A2_4", #primary occupation salary
  "A3_5", #secondary occupation salary
  "total_weekly_income", #cleaned version of household income
  "total_monthly_income",
  "total_savings",
  "H4_2", #candle expenditure
  "H5_4", #biomass expenditure
  "H6_2", #kerosene
  "B4_1", #energy reliability
  "B4_2", #energy efficiency
  "B4_3", #energy accessibility
  "B4_4", #light accessbility
  "B4_5", #energy service satisfaction
  "B4_6", #anxiety related to energy
  "B4_7", #energy challenges
  "B4_8", #energy and peace of mind
  
  "B5_1", #personal ladder
  "B5_2", #ladder one year back
  "B5_3", #financial situation
  "B5_4", #cooking ladder
  "B5_5", #mobile phone charging ladder
  "B5_6", #lighting ladder
  "B5_7", #energy satisfaction
  "B5_8", #energy general
  
  "J1_final", #wtp_fixed
  "J2_1", #wtp_fixed_appliance
  "J3_1", #wtp_fixed_low_reliability
  "J4_2", #wtp_paygo_12
  "J5_2", #wtp_paygo_24
  "wtp_12",
  "wtp_24",
  "J6_1", #wtp_lightbulb
  
  "E2_3", #formal savings
  "E3_3", #informal savings

  "I4_1", #fuel preparation women
  "I4_2" #fuel preparation men
  
)



#2. Balance table No.1------
# Expect a column named `treatment` with values Control, T1, T2, T3.
# If it's numeric (0/1/2/3), map it.
hfc_bal_df <- hfc_constr %>%
  mutate(treatment = factor(as.character(treatment), levels = c("C","T1","T2","T3"))) %>%
  filter(!is.na(treatment) & treatment %in% levels(treatment))

bal_hh <- make_balance_table(
  df            = hfc_bal_df,          # your HH-level dataframe
  treat_var     = "treatment",
  vars          = desc_stats_variables,
  id_var        = "hh_id",
  cluster       = TRUE,
  cluster_var   = "village",     # <- you supply this
  binary_as_pct = TRUE
)


# Make a named vector of labels
var_labels <- c(
  A2_4  = "primary occupation salary",
  A3_5  = "secondary occupation salary",
  total_weekly_income = "cleaned version of household income (weekly)",
  total_monthly_income = "total monthly income",
  total_savings = "total savings",
  H4_2  = "candle expenditure",
  H5_4  = "biomass expenditure",
  H6_2  = "kerosene",
  B4_1  = "energy reliability",
  B4_2  = "energy efficiency",
  B4_3  = "energy accessibility",
  B4_4  = "light accessibility",
  B4_5  = "energy service satisfaction",
  B4_6  = "anxiety related to energy",
  B4_7  = "energy challenges",
  B4_8  = "energy and peace of mind",
  B5_1  = "personal ladder",
  B5_2  = "ladder one year back",
  B5_3  = "financial situation",
  B5_4  = "cooking ladder",
  B5_5  = "mobile phone charging ladder",
  B5_6  = "lighting ladder",
  B5_7  = "energy satisfaction",
  B5_8  = "energy general",
  J1_final = "wtp_fixed",
  J2_1     = "wtp_fixed_appliance",
  J3_1     = "wtp_fixed_low_reliability",
  J4_2     = "wtp_paygo_12",
  J5_2     = "wtp_paygo_24",
  wtp_12   = "wtp_12",
  wtp_24   = "wtp_24",
  J6_1     = "wtp_lightbulb",
  E2_3     = "formal savings",
  E3_3     = "informal savings",
  I4_1     = "fuel preparation women",
  I4_2     = "fuel preparation men"
)

# Add labels to balance_results
balance_results <- balance_results %>%
  mutate(label = var_labels[variable] %||% variable) %>%
  relocate(variable, label,
           starts_with("mean_C"), starts_with("mean_T1"), starts_with("mean_T2"), starts_with("mean_T3"),
           starts_with("diff_T1–C"), starts_with("diff_T2–C"), starts_with("diff_T3–C"),
           starts_with("p_"),
           starts_with("N_"),
           starts_with("clusters_"))



write_xlsx(balance_results, path = file.path(output_path, "balance_table_results.xlsx"))



# 3. With all 193 villages------



hfc_constr_193 <- hfc_constr_raw %>% 
  filter(consent == 1) %>%
  filter(!is.na(A1_1)) %>%
  distinct(hh_head_name, hh_id, A1_2, A1_3, .keep_all = TRUE) %>%
  group_by(hh_id) %>%
  arrange(desc(hh_head_name %in% complete_status$hh_head_name)) %>% 
  slice(1) %>%
  ungroup() 

complete_status_181 <- complete_status %>% 
  filter(villageid_key %in% village_181$villageid_key)


village_bal <- complete_status_181 %>% 
  group_by(villageid_key) %>% 
  summarise(
    `n_vulnerable` = n(),
    `n_reached` = sum(`Approached by Lattanzio(both rounds)` == "Yes"),
    `n_surveyed` = sum(`Completed by Lattanzio` == "Yes")
  )  %>% ungroup %>% 
  mutate(villageid_key = as.numeric(villageid_key))

village_bal <- left_join(treatment, village_bal) %>% 
  mutate(in_scope = ifelse(villageid_key %in% village_181$villageid_key, 1, 0))

desc_stats_variables.2 <- c("in_scope","n_vulnerable","n_reached","n_surveyed")

#balance table
balance_results.2 <- make_balance_table(
  df            = village_bal,         
  treat_var     = "treatment",
  vars          = desc_stats_variables.2,
  id_var        = "villageid_key",
  cluster       = FALSE,     
  binary_as_pct = TRUE
)


var_labels <- c(
  in_scope     = "In scope (1/0)",
  n_vulnerable = "# vulnerable HHs listed",
  n_reached    = "# HHs reached",
  n_surveyed   = "# HHs surveyed"
)

balance_results.2 <- balance_results.2 %>%
  mutate(label = var_labels[variable] %||% variable) %>%
  relocate(variable, label,
           starts_with("mean_C"), starts_with("mean_T1"),
           starts_with("mean_T2"), starts_with("mean_T3"),
           starts_with("diff_T1–C"), starts_with("diff_T2–C"), starts_with("diff_T3–C"),
           starts_with("p_"),
           starts_with("N_"))


write_xlsx(balance_results.2, path = file.path(output_path, "balance_table_results(village_level).xlsx"))


















