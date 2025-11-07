#######################################
#Purpose: Predict EARP using infrastructure
#Author: XIAOMING ZHANG
#Date: September 30th 2025
######################################################
pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, kableExtra,tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()




#EARP predict-------

# 0) Prep: ensure key variables exist / are well-typed

dat <- earp_did_all %>%
  mutate(
    village_id = as.character(village_id),
    cell_id    = as.character(cell_id),
    sector_id  = as.character(sector_id)
  )

# Infra dummies we’ll use
infra_vars <- c("health_center_electrified","primary_school_electrified",
                "secondary_school_electrified",
                "market_electrified","industry_electrified",
                "cell_office_electrified","sector_district_office_electrified")

# Create any missing infra columns and coerce to 0/1
for (v in infra_vars) {
  if (!v %in% names(dat)) dat[[v]] <- 0
  dat[[v]] <- as.integer(dplyr::coalesce(dat[[v]], 0))
  # if any counts slipped in, binarize
  dat[[v]] <- as.integer(dat[[v]] > 0)
}

# 1) Restrict to cells with variation in EARP (not all 0 / not all 1)
#    (Use the full panel to detect variation, then apply to 2011 subset)
cells_with_variation <- dat %>%
  group_by(cell_id) %>%
  summarise(mn = min(EARP, na.rm = TRUE), mx = max(EARP, na.rm = TRUE), .groups = "drop") %>%
  filter(mn == 0 & mx == 1) %>%
  pull(cell_id)

# 2) Prediction sample = YEAR 2011 & cells with EARP variation
#    Run logit with cell FE for each infra var; get predicted probs
dat_2011 <- dat %>%
  filter(year == 2011, cell_id %in% cells_with_variation)

# Define formula with all infra vars and cell fixed effects
joint_formula <- as.formula(
  paste("EARP ~", paste(infra_vars, collapse = " + "), "| cell_id")
)

# Fit logistic model
joint_model <- feglm(joint_formula, family = "binomial", data = dat_2011)

# Predict probability of EARP connection
dat_2011 <- dat_2011 %>%
  ungroup() %>% 
  mutate(PRED_EARP_2011 = predict(joint_model, type = "response")) %>%
  select(village_id, cell_id, PRED_EARP_2011)



# 3) Attach PRED_EARP (from 2011) back to the full panel
dat <- dat %>%
  left_join(dat_2011, by = c("village_id", "cell_id")) %>%
  rename(PRED_EARP = PRED_EARP_2011)  # use the same name going forward

# 4) IDENTIFICATION TEST (2011 only):
#    Regress 2011 outcomes on EARP + PRED_EARP | cell FE
#    (Assumes columns num_establishment, total_employee exist per year)
id_2011 <- dat %>% filter(year == 2011)

id_est <- feols(num_establishment ~ EARP + PRED_EARP | cell_id, data = id_2011)
id_emp <- feols(total_employee   ~ EARP + PRED_EARP | cell_id, data = id_2011)

etable(id_est, id_emp, headers = c("Establishments (2011)", "Employees (2011)"))

etable(
  id_est, id_emp,
  headers = c("Establishments (2011)", "Employees (2011)"),
  tex = TRUE, style.tex = style.tex("aer"),
  file = file.path(output_path,"regressions", "identification_2011.tex"),
  replace = TRUE
)

# 5) EVENT STUDY:
#    (EARP + PRED_EARP) x (2014, 2017, 2020) with Village FE + Cell×Year FE

dat <- dat %>%
  mutate(
    p0_2014  = as.integer(year == 2014),
    p1_2017  = as.integer(year == 2017),
    p2_2020  = as.integer(year == 2020),
    cell_year = paste0(cell_id, "_", year)
  )

es_est <- feols(
  num_establishment ~ (EARP + PRED_EARP) * (p0_2014 + p1_2017 + p2_2020) |
    village_id + cell_year,
  cluster = ~ sector_id,
  data = dat
)

es_emp <- feols(
  total_employee ~ (EARP + PRED_EARP) * (p0_2014 + p1_2017 + p2_2020) |
    village_id + cell_year,
  cluster = ~ sector_id,
  data = dat
)

etable(es_est, es_emp, headers = c("Establishments ES", "Employees ES"))

etable(
  es_est, es_emp,
  headers = c("Establishments ES", "Employees ES"),
  tex = TRUE,
  style.tex = style.tex("aer"),
  file = file.path(output_path, "regressions", "event_study_predict_earp.tex"),
  replace = TRUE
)

#Question------
#1. Are we suppose to use the N of type infrastructure instead of type of electrified infrastructure
#2. There is one village with 23 markets ...




#Correlation matrix of difference infrastructure year by year-------
library(dplyr)
library(fixest)
library(purrr)
library(tidyr)

infra_vars <- c("health_center", "primary_school", "cell_office", "secondary_school", 
                "sector_district_office", "industry", "market")

infra_df <- expansion_join %>% 
  select(village_id, cell_id, all_of(infra_vars))

# Function to compute clustered correlation between two vars
get_clustered_corr <- function(var1, var2, data) {
  fml <- as.formula(paste0(var1, " ~ ", var2))
  model <- feols(fml, cluster = ~cell_id, data = data)
  tibble(
    corr = coef(model)[[1]],
    se = se(model)[[1]],
    stars = case_when(
      pvalue(model)[[1]] < 0.01 ~ "***",
      pvalue(model)[[1]] < 0.05 ~ "**",
      pvalue(model)[[1]] < 0.10 ~ "*",
      TRUE ~ ""
    )
  )
}

# Run all pairwise combinations, excluding self-pairs
clustered_corr_matrix <- expand_grid(var1 = infra_vars, var2 = infra_vars) %>%
  filter(var1 != var2) %>%
  mutate(results = pmap(list(var1, var2), ~ get_clustered_corr(..1, ..2, infra_df))) %>%
  unnest(results)

# Format and reshape for LaTeX
corr_table_latex <- clustered_corr_matrix %>%
  mutate(label = sprintf("%.2f (%.2f)%s", corr, se, stars)) %>%
  select(var1, var2, label) %>%
  pivot_wider(names_from = var2, values_from = label)

latex_code <- kable(
  corr_table_latex,
  format = "latex",
  booktabs = TRUE,
  caption = "Clustered correlation matrix (clustered at cell level)",
  label = "tab:infra_corr_clustered",
  align = "lccccccc"
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    position = "center"
  )


writeLines(
  as.character(latex_code),
  file.path(output_path, "regressions", "infra_corr_clustered.tex")
)
