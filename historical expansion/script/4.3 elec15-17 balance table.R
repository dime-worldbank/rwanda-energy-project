#######################################
#Purpose: elec15_17 as a control for analysis
#Author: XIAOMING ZHANG
#Date: September 10th 2025
######################################################

library(knitr)
library(kableExtra)


#Balance table--------




balance_table <- function(data, treatment_vars, cluster, fe = NULL,
                          outcomes = c("num_establishment", "total_employee"),
                          extract_var = "elec15_17") {
  
  cluster_var <- enquo(cluster)
  cluster_name <- quo_name(cluster_var)
  
  rhs_string <- paste(treatment_vars, collapse = " + ")
  fe_string  <- if (!is.null(fe)) paste(fe, collapse = " + ") else "0"
  
  # helper function for significance stars
  starify <- function(p) {
    if (p < 0.001) return("***")
    else if (p < 0.01) return("**")
    else if (p < 0.05) return("*")
    else if (p < 0.1) return(".")
    else return("")
  }
  
  results <- list()
  
  for (outcome in outcomes) {
    
    # ---- Summary statistics by treatment ----
    summ_stats <- data %>%
      group_by(.data[[extract_var]]) %>%
      summarise(
        mean = mean(.data[[outcome]], na.rm = TRUE),
        sd   = sd(.data[[outcome]], na.rm = TRUE),
        n    = n(),
        .groups = "drop"
      )
    
    # ---- Run regression ----
    fml <- as.formula(paste0(outcome, " ~ ", rhs_string, " | ", fe_string, " | 0 | ", cluster_name))
    reg <- felm(fml, data = data)
    reg_summary <- summary(reg)
    
    coef_row <- reg_summary$coefficients[extract_var, ]
    diff_est <- coef_row["Estimate"]
    diff_se  <- coef_row["Cluster s.e."]
    p_value  <- coef_row["Pr(>|t|)"]
    stars    <- starify(p_value)
    
    # ---- Store results ----
    results[[length(results) + 1]] <- tibble(
      outcome = outcome,
      control_stats = sprintf("%.2f (%.2f), N=%d",
                              summ_stats$mean[summ_stats[[extract_var]] == 0],
                              summ_stats$sd[summ_stats[[extract_var]] == 0],
                              summ_stats$n[summ_stats[[extract_var]] == 0]),
      treated_stats = sprintf("%.2f (%.2f), N=%d",
                              summ_stats$mean[summ_stats[[extract_var]] == 1],
                              summ_stats$sd[summ_stats[[extract_var]] == 1],
                              summ_stats$n[summ_stats[[extract_var]] == 1]),
      diff = sprintf("%.2f (%.2f)%s", diff_est, diff_se, stars),
      p_value = sprintf("%.4f", p_value)
    )
  }
  
  bind_rows(results)
}



#All balance table-----


balance_cell_elec15_17 <- balance_table(
  data = elec15_17_p %>% filter(year == 2014),
  treatment = c("elec15_17"),
  cluster = sector_id,
  fe = c("cell_id")
)

balance_consumer_elec15_17 <- balance_table(
  data = elec15_17_p %>% filter(year == 2014),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = sector_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)




balance_cell_elec15_17.3 <- balance_table(
  data = elec15_17_p.3 %>% filter(year == 2014),
  treatment = c("elec15_17"),
  cluster = sector_id,
  fe = c("cell_id")
)

balance_consumer_elec15_17.3 <- balance_table(
  data = elec15_17_p.3 %>% filter(year == 2014),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = sector_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)





balance_cell_elec15_17.7 <- balance_table(
  data = elec15_17_p.7 %>% filter(year == 2014),
  treatment = c("elec15_17"),
  cluster = sector_id,
  fe = c("cell_id")
)

balance_consumer_elec15_17.7 <- balance_table(
  data = elec15_17_p.7 %>% filter(year == 2014),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = sector_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)




balance_cell_elec15_17.9 <- balance_table(
  data = elec15_17_p.9 %>% filter(year == 2014),
  treatment = c("elec15_17"),
  cluster = sector_id,
  fe = c("cell_id")
)

balance_consumer_elec15_17.9 <- balance_table(
  data = elec15_17_p.9 %>% filter(year == 2014),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = sector_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)




balance_cell_elec15_17.19 <- balance_table(
  data = elec15_17_p.19 %>% filter(year == 2014),
  treatment = c("elec15_17"),
  cluster = sector_id,
  fe = c("cell_id")
)

balance_consumer_elec15_17.19 <- balance_table(
  data = elec15_17_p.19 %>% filter(year == 2014),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = sector_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)






# 1️⃣ Combine all "cell FE" balance tables
balance_cell_all <- bind_rows(
  balance_cell_elec15_17      %>% mutate(sector = "All sectors"),
  balance_cell_elec15_17.3    %>% mutate(sector = "Manufacture (.3)"),
  balance_cell_elec15_17.7    %>% mutate(sector = "Wholesale (.7)"),
  balance_cell_elec15_17.9    %>% mutate(sector = "Food and Accomm (.9)"),
  balance_cell_elec15_17.19   %>% mutate(sector = "Other Services (.19)")
)


# 2️⃣ Combine all "cell + targeting FE" balance tables
balance_consumer_all <- bind_rows(
  balance_consumer_elec15_17      %>% mutate(sector = "All sectors"),
  balance_consumer_elec15_17.3    %>% mutate(sector = "Manufacture (.3)"),
  balance_consumer_elec15_17.7    %>% mutate(sector = "Wholesale (.7)"),
  balance_consumer_elec15_17.9    %>% mutate(sector = "Food and Accomm (.9)"),
  balance_consumer_elec15_17.19   %>% mutate(sector = "Other Services (.19)")
)



# Step 1. Extract only relevant columns
balance_cell_extract <- balance_cell_all %>%
  select(sector, outcome, control_stats, treated_stats, diff, p_value) %>%
  rename(
    diff_cellFE = diff,
    p_cellFE = p_value
  )

balance_consumer_extract <- balance_consumer_all %>%
  select(sector, outcome, control_stats, treated_stats, diff, p_value) %>%
  rename(
    diff_allFE = diff,
    p_allFE = p_value
  )

# Step 2. Merge both on sector + outcome (include p-values)
balance_final <- left_join(
  balance_cell_extract %>%
    select(sector, outcome, control_stats, treated_stats, diff_cellFE, p_cellFE),
  balance_consumer_extract %>%
    select(sector, outcome, diff_allFE, p_allFE),
  by = c("sector", "outcome")
)

# Step 3. Arrange columns
balance_final <- balance_final %>%
  relocate(sector, outcome,
           control_stats, treated_stats,
           diff_cellFE, p_cellFE,
           diff_allFE, p_allFE) %>%
  mutate(
    outcome = case_when(
      outcome == "num_establishment" ~ "number of establishments",
      outcome == "total_employee" ~ "number of employees",
      TRUE ~ outcome
    )
  )

# Step 4. Check output


View(balance_final)



# ---- Expand balance_final ----
extract_stats <- function(x) {
  mean <- str_extract(x, "^[0-9\\.\\-]+")
  sd   <- str_extract(x, "(?<=\\().*?(?=\\))")
  n    <- str_extract(x, "(?<=N=)\\d+")
  tibble(mean, sd, n)
}

# ---- Expand balance_final ----
expanded <- balance_final %>%
  rowwise() %>%
  mutate(
    control_mean = extract_stats(control_stats)$mean,
    control_sd   = extract_stats(control_stats)$sd,
    control_n    = extract_stats(control_stats)$n,
    treated_mean = extract_stats(treated_stats)$mean,
    treated_sd   = extract_stats(treated_stats)$sd,
    treated_n    = extract_stats(treated_stats)$n,
    diff_cell_est = str_extract(diff_cellFE, "^[0-9\\.\\-]+"),
    diff_cell_se  = str_extract(diff_cellFE, "(?<=\\().*?(?=\\))"),
    diff_all_est  = str_extract(diff_allFE, "^[0-9\\.\\-]+"),
    diff_all_se   = str_extract(diff_allFE, "(?<=\\().*?(?=\\))"),
    p_cell        = as.numeric(p_cellFE),
    p_all         = as.numeric(p_allFE)
  ) %>%
  ungroup()

# Ns (assumed common)
N_control <- expanded$control_n[1]
N_treated <- expanded$treated_n[1]


# --- Build LaTeX from scratch (reset the accumulator!) ---
latex_lines <- c(
  "\\begin{tabular}{llllll}",
  "\\toprule\\toprule",
  "Sector & Outcome & Control & Treated & Diff (Cell FE) & Diff (All FE) \\\\",
  "& &(SD) & (SD)& (SE) & (SE) \\\\",
  
  sprintf(" &  & N=%s & N=%s & p-value &  p-value \\\\", expanded$control_n[1], expanded$treated_n[1]),
  "\\midrule"
)

for (i in seq_len(nrow(expanded))) {
  r <- expanded[i, ]
  
  # only show sector name on the first line of each pair
  sector_label <- ifelse(i %% 2 == 1, r$sector, "")
  
  # main row with means and diff estimates
  latex_lines <- c(
    latex_lines,
    sprintf("%s & %s & %s & %s & %s & %s \\\\",
            sector_label, r$outcome,
            r$control_mean, r$treated_mean,
            r$diff_cell_est, r$diff_all_est),
    sprintf(" &  & (%s) & (%s) & (%s) & (%s) \\\\",
            r$control_sd, r$treated_sd, r$diff_cell_se, r$diff_all_se)
  )
  
  
  # p-values row for every outcome
  latex_lines <- c(
    latex_lines,
    sprintf(" &  &  &  & %.4f & %.4f \\\\",
            as.numeric(r$p_cell), as.numeric(r$p_all)),
    "& & & &  &  \\\\"
    
  )
  
  # # midrule after each pair (num_establishments + total_employee)
  # if (i %% 2 == 0) {
  #   latex_lines <- c(latex_lines, "\\midrule")
  # }
}


# FE footer + close the environment (only once!)



controls_note <- paste(
  "Controls included:",
  '"log1\\_residential\\_consumer", "log1\\_non\\_residential\\_consumer",',
  '"cell\\_office", "health\\_center", "primary\\_school", "secondary\\_school",',
  '"sector\\_district\\_office", "industry", "market", "imidugudu".'
)

latex_lines_final <- c(
  latex_lines,
  "\\midrule\\midrule",
  "Cell FE &  &  &  & X & X \\\\",
  "Control &  &  &  &  & X \\\\",
  "\\midrule",
  "Note: & \\multicolumn{5}{p{12cm}}{Standard errors clustered at the sector level (cluster = sector id).} \\\\",
  "Controls included: & \\multicolumn{5}{p{12cm}}{log1(residential consumer), log1(non residential consumer), cell office, health center, primary school, secondary school, sector district office, industry, market, imidugudu.} \\\\",
  "\\bottomrule\\bottomrule",
  "\\end{tabular}"
)

cat(paste(latex_lines_final, collapse = "\n"))


# ---- Write to file or print ----
writeLines(latex_lines_final, file.path(output_path, "balance table (2014).tex"))







































# === Combine and Export to LaTeX (elec15_17) ===


combined_diff <- balance_cell_elec15_17 %>%
  select(outcome, N_control, N_treated, control, treated, diff_cell = diff) %>%
  left_join(balance_infra_elec15_17 %>%
              select(outcome, diff_infra = diff), by = "outcome") %>%
  left_join(balance_imidugudu_elec15_17 %>%
              select(outcome, diff_imidugudu = diff), by = "outcome") %>%
  left_join(balance_consumer_elec15_17 %>%
              select(outcome, diff_consumer = diff), by = "outcome") %>%
  rename(
    `Control (Mean (SD))` = control,
    `Treated (Mean (SD))` = treated,
    `Diff (Cell FE)` = diff_cell,
    `Diff (Infra FE)` = diff_infra,
    `Diff (Infra + Imidugudu FE)` = diff_imidugudu,
    `Diff (Infra + Imidugudu FE + Consumer)` = diff_consumer
  )

# Create LaTeX table
kable_output_elec15_17 <- combined_diff %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc",
    caption = "Electrified 2015–2017 on 2011 EC Outcomes (Balance Table)",
    label = "tab:balance_elec15_17_fe"
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    full_width = FALSE
  )

# Save LaTeX to file
writeLines(
  as.character(kable_output_elec15_17),
  file.path(output_path, "balance_table", "balance_table_elec15_17_p.tex")
)

# 
# 
# 
# #ImBalance table--------
# 
# elec15_17_median <- elec15_17_did_all %>% 
#   filter(elec15_17 == 1) %>% 
#   left_join(utility_long_2020)
# 
# elec15_17_did_median <- elec15_17_median %>% 
#   mutate(usage_2020_median = if_else(usage_2020 > median(elec15_17_median$usage_2020), 1, 0)) 
# 
# 
# balance_none_elec15_17 <- balance_table(
#   data = elec15_17_did_median %>% filter(year == 2011),
#   treatment = usage_2020_median,
#   cluster = cell_id
# )
# 
# balance_cellid_elec15_17 <- balance_table(
#   data = elec15_17_did_median %>% filter(year == 2011),
#   treatment = usage_2020_median,
#   cluster = cell_id,
#   fe = c("cell_id")
# )
# 
# balance_allfe_elec15_17 <- balance_table(
#   data = elec15_17_did_median %>% filter(year == 2011),
#   treatment = usage_2020_median,
#   cluster = cell_id,
#   fe = c("cell_id", "cell_office", "health_center",
#          "primary_school", "secondary_school",  "sector_district_office",
#          "industry", "market")
# )
# # === Combine and Export to LaTeX (elec15_17) ===
# 
# combined_diff_elec15_17 <- balance_none_elec15_17 %>%
#   select(outcome, N_control, N_treated,control, treated, diff_none = diff) %>%
#   left_join(balance_cellid_elec15_17 %>% select(outcome, diff_cellid = diff), by = "outcome") %>%
#   left_join(balance_allfe_elec15_17 %>% select(outcome, diff_allfe = diff), by = "outcome") %>%
#   rename(
#     `Control (Mean (SD))` = control,
#     `Treated (Mean (SD))` = treated,
#     `Diff (No FE)` = diff_none,
#     `Diff (Cell FE)` = diff_cellid,
#     `Diff (All FE)` = diff_allfe
#   )
# 
# kable_output_elec15_17 <- combined_diff_elec15_17 %>%
#   kable(
#     format = "latex",
#     booktabs = TRUE,
#     align = "lccccc",
#     caption = "Above vs below median 2020 usage among electrified 2012-2014 villages",
#     label = "tab:balance_elec15_17_fe"
#   ) %>%
#   kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)
# 
# # Save to file
# writeLines(as.character(kable_output_elec15_17),
#            file.path(output_path, "balance_table", "balance_table_elec15_17_usage.tex"))
# 



#Private sector-------


join_drop15_17 <- expansion_join_drop15_17 %>% 
  select(elec15_17, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer, non_residential_consumer, imidugudu)


elec15_17_p <- elec15_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop15_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    p_2_2011 = ifelse(year == 2011, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  mutate(
    total_employee = pmin(total_employee, 100),
    num_establishment = pmin(num_establishment, 10)
  )  %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )





balance_none_p <- balance_table(
  data = elec15_17_p %>% filter(year == 2011),
  treatment = c("elec15_17"),
  cluster = cell_id
)


balance_cellid_p <- balance_table(
  data = elec15_17_p %>% filter(year == 2011),
  treatment = c("elec15_17"),
  cluster = cell_id,
  fe = c("cell_id")
)

balance_allfe_p <- balance_table(
  data = elec15_17_p %>% filter(year == 2011),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)

# === Combine and format for LaTeX ===

combined_diff_p <- balance_none_p %>%
  select(outcome, N_control, N_treated, control, treated, diff_none = diff) %>%
  left_join(balance_cellid_p %>% select(outcome, diff_cellid = diff), by = "outcome") %>%
  left_join(balance_allfe_p %>% select(outcome, diff_allfe = diff), by = "outcome") %>%
  rename(
    `Control (Mean (SD))` = control,
    `Treated (Mean (SD))` = treated,
    `Diff (No FE)` = diff_none,
    `Diff (Cell FE)` = diff_cellid,
    `Diff (All FE)` = diff_allfe
  )

# === Export to LaTeX ===
kable_output <- combined_diff_p %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc", # adjust if you add/remove columns
    caption = "elec15-17 on 2011 establishment census private sector output",
    label = "tab:balance_fe_compare"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# Save to file
writeLines(kable_output,
           file.path(output_path, "balance_table", "balance_table_elec15_17_p.tex"))


#Public sector------


elec15_17_pu <- elec15_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 0) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop15_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )%>% 
  mutate(
    total_employee = pmin(total_employee, 100),
    num_establishment = pmin(num_establishment, 10)
  )  %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )



balance_none_pu <- balance_table(
  data = elec15_17_pu %>% filter(year == 2011),
  treatment = c("elec15_17"),
  cluster = cell_id
)


balance_cellid_pu <- balance_table(
  data = elec15_17_pu %>% filter(year == 2011),
  treatment = c("elec15_17"),
  cluster = cell_id,
  fe = c("cell_id")
)

balance_allfe_pu <- balance_table(
  data = elec15_17_pu %>% filter(year == 2011),
  treatment = c("elec15_17", "log1_residential_consumer", "log1_non_residential_consumer"),
  cluster = cell_id,
  fe = c("cell_id", "cell_office", "health_center",
         "primary_school", "secondary_school",  "sector_district_office",
         "industry", "market", "imidugudu")
)

# === Combine and format for LaTeX ===

combined_diff_pu <- balance_none_pu %>%
  select(outcome, N_control, N_treated, control, treated, diff_none = diff) %>%
  left_join(balance_cellid_pu %>% select(outcome, diff_cellid = diff), by = "outcome") %>%
  left_join(balance_allfe_pu %>% select(outcome, diff_allfe = diff), by = "outcome") %>%
  rename(
    `Control (Mean (SD))` = control,
    `Treated (Mean (SD))` = treated,
    `Diff (No FE)` = diff_none,
    `Diff (Cell FE)` = diff_cellid,
    `Diff (All FE)` = diff_allfe
  )

# === Export to LaTeX ===
kable_output <- combined_diff_pu %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc", # adjust if you add/remove columns
    caption = "elec15-17 on 2011 establishment census public sector output",
    label = "tab:balance_fe_compare"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

# Save to file
writeLines(kable_output,
           file.path(output_path, "balance_table", "balance_table_elec15_17_pu.tex"))








#Combined------

# Tag each set (all, private, public) before combining
combined_diff <- combined_diff %>%
  mutate(outcome = paste0(outcome, "_all"))

combined_diff_p <- combined_diff_p %>%
  mutate(outcome = paste0(outcome, "_private"))

combined_diff_pu <- combined_diff_pu %>%
  mutate(outcome = paste0(outcome, "_public"))

# Stack them
combined_all_private_public <- bind_rows(
  combined_diff,
  combined_diff_p,
  combined_diff_pu
)

# Export to LaTeX
kable_output <- combined_all_private_public %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc",
    caption = "Elec15-17 on 2011 establishment census",
    label = "tab:balance_all_private_public"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

writeLines(
  kable_output,
  file.path(output_path, "balance_table", "balance_table_elec15_17_all.tex")
)

