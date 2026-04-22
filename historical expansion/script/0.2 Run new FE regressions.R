#######################################
#Purpose: Run regression on private sector establishments
#Author: XIAOMING ZHANG
#Date: April 2026
#Description: Run regression in one place filtering the fe_dataset_long.rds (12-14, 15-17, post-13)
######################################################

pacman::p_load(knitr, lfe, fixest, modelsummary, stargazer, tidyverse, dplyr, 
               here, sf, haven, ggplot2, readxl, writexl, janitor, randomizr, 
               RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)

#Read in the unified FE dataset created in 0.1 Unified FE Dataset.R
fe_dataset_long <- readRDS(file.path(output_path, "fe_dataset_long.rds"))

# Define a function to run regression and export results------

# Generalized function to run regression and export LaTeX table for any given treatment variable------


run_and_export_reg <- function(df, elec_var, output_path, label_suffix = NULL) {
  
  # Build label/filename suffix from elec_var if not provided
  if (is.null(label_suffix)) label_suffix <- elec_var
  
  # --- 1. Build formulas dynamically ---
  make_formula <- function(outcome, include_consumers = FALSE) {
    base <- paste0(
      outcome, " ~ year * ", elec_var,
      if (include_consumers) paste0(
        " + year * log1_residential_consumer",
        " + year * log1_non_residential_consumer"
      ) else "",
      " | village_id + cell_year + cell_office_year + health_center_year +",
      " primary_school_year + secondary_school_year + sector_district_office_year +",
      " industry_year + market_year + imidugudu_year | 0 | sector_id"
    )
    as.formula(base)
  }
  
  # --- 2. Run regressions ---
  regs <- list(
    spec1_est = felm(make_formula("num_establishment"),                data = df),
    spec1_emp = felm(make_formula("total_employee"),                   data = df),
    spec2_est = felm(make_formula("num_establishment", include_consumers = TRUE), data = df),
    spec2_emp = felm(make_formula("total_employee",    include_consumers = TRUE), data = df)
  )
  
  # --- 3. Define terms to keep ---
  keep_terms <- c(
    paste0("year2014:", elec_var), paste0("year2017:", elec_var), paste0("year2020:", elec_var),
    "year2014:log1_residential_consumer", "year2017:log1_residential_consumer", "year2020:log1_residential_consumer",
    "year2014:log1_non_residential_consumer", "year2017:log1_non_residential_consumer", "year2020:log1_non_residential_consumer"
  )
  
  elec_label <- gsub("_", " ", label_suffix)  # e.g. "elec1214" -> "elec1214", or custom
  
  covariate_labels <- c(
    paste(elec_label, "\u00d7 2014"),  # ×
    paste(elec_label, "\u00d7 2017"),
    paste(elec_label, "\u00d7 2020"),
    "Log1(Res. Consumer) \u00d7 2014", "Log1(Res. Consumer) \u00d7 2017", "Log1(Res. Consumer) \u00d7 2020",
    "Log1(Non-Res. Consumer) \u00d7 2014", "Log1(Non-Res. Consumer) \u00d7 2017", "Log1(Non-Res. Consumer) \u00d7 2020"
  )
  
  # --- 4. Export raw stargazer .tex ---
  tex_raw <- file.path(output_path, "regressions", "raw", paste0(label_suffix, "_Private_combined.tex"))
  
  stargazer(
    list(regs$spec1_est, regs$spec1_emp, regs$spec2_est, regs$spec2_emp),
    type = "latex",
    out = tex_raw,
    title = paste("Regression Results: Private Sector (Establishments and Employment) —", label_suffix),
    label = paste0("tab:private_combined_", label_suffix),
    column.labels = c("Spec 1", "Spec 1", "Spec 2", "Spec 2"),
    covariate.labels = covariate_labels,
    keep = keep_terms,
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    keep.stat = c("n", "rsq"),
    header = FALSE,
    font.size = "small",
    digits = 3
  )
  
  # --- 5. Extract terms from raw .tex ---
  extract_terms <- function(tex_file_path, elec_var_label) {
    lines <- readLines(tex_file_path)
    
    extract_chunk <- function(pattern) {
      i <- grep(pattern, lines, perl = TRUE)
      if (length(i) == 0) return(character(0))
      unlist(lapply(i, function(idx) lines[idx:min(idx + 2, length(lines))]))
    }
    
    elec_label_escaped <- gsub("([.^$*+?{}|\\[\\]()])","\\\\\\1", elec_var_label)
    
    list(
      p0 = unlist(lapply(c("2014","2017","2020"), function(y)
        extract_chunk(paste0("^\\s*", elec_label_escaped, "\\s*\u00d7\\s*", y)))),
      p1 = unlist(lapply(c("2014","2017","2020"), function(y)
        extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*\u00d7\\s*", y)))),
      p2 = unlist(lapply(c("2014","2017","2020"), function(y)
        extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*\u00d7\\s*", y)))),
      obs = lines[grep("^Observations\\s*&", lines)],
      r2  = lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
    )
  }
  
  extracted <- extract_terms(tex_raw, elec_label)
  
  # --- 6. Write final formatted .tex ---
  tex_final <- file.path(output_path, "regressions", paste0(label_suffix, "_Private_combined.tex"))
  
  writeLines(c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{Regression Results: Private Sector — ", label_suffix, "}"),
    paste0("\\label{tab:private_combined_", label_suffix, "}"),
    "\\small",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{@{\\extracolsep{5pt}}lcccc}",
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    " & \\multicolumn{4}{c}{\\textit{Dependent variable:}} \\\\",
    "\\cline{2-5}",
    "\\\\[-1.8ex] & \\multicolumn{2}{c}{Infra FE only} & \\multicolumn{2}{c}{Infra FE + Consumer Controls} \\\\",
    "\\\\[-1.8ex] & Number of Establishments & Total Employees & Number of Establishments & Total Employees \\\\",
    " & (1) & (2) & (3) & (4) \\\\",
    "\\hline \\\\[-1.8ex]",
    extracted$p0,
    extracted$p1,
    extracted$p2,
    "\\hline \\\\[-1.8ex]",
    extracted$obs,
    extracted$r2,
    "FE: Village & X & X & X & X \\\\",
    "FE: Cell\\_Year & X & X & X & X \\\\",
    "FE: Cell Office\\_Year & X & X & X & X \\\\",
    "FE: Health Center\\_Year & X & X & X & X \\\\",
    "FE: Primary School\\_Year & X & X & X & X \\\\",
    "FE: Secondary School\\_Year & X & X & X & X \\\\",
    "FE: District Office\\_Year & X & X & X & X \\\\",
    "FE: Industry\\_Year & X & X & X & X \\\\",
    "FE: Market\\_Year & X & X & X & X \\\\",
    "FE: Imidugudu\\_Year & X & X & X & X \\\\",
    "\\hline",
    "\\end{tabular}",
    "}%",
    "\\end{table}"
  ), tex_final)
  
  message("Done. Files written to:\n  ", tex_raw, "\n  ", tex_final)
  invisible(regs)
}




#EARP private sector regression-----

expansion_join_drop13 <- fe_dataset_long |>
  filter(electrified_year > 2013) |>
  filter(private_sector == 1) |>
  group_by(year, village_id) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    across(-c(num_establishment, total_employee), first),
    .groups = "drop"
  ) |>
  mutate(
    total_employee = pmin(total_employee, 100),
    year = factor(year, levels = c("2011", "2014", "2017", "2020"))
  )

run_and_export_reg(expansion_join_drop13, "EARP", output_path)


#elec1214  private sector regression-----

expansion_join_drop12_14 <- fe_dataset_long |>
  filter(elec1214 == 1 | neverelectrified == 1) |>
  filter(private_sector == 1) |>
  group_by(year, village_id) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    across(-c(num_establishment, total_employee), first),
    .groups = "drop"
  ) |>
  mutate(
    total_employee = pmin(total_employee, 100),
    year = factor(year, levels = c("2011", "2014", "2017", "2020"))
  )


run_and_export_reg(expansion_join_drop12_14, "elec1214", output_path)


#elec1517  private sector regression-----

expansion_join_drop15_17 <- fe_dataset_long |>
  filter(elec1517 == 1 | neverelectrified == 1) |>
  filter(private_sector == 1) |>
  group_by(year, village_id) |>
  summarise(
    num_establishment = sum(num_establishment, na.rm = TRUE),
    total_employee = sum(total_employee, na.rm = TRUE),
    across(-c(num_establishment, total_employee), first),
    .groups = "drop"
  ) |>
  mutate(
    total_employee = pmin(total_employee, 100),
    year = factor(year, levels = c("2011", "2014", "2017", "2020"))
  )

run_and_export_reg(expansion_join_drop15_17, "elec1517", output_path)




#One where you can change the name of the label_suffix

run_and_export_reg <- function(df, elec_var, output_path, label_suffix = NULL, table_title = NULL) {
  
  # Build label/filename suffix from elec_var if not provided
  if (is.null(label_suffix)) label_suffix <- elec_var
  
  # Use custom title if provided, otherwise auto-generate
  if (is.null(table_title)) {
    table_title <- paste("Regression Results: Private Sector —", label_suffix)
  }
  
  # --- 1. Build formulas dynamically ---
  make_formula <- function(outcome, include_consumers = FALSE) {
    base <- paste0(
      outcome, " ~ year * ", elec_var,
      if (include_consumers) paste0(
        " + year * log1_residential_consumer",
        " + year * log1_non_residential_consumer"
      ) else "",
      " | village_id + cell_year + cell_office_year + health_center_year +",
      " primary_school_year + secondary_school_year + sector_district_office_year +",
      " industry_year + market_year + imidugudu_year | 0 | sector_id"
    )
    as.formula(base)
  }
  
  # --- 2. Run regressions ---
  regs <- list(
    spec1_est = felm(make_formula("num_establishment"),                       data = df),
    spec1_emp = felm(make_formula("total_employee"),                          data = df),
    spec2_est = felm(make_formula("num_establishment", include_consumers = TRUE), data = df),
    spec2_emp = felm(make_formula("total_employee",    include_consumers = TRUE), data = df)
  )
  
  # --- 3. Define terms to keep ---
  keep_terms <- c(
    paste0("year2014:", elec_var), paste0("year2017:", elec_var), paste0("year2020:", elec_var), paste0("year2023:", elec_var),
    "year2014:log1_residential_consumer", "year2017:log1_residential_consumer", "year2020:log1_residential_consumer", "year2023:log1_residential_consumer",
    "year2014:log1_non_residential_consumer", "year2017:log1_non_residential_consumer", "year2020:log1_non_residential_consumer", "year2023:log1_non_residential_consumer"
  )
  
  elec_label <- gsub("_", " ", label_suffix)
  
  covariate_labels <- c(
    paste(elec_label, "\u00d7 2014"),
    paste(elec_label, "\u00d7 2017"),
    paste(elec_label, "\u00d7 2020"),
    paste(elec_label, "\u00d7 2023"),
    "Log1(Res. Consumer) \u00d7 2014", "Log1(Res. Consumer) \u00d7 2017", "Log1(Res. Consumer) \u00d7 2020", "Log1(Res. Consumer) \u00d7 2023",
    "Log1(Non-Res. Consumer) \u00d7 2014", "Log1(Non-Res. Consumer) \u00d7 2017", "Log1(Non-Res. Consumer) \u00d7 2020", "Log1(Non-Res. Consumer) \u00d7 2023"
  )
  
  # --- 4. Export raw stargazer .tex ---
  tex_raw <- file.path(output_path, "regressions", "raw", paste0(label_suffix, "_Private_combined.tex"))
  
  stargazer(
    list(regs$spec1_est, regs$spec1_emp, regs$spec2_est, regs$spec2_emp),
    type = "latex",
    out = tex_raw,
    title = table_title,
    label = paste0("tab:private_combined_", label_suffix),
    column.labels = c("Spec 1", "Spec 1", "Spec 2", "Spec 2"),
    covariate.labels = covariate_labels,
    keep = keep_terms,
    omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
    keep.stat = c("n", "rsq"),
    header = FALSE,
    font.size = "small",
    digits = 3
  )
  
  # --- 5. Extract terms from raw .tex ---
  extract_terms <- function(tex_file_path, elec_var_label) {
    lines <- readLines(tex_file_path)
    
    extract_chunk <- function(pattern) {
      i <- grep(pattern, lines, perl = TRUE)
      if (length(i) == 0) return(character(0))
      unlist(lapply(i, function(idx) lines[idx:min(idx + 2, length(lines))]))
    }
    
    elec_label_escaped <- gsub("([.^$*+?{}|\\[\\]()])","\\\\\\1", elec_var_label)
    
    list(
      p0 = unlist(lapply(c("2014","2017","2020","2023"), function(y)
        extract_chunk(paste0("^\\s*", elec_label_escaped, "\\s*\u00d7\\s*", y)))),
      p1 = unlist(lapply(c("2014","2017","2020","2023"), function(y)
        extract_chunk(paste0("Log1\\(Res\\. Consumer\\)\\s*\u00d7\\s*", y)))),
      p2 = unlist(lapply(c("2014","2017","2020","2023"), function(y)
        extract_chunk(paste0("Log1\\(Non-Res\\. Consumer\\)\\s*\u00d7\\s*", y)))),
      obs = lines[grep("^Observations\\s*&", lines)],
      r2  = lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
    )
  }
  
  extracted <- extract_terms(tex_raw, elec_label)
  
  # --- 6. Write final formatted .tex ---
  tex_final <- file.path(output_path, "regressions", paste0(label_suffix, "_Private_combined.tex"))
  
  writeLines(c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", table_title, "}"),
    paste0("\\label{tab:private_combined_", label_suffix, "}"),
    "\\small",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{@{\\extracolsep{5pt}}lcccc}",
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    " & \\multicolumn{4}{c}{\\textit{Dependent variable:}} \\\\",
    "\\cline{2-5}",
    "\\\\[-1.8ex] & \\multicolumn{2}{c}{Infra FE only} & \\multicolumn{2}{c}{Infra FE + Consumer Controls} \\\\",
    "\\\\[-1.8ex] & Number of Establishments & Total Employees & Number of Establishments & Total Employees \\\\",
    " & (1) & (2) & (3) & (4) \\\\",
    "\\hline \\\\[-1.8ex]",
    extracted$p0,
    extracted$p1,
    extracted$p2,
    "\\hline \\\\[-1.8ex]",
    extracted$obs,
    extracted$r2,
    "FE: Village & X & X & X & X \\\\",
    "FE: Cell\\_Year & X & X & X & X \\\\",
    "FE: Cell Office\\_Year & X & X & X & X \\\\",
    "FE: Health Center\\_Year & X & X & X & X \\\\",
    "FE: Primary School\\_Year & X & X & X & X \\\\",
    "FE: Secondary School\\_Year & X & X & X & X \\\\",
    "FE: District Office\\_Year & X & X & X & X \\\\",
    "FE: Industry\\_Year & X & X & X & X \\\\",
    "FE: Market\\_Year & X & X & X & X \\\\",
    "FE: Imidugudu\\_Year & X & X & X & X \\\\",
    "\\hline",
    "\\end{tabular}",
    "}%",
    "\\end{table}"
  ), tex_final)
  
  message("Done. Files written to:\n  ", tex_raw, "\n  ", tex_final)
  invisible(regs)
}


# Auto-generated title
run_and_export_reg(expansion_join_drop12_14, "elec1214", output_path)

# Custom title
run_and_export_reg(expansion_join_drop12_14, "elec1214", output_path,
                   table_title = "Effect of 2012--2014 Electrification on Private Sector Outcomes")

run_and_export_reg(df, "elec1214", output_path, table_title = "Effect of 2012--2014 Electrification on Private Sector Outcomes", label_suffix = "elec1214_robustness")