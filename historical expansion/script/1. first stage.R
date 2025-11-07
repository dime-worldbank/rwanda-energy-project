#######################################
#Purpose: First stage on electrified
#Author: XIAOMING ZHANG
#Date: September 10th 2025
######################################################
pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

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


#Read files-----


expansion_join <- read_xlsx(path = file.path(output_path, "expansion_join.xlsx"))


#Infrastructure results-----


### DATA VALIDATION 1: 2011 ELECTRIFICATION STATUS IS PREDICTED BY electrific
### SAMPLE ADJUSTMENT: RESTRICT SAMPLE TO electrified as of 2013 = 0, electrific = F (NEW!!!, remove it),
###                      NO MV LINES, ....
### PREDICTION: RESTRICTING TO CELLS THAT ARE EITHER (a) NOT ALL NOT EARP AND NOT ALL EARP IN SAMPLE
###               RUN fegls LOGIT WITH CELL FE ON EACH INFRA TYPE
###               --> PREDICTIONS OF EARP PROBABILITY AS FUNCTION OF CELL AND INFRA TYPE
###               --> "PRED_EARP" = predict(feglsoutput)
### IDENTIFICATION TEST: regression of 2011 establishment census outcomes on EARP + PRED_EARP | CELL FE yields balance wrt EARP
### ESTIMATION: event studies of EARP * 2014 + EARP * 2017 + EARP * 2020 + PRED_EARP * 2014 + PRED_EARP * 2017 + PRED_EARP * 2020 | CELLxYEAR FE + VILLAGE FE











#Electrified first stage---------
expansion_join_drop4 <- expansion_join %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(electrified_year > 2013)

# Run regressions for electrification status in 2011, 2014, 2017, 2020
electrified_2011 <- felm(electrified_2011 ~ `EARP` | cell_id | 0 | sector_id,
                         data = expansion_join_drop4)
summary(electrified_2011)

electrified_2014 <- felm(electrified_2014 ~ `EARP` | cell_id | 0 | sector_id,
                         data = expansion_join_drop4)
summary(electrified_2014)

electrified_2017 <- felm(electrified_2017 ~ `EARP` | cell_id | 0 | sector_id,
                         data = expansion_join_drop4)
summary(electrified_2017)


electrified_2020 <- felm(electrified_2020 ~ `EARP` | cell_id | 0 | sector_id,
                         data = expansion_join_drop4)
summary(electrified_2020)


# Collect results
regs <- list(
  "2011" = electrified_2011,
  "2014" = electrified_2014,
  "2017" = electrified_2017,
  "2020" = electrified_2020
)


# Function to compute mean for a given variable when earp == 0
compute_mean <- function(df, var, na.rm = TRUE) {
  stopifnot(var %in% names(df))
  x <- df[[var]]
  if (!is.numeric(x)) x <- as.numeric(x)
  mean(x, na.rm = na.rm)
}




# assumes regs, output_path, expansion_join_drop4, and compute_mean() exist
tf <- tempfile(fileext=".tex"); stargazer(regs, type="latex", out=tf, keep="^EARP$", keep.stat="n",
                                          omit.stat=c("rsq","adj.rsq","ser","f","ll","aic","bic"), header=FALSE)


sl <- readLines(tf); ei <- grep("^\\s*EARP\\s*&", sl); earp <- sl[ei]; se <- sl[ei+1]

# reformat Observations with commas (stargazer prints without)
Ns <- vapply(regs, function(m) tryCatch(stats::nobs(m), error=function(e) NA), numeric(1))

obs <- paste("Observations &", paste(format(Ns, big.mark=","), collapse=" & "), "\\\\")

mean_line <- paste("Mean &", paste(sprintf("%.3f", c(
  compute_mean(expansion_join_drop4,"electrified_2011"),
  compute_mean(expansion_join_drop4,"electrified_2014"),
  compute_mean(expansion_join_drop4,"electrified_2017"),
  compute_mean(expansion_join_drop4,"electrified_2020"))), collapse=" & "), "\\\\")

writeLines(c(
  "\\begin{table}[!htbp] \\centering",
  "  \\caption{Regression Results: Electrification by Year}",
  "  \\label{}",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccc}",
  "\\\\[-1.8ex]\\hline","\\hline \\\\[-1.8ex]",
  " & \\multicolumn{4}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-5}",
  "\\\\[-1.8ex] & electrified\\_2011 & electrified\\_2014 & electrified\\_2017 & electrified\\_2020 \\\\",
  "\\\\[-1.8ex] & (1) & (2) & (3) & (4)\\\\",
  "\\hline \\\\[-1.8ex]",
  earp, se, "  & & & & \\\\",
  "\\hline \\\\[-1.8ex]",
  obs, "FE: Cell ID & X & X & X & X \\\\", mean_line,
  "\\hline","\\hline \\\\[-1.8ex]",
  "\\textit{Note:}  & \\multicolumn{4}{r}{$^{*}$p$<0.1; $^{**}$p$<0.05; $^{***}$p$<0.01} \\\\",
  "\\end{tabular}","\\end{table}"
), file.path(output_path,"first_stage.tex"))


#Graph----


years <- 2014:2022

coef_df <- map_dfr(years, function(y) {
  dep <- paste0("electrified_", y)
  if (!dep %in% names(expansion_join_drop4)) return(NULL)
  
  f <- as.formula(paste0(dep, " ~ EARP"))
  m <- lm(f, data = expansion_join_drop4)
  
  cf <- coef(m)
  intercept <- unname(cf["(Intercept)"])
  beta_earp <- unname(cf["EARP"])
  
  tibble(
    year = y,
    intercept = intercept,
    beta_earp = beta_earp,
    pred_EARP0 = intercept,              # baseline
    pred_EARP1 = intercept + beta_earp   # baseline + effect of EARP
  )
})

# reshape for plotting
plot_df <- coef_df %>%
  select(year, pred_EARP0, pred_EARP1) %>%
  pivot_longer(c(pred_EARP0, pred_EARP1),
               names_to = "group", values_to = "value") %>%
  mutate(group = recode(group,
                        pred_EARP0 = "EARP = 0",
                        pred_EARP1 = "EARP = 1"))


p <- ggplot(plot_df, aes(x = year, y = value, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(value, 3)),
            vjust = -0.8,
            size = 4) +
  scale_x_continuous(breaks = years) +
  labs(x = "Electrified Year", y = "Probility of eletrification", color = NULL,
       title = "") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p

# Save to output path
ggsave(
  filename = file.path(output_path, "linear_model_predictions.png"),
  plot = p,
  width = 10, height = 6, dpi = 300, bg = "white"
)

