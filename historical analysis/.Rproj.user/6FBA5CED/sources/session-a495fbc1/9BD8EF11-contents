
##########################
#Author: Xiaoming Zhang
#Date: 02272024
#purpose:Difference in difference analysis 
############################



pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr, stargazer, janitor, modelsummary, fixest)

getwd()

#ReadDropbox----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


# rwa_wide----
rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))

rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )

#Grid check

mv_2022 <- lm(`2021` ~ connect22_mv , data = rwa_wide)

summary(mv_2022)

##lv----
lv_2022 <- lm(`2021` ~ connect22_lv + connect22_mv + connect22_mv*connect22_lv, data = rwa_wide)

summary(lv_2022)

mv_2011 <- lm(`2011` ~ connect11_mv, data = rwa_wide)

summary(mv_2011)

stargazer(
  # mv_2011,
  mv_2022,
  lv_2022,
  type = "html",
  title = "Table: Gridlines and nightlight value",
  out = "grid_check.html")



#Event study-------

rwa_es <- rwa_long

for (year in 1992:2021) {
  column_name <- as.character(year)
  rwa_es <- mutate(rwa_es, !!column_name := ifelse(Year == year, 1, 0))
}


#All year es----

years <- 1992:2021

# Create a formula for the main effects of each year
main_effects <- paste0("`", years, "`")

# Create a formula for interaction terms with connect11_mv for each year
interaction_terms_connect11 <- paste0("connect11_mv * `", years, "`")

# Create a formula for interaction terms with connect22_mv for each year
interaction_terms_connect22 <- paste0("connect22_mv * `", years, "`")

# Combine all terms into a single formula
formula <- paste("Value ~", 
                 paste(c(main_effects, interaction_terms_connect11, interaction_terms_connect22), collapse = " + "))

# Fit the linear model----
es <- lm(formula, data = rwa_es)

summary(es)


stargazer(
  es,
  type = "html",
  title = "Table:Event Studies",
  out = "Event study 1992-2011.html")



#####extract coefs---- 
coefs <- coef(summary(es))
coefs_df <- data.frame(
  Term = rownames(coefs),
  Estimate = coefs[, 1],
  Std.Error = coefs[, 2]
)



#####Restructure to wide----
print(coefs_df)

coefs_year <- coefs_df %>%
  filter(str_ends(Term, "`")) 

coefs_year$year <-  as.numeric(gsub("`", "", coefs_year$Term))

coefs_year <- coefs_year %>% 
 clean_names()
  

  
#####2011 coeffs----
coefs_11 <- coefs_df%>% 
  filter(str_ends(Term, ":connect11_mv"))

year <- 1992:2020
coefs_11 $ year <- year

coefs_11_join <- coefs_11 %>% 
  rename(
    connect11_mv = Estimate,
    se11_mv = Std.Error
  ) %>% 
  select(connect11_mv, se11_mv, year)

#####2022 coeffs----
coefs_22 <- coefs_df %>% 
  filter(str_ends(Term, ":connect22_mv"))

year <- 1992:2020
coefs_22 $ year <- year

coefs_22_join <- coefs_22 %>% 
  rename(
    connect22_mv = Estimate,
    se22_mv = Std.Error
  ) %>% 
  select(connect22_mv, se22_mv, year)


####join----

coefs_summary <- left_join(coefs_year, coefs_11_join, by = c("year"))
coefs_summary <- left_join(coefs_summary, coefs_22_join, by = c("year"))

coefs_summary <- coefs_summary %>% 
  select(-term)

coefs_summary <- coefs_summary %>% 
  mutate_all(as.numeric)


####Restructure to long----
rm(group)

coefs_long_11 <- coefs_summary %>% 
  select(year, connect11_mv, se11_mv) %>% 
  mutate(group = "connect_2011") %>% 
  rename(estimate = connect11_mv, 
         se = se11_mv)
  
coefs_long_22 <- coefs_summary %>% 
  select(year, connect22_mv, se22_mv) %>% 
  mutate(group = "connect_2022") %>% 
  rename(estimate = connect22_mv, 
         se = se22_mv)

coefs_long.1 <- coefs_summary %>% 
  select(year, estimate, std_error) %>% 
  mutate(group = "control") %>% 
  rename(se = std_error)



coefs_long <- bind_rows(coefs_long.1, coefs_long_11, coefs_long_22)

#####change estimates to the actual number by adding interecepts etc.----

coefs_other <- coefs_df %>% 
  filter(
    Term %in% c("(Intercept)", "connect11_mv", "connect22_mv")
  )

intercept <- coefs_other$Estimate[coefs_other$Term == "(Intercept)"]
connectcoefs_other$Estimate[coefs_other$Term == "connect11_mv"]
connect22 <- coefs_other$Estimate[coefs_other$Term == "(connect22_mv)"]


coefs_long <- coefs_long %>% 
  mutate(
    value = case_when(
      group == "control" ~ estimate + coefs_other$Estimate[coefs_other$Term == "(Intercept)"],
      group == "connect_2011" ~ estimate + coefs_other$Estimate[coefs_other$Term == "(Intercept)"] + coefs_other$Estimate[coefs_other$Term == "connect11_mv"],
      group == "connect_2022" ~ estimate + coefs_other$Estimate[coefs_other$Term == "(Intercept)"] + coefs_other$Estimate[coefs_other$Term == "connect22_mv"],
      .default = NA
    )
  )

?case_when()

###graph----


ggplot(coefs_long, aes(x = as.numeric(year), y = estimate, group = group, color = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                colour = "black", width = 0.1, position = position_dodge(0.1)) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  labs(title = "Event Study Plot",
       x = "Year",
       y = "Coefficient")


ggplot(coefs_long, aes(x = as.numeric(year), y = value, group = group, color = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = value- 1.96 * se, ymax = value + 1.96 * se),
                colour = "black", width = 0.1, position = position_dodge(0.1)) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  labs(title = "Event Study Plot",
       x = "Year",
       y = "Estimated Nightlight")











#Village fixed effects------


# Create a formula for the main effects of each year
main_effects <- paste0("`", years, "`")

# Create a formula for interaction terms with connect11_mv for each year
interaction_terms_connect11 <- paste0("connect11_mv * `", years, "`")

# Create a formula for interaction terms with connect22_mv for each year
interaction_terms_connect22 <- paste0("connect22_mv * `", years, "`")

# Assuming Village_ID is a factor variable
rwa_es$Village_ID <- as.factor(rwa_es$Village_ID)

# Create the formula with village fixed effects
formula_fe <- as.formula(paste("Value ~", 
                               paste(c(main_effects, interaction_terms_connect11), collapse = " + "), 
                               "| Village_ID"))

# Fit the linear model with village fixed effects
es_fe <- feols(formula_fe, data = rwa_es)

summary(es_fe)

modelsummary(es_fe,
  type = "html",
  title = "Table:Event Studies",
  out = "Event study with village fe 1992-2011.html")



#####extract coefs---- 
# Extract coefficient table with estimates and standard errors
coef_table <- coeftable(es_fe)

# Extract standard errors
standard_errors <- sqrt(diag(vcov(es_fe)))

# Convert coeftable to a data frame
coef_table_df <- as.data.frame(coef_table)

# Add standard errors to the data frame
coef_table_df$Std_Error <- standard_errors

# Print the coefficient table with estimates and standard errors
print(coef_table_df)


####Restructure----

coefs_fe_plot <- coef_table_df %>% 
  mutate(term = row.names(coef_table_df))

coefs_fe_plot <- coefs_fe_plot %>% 
  filter(str_ends(term, "mv"))


year <- 1992:2021
coefs_fe_plot $ year <- year



coefs_fe_plot <- coefs_fe_plot %>% 
  clean_names() %>% 
  select(-std_error_2) 

#Center at year 2010 

coefs_fe_plot <- coefs_fe_plot %>% 
  mutate(estimate.1 = estimate -3.0157166)
  

ggplot(coefs_fe_plot, aes(x = as.numeric(year), y = estimate.1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = estimate.1 - 1.96 * std_error, ymax = estimate.1 + 1.96 * std_error),
                colour = "black", width = 0.1, position = position_dodge(0.1)) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  geom_vline(xintercept = c(2008), linetype = "dashed") +
  labs(title = "Event Study Plot",
       x = "Year",
       y = "Estimated Nightlight")
  
















