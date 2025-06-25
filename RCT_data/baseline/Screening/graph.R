#################
#Number in sample
#############################


data_path_1 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

hfc_constr <- read_xlsx(path = file.path(data_path, "hfc_constr.xlsx"))

complete  <- read_xlsx(path = file.path(data_path_1, "Updated scope villages& households","vulnerable households in sample villages.xlsx"))

check <- complete %>% 
  filter(villageid_key == "41140403")

check <- hfc_constr %>% 
  filter(village == "41140403")

hfc_sum <- hfc_constr %>% 
  group_by(village) %>% 
  summarise(
    n_firstround = n()
  )


eligible_number <- village_check %>% 
  select(
    villageid_key, surveys_expected
  ) %>% 
  rename(
    n_secondround = surveys_expected,
    village = villageid_key
  ) %>% 
  mutate(
    village = as.numeric(village)
  )


hfc_sum.1 <- full_join(hfc_sum, eligible_number, by = c("village"))



hfc_sum.1 <- hfc_sum.1 %>% 
  # Remove villages in out15
  filter(!village %in% out15$villageid_key) %>% 
  
  # Replace all NA values with 0
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  
  # Calculate check_number as 20 minus n_firstround
  mutate(
    check_number = 20 - n_firstround,
    check_number = ifelse(check_number <0, 0, check_number),
    
    # n_secondround_check is min(n_secondround, check_number)
    n_secondround_check = ifelse(n_secondround > check_number, check_number, n_secondround),
  ) %>% 
  mutate(
    n_final = n_secondround_check+n_firstround
  )




# Make sure hfc_sum is ungrouped for plotting
hfc_sum.1 <- hfc_sum.1 %>% ungroup()

hfc_sum_save <- hfc_sum.1 %>% 
  rename(
    firstround_complete = n_firstround,
  ) %>% 
  select(
    village, firstround_complete
  )

write_xlsx(hfc_sum, path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/Baseline second round/first_round.xlsx")

# Plot histogram
ggplot(hfc_sum.1, aes(x = n_final)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  labs(
    title = "Histogram of frequency of num_survey ",
    x = "number of survey per village",
    y = "Count of villages"
  ) +
  theme_minimal()


ggplot(hfc_sum.1, aes(x = n_final)) +
  geom_histogram(
    binwidth = 1,
    color = "black",
    fill = "skyblue"
  ) +
  stat_bin(
    binwidth = 1,
    geom = "text",
    aes(label = ..count..),
    vjust = -0.5
  ) +
  labs(
    title = "Histogram of frequency of num_survey",
    x = "Number of surveys per village",
    y = "Count of villages"
  ) +
  theme_minimal()
