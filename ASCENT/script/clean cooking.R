##################################
#Author: Xiaoming Zhang
#Purpose: Clean cooking sheet read, prepare for mission
#Date 12.22.2025
#####################################################



pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, ggpubr, purrr, lfe, ggrepel, scales, install = TRUE)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

output_path <- file.path(
  dropbox,
  "Rwanda Energy/ASCENT/datawork/output"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/ASCENT/datawork/data"
)


#Read rwa_villages file-----



sales <- read_xlsx(path = file.path(data_path,  "rbf_edcl_overview.xlsx"), sheet = "Sales")

sales <- sales %>% 
  clean_names() %>% 
  filter(purpose_of_sale == "Home use")

pre_registration <- read_xlsx(path = file.path(data_path,  "rbf_edcl_overview.xlsx"), sheet = "Pre-registrations")

pre_registration <- pre_registration %>% 
  clean_names() %>% 
  filter(purpose_of_sale == "Home use")

#Full sample------
##Company plot-------

company_df <- sales %>%
  count(company) %>%
  mutate(
    pct = n / sum(n),
    pct_label = percent(pct, accuracy = 1),
    label_plot = paste0(pct_label, "(", n, ")")
  ) %>% 
  arrange(desc(pct))  # sort by percentage

p_company_all <- ggplot(company_df, aes(x = reorder(company, pct), y = pct, fill = company)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label =label_plot),
            hjust = -0.05, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(title = "Share of Sales by Company(Full sample)",
       x = NULL,
       y = "Share of Sales") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  )

p_company_all

ggsave(
  filename = file.path(output_path, "sales_company_all.png"),
  plot = p_company_all,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white",
  scale = 0.6
)


##Product plot-------
product_df <- sales %>%
  count(product_fuel) %>%
  mutate(
    pct = n / sum(n),
    pct_label = percent(pct, accuracy = 1),
    label_plot = paste0(product_fuel, "(",n, ")"," – ", pct_label)
  )

p_fuel_all <- ggplot(product_df, aes(x = "", y = pct, fill = product_fuel)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label_plot),
            position = position_stack(vjust = 0.5),
            color = "black") +
  labs(title = "Share of Sales by Product Fuel (Full sample)",
       x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

p_fuel_all




ggsave(
  filename = file.path(output_path, "sales_fuel_all.png"),
  plot = p_fuel_all,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white",
  scale = 0.6
)



#Filter for cells in these cities-------
rwa_cells <- st_read(dsn = file.path(data_path, "rwa_cell", "Cell.shp"))

rwa_cells <- rwa_cells %>% 
  clean_names() %>% 
  mutate(across(c(district, sector, name), str_to_title)) %>% 
  rename(cell = name) %>% 
  select(cell_id, district, sector, cell) %>% 
  distinct(district, sector, cell, .keep_all = TRUE)

sales_cell <-  sales %>% 
  mutate(across(c(district, sector, cell, village), str_to_title)) %>% 
  mutate(cell = ifelse(district == "Kirehe" & sector == "Mahama" & cell == "Munini", "Umunini", cell)) %>% 
  left_join(rwa_cells, by = c("district", "sector", "cell")) %>% 
  st_as_sf() %>%
  st_transform(crs = st_crs(rwa_cells))

pre_registration_cell <- pre_registration %>% 
  mutate(across(c(district, sector, cell, village), str_to_title)) %>% 
  mutate(cell = ifelse(district == "Kirehe" & sector == "Mahama" & cell == "Munini", "Umunini", cell)) %>% 
  left_join(rwa_cells, by = c("district", "sector", "cell")) %>% 
  st_as_sf() %>%
  st_transform(crs = st_crs(rwa_cells))


secondary_cities <- st_read(dsn = file.path(data_path, "Secondary cities", "Secondary cities.shp"))

secondary_cities <- st_transform(secondary_cities, crs = st_crs(rwa_cells))

sales_in_city <- st_intersection(sales_cell, secondary_cities, join = st_within) %>% 
  filter(!is.na(Name)) 

sales_city_sum <- sales_in_city %>% 
  group_by(Name) %>% 
  summarise(sales =n()) %>% 
  st_drop_geometry() 


pre_in_city   <-  st_intersection(pre_registration_cell, secondary_cities, join = st_within)%>% 
  filter(!is.na(Name)) 


pre_city_sum <- pre_in_city %>% 
  group_by(Name) %>% 
  summarise(pre_registration = n()) %>% 
  st_drop_geometry()


secondary_cities_join <- secondary_cities%>% 
  st_drop_geometry() %>% 
  left_join(sales_city_sum) %>% 
  left_join(pre_city_sum)


##City sales plot-----

p_city_sales <- ggplot(sales_city_sum, aes(x = reorder(Name, sales), y = sales, fill = Name)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sales),
            hjust = -0.05, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(title = "Number of Sales by Secondary City",
       x = NULL,
       y = "Number of Sales") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

p_city_sales

# Save the figure
ggsave(
  filename = file.path(output_path, "sales_by_secondary_city.png"),
  plot = p_city_sales,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white",
  scale = 0.6
)

##Company plot-------

company_df <- sales_in_city%>%
  count(company) %>%
  mutate(
    pct = n / sum(n),
    pct_label = percent(pct, accuracy = 1),
    label_plot = paste0(pct_label, "(", n, ")")
  ) %>% 
  arrange(desc(pct))  # sort by percentage

p_company_sec <- ggplot(company_df, aes(x = reorder(company, pct), y = pct, fill = company)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label =label_plot),
            hjust = -0.05, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(title = "Share of Sales by Company(Secondary Cities)",
       x = NULL,
       y = "Share of Sales") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  )

p_company_sec

ggsave(
  filename = file.path(output_path, "sales_company_sec.png"),
  plot = p_company_sec,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white",
  scale = 0.6
)


##Product plot-------
product_df <- sales_in_city %>%
  count(product_fuel) %>%
  mutate(
    pct = n / sum(n),
    pct_label = percent(pct, accuracy = 1),
    label_plot = paste0(product_fuel, "(",n, ")"," – ", pct_label)
  )

p_fuel_sec <- ggplot(product_df, aes(x = "", y = pct, fill = product_fuel)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label_plot),
            position = position_stack(vjust = 0.5),
            color = "black") +
  labs(title = "Share of Sales by Product Fuel (Secondary Cities)",
       x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

p_fuel_sec




ggsave(
  filename = file.path(output_path, "sales_fuel_sec.png"),
  plot = p_fuel_sec,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white",
  scale = 0.6
)

