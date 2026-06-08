############################################################
# Packages
############################################################

library(tidyverse)
library(readxl)
library(modelsummary)
############################################################
# Paths
############################################################

dropbox <- "C:/Users/wb614406/Dropbox"

aggregator_path <- file.path(
  dropbox,
  "Rwanda Feeder Roads",
  "Trucker Experiment",
  "SCTO_backup",
  "Aggregator"
)

data_dir <- file.path(aggregator_path, "Baseline", "data")
form_dir <- file.path(aggregator_path, "Baseline", "forms")
out_dir <- file.path(aggregator_path, "Baseline", "outputs")

dir.create(out_dir, showWarnings = FALSE)

############################################################
# Read data
############################################################

agg <- read_csv(file.path(data_dir, "Aggregator Baseline.csv"))

crops_lookup <- read_excel(
  file.path(form_dir, "aggregator_baseline.xlsx"),
  sheet = "choices"
) %>%
  filter(list_name == "crops") %>%
  transmute(
    code = as.integer(value),
    Crop = `label:english`
  )

############################################################
# Clean data
############################################################

agg <- agg %>%
  filter(consent == 1) %>%
  distinct(aggregator_id, .keep_all = TRUE) %>%
  mutate(
    aggown_rent = factor(
      aggown_rent,
      c(1, 2, 3),
      c("Own", "Rent", "Other")
    ),
    pay_transp = factor(
      pay_transp,
      c(0, 1),
      c("No", "Yes")
    ),
    transp_trucker = factor(
      transp_trucker,
      c(0, 1),
      c("No", "Yes")
    ),
    aggregator_accept = factor(
      aggregator_accept,
      c(0, 1),
      c("No", "Yes")
    ),
    proposed_market = factor(
      proposed_market,
      c(0, 1),
      c("No", "Yes")
    ),
    agg_size = as.numeric(agg_size),
    num_trips = as.numeric(num_trips),
    number_trucker = as.numeric(number_trucker)
  )


# Own vs rent aggregation point----


own_rent <- agg %>%
  count(aggown_rent) %>%
  mutate(
    Percent = round(
      100 * n / sum(n),
      1
    )
  )

write_csv(
  own_rent,
  file.path(out_dir, "own_rent.csv")
)

datasummary_df(
  own_rent,
  output = file.path(out_dir, "own_rent.tex")
)

# Size----
agg_size_summary <- agg %>%
  summarise(
    Min = min(agg_size, na.rm = TRUE),
    P25 = quantile(agg_size, 0.25, na.rm = TRUE),
    Median = median(agg_size, na.rm = TRUE),
    P75 = quantile(agg_size, 0.75, na.rm = TRUE),
    Max = max(agg_size, na.rm = TRUE)
  )

write_csv(
  agg_size_summary,
  file.path(out_dir, "agg_size_summary(tons).csv")
)

datasummary_df(
  agg_size_summary,
  output = file.path(out_dir, "agg_size_summary(tons).tex")
)




# Crops handled----

crop_level <- agg %>%
  pivot_longer(starts_with("agg_crops_"),
               names_to = "var",
               values_to = "N") %>%
  mutate(code = parse_number(var)) %>%
  left_join(crops_lookup, by = "code") %>%
  mutate(Crop = ifelse(is.na(Crop), "Other", Crop))


crop_summary <- crop_level %>%
  filter(N == 1) %>%
  count(Crop, name = "N") %>%
  mutate(Percent = round(100 * N / nrow(agg), 1)) %>%
  arrange(desc(N))

write_csv(
  crop_summary,
  file.path(out_dir, "crop_major_buy.csv")
)

datasummary_df(
  crop_summary,
  output = file.path(out_dir, "crop_major_buy.tex")
)

# Quantity of crops anticipate------



agg_sold <- read_csv(file.path(data_dir, "Aggregator Baseline-survey-aggregator-crop_sales.csv"))

choices <- read_excel(
  file.path(form_dir, "aggregator_baseline.xlsx"),
  sheet = "choices"
)

markup_lookup <- choices %>%
  filter(list_name == "markup_range") %>%
  transmute(
    code = as.integer(value),
    markup = `label:english`
  )
##anticipate season 23A-----
crop_anticipate <- agg_sold %>%
left_join(
    crops_lookup, by = c("crop_index" = "code")
  ) |> 
  left_join(
    markup_lookup, by = c("crop_markup" = "code")
  )
 
crop_antici_summary <- crop_anticipate %>%
  group_by(Crop) %>%
  summarise(
    N = n(),
    Min = min(quant_crops, na.rm = TRUE),
    P25 = quantile(quant_crops, 0.25, na.rm = TRUE),
    Median = median(quant_crops, na.rm = TRUE),
    P75 = quantile(quant_crops, 0.75, na.rm = TRUE),
    Max = max(quant_crops, na.rm = TRUE),
    Mean = mean(quant_crops, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(N))


write_csv(
  crop_antici_summary,
  file.path(out_dir, "crop_antici_summary(kg).csv")
)

datasummary_df(
  crop_antici_summary,
  output = file.path(out_dir, "crop_antici_summary(kg).tex")
)

##Markup-----
crop_markup_summary <- crop_anticipate %>%
  filter(!is.na(markup)) %>%
  count(Crop, markup) %>%
  group_by(Crop) %>%
  mutate(Percent = 100 * n / sum(n)) |> 
  mutate(
    markup = str_remove(markup, " rwf/kg")
  )

p_markup <- ggplot(
  crop_markup_summary,
  aes(x = markup, y = Percent)
) +
  geom_col() +
  facet_wrap(~Crop) +
  labs(
    title = "Usual Price Mark-up (RwF/kg)",
    x = "Mark-up",
    y = "Percent"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(out_dir, "markup_distribution.png"),
  p_markup,
  scale = 0.8
)


#Transportation----

yesno_lookup <- choices %>%
  filter(list_name == "yesno") %>%
  transmute(code = as.integer(value), yesno = `label:english`)

transp_lookup <- choices %>%
  filter(list_name == "transp_means") %>%
  transmute(code = as.integer(value), transport = `label:english`)

reasons_lookup <- choices %>%
  filter(list_name == "reasons") %>%
  transmute(code = as.integer(value), reason = `label:english`)

pie_theme <- theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

#transportation summary=----
transport_summary <- agg %>%
  summarise(across(starts_with("transp_means_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "N") %>%
  mutate(code = parse_number(var)) %>%
  left_join(transp_lookup, by = "code") %>%
  mutate(
    Percent = round(100 * N / nrow(agg), 1)
  ) %>%
  arrange(desc(N)) |> 
  select(transport, Percent)

p_transport <- ggplot(
  transport_summary,
  aes(x = "",y = Percent,fill = paste0(transport, " (", round(Percent, 1), "%)"))
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Transportation Methods", fill = NULL) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

p_transport

ggsave(
  file.path(out_dir, "transport_summary.png"),
  p_transport,
  width = 8,
  height = 6,
  scale = 0.8
)


#pay transportation-----
pay_transport_summary <- agg %>%
  count(pay_transp, name = "N") |> 
  mutate(
    Percent = round(100 * N / sum(N), 1)
  )

p_pay_transport <- ggplot(
  pay_transport_summary, aes(x = "",y = Percent, fill = paste0(pay_transp, " (", round(Percent, 1), "%)")
  )
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Pay for Transportation", fill = NULL) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

p_pay_transport
ggsave(
  file.path(out_dir, "pay_transport.png"),
  p_pay_transport,
  width = 8,
  height = 6,
  scale = 0.8
)
#pay truckers----

trucker_summary <- agg %>%
  count(transp_trucker, name = "N") |> 
  mutate(
    Percent = round(100 * N / sum(N), 1)
  )

p_trucker <- ggplot(
  trucker_summary,
  aes(
    x = "",
    y = Percent,
    fill = paste0(transp_trucker, " (", round(Percent, 1), "%)")
  )
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Ever Pay Truckers", fill = NULL) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

p_trucker

ggsave(
  file.path(out_dir, "ever_pay_trucker.png"),
  p_trucker,
  width = 8,
  height = 6,
  scale = 0.8
)
#no use truckers-----

reasons_summary <- agg %>%
  summarise(across(starts_with("reasons_not_") & !matches("other"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "N") %>%
  mutate(code = parse_number(var)) %>%
  left_join(reasons_lookup, by = c("code")) %>%
  mutate(
    Percent = round(100 * N / nrow(agg), 1)
  ) %>%
  arrange(desc(N))

p_reasons <- ggplot(
  reasons_summary,
  aes(
    x = "",
    y = Percent,
    fill = paste0(reason, " (", round(Percent, 1), "%)")
  )
) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Reasons for Not Using Truckers",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )

p_reasons

ggsave(
  file.path(out_dir, "reasons_summary.png"),
  p_reasons,
  width = 10,
  height = 8,
  scale = 0.8
)

#num trips taken----
num_trips_summary <- agg %>%
  summarise(
    Min = min(num_trips, na.rm = TRUE),
    P25 = quantile(num_trips, 0.25, na.rm = TRUE),
    Median = median(num_trips, na.rm = TRUE),
    P75 = quantile(num_trips, 0.75, na.rm = TRUE),
    Max = max(num_trips, na.rm = TRUE)
  )

write_csv(
  num_trips_summary,
  file.path(out_dir, "num_trips_paid(last12).csv")
)

datasummary_df(
  num_trips_summary,
  output = file.path(out_dir, "num_trips_paid(last12).tex")
)