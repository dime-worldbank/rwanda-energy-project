library(tidyverse)

dropbox <- "C:/Users/wb614406/Dropbox"

aggregator_path <- file.path(
  dropbox,
  "Rwanda Feeder Roads",
  "Trucker Experiment",
  "SCTO_backup",
  "Aggregator"
)

data <- list(
  baseline = read_csv(file.path(
    aggregator_path,
    "Baseline",
    "data",
    "Aggregator Baseline.csv"
  )),
  
  midline = read_csv(file.path(
    aggregator_path,
    "Midline",
    "data",
    "Aggregator Midline.csv"
  )),
  
  endline = read_csv(file.path(
    aggregator_path,
    "Endline",
    "data",
    "Aggregator endline survey.csv"
  ))
)