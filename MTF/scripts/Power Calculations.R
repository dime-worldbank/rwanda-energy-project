library(tidyverse)
library(dplyr)
library(here)
library(stringr)
library(haven)
library(lmtest)
library(fixest)
library(kableExtra)
library(stargazer)
library(modelsummary)
library(readxl)

getwd()


##Ready board households----
rho <- 0.5
z <- 2.80
signma <- 1
signma_sqr <- signma^2
mdf <- 0.17
mdf_sqr <- mdf^2
m1 <- 162 + 179 + 117 + 280 # number using 

n1 <- ((4 * z * signma_sqr) / mdf_sqr) * (1 + rho * (m1 - 1))

n1 = 143197


##Off-grid households

karongi_priority <- read_xlsx(here("data", "1.Karongi Village Ubudehe & Priority.xlsx"))
rusizi_priority <- read_xlsx(here("data", "2.Rusizi Village Ubudehe & Priority.xlsx"))
rutsiro_priority <- read_xlsx(here("data", "3.Rutsiro Village Ubudehe & Priority.xlsx"))
rulindo_priority <- read_xlsx(here("data", "4.Rulindo Village Ubudehe & Priority.xlsx"))

rho <- 0.5
z <- 2.80
signma <- 1
signma_sqr <- signma^2
mdf <- 0.17
mdf_sqr <- mdf^2
m2 <- 352 # using numbers of first priority villages

n2 <- ((4 * z * signma_sqr) / mdf_sqr) * (1 + rho * (m2 - 1))

n2 = 68401
