#####################################
# Title: Power Calculation for Grid
# Author: Xiaoming Zhang
# Date: 2024-01-31
#####################################

#load---

pacman::p_load(tidyverse, here, readxl)

#calculating ICC from WFP gender window----

d<- 0.2 # mde, observed effect
P<- 0.5 
n<-783 # number of households surveyed 
z<- 2.8 
signma_square <- 1
m<- 54 # number of villages/cluster

rho <- (d^2 * n * P * (1 - P) - z^2 * signma_square) / (z^2 * signma_square * (m - 1))

rho

#Power calculation----

# Function to calculate mdf_wellbeing
mde <- function(m, n, signma, base) {
  rho <- rho
  z <- 2.80
  P <- 0.66
  m <- m*0.75
  n <- n*0.75

  mde <- z * signma * sqrt((1 + rho * (m - 1)) / (n * P * (1 - P)))
  return(mde/base)
}

#well-being----

#signma set at 1 because there was no baseline data and it was normal distribution

mde(70, 2100, 1, 1)


#monthly energy spending----

#signma = 21.83, base = 10.83, Table 3--B8: monthly total energy spending in Lee et al.

mde(70, 2100, 21.83, 10.83)


#monthly kerosene spending----
# signma = 2.75, base = 2.64, Table 2 -- B7: monthly kerosene spending 

mde(70, 2100, 2.75, 2.64)


#Electricity consumption----
#signma = 16.31, base = 7.9, 
# Standard deviation and minimum detectable effect come from the Lee et al. paper. 
# Specifically the electricity consumption measurement is in 
# **Table B7: Benchmarking average monthly electricity consumption in kWh and USD - Panel A: Newly connected households in sample**.
# Standard deviation: we are given Q1 and Q3, the standard deviation is calculated using the formula below. 
# Sepcifically the standard deviation is taking the IQR of households that are newly connected in the sample and their consumption in 2016 (Round 1 survey)

mde(70, 2100, 16.31, 7.9)



# Values for m, n, and signma
m_values <- c(38 * 0.75, )  
n_values <- c(1520 * 0.75, )  
signma_values <- c(1, ... )  
base_values <- c(1, )

# Loop through the values and calculate mdf_wellbeing
results <- data.frame()

for (m in m_values) {
  for (n in n_values) {
    for (signma in signma_values) {
      for (base in base_values){
      mde <- mde(m, n, signma)
      result <- data.frame(m = m, n = n, signma = signma, mdf_wellbeing = mdf_wellbeing)
      results <- rbind(results, result)
      }
    }
  }
}

# Display the results
print(results)


























