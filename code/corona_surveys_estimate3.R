## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)
#source("get_jh_data.R") # function to get data from jh csce
#source("get_twitter_data.R") # function to get twitter data
#source("spain_regional_estimate.R")
#source("portugal_regional_estimates.R")
#source("spain_region_based_estimate.R")
#source("portugal_region_based_estimate.R")
#source("italy_region_based_estimate.R")
#source("ukraine_region_based_estimate.R")

# compute new decentralized estimates
try(source("script-get-oxford-data.R"), silent = F)
try(source("script-confirmed2.R"), silent = F)
try(source("script-ccfr-based3.R"), silent = F)
try(source("script-ccfr-fatalities-country.R"), silent = F)
try(source("script-ES-ccfr-based.R"), silent = F)

try(source("script-umd_batch_symptom_country.R"), silent = F)
try(source("script-umd_batch_symptom_region.R"), silent = F)

try(source("script-30responses.R"), silent = F)
try(source("script-300responses-v2.R"), silent = F)
try(source("script-300responses-smooth.R"), silent = F)

try(source("script-W-alpha.R"), silent = F)
try(source("script-W.R"), silent = F)

# try(source("script-rivas-arganda-daily.R"), silent = F)

# try(source("script-liverpool-daily.R"), silent = F)

try(source("script-provinces-daily.R"), silent = F)
try(source("script-provinces-map.R"), silent = F)
try(source("script-provinces-plot.R"), silent = F)
