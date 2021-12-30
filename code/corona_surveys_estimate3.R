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
try(source("script-get-oxford-data.R"), silent = T)
try(source("script-get-jhu-data-v2.R"), silent = T)
try(source("script-confirmed3.R"), silent = T)

# UMD estimates from the API
# try(source("script-umd_batch_symptom_country.R"), silent = T)
# try(source("script-umd_batch_symptom_region.R"), silent = T)
# UMD estimates from the microdata
# try(source("script-umd-country-v4.R"), silent = T)
# try(source("script-umd-region-v2.R"), silent = T)
# try(source("script-umd-age-v2.R"), silent = T)
# try(source("script-umd-regions-plot.R"), silent = T)

# try(source("script-30responses.R"), silent = T)
# try(source("script-300responses-v2.R"), silent = T) # To be revised. It crashes because of the format of file regions-tree-population.csv
# try(source("script-300responses-smooth.R"), silent = T)

# try(source("script-W-alpha.R"), silent = T)
try(source("script-W.R"), silent = T)

try(source("script-regions-daily.R"), silent = T)
try(source("script-regions-no-province-daily.R"), silent = T)
try(source("script-provinces-daily.R"), silent = T)
try(source("script-provinces-map.R"), silent = T)
try(source("script-provinces-plot.R"), silent = T)

try(source("script-combined-region-province.R"), silent = T)

# The estimates based on fatalities use the Oxford official data. Has to be changes if using JHU
try(source("script-ccfr-fatalities-country.R"), silent = T)

try(source("script-ccfr-based-v4.R"), silent = T)
# try(source("script-ES-ccfr-based.R"), silent = T)

try(source("script-rivas-arganda-daily.R"), silent = T)

# try(source("script-liverpool-daily.R"), silent = T)

try(source("participation-ranking-v2.R"), silent = T)

