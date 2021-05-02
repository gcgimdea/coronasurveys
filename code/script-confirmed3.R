# load library
library(tidyverse)
library(readxl)
library(httr)

jhu_data_path <- "../data/jhu/"
pop_data_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
output_path <- "../data/estimates-confirmed/PlotData/"

contagious_window <- 12
active_window <- 18

plot_estimates <- function(country_geoid = "AF", dts, 
                           contagious_window,
                           active_window){
  cat("::- script-confirmed: Working on", country_geoid, "::\n")
  dt <- dts %>% select(date, countrycode=ISO2, population, cases_infected=confirmed, cum_deaths=deaths)
  
  dt$cases_infected[is.na(dt$cases_infected)] <- 0
  dt$cum_deaths[is.na(dt$cum_deaths)] <- 0
  
  dt$cases_daily <- c(NA,diff(dt$cases_infected))
  dt$deaths <- c(NA,diff(dt$cum_deaths))
  
  dt$date <- gsub("-", "/", as.Date(dt$date, format = "%Y-%m-%d"))
  
  if (nrow(dt) >= contagious_window){
    dt$cases_contagious <- 
      cumsum(c(dt$cases_daily[1:contagious_window], diff(dt$cases_daily, lag = contagious_window))) # Carlos active cases
  }
  else {
    dt$cases_contagious <- NA
  }
  
  #symptomatic
  if (nrow(dt) >= active_window){
    dt$cases_active <- cumsum(c(dt$cases_daily[1:active_window], diff(dt$cases_daily, lag = active_window)))
  }
  else {
    dt$cases_active <- NA
  }
  
  # - Cases_infected: Population that is or has been infected of COVID-19.
  # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
  # - Cases_contagious: Those infected that can transmit the virus on a given day (assumes a case is contagious 12 days after infected)
  # - Cases_active: Those infected whose case is still active on a given day (assumes a case is active 18 days after infected)
  
  dt$p_cases_infected <- abs(dt$cases_infected/dt$population)
  dt$p_cases_daily <- abs(dt$cases_daily/dt$population)
  dt$p_cases_contagious <- abs(dt$cases_contagious/dt$population)
  dt$p_cases_active <- abs(dt$cases_active/dt$population)
    
  dir.create(output_path, showWarnings = F)
  # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0(output_path, country_geoid, "-estimate.csv"))
}

# find countries with oxford data available
generate_estimates <- function(c_window = contagious_window,
                               a_window = active_window){
  country_codes <- sapply(str_split(list.files(jhu_data_path), pattern = "-"), function(x) x[[1]])
  # country_codes <- country_codes[!grepl("_", country_codes, fixed = T)]
  dx <- sapply(country_codes, function(x){
    df <- read.csv(paste0(jhu_data_path, x, "-data.csv"), as.is = T)
    plot_estimates(x, dts = df,
                   contagious_window = c_window,
                   active_window = a_window)
  })
}
generate_estimates()
