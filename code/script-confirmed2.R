# load library
library(tidyverse)
library(readxl)
library(httr)

# pop_file <- "../data/common-data/oxford-umd-country-population.csv"
pop_file <- "../data/common-data/unified-country-list.csv"
data_path <- "../data/jhu/PlotData/"
estimates_path <- "../data/estimates-confirmed/PlotData/"

# pop_file <- "../coronasurveys/data/common-data/oxford-umd-country-population.csv"
# data_path <- "../coronasurveys/data/jhu/PlotData/"
# estimates_path <- "./estimates-confirmed/PlotData/"

contagious_window <- 12
active_window <- 10 # Changed from 18 on May 30th, 2021

plot_estimates <- function(country_geoid = "AF", dts, 
                           contagious_window,
                           active_window){
  cat("::- script-confirmed: Working on", country_geoid, "::\n")
  # data <- dts %>% 
  #   select(dateRep:popData2019, "Alpha.2.code" )
  dt <- dts %>% select(date, cum_cases=Confirmed, cum_deaths=Deaths, countrycode=ISO2)
  
  # data$geoId <- data$Alpha.2.code 
  # data <- data %>% select(dateRep:popData2019)
  # data <- data[data$geoId == country_geoid,]
  
  #dt <- as.data.frame(data[rev(1:nrow(data)),])
  ####### fix NAs in cases and deaths #######
  # dt$cases[is.na(dt$cases)] <- 0
  # dt$deaths[is.na(dt$deaths)] <- 0
  # ##########################################
  # dt$cum_cases <- cumsum(dt$cases)
  # dt$cum_deaths <- cumsum(dt$deaths)
  dt$cum_cases[is.na(dt$cum_cases)] <- 0
  dt$cum_deaths[is.na(dt$cum_deaths)] <- 0
  dt$cases <- c(0,diff(dt$cum_cases))
  dt$deaths <- c(0,diff(dt$cum_deaths))
  
  dt$cases_daily <- dt$cases
  dt$cases_infected <- dt$cum_cases
  
  #dt$date <- gsub("-", "/", as.Date(dt$dateRep, format = "%d/%m/%Y"))
  # dt$date <- gsub("-", "/", as.Date(dt$Date, format = "%Y-%m-%d"))
  
  if (nrow(dt) >= contagious_window){
    dt$cases_contagious <- cumsum(c(dt$cases[1:contagious_window], diff(dt$cases, lag = contagious_window))) # Carlo active cases
  }
  else {
    dt$cases_contagious <- NA
  }
  
  #symptomatic
  if (nrow(dt) >= active_window){
    dt$cases_active <- cumsum(c(dt$cases[1:active_window], diff(dt$cases, lag = active_window)))
  }
  else {
    dt$cases_active <- NA
  }
  
  # - Cases_infected: Population that is or has been infected of COVID-19.
  # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
  # - Cases_contagious: Those infected that can transmit the virus on a given day (assumes a case is contagious 12 days after infected)
  # - Cases_active: Those infected whose case is still active on a given day (assumes a case is active 18 days after infected)
  
  pop_data <- read.csv(pop_file, as.is = T)
  dt$population <- pop_data$population[pop_data$ISO2 == country_geoid][1]

  # dt <- dt %>% 
  #   mutate(countrycode = ifelse(country_geoid == "NA", "NA", pop_data$geo_id[pop_data$CountryCode == dt$CountryCode[1]])) %>% 
  #   select(date, countrycode, population, cases, deaths, cases_infected, cum_deaths, 
  #          cases_contagious, cases_active) %>% 
  #   rename(cases_daily = cases) 
  # mutate(p_cases_infected = cases_infected/population,
  #        p_cases_daily = abs(cases_daily/population),
  #        p_cases_contagious = abs(cases_contagious/population),
  #        p_cases_active = abs(cases_active/population)) %>% 
  # %>% 
  #   select(date, cases, deaths, cases_infected, cum_deaths, cases_contagious, cases_infect, cases_active, p_cases, p_cases_daily, p_cases_contagious, p_infect, p_cases_active, population)


  
  dt$p_cases_infected <- abs(dt$cases_infected/dt$population)
  dt$p_cases_daily <- abs(dt$cases_daily/dt$population)
  dt$p_cases_contagious <- abs(dt$cases_contagious/dt$population)
  dt$p_cases_active <- abs(dt$cases_active/dt$population)
    
  dir.create(estimates_path, showWarnings = F)
  # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0(estimates_path, country_geoid, "-estimate.csv"), row.names = FALSE)
}

# find countries with data available
generate_estimates <- function(c_window = contagious_window,
                               a_window = active_window){
  allFiles<-list.files(data_path)
  country_codes <- unique(substr(allFiles,1,2))
  # country_codes <- sapply(str_split(list.files(data_path), pattern = "*.csv"), function(x) x[[1]])
  # country_codes <- country_codes[!grepl("_", country_codes, fixed = T)]
  dx <- sapply(country_codes, function(x){
    df <- read.csv(paste0(data_path, x, ".csv"), as.is = T)
    plot_estimates(x, dts = df,
                   contagious_window = c_window,
                   active_window = a_window)
  })
}
generate_estimates()
