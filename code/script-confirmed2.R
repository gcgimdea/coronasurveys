# load library
library(tidyverse)
library(readxl)
library(httr)

contagious_window <- 12
active_window <- 10 # Changed from 18 on May 30th, 2021

plot_estimates <- function(country_geoid = "AF", dts, 
                           contagious_window,
                           active_window){
  cat("::- script-confirmed: Working on", country_geoid, "::\n")
  # data <- dts %>% 
  #   select(dateRep:popData2019, "Alpha.2.code" )
  dt <- dts %>% select(Date, cases, deaths, CountryCode)
  
  # data$geoId <- data$Alpha.2.code 
  # data <- data %>% select(dateRep:popData2019)
  # data <- data[data$geoId == country_geoid,]
  
  #dt <- as.data.frame(data[rev(1:nrow(data)),])
  ####### fix NAs in cases and deaths #######
  dt$cases[is.na(dt$cases)] <- 0
  dt$deaths[is.na(dt$deaths)] <- 0
  ##########################################
  dt$cases_infected <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  
  #dt$date <- gsub("-", "/", as.Date(dt$dateRep, format = "%d/%m/%Y"))
  dt$date <- gsub("-", "/", as.Date(dt$Date, format = "%Y-%m-%d"))
  
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
  
  pop_data <- read.csv("../data/common-data/oxford-umd-country-population.csv", as.is = T)
  dt$population <- pop_data$population[pop_data$CountryCode == dt$CountryCode[1]]
  
  dt <- dt %>% 
    mutate(countrycode = ifelse(country_geoid == "NA", "NA", pop_data$geo_id[pop_data$CountryCode == dt$CountryCode[1]])) %>% 
    select(date, countrycode, population, cases, deaths, cases_infected, cum_deaths, 
           cases_contagious, cases_active) %>% 
    rename(cases_daily = cases) 
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
    
  dir.create("../data/estimates-confirmed/PlotData/", showWarnings = F)
  # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0("../data/estimates-confirmed/PlotData/", country_geoid, "-estimate.csv"))
}

# find countries with oxford data available
generate_estimates <- function(c_window = contagious_window,
                               a_window = active_window){
  country_codes <- sapply(str_split(list.files("../data/oxford/"), pattern = "-"), function(x) x[[1]])
  country_codes <- country_codes[!grepl("_", country_codes, fixed = T)]
  dx <- sapply(country_codes, function(x){
    df <- read.csv(paste0("../data/oxford/", x, "-estimate.csv"), as.is = T)
    plot_estimates(x, dts = df,
                   contagious_window = c_window,
                   active_window = a_window)
  })
}
generate_estimates()

# 
# generate_estimates <- function(contagious_window,
#                                active_window){
#     url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
#                  Sys.Date(), ".xlsx", sep = "")
#     GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#     cat("::- script-confirmed: Checking the ECDC data for the day ::\n")
#     #try( data_ecdc <- read_excel(tf), silent = T) # ECDC daily excel seems unvailable for now
#     try( data_ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
#                                na.strings = "", fileEncoding = "UTF-8-BOM"), silent = T)
#     
#     if(!exists("data_ecdc")){
#       cat("::- script-confirmed: Seems the ECDC data for the day is not available yet ::\n")
#       cat("::- script-confirmed: Trying to get data for the previous day ::\n")
#       url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
#                    Sys.Date()-1, ".xlsx", sep = "")
#       GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#       try( data_ecdc <- read_excel(tf), silent = T)
#       if(!exists("data_ecdc")){
#         stop("::- script-confirmed: Unfortunately, the ECDC data for yesterday is not availabe neither ::\n")
#       }else{
#         cat("::- script-confirmed: Using ECDC data for previous day ::\n")
#         data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
#         data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
#         names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
#                                       "Alpha.3.code", "Numeric.code", "ISO.3166.2")
#         
#         data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
#         
#         all_geo_ids <- unique(data_ecdc$Alpha.2.code)
#         sapply(all_geo_ids, plot_estimates, dts = data_ecdc,
#                contagious_window = contagious_window, 
#                active_window = active_window)
#       }
#     } else{
#       cat("::- script-confirmed: ECDC data for the day available! ::\n")
#       data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
#       data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
#       names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
#                                     "Alpha.3.code", "Numeric.code", "ISO.3166.2")
#       data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
#       all_geo_ids <- unique(data_ecdc$Alpha.2.code) 
#       go <- sapply(all_geo_ids, plot_estimates, dts =  data_ecdc, 
#                    contagious_window = contagious_window, 
#                    active_window = active_window)
#     }
#   
# }
# generate_estimates(contagious_window = contagious_window,
#                    active_window = active_window)
