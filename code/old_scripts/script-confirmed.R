# load library
library(tidyverse)
library(readxl)
library(httr)
library(stringr)

infectious_window_cases <- 12
symptomatic_window_cases <- 18

plot_estimates <- function(country_geoid = "AF", dts, 
                           ac_window,
                           symptom_window){
  cat("::- script-confirmed: Working on", country_geoid, "::\n")
  # data <- dts %>% 
  #   select(dateRep:popData2019, "Alpha.2.code" )
  dt <- dts %>% select(Date, cases, deaths, CountryCode)
  #data$geoId <- data$Alpha.2.code 
  #data <- data %>% select(dateRep:popData2019)
  #data <- data[data$geoId == country_geoid,]
  
  #dt <- as.data.frame(data[rev(1:nrow(dt)),])
  ####### fix NAs in cases and deaths #######
  dt$cases[is.na(dt$cases)] <- 0
  dt$deaths[is.na(dt$deaths)] <- 0
  ##########################################
  dt$cum_cases <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  
  dt$date <- gsub("-", "/", as.Date(dt$Date, format = "%Y-%m-%d"))
  if (nrow(dt) >= ac_window){
    dt$cases_active <- cumsum(c(dt$cases[1:ac_window], diff(dt$cases, lag = ac_window))) # Carlo active cases
    dt$cases_infect <- cumsum(c(dt$cases[1:ac_window], diff(dt$cases, lag = ac_window))) # Carlo active cases
  }
  else {
    dt$cases_active <- dt$cases_infect <- NA
  }
  
  #symptomatic
  if (nrow(dt) >= symptom_window){
    dt$cases_symptom <- cumsum(c(dt$cases[1:symptom_window], diff(dt$cases, lag = symptom_window)))
  }
  else {
    dt$cases_symptom <- NA
  }
  pop_data <- read.csv("../data/common-data/oxford-umd-country-population.csv", as.is = T)
  dt$population <- pop_data$population[pop_data$CountryCode == dt$CountryCode[1]]
  dt <- dt %>% 
    select(date, cases, deaths, cum_cases, cum_deaths, cases_active, cases_infect, cases_symptom, population) %>% 
    # rename(population = popData2019) %>% 
    mutate(p_cases = cum_cases/population,
           p_cases_daily = cases/population,
           p_cases_active = abs(cases_active/population),
           p_infect = abs(cases_infect/population),
           p_symptom = abs(cases_symptom/population)) %>% 
    select(date, cases, deaths, cum_cases, cum_deaths, cases_active, cases_infect, cases_symptom, p_cases, p_cases_daily, p_cases_active, p_infect, p_symptom, population)
  
  dir.create("../data/estimates-confirmed/PlotData/", showWarnings = F)
  cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0("../data/estimates-confirmed/PlotData/", country_geoid, "-estimate.csv"))
}

# find countries with oxford data available
generate_estimates <- function(active_window_cases = infectious_window_cases,
                               symptom_window_cases = symptomatic_window_cases){
  country_codes <- sapply(str_split(list.files("../data/oxford/"), pattern = "-"), function(x) x[[1]])
  country_codes <- country_codes[!grepl("_", country_codes, fixed = T)]
  dx <- sapply(country_codes, function(x){
    df <- read.csv(paste0("../data/oxford/", x, "-estimate.csv"), as.is = T)
    plot_estimates(x, dts = df, ac_window = active_window_cases, 
                   symptom_window = symptom_window_cases)
  })
}
generate_estimates()



# generate_estimates <- function(active_window_cases,
#                                symptom_window_cases){
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
#                ac_window = active_window_cases, 
#                symptom_window = symptom_window_cases)
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
#                    ac_window = active_window_cases, 
#                    symptom_window = symptom_window_cases)
#     }
#   
# }
# generate_estimates(active_window_cases = infectious_window_cases,
#                    symptom_window_cases = symptomatic_window_cases)
