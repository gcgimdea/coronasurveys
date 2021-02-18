library(tidyverse)
# library(readxl)
# library(httr)
library(stringi)

source("smooth_greedy_monotone.R")

country_codes_file <- "../data/common-data/wikipedia-iso-country-codes.xlsx"
estimates_path <- "../data/estimates-ccfr-fatalities/PlotData/"
ox_country_path <- "../data/oxford/" # Oxford data

# country_codes_file <- "../coronasurveys/data/common-data/wikipedia-iso-country-codes.xlsx"
# estimates_path <- "./estimates-ccfr-fatalities/PlotData/"

#url_ecdc <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"

contagious_window <- 12
active_window <- 18
onset_to_death_window <- 13 # https://www.cdc.gov/coronavirus/2019-ncov/hcp/planning-scenarios.html#table-2
CFR <- 0.0138
# factor_window <- 14

plot_estimates <- function(country_geoid, code3,
                           dts){
  cat("::- script-ccfr-fatalities: Computing estimates for", country_geoid, "::\n")
  # data <- dts %>% 
  #   select(dateRep:popData2019, "Alpha.2.code" )
  # data$geoId <- data$Alpha.2.code 
  # data <- data %>% select(dateRep:popData2019)
  # data <- data[data$geoId == country_geoid,]
  data <- dts[dts$CountryCode == code3,]
  
  #cat(min(data$Date), nrow(data), country_geoid, code3, ".\n")
  
  dt <- as.data.frame(data[rev(1:nrow(data)),])
  
  dt$date <- as.Date(dt$Date)

  dt <- dt %>% 
  #   select(date, geoId, popData2019, cases, deaths) %>% 
    rename(countrycode = iso2) %>% 
    select(date, countrycode, population, cases, deaths)
  
  # - Cases_infected: Population that is or has been infected of COVID-19.
  # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have symptoms or not.
  # - Cases_contagious: Those infected that can transmit the virus on a given day (assumes a case is contagious 12 days after infected)
  # - Cases_active: Those infected whose case is still active on a given day (assumes a case is active 18 days after infected)
  
  ####### fix NAs values in cases and deaths #######
  dt$cases[is.na(dt$cases)] <- 0
  dt$deaths[is.na(dt$deaths)] <- 0
  
  cat(min(dt$date), nrow(dt), ".\n")
  # Add onset_to_death_window rows to dt
  ds <- setdiff(seq(as.Date(min(dt$date)-onset_to_death_window), as.Date(max(dt$date)), by="days"),
                dt$date)
  cat(".\n")
  dt2 <- data.frame("date" = ds, 
                    "countrycode" = country_geoid, 
                    "population" = dt$population[1], 
                    "cases" = 0, 
                    "deaths" = 0)

  dt <- rbind(dt2,dt)
  
  dt <- dt[order(dt$date),]
 
  dt$cum_cases <- smooth_greedy(cumsum(dt$cases))
  dt$cum_deaths <- smooth_greedy(cumsum(dt$deaths))
  
  if (nrow(dt)>onset_to_death_window){
    aux <- dt$cum_deaths / CFR
    len <- length(aux)
    
    # factor = detectio ratio of cumulative cases
    # factor <- max(1, (aux[len]-aux[len-factor_window])/ (dt$cum_cases[len]-dt$cum_cases[len-factor_window]))

    aux[1:(len-onset_to_death_window)] <- aux[-(1:onset_to_death_window)]
    
    # aux[(len-onset_to_death_window+1):len] <- aux[len-onset_to_death_window] + factor *
    #   (dt$cum_cases[(len-onset_to_death_window+1):len] - dt$cum_cases[len-onset_to_death_window])
    
    aux[(len-onset_to_death_window+1):len] <- NA
   
     # cat("::- script-ccfr-fatalities: Factor ", factor, "::\n")
    
    dt$cases_infected <- aux
    dt$cases_daily <- c(0, diff(dt$cases_infected))
  }
  else {
    dt$cases_infected <- 
      dt$cases_daily <- NA
  }
  
  #total contagious cases
  if (nrow(dt) >= contagious_window){
    dt$cases_contagious <- cumsum(c(dt$cases_daily[1:contagious_window],
                                diff(dt$cases_daily, lag = contagious_window)))
  }
  else {dt$cases_contagious <- NA}
  
  #active
  if (nrow(dt) >= active_window){
    dt$cases_active <- cumsum(c(dt$cases_daily[1:active_window],
                                 diff(dt$cases_daily, lag = active_window)))
  }
  else {dt$cases_active <- NA}
  
  dt$p_cum_cases <- dt$cum_cases/dt$population
  dt$p_cum_deaths <- dt$cum_deaths/dt$population
  dt$p_cases_infected <- dt$cases_infected/dt$population
  dt$p_cases_daily <- dt$cases_daily/dt$population
  dt$p_cases_contagious <- dt$cases_contagious/dt$population
  dt$p_cases_active <- dt$cases_active/dt$population
  
  dt$date <- as.Date(dt$date, origin="1970-01-01")
  dt$date <- gsub("-", "/", as.Date(dt$date))
  
  dir.create(estimates_path, showWarnings = F)
  # cat("::- script-ccfr-based: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0(estimates_path, country_geoid, "-estimate.csv"), row.names = FALSE)
}


# find countries with oxford data available
generate_estimates <- function(){
  country_codes <- list.files(ox_country_path, pattern="*.csv", full.names=FALSE)
  country_codes <- word(country_codes,1,sep = "-")
  country_codes <- country_codes[stri_length(country_codes)==2]
  # country_codes <- sapply(str_split(list.files("../data/oxford/country/"), pattern = "-"), function(x) x[[1]])
  # country_codes <- country_codes[!grepl("_", country_codes, fixed = T)]
  # country_codes <- country_codes[!is.na(country_codes)]
  # country_codes <- country_codes[country_codes != ""]
  cat(country_codes, "\n")
  for (c in country_codes) {
    cat(paste0(ox_country_path, c, "-estimate.csv"))
    df <- read.csv(paste0(ox_country_path, c, "-estimate.csv"))
    plot_estimates(c, df$CountryCode[1], dts = df)
  }
                   
  # dx <- sapply(country_codes, function(x){
  #   df <- read.csv(paste0("../data/oxford/country/", x, "-estimate.csv"), as.is = T)
  #   plot_estimates(x, dts = df)})
}


# generate_estimates <- function(){
# 
#   url <- paste(url_ecdc, Sys.Date(), ".xlsx", sep = "")
#   GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#   cat("::- script-ccfr-fatalities: Checking the ECDC data for the day ::\n")
#   try( data_ecdc <- read_excel(tf), silent = T) # ECDC daily excel seems unvailable for now
#   # try( data_ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
#   #                            na.strings = "", fileEncoding = "UTF-8-BOM"), silent = T)
#   
#   if(!exists("data_ecdc")){
#     cat("::- script-ccfr-fatalities: Seems the ECDC data for the day is not available yet ::\n")
#     cat("::- script-ccfr-fatalities: Trying to get data for the previous day ::\n")
#     url <- paste(url_ecdc, Sys.Date()-1, ".xlsx", sep = "")
#     GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#     try( data_ecdc <- read_excel(tf), silent = T)
#     if(!exists("data_ecdc")){
#       stop("::- script-ccfr-fatalities: Unfortunately, the ECDC data for yesterday is not availabe neither ::\n")
#     }else{
#       cat("::- script-ccfr-fatalities: Using ECDC data for previous day ::\n")
#       data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
#       data_country_code <- read_excel(country_codes_file)
#       names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
#                                     "Alpha.3.code", "Numeric.code", "ISO.3166.2")
#       
#       data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
#       
#       all_geo_ids <- unique(data_ecdc$Alpha.2.code)
#       go <- sapply(all_geo_ids, plot_estimates, dts = data_ecdc)
#     }
#   } else{
#     cat("::- script-ccfr-fatalities: ECDC data for the day available! ::\n")
#     data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
#     data_country_code <- read_excel(country_codes_file)
#     names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
#                                   "Alpha.3.code", "Numeric.code", "ISO.3166.2")
#     data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
#     all_geo_ids <- unique(data_ecdc$Alpha.2.code)
#     go <- sapply(all_geo_ids, plot_estimates, dts = data_ecdc)
#   }
#   
# }




generate_estimates()
