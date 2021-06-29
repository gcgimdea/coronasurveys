# <!-- Copyright {{ 2021 }} {{ IMDEA Networks Institute }} -->
# <!-- Author {{ Jesús Rufino, Oluwasegin Ojo, Antonio Fernández Anta }} {{https://coronasurveys.org/}} -->
# <!-- Licensed under the Apache License, Version 2.0 (the "License"); -->
# <!-- you may not use this file except in compliance with the License. -->
# <!-- You may obtain a copy of the License at -->
#
# <!-- http://www.apache.org/licenses/LICENSE-2.0 -->
#
# <!-- Unless required by applicable law or agreed to in writing, software -->
# <!-- distributed under the License is distributed on an "AS IS" BASIS, -->
# <!-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express -->
# <!-- or implied. See the License for the specific language governing -->
# <!-- permissions and limitations under the License. -->

# load library
library(tidyverse)
library(readxl)
library(httr)

pop_file <- "../data/common-data/unified-country-list.csv"
data_path_JHU <- "../data/jhu/PlotData/"
data_path_Ox <- "../data/oxford/PlotData/"
estimates_path <- "../data/estimates-confirmed/PlotData/"

# pop_file <- "../coronasurveys/data/common-data/unified-country-list.csv"
# data_path_JHU <- "../coronasurveys/data/jhu/PlotData/"
# data_path_Ox <- "../coronasurveys/data/oxford/PlotData/"
# estimates_path <- "./estimates-confirmed/"


contagious_window <- 12
active_window <- 10 # Changed from 18 on May 30th, 2021

plot_estimates <- function(dt,country_geoid = "AF", 
                           contagious_window,
                           active_window){
  # cat("::- script-confirmed: Working on", country_geoid, "::\n")
  
  pop_data <- read.csv(pop_file, as.is = T, na.string = "NaN")
  pop <- which(pop_data$ISO2 == country_geoid)
  dt$population <- pop_data$population[pop[1]]
  
  dt$cum_cases[is.na(dt$cum_cases)] <- 0
  dt$cum_deaths[is.na(dt$cum_deaths)] <- 0
  dt$cases <- c(0,diff(dt$cum_cases))
  dt$deaths <- c(0,diff(dt$cum_deaths))
  
  dt$cases_daily <- dt$cases
  dt$cases_infected <- dt$cum_cases
  
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
  
  dt$p_cases_infected <- abs(dt$cases_infected/dt$population)
  dt$p_cases_daily <- abs(dt$cases_daily/dt$population)
  dt$p_cases_contagious <- abs(dt$cases_contagious/dt$population)
  dt$p_cases_active <- abs(dt$cases_active/dt$population)
    
  dir.create(estimates_path, showWarnings = F)
  # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0(estimates_path, country_geoid, "-estimate.csv"), row.names = FALSE)
}

#Takes the countries ISO2 and make a union between Oxford data and JHU data
read_country_list <- function(data_path_JHU,data_path_Ox){
  allFiles<-list.files(data_path_JHU, pattern = ".csv")
  allFiles1<-list.files(data_path_Ox, pattern = ".csv")
  country_codes_JHU <- unique(substr(allFiles,1,2))
  country_codes_Ox <- unique(substr(allFiles1,1,2))
  country_codes_Total<- union(country_codes_JHU,country_codes_Ox)
  return(country_codes_Total)
}


Read_country_data<- function(country,data_path_JHU,data_path_Ox){
  cat("Country ",country,"\n")
  
  if(file.exists(paste0(data_path_JHU, country, ".csv")) & file.exists(paste0(data_path_Ox, country, "-estimate.csv"))){
    # cat("HERE 1","\n")
    df <- read.csv(paste0(data_path_JHU, country, ".csv"), as.is = T) %>% 
      select(date, countrycode=ISO2, cum_cases=Confirmed, cum_deaths=Deaths) %>% 
      filter(!is.na(cum_cases))
    
    df1 <- read.csv(paste0(data_path_Ox, country, "-estimate.csv"), as.is = T) %>% 
      select(date=Date, countrycode=iso2, cum_cases=ConfirmedCases, cum_deaths=ConfirmedDeaths) %>%
      filter(!is.na(cum_cases))

    if(max(df1$date) > max(df$date)){
      df <- df1
    }
  }
  else if(file.exists(paste0(data_path_JHU, country, ".csv"))){
    df <- read.csv(paste0(data_path_JHU, country, ".csv"), as.is = T) %>% 
      select(date, countrycode=ISO2, cum_cases=Confirmed, cum_deaths=Deaths) %>%
      filter(!is.na(cum_cases))
  }
  else if(file.exists(paste0(data_path_Ox, country, "-estimate.csv"))){
    df <- read.csv(paste0(data_path_Ox, country, "-estimate.csv"), as.is = T) %>% 
      select(date=Date, countrycode=iso2, cum_cases=ConfirmedCases, cum_deaths=ConfirmedDeaths) %>%
      filter(!is.na(cum_cases))
  }
  else{
    cat("More than one data with this iso2","\n")
    print=F
  }
  return(df)
}

#MAIN code
countries<- read_country_list(data_path_JHU,data_path_Ox) #list
for (i in countries) {
  df<- Read_country_data(i,data_path_JHU,data_path_Ox)
  # cat("Just read")
  if (nrow(df)>0) {
    plot_estimates(df,i,contagious_window,active_window)
  }
}

