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
library(zoo)
library(data.table)

pop_file <- "../data/common-data/unified-country-list.csv"
data_path_OWID <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
estimates_path <- "../data/estimates-confirmed/PlotData/"

smooth_param <- 14
smooth_col <- function(x) {
  return(rollmean(x, smooth_param, fill=0, align = "right"))
}

active_window <- 10 # Changed from 18 on May 30th, 2021

plot_estimates <- function(dt,country_geoid, active_window){
  cat("Working on", country_geoid, dim(dt), "::\n")
  
  # dt$date <- as.Date(dt$date)
  
  pop_data <- read.csv(pop_file, as.is = T, na.string = "NaN")
  pop_ISO2 <- pop_data[which(pop_data$ISO3 == country_geoid),]
  dt$ISO2 <- pop_ISO2$ISO2[1]
  dt$countrycode <- pop_ISO2$ISO2[1] # Maintained for historical reasons
  dt <- dt %>% relocate(ISO2, .before = cases)
  dt$population <- pop_ISO2$population[1]

  # - Cases_infected: Population that is or has been infected of COVID-19.
  # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
  # - Cases_contagious: Those infected that can transmit the virus on a given day
  # - Cases_active: Those infected whose case is still active on a given day. Since we have defined a case to be active if it
  # is infectious, this subsumes Cases_contagious, which is removed.
  dt$cases_daily <- smooth_col(dt$cases)
  dt$cases_infected <- smooth_col(dt$cum_cases)
  dt$cases_active <- smooth_col(active_window * dt$cases)
  
  # To be removed
  dt$p_cases_infected <- pmax(0,dt$cases_infected/dt$population)
  dt$p_cases_daily <- pmax(0,dt$cases_daily/dt$population)
  dt$p_cases_active <- pmax(0,dt$cases_active/dt$population)

  dt$p_infected <- pmax(0,dt$cases_infected/dt$population)
  dt$p_daily <- pmax(0,dt$cases_daily/dt$population)
  dt$p_active <- pmax(0,dt$cases_active/dt$population)
  
  if(! is.na(pop_ISO2$ISO2[1])){
    write.csv(dt, paste0(estimates_path, pop_ISO2$ISO2[1], "-estimate.csv"), row.names = FALSE)
  }
  # else{
  #   write.csv(dt, paste0(estimates_path, country_geoid, "-estimate.csv"), row.names = FALSE)
  # }
}

#MAIN code

df <- fread(data_path_OWID, data.table = FALSE)
df<- df %>% select(date, 
                   country = location,
                   ISO3 = iso_code,
                   cases = new_cases, 
                   deaths = new_deaths, 
                   cum_cases = total_cases, 
                   cum_deaths = total_deaths)
df$date <- as.Date(df$date)
df$cases[is.na(df$cases)] <- 0
df$deaths[is.na(df$deaths)] <- 0
df$cum_cases[is.na(df$cum_cases)] <- 0
df$cum_deaths[is.na(df$cum_deaths)] <- 0
cat("Input file:", dim(df), "\n")
countries <- unique(df$ISO3)
for (i in countries) {
  if (nrow(df)>0) {
    df_now <- df[which(df$ISO3 == i),]
    plot_estimates(df_now, i, active_window)
  }
}

