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

pop_file <- "../data/common-data/unified-country-list.csv"
# data_path_JHU <- "../data/jhu/PlotData/"
# data_path_Ox <- "../data/oxford/PlotData/"
data_path_OWID <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
estimates_path <- "../data/estimates-confirmed-OWID/PlotData/"

# pop_file <- "../coronasurveys/data/common-data/unified-country-list.csv"
# data_path_JHU <- "../coronasurveys/data/jhu/PlotData/"
# data_path_Ox <- "../coronasurveys/data/oxford/PlotData/"
# estimates_path <- "./estimates-confirmed/"

smooth_param <- 30

contagious_window <- 10 # Changed from 12 on Nov 28th, 2021
active_window <- 10 # Changed from 18 on May 30th, 2021

get_slope7 <- function(x) {
  slp <- NA
  if(sum(is.na(x) | is.infinite(x)) == 0) {
    # dt <- data.table(index=seq(1,7), value=x)
    # slp <- lm(value ~ index, data=dt)$coefficients[2]
    slp <- (x[7]-x[1])/6
  }
  return(slp)
}

plot_estimates <- function(dt,country_geoid = "AF", 
                           contagious_window,
                           active_window){
  # cat("::- script-confirmed: Working on", country_geoid, "::\n")
  
  dt$date <- as.Date(dt$date)
  
  pop_data <- read.csv(pop_file, as.is = T, na.string = "NaN")
  pop <- which(pop_data$ISO3 == country_geoid)
  dt$population <- pop_data$population[pop[1]]
  
  print(dt[1,])
  
  dt$cases_daily <- dt$cases
  dt$cases_infected <- dt$cum_cases
  
  # Contagious cases
  # Version 1: A new case is propagated to the future
  # if (nrow(dt) >= contagious_window){
  #   dt$cases_contagious <- cumsum(c(dt$cases[1:contagious_window], diff(dt$cases, lag = contagious_window))) # Carlo active cases
  # }
  # else {
  #   dt$cases_contagious <- NA
  # }
  # Version 2: A new case is scaled up by active_window (added 2021-11-28)
  # dt$cases_contagious <- contagious_window * dt$cases
  
  # Active cases
  # Version 1: A new case is propagated to the future
  # if (nrow(dt) >= active_window){
  #   dt$cases_active <- cumsum(c(dt$cases[1:active_window], diff(dt$cases, lag = active_window)))
  # }
  # else {
  #   dt$cases_active <- NA
  # }
  # Version 2: A new case is scaled up by active_window (added 2021-09-26)
  dt$cases_active <- active_window * dt$cases

  # - Cases_infected: Population that is or has been infected of COVID-19.
  # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
  # - Cases_contagious: Those infected that can transmit the virus on a given day
  # - Cases_active: Those infected whose case is still active on a given day. Since we have defined a case to be active if it
  # is infectious, this subsumes Cases_contagious, which is removed.
  
  dt$p_cases_infected <- abs(dt$cases_infected/dt$population)
  dt$p_cases_daily <- abs(dt$cases_daily/dt$population)
  # dt$p_cases_contagious <- abs(dt$cases_contagious/dt$population)
  dt$p_cases_active <- abs(dt$cases_active/dt$population)
  
  dt$p_cases_active_smooth <- 
    with(dt, ksmooth(date, dt$p_cases_active, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  # dt$p_cases_active_smooth_slope <- rollapply(dt$p_cases_active_smooth,7,get_slope7,fill=0,align="right")
  # dt$p_cases_active_smooth_slope2 <- rollapply(dt$p_cases_active_smooth_slope,7,get_slope7,fill=0,align="right")
  cat("HERE")
  View(dt)
  # dir.create(estimates_path, showWarnings = F)
  # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  # write.csv(dt, paste0(estimates_path, country_geoid, "-estimate.csv"), row.names = FALSE)
}

#MAIN code

df<- read.csv(data_path_OWID, as.is = T) %>% select(date, location,
                                                    ISO3 = iso_code,
                                                    cases = new_cases, 
                                                    deaths = new_deaths, 
                                                    cum_cases = total_cases, 
                                                    cum_deaths = total_cases)
countries <- unique(df$ISO3)
for (i in countries) {
  if (nrow(df)>0) {
    df_now <- df[which(df$ISO3 == i),]
    print(dim(df_now))
    plot_estimates(df_now,i,contagious_window,active_window)
  }
}

