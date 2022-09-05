
#!/usr/bin/env Rscript

# <!-- Copyright {{ 2021 }} {{ IMDEA Networks Institute }} -->
# <!-- Author {{ Ananth Venkatesh, Oluwasegun Ojo, Antonio Fernández Anta }} {{https://coronasurveys.org/}} -->
# <!-- Licensed under the Apache License, Version 2.0 (the 'License'); -->
# <!-- you may not use this file except in compliance with the License. -->
# <!-- You may obtain a copy of the License at -->
#
# <!-- http://www.apache.org/licenses/LICENSE-2.0 -->
#
# <!-- Unless required by applicable law or agreed to in writing, software -->
# <!-- distributed under the License is distributed on an 'AS IS' BASIS, -->
# <!-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express -->
# <!-- or implied. See the License for the specific language governing -->
# <!-- permissions and limitations under the License. -->

# load libraries
library(parallel)
library(tidyr)
library(dplyr)
library('xtable')  # install.packages('xtable')

# resource identifiers
responses_path <- '../data/aggregate/'
data_path <- '../data/common-data/unified-country-list.csv'
# provinces_path <- '../data/common-data/provinces-tree-population.csv'
estimates_path <- '../data/estimates-nsum-2022/PlotData/'

# for testing:
# responses_path <- 'https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/aggregate/'
# data_path <- 'https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv'
# provinces_path <- 'https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/provinces-tree-population.csv'
# estimates_path <- './data/'

start_date <- as.Date('2022-07-01')

countries <- c(
  'DE', 'GB', 'PT',
  'GR', 'US', 'CL',
  'ZA', 'JP', 'ES',
  'IT', 'FR'
)

# data info
ci_level <- 0.95
cases_cutoff <- 1  # 3/4 # 1/2 changed on 2022-04-12
fatalities_cutoff <- 3/4 # 1/2
recent_cutoff <- 1  # 3/4 # 1/2 changed on 2022-04-12

max_responses <- 300
max_age <- 30
max_age_recent <- 14
sampling <- 100000  # If the reach is < population/sampling the estimate is NA
sampling_recent <- 100000  # If the reach is < population/sampling_recent the estimate is NA

# helper functions
remove_outliers <- function(dt, ratio_cutoff, fatalities_cutoff) {

  # cat('Total responses :', nrow(dt), '\n')
  
  # remove outliers of reach
  dt <- dt[!is.na(dt$reach),]
  dt <- dt[dt$reach != 0, ]
  dt <- dt[!is.na(dt$cases),]
  # cat('Responses after removing reach=NA or cases=NA or reach=0 :', nrow(dt), '\n')
  
  # compute cutoffs
  reach_cutoff <- boxplot.stats(dt$reach, coef=1.5)$stats[5]  # changed cutoff to upper fence
  
  # dt$ratio <- dt$cases/dt$reach
  # cases_cutoff <- boxplot.stats(dt$ratio, coef=1.5)$stats[5] # changed cutoff to upper fence
  # 
  # dt$ratio <- dt$fatalities/dt$reach
  # fatalities_cutoff <- boxplot.stats(dt$ratio, coef=1.5)$stats[5] # changed cutoff to upper fence
  # 
  # dt$ratio <- dt$recentcases/dt$reach
  # recent_cutoff <- boxplot.stats(dt$ratio, coef=1.5)$stats[5] # changed cutoff to upper fence
  
  # remove outliers based on ratios
  dt <- dt[dt$reach <= reach_cutoff, ]
  # cat(
  #   'Responses after removing outliers with reach cutoff', reach_cutoff, ':', 
  #   nrow(dt), '\n'
  # )
  
  dt <- dt[(dt$cases/dt$reach) <= cases_cutoff, ]
  # cat(
  #   'Responses after removing outliers with cases/reach cutoff', 
  #   cases_cutoff, ':', nrow(dt), '\n'
  # )
  
  dt <- dt %>% filter(is.na(dt$fatalities) | (dt$fatalities/dt$reach) <= fatalities_cutoff)
  # cat(
  #   'Responses after removing outliers with fatalities/reach cutoff', 
  #   fatalities_cutoff, ':', nrow(dt), '\n'
  # )

  dt <- dt %>% filter(is.na(dt$recentcases) | (dt$recentcases/dt$reach) <= recent_cutoff)
  # cat(
  #   'Responses after removing outliers with recent/reach cutoff', 
  #   recent_cutoff, ':', nrow(dt), '\n'
  # )
  
  # cat('\n')
  return(dt)
  
}

process_ratio <- function(dt, numerator, denominator, control) {

  dta <- dt[!is.na(dt[[numerator]]),]
  dta <- dta[!is.na(dta[[denominator]]),]
  dta <- dta[dta[[numerator]] <= dta[[control]],]  # Additional control

  if (nrow(dta) > 0) {

    p_est <- sum(dta[[numerator]])/sum(dta[[denominator]])
    level <- ci_level
    z <- qnorm(level+(1-level)/2)
    se <- sqrt(p_est*(1-p_est))/sqrt(sum(dta[[denominator]]))

    return(list(
      val=p_est, 
      low=max(0,p_est-z*se), 
      high=min(1,p_est+z*se), 
      error=z*se, 
      std=se
    ))
  
  } else {
    return(list(val=NA, low=NA, high=NA, error=NA, std=NA))
  }

}

process_responses <- function(
    dt, iso2, pop, dates, max_responses, max_age, recent_max_age
) {

  ISO2 <- c()
  sample_size <- c()
  reach <- c()
  sample_size_recent <- c()
  reach_recent <- c()
  
  p_infected <- c()
  #p_infected_error <- c()
  p_infected_low <- c()
  p_infected_high <- c()

  p_fatalities <- c()
  #p_fatalities_error <- c()
  p_fatalities_low <- c()
  p_fatalities_high <- c()

  p_hospital <- c()
  #p_hospital_error <- c()
  p_hospital_low <- c()
  p_hospital_high <- c()
  
  p_recent <- c()
  #p_recent_error <- c()
  p_recent_low <- c()
  p_recent_high <- c()
  
  p_daily <- c()
  #p_daily_error <- c()
  p_daily_low <- c()
  p_daily_high <- c()
  
  p_active <- c()
  #p_active_error <- c()
  p_active_low <- c()
  p_active_high <- c()
  
  population <- c()
  
  for (j in dates) {

    # keep responses at most 'max_age' old
    subcondition <- (
      as.Date(dt$timestamp) 
      > (as.Date(j) - max_age) & as.Date(dt$timestamp) <= as.Date(j)
    )
    dt_date <- dt[subcondition, ]
    
    # remove duplicated cookies keeping the most recent response
    dt_date <- dt_date[
      !duplicated(dt_date$cookie, fromLast=TRUE, incomparables = c('')),
    ]
    
    # # keep all the responses of the day or at most max_responses
    # nr <- nrow(dt[as.Date(dt_date$timestamp) == as.Date(j), ])
    # dt_date <- tail(dt_date, max(max_responses,nr))
    
    # keep at most max_responses
    dt_date <- tail(dt_date, max_responses)
    
    # keep responses at most 'max_age_recent' old for recent computations
    dt_recent <- dt_date
    subcondition <- (
      as.Date(dt_recent$timestamp) > 
      (as.Date(j) - max_age_recent) 
      & as.Date(dt_recent$timestamp) <= as.Date(j)
    )
    dt_recent <- dt_recent[subcondition, ]
    
    # cat('Responses for the date', nrow(dt_date), 'recent:', nrow(dt_recent), '\n')
    
    ISO2 <- c(ISO2, iso2)
    sample_size <- c(sample_size, nrow(dt_date))
    reach <- c(reach, sum(dt_date$reach))
    
    sample_size_recent <- c(sample_size_recent, nrow(dt_recent))
    reach_recent <- c(reach_recent, sum(dt_recent$reach))

    if (sum(dt_date$reach) >= pop / sampling) {

      est <- process_ratio(dt_date, 'cases', 'reach', 'reach')
      p_infected <- c(p_infected, est$val)
      #p_infected_error <- c(p_infected_error, est$error)
      p_infected_low <- c(p_infected_low, est$low)
      p_infected_high <- c(p_infected_high, est$high)
      
      est <- process_ratio(dt_date, 'fatalities', 'reach', 'cases')
      p_fatalities <- c(p_fatalities, est$val)
      #p_fatalities_error <- c(p_fatalities_error, est$error)
      p_fatalities_low <- c(p_fatalities_low, est$low)
      p_fatalities_high <- c(p_fatalities_high, est$high)
      
      est <- process_ratio(dt_date, 'hospital', 'reach', 'cases')
      p_hospital <- c(p_hospital, est$val)
      #p_hospital_error <- c(p_hospital_error, est$error)
      p_hospital_low <- c(p_hospital_low, est$low)
      p_hospital_high <- c(p_hospital_high, est$high)
      
    } else {

      # cat('Low reach\n')
      p_infected <- c(p_infected, NA)
      # p_infected_error <- c(p_infected_error, NA)
      p_infected_low <- c(p_infected_low, NA)
      p_infected_high <- c(p_infected_high, NA)
      
      p_fatalities <- c(p_fatalities, NA)
      # p_fatalities_error <- c(p_fatalities_error, NA)
      p_fatalities_low <- c(p_fatalities_low, NA)
      p_fatalities_high <- c(p_fatalities_high, NA)
      
      p_hospital <- c(p_hospital, NA)
      # p_hospital_error <- c(p_hospital_error, NA)
      p_hospital_low <- c(p_hospital_low, NA)
      p_hospital_high <- c(p_hospital_high, NA)
      
    }

    if (sum(dt_recent$reach) >= pop/sampling_recent) {

      est <- process_ratio(dt_recent, 'recentcases', 'reach', 'cases')
      p_recent <- c(p_recent, est$val)
      # p_recent_error <- c(p_recent_error, est$error)
      p_recent_low <- c(p_recent_low, est$low)
      p_recent_high <- c(p_recent_high, est$high)
      
      dt_recent$cases_daily <- dt_recent$recentcases / 7
      est <- process_ratio(dt_recent, 'cases_daily', 'reach', 'cases')
      p_daily <- c(p_daily, est$val)
      # p_daily_error <- c(p_daily_error, est$error)
      p_daily_low <- c(p_daily_low, est$low)
      p_daily_high <- c(p_daily_high, est$high)
      
      est <- process_ratio(dt_recent, 'stillsick', 'reach', 'cases')
      p_active <- c(p_active, est$val)
      # p_active_error <- c(p_active_error, est$error)
      p_active_low <- c(p_active_low, est$low)
      p_active_high <- c(p_active_high, est$high)

    }
    else {

      # cat('Low reach_recent\n'  )
      p_recent <- c(p_recent, NA)
      # p_recent_error <- c(p_recent_error, NA)
      p_recent_low <- c(p_recent_low, NA)
      p_recent_high <- c(p_recent_high, NA)
      
      p_daily <- c(p_daily, NA)
      # p_daily_error <- c(p_daily_error, NA)
      p_daily_low <- c(p_daily_low, NA)
      p_daily_high <- c(p_daily_high, NA)
      
      p_active <- c(p_active, NA)
      # p_active_error <- c(p_active_error, NA)
      p_active_low <- c(p_active_low, NA)
      p_active_high <- c(p_active_high, NA)
      
    }
    
    population <- c(population, pop)

  }
  
  dd <- data.frame(
    date = dates,
    ISO2,
    population, sample_size, reach,
    sample_size_recent, reach_recent,
    
    p_infected,
    # p_infected_error,
    p_infected_low, p_infected_high,

    p_fatalities,
    # p_fatalities_error,
    p_fatalities_low, p_fatalities_high,

    p_hospital,
    # p_hospital_error,
    p_hospital_low, p_hospital_high,
    
    p_recent,
    # p_recent_error,
    p_recent_low, p_recent_high,
    
    p_daily,
    # p_daily_error,
    p_daily_low, p_daily_high,
    
    p_active,
    # p_active_error,
    p_active_low, p_active_high,
    
    stringsAsFactors = F
  )
  
  return(dd)

}

# process_responses <- function(dt) {
# 
#     dt_last <- tail(dt, max_responses)
#     dates <- unique(dt_last["timestamp"])[[1]]
#     
#     aggregated <- data.frame()
#     for (i in 1:length(dates)){
# 
#         dateStr <- dates[i]
#         data <- dt_last[dt_last$timestamp == dateStr, ]
# 
#         slice <- data.frame(
#             date = dateStr,
#             cases = mean(data$cases, na.rm=TRUE),
#             recovered = mean(data$recovered, na.rm=TRUE),
#             fatalities = mean(data$fatalities, na.rm=TRUE),
#             hospital = mean(data$hospital, na.rm=TRUE),
#             recent_cases = mean(data$recentcases, na.rm=TRUE),
#             recent_cases_14 = mean(data$recentcases14, na.rm=TRUE),
#             positive_test_14 = mean(data$positivetest14days, na.rm=TRUE),
#             positive_test_07 = mean(data$positivetest07days, na.rm=TRUE),
#             long_covid = mean(data$longcovid, na.rm=TRUE),
#             still_sick = mean(data$stillsick, na.rm=TRUE),
#             tested = mean(data$tested, na.rm=TRUE),
#             positive = mean(data$positive, na.rm=TRUE),
#             hospital = mean(data$hospital, na.rm=TRUE),
#             severe = mean(data$severe, na.rm=TRUE),
#             icu = mean(data$icu, na.rm=TRUE),
#             vaccinated = mean(data$vaccinated, na.rm=TRUE),
#             recent_vaccinated = mean(data$recentvaccinated, na.rm=TRUE),
#             vaccine_side_effects = mean(data$vaccinesideeffects, na.rm=TRUE),
#             refuse_vaccine = mean(data$refusevaccine, na.rm=TRUE),
#             stringsAsFactors = F
#         )
#         aggregated <- rbind(aggregated, slice)
# 
#     }
# 
#     return(aggregated)
#     
# }



# == MAIN ROUTINE ==

cat('Reading population file...\n')

pop_data <- read.csv(data_path, as.is = T)

cat('Processing all countries now...\n')
process_country <- function(country_iso) {

  pop <- pop_data$population[which(pop_data$ISO2==country_iso)]
  
  file_path <- paste0(responses_path, country_iso, '-aggregate.csv')
  dt <- read.csv(file_path, as.is = T)
  names(dt) <- tolower(names(dt))

  # list of dates
  dates_dash <- as.character(seq.Date(start_date, Sys.Date(), by = 'days'))
  dates <- gsub('-','/', dates_dash)

  # filter by dates
  dt$timestamp <- as.Date(dt$timestamp)
  dt <- dt[which(dt$timestamp >= start_date-max_age), ]
  dt <- remove_outliers(dt, cases_cutoff, fatalities_cutoff)

  # process responses
  dt <- process_responses(dt, country_iso, pop, dates, max_responses, max_age, recent_max_age)

  # write estimates to file
  write.csv(
    dt, 
    paste0(estimates_path, country_iso, '-estimate.csv'), 
    row.names = FALSE
  )

}

# lapply(countries, process_country)
invisible(mclapply(countries, process_country))

cat('Done!')
