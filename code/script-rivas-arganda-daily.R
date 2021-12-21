library(tidyr)
library(dplyr)
library(lubridate)

# smoothed p_cases and CI:
source("smooth_column-v2.R")

start_date <- ymd("2020-07-30")
end_date <- Sys.Date()

responses_path <- "../data/aggregate/rivas/"
data_path <- "../data/common-data/rivas-arganda/regions-tree-population.csv"
estimates_path <- "../data/estimates-rivas-arganda/"

# responses_path <- "../coronasurveys/data/aggregate/rivas-arganda/"
# data_path <- "../coronasurveys/data/common-data/rivas-arganda/regions-tree-population.csv"
# estimates_path <- "./estimates-rivas-arganda/"

default_reach <- 15

country_iso <- "ES"
ci_level <- 0.95 # Confident interval
max_ratio <- 1/3 # Maximum allowed ratio of cases/reach
num_responses = 1000 # Maximum number of responses used for one estimate
age <- 7 # Number of days that are aggregated 
smooth_param <- 30 # Parameter of the smoothing function
sampling <- 1000 # If the reach is < population/sampling the estimate is NA


remove_outliers <- function(dt, max_ratio = 1/3) {
  #remove outliers of reach.
  dt <- dt[!is.na(dt$reach),]
  dt <- dt[dt$reach != 0, ]
  dt <- dt[!is.na(dt$cases),]
  cat("Responses after removing reach=NA or cases=NA or reach=0 :", nrow(dt), "\n")
  
  cutoff <- boxplot.stats(dt$reach, coef=1.5)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= cutoff, ]
  cat("Responses after removing ouliers with reach cutoff", cutoff, ":", nrow(dt), "\n")

  # remove outliers based on max cases/reach ratio
  dt$ratio <- dt$cases/dt$reach
  #cutoff <- boxplot.stats(dt$ratio, coef=1.5)$stats[5] # changed cutoff to upper fence
  cutoff <- max_ratio
  dt <- dt[dt$ratio<cutoff, ]
  cat("Responses after removing ouliers with cases/reach cutoff", cutoff, ":", nrow(dt), "\n")
  
  # remove outliers based on max fatalities/reach ratio
  #dt <- dt[!is.na(dt$fatalities),]
  dt$ratio <- dt$fatalities/dt$reach
  #cutoff <- boxplot.stats(dt$ratio, coef=1.5)$stats[5] # changed cutoff to upper fence
  cutoff <- 1/10
  #dt <- dt[dt$ratio<cutoff, ]
  dt <- dt %>% filter(is.na(dt$ratio) | dt$ratio<cutoff)
  cat("Responses after removing ouliers with fatalities/reach cutoff", cutoff, ":", nrow(dt), "\n")

  return(dt)
}

process_ratio <- function(dt, numerator, denominator, control, cummulative=TRUE){
  dta <- dt[!is.na(dt[[numerator]]),]
  dta <- dta[!is.na(dta[[denominator]]),]
  dta <- dta[dta[[numerator]] <= dta[[control]],]
  if (cummulative){
    #Remove duplicated cookies keeping the most recent response
    dta <- dta[!duplicated(dta$cookie, fromLast=TRUE, incomparables = c("")),]
  }
  
  if (nrow(dta)>0){
    #cat("- Max ", numerator, max(dta[[numerator]]), "\n"  )
    p_est <- sum(dta[[numerator]])/sum(dta[[denominator]])
    level <- ci_level
    z <- qnorm(level+(1-level)/2)
    se <- sqrt(p_est*(1-p_est))/sqrt(sum(dta[[denominator]]))
    return(list(val=p_est, low=max(0,p_est-z*se), upp=p_est+z*se, error=z*se, std=se, suma=sum(dta[[numerator]])))
  }
  else {
    return(list(val=NA, low=NA, upp=NA, error=NA, std=NA, suma=NA))
  }
}

# calculate_ci <- function(p_est, level, pop_size) {
#  z <- qnorm(level+(1-level)/2)
#  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
#  return(list(p_est=est, low=max(0,p_est-z*se), upp=p_est+z*se)) #, error=z*se))
# }

process_region <- function(dt, reg, name, pop, dates, num_responses = 100, age = 7){
  cat("Working with", nrow(dt), "responses\n"  )
  #list of dates
  # dates <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
  # dates <- gsub("-","/", dates)
  
  region <- c()
  regionname <- c()
  population <- c()
  
  sample_size <- c()
  reach <- c()
  
  p_cases <- c()
  p_cases_low <- c()
  p_cases_high <- c()

  cases  <- c()
  cases_est <- c()
  cases_low <- c()
  cases_high <- c()
  
  p_recovered <- c()
  p_recovered_low <- c()
  p_recovered_high <- c()
  
  p_fatalities <- c()
  p_fatalities_low <- c()
  p_fatalities_high <- c()

  fatalities <- c()
  fatalities_est <- c()
  fatalities_low <- c()
  fatalities_high <- c()
  
  p_recentcases <- c()
  p_recentcases_low <- c()
  p_recentcases_high <- c()

  recentcases <- c()
  recentcases_est <- c()
  recentcases_low <- c()
  recentcases_high <- c()
  
  p_recentcasesnursing <- c()
  p_recentcasesnursing_low <- c()
  p_recentcasesnursing_high <- c()
  
  recentcasesnursing_est <- c()
  recentcasesnursing_low <- c()
  recentcasesnursing_high <- c()
  
  p_stillsick <- c()
  p_stillsick_low <- c()
  p_stillsick_high <- c()
  
  p_hospital <- c()
  p_hospital_low <- c()
  p_hospital_high <- c()
  
  hospital_est <- c()
  hospital_low <- c()
  hospital_high <- c()

  p_recenthospital <- c()
  p_recenthospital_low <- c()
  p_recenthospital_high <- c()
  
  p_severe <- c()
  p_severe_low <- c()
  p_severe_high <- c()
  
  p_icu <- c()
  p_icu_low <- c()
  p_icu_high <- c()
  
  icu_est <- c()
  icu_low <- c()
  icu_high <- c()
  
  p_tested <- c()
  p_tested_low <- c()
  p_tested_high <- c()
  
  p_positive <- c()
  p_positive_low <- c()
  p_positive_high <- c()
  
  for (j in dates){
    #Keep responses at most "age" old
    subcondition <- (as.Date(dt$timestamp) > (as.Date(j)-age)  & as.Date(dt$timestamp) <= as.Date(j) )
    dt_date <- dt[subcondition, ]
    #Remove duplicated cookies keeping the most recent response
    dt_date <- dt_date[!duplicated(dt_date$cookie, fromLast=TRUE, incomparables = c("")),]
    #Keep all the responses of the day or at most num_responses
    nr <- nrow(dt[as.Date(dt_date$timestamp) == as.Date(j), ])
    dt_date <- tail(dt_date, max(num_responses,nr))

    region <- c(region, reg)
    regionname <- c(regionname, name)
    population <- c(population, pop)
    
    sample_size <- c(sample_size, nrow(dt_date))
    reach <- c(reach, sum(dt_date$reach))
    
    if (sum(dt_date$reach) >= pop/sampling){
      est <- process_ratio(dt_date, "cases", "reach", "reach")
      p_cases <- c(p_cases, est$val)
      p_cases_low <- c(p_cases_low, est$low)
      p_cases_high <- c(p_cases_high, est$upp)
      
      cases  <- c(cases, est$suma)
      cases_est <- c(cases_est, pop*est$val)
      cases_low <- c(cases_low, pop*est$low)
      cases_high <- c(cases_high, pop*est$upp)
      
      est <- process_ratio(dt_date, "recentcases", "reach", "cases", cummulative=FALSE)
      p_recentcases <- c(p_recentcases, est$val)
      p_recentcases_low <- c(p_recentcases_low, est$low)
      p_recentcases_high <- c(p_recentcases_high, est$upp)
      
      est <- process_ratio(dt_date, "recentcases", "reach", "cases", cummulative=FALSE)
      recentcases <- c(recentcases, est$suma)
      recentcases_est <- c(recentcases_est, pop * est$val)
      recentcases_low <- c(recentcases_low, pop * est$low)
      recentcases_high <- c(recentcases_high, pop * est$upp)
      
      est <- process_ratio(dt_date, "stillsick", "reach", "cases")
      p_stillsick <- c(p_stillsick, est$val)
      p_stillsick_low <- c(p_stillsick_low, est$low)
      p_stillsick_high <- c(p_stillsick_high, est$upp)
      
      est <- process_ratio(dt_date, "recenthospital", "reach", "cases", cummulative=FALSE)
      p_recenthospital <- c(p_recenthospital, est$val)
      p_recenthospital_low <- c(p_recenthospital_low, est$low)
      p_recenthospital_high <- c(p_recenthospital_high, est$upp)
    }
    else {
      p_cases <- c(p_cases, NA)
      p_cases_low <- c(p_cases_low, NA)
      p_cases_high <- c(p_cases_high, NA)
      cases  <- c(cases, NA)
      cases_est <- c(cases_est, NA)
      cases_low <- c(cases_low, NA)
      cases_high <- c(cases_high, NA)
      p_recentcases <- c(p_recentcases, NA)
      p_recentcases_low <- c(p_recentcases_low, NA)
      p_recentcases_high <- c(p_recentcases_high, NA)
      recentcases <- c(recentcases, NA)
      recentcases_est <- c(recentcases_est, NA)
      recentcases_low <- c(recentcases_low, NA)
      recentcases_high <- c(recentcases_high, NA)
      p_stillsick <- c(p_stillsick, NA)
      p_stillsick_low <- c(p_stillsick_low, NA)
      p_stillsick_high <- c(p_stillsick_high, NA)
      p_recenthospital <- c(p_recenthospital, NA)
      p_recenthospital_low <- c(p_recenthospital_low, NA)
      p_recenthospital_high <- c(p_recenthospital_high, NA)
    }
    
    
    if ( (sum(dt_date$reach) >= pop/sampling) && (j < as.POSIXct("2020-08-19"))) {
      est <- process_ratio(dt_date, "recovered", "reach", "cases")
      p_recovered <- c(p_recovered, est$val)
      p_recovered_low <- c(p_recovered_low, est$low)
      p_recovered_high <- c(p_recovered_high, est$upp)
      
      est <- process_ratio(dt_date, "fatalities", "reach", "cases")
      p_fatalities <- c(p_fatalities, est$val)
      p_fatalities_low <- c(p_fatalities_low, est$low)
      p_fatalities_high <- c(p_fatalities_high, est$upp)
      
      est <- process_ratio(dt_date, "fatalities", "reach", "cases")
      fatalities <- c(fatalities, est$suma)
      fatalities_est <- c(fatalities_est, pop * est$val)
      fatalities_low <- c(fatalities_low, pop * est$low)
      fatalities_high <- c(fatalities_high, pop * est$upp)
      
      est <- process_ratio(dt_date, "recentcasesnursing", "reach", "recentcases", cummulative=FALSE)
      p_recentcasesnursing <- c(p_recentcasesnursing, est$val)
      p_recentcasesnursing_low <- c(p_recentcasesnursing_low, est$low)
      p_recentcasesnursing_high <- c(p_recentcasesnursing_high, est$upp)
      
      est <- process_ratio(dt_date, "recentcasesnursing", "reach", "recentcases", cummulative=FALSE)
      recentcasesnursing_est <- c(recentcasesnursing_est, pop * est$val)
      recentcasesnursing_low <- c(recentcasesnursing_low, pop * est$low)
      recentcasesnursing_high <- c(recentcasesnursing_high, pop * est$upp)
      
      est <- process_ratio(dt_date, "hospital", "reach", "cases")
      p_hospital <- c(p_hospital, est$val)
      p_hospital_low <- c(p_hospital_low, est$low)
      p_hospital_high <- c(p_hospital_high, est$upp)
      
      est <- process_ratio(dt_date, "hospital", "reach", "cases")
      hospital_est <- c(hospital_est, pop * est$val)
      hospital_low <- c(hospital_low, pop * est$low)
      hospital_high <- c(hospital_high, pop * est$upp)
      
      est <- process_ratio(dt_date, "severe", "reach", "cases")
      p_severe <- c(p_severe, est$val)
      p_severe_low <- c(p_severe_low, est$low)
      p_severe_high <- c(p_severe_high, est$upp)
      
      est <- process_ratio(dt_date, "icu", "reach", "cases")
      p_icu <- c(p_icu, est$val)
      p_icu_low <- c(p_icu_low, est$low)
      p_icu_high <- c(p_icu_high, est$upp)
      
      est <- process_ratio(dt_date, "icu", "reach", "cases")
      icu_est <- c(icu_est, pop * est$val)
      icu_low <- c(icu_low, pop * est$low)
      icu_high <- c(icu_high, pop * est$upp)
      
      est <- process_ratio(dt_date, "tested", "reach", "reach")
      p_tested <- c(p_tested, est$val)
      p_tested_low <- c(p_tested_low, est$low)
      p_tested_high <- c(p_tested_high, est$upp)
      
      est <- process_ratio(dt_date, "positive", "tested", "tested")
      p_positive <- c(p_positive, est$val)
      p_positive_low <- c(p_positive_low, est$low)
      p_positive_high <- c(p_positive_high, est$upp)
    }
    else {
      p_recovered <- c(p_recovered, NA)
      p_recovered_low <- c(p_recovered_low, NA)
      p_recovered_high <- c(p_recovered_high, NA)
      
      p_fatalities <- c(p_fatalities, NA)
      p_fatalities_low <- c(p_fatalities_low, NA)
      p_fatalities_high <- c(p_fatalities_high, NA)
      
      fatalities <- c(fatalities, NA)
      fatalities_est <- c(fatalities_est, NA)
      fatalities_low <- c(fatalities_low, NA)
      fatalities_high <- c(fatalities_high, NA)
      
      p_recentcasesnursing <- c(p_recentcasesnursing, NA)
      p_recentcasesnursing_low <- c(p_recentcasesnursing_low, NA)
      p_recentcasesnursing_high <- c(p_recentcasesnursing_high, NA)
      
      recentcasesnursing_est <- c(recentcasesnursing_est, NA)
      recentcasesnursing_low <- c(recentcasesnursing_low, NA)
      recentcasesnursing_high <- c(recentcasesnursing_high, NA)
      
      p_hospital <- c(p_hospital, NA)
      p_hospital_low <- c(p_hospital_low, NA)
      p_hospital_high <- c(p_hospital_high, NA)
      
      hospital_est <- c(hospital_est, NA)
      hospital_low <- c(hospital_low, NA)
      hospital_high <- c(hospital_high, NA)
      
      p_severe <- c(p_severe, NA)
      p_severe_low <- c(p_severe_low, NA)
      p_severe_high <- c(p_severe_high, NA)
      
      p_icu <- c(p_icu, NA)
      p_icu_low <- c(p_icu_low, NA)
      p_icu_high <- c(p_icu_high, NA)
      
      icu_est <- c(icu_est, NA)
      icu_low <- c(icu_low, NA)
      icu_high <- c(icu_high, NA)
      
      p_tested <- c(p_tested, NA)
      p_tested_low <- c(p_tested_low, NA)
      p_tested_high <- c(p_tested_high, NA)
      
      p_positive <- c(p_positive, NA)
      p_positive_low <- c(p_positive_low, NA)
      p_positive_high <- c(p_positive_high, NA)
    }
    
  }
  
  dd <- data.frame(date = dates,
                   region,
                   regionname,
                   population,
                   sample_size,
                   reach,
                   cases,
                   recentcases,
                   fatalities,
                   
                   cases_est,
                   cases_low,
                   cases_high,
                   
                   p_cases,
                   p_cases_low,
                   p_cases_high,
                   
                   recentcases_est,
                   recentcases_low,
                   recentcases_high,
                   
                   p_recentcases,
                   p_recentcases_low,
                   p_recentcases_high,
                   
                   p_recenthospital,
                   p_recenthospital_low,
                   p_recenthospital_high,
                   
                   recentcasesnursing_est,
                   recentcasesnursing_low,
                   recentcasesnursing_high,
                   
                   fatalities_est,
                   fatalities_low,
                   fatalities_high,
                   
                   hospital_est,
                   hospital_low,
                   hospital_high,
                   
                   icu_est,
                   icu_low,
                   icu_high,
                   
                   p_recovered,
                   p_recovered_low,
                   p_recovered_high,
                   
                   p_fatalities,
                   p_fatalities_low,
                   p_fatalities_high,
                   
                   p_recentcasesnursing,
                   p_recentcasesnursing_low,
                   p_recentcasesnursing_high,
                   
                   p_stillsick,
                   p_stillsick_low,
                   p_stillsick_high,
                   
                   p_hospital,
                   p_hospital_low,
                   p_hospital_high,
                   
                   p_severe,
                   p_severe_low,
                   p_severe_high,
                   
                   p_icu,
                   p_icu_low,
                   p_icu_high,
                   
                   p_tested,
                   p_tested_low,
                   p_tested_high,
                   
                   p_positive,
                   p_positive_low,
                   p_positive_high,
                   
                   stringsAsFactors = F)
  
  return(dd)
}




cat("Rivas-Arganda daily script run at ", as.character(Sys.time()), "\n\n")

file_path <- paste0(responses_path, country_iso, "-aggregate.csv")
dt <- read.csv(file_path, as.is = T)
cat("Received ", nrow(dt), " responses\n\n")
names(dt) <- tolower(names(dt))

#list of regions
region_tree <- read.csv(data_path, as.is = T)
names(region_tree) <- tolower(names(region_tree))
regions <- region_tree$provincecode
region_names <- region_tree$regionname
populations <- region_tree$population

#list of dates
# dates_dash <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
dates_dash <- as.character(seq.Date(start_date, end_date, by = "days"))
dates <- gsub("-","/", dates_dash)

# #list responses per date
# for (i in 1:length(regions)){
#   dta <- dt[dt$iso_3166_2==regions[i],]
#   cat("From ", regions[i], " received ", nrow(dta), " responses\n")
#   for (j in 1:length(dates)){
#     dtaa <- dta[dta$timestamp==dates[j],]
#     cat("-- On day ", dates[j], "received", nrow(dtaa), " responses, \n")
#   }
# }
# cat("\n")

dt[which(is.na(dt$reach)), "reach"] <- default_reach
dt <- remove_outliers(dt,max_ratio)

dw <- data.frame(date=c(),
                 region=c(),
                 regionname=c(),
                 population=c(),
                 sample_size=c(),
                 reach=c(),
                 cases=c(),
                 recentcases=c(),
                 fatalities=c(),
                 
                 
                 cases_est=c(),
                 cases_low=c(),
                 cases_high=c(),
                 
                 p_cases=c(),
                 p_cases_low=c(),
                 p_cases_high=c(),
                 
                 recentcases_est=c(),
                 recentcases_low=c(),
                 recentcases_high=c(),
                 
                 p_recentcases=c(),
                 p_recentcases_low=c(),
                 p_recentcases_high=c(),
                 
                 p_recenthospital=c(),
                 p_recenthospital_low=c(),
                 p_recenthospital_high=c(),
                 
                 recentcasesnursing_est=c(),
                 recentcasesnursing_low=c(),
                 recentcasesnursing_high=c(),
                 
                 fatalities_est=c(),
                 fatalities_low=c(),
                 fatalities_high=c(),
                 
                 hospital_est=c(),
                 hospital_low=c(),
                 hospital_high=c(),
                 
                 icu_est=c(),
                 icu_low=c(),
                 icu_high=c(),
                 
                 p_recovered=c(),
                 p_recovered_low=c(),
                 p_recovered_high=c(),
                 
                 p_fatalities=c(),
                 p_fatalities_low=c(),
                 p_fatalities_high=c(),
                 
                 p_recentcasesnursing=c(),
                 p_recentcasesnursing_low=c(),
                 p_recentcasesnursing_high=c(),
                 
                 p_stillsick=c(),
                 p_stillsick_low=c(),
                 p_stillsick_high=c(),
                 
                 p_hospital=c(),
                 p_hospital_low=c(),
                 p_hospital_high=c(),
                 
                 p_severe=c(),
                 p_severe_low=c(),
                 p_severe_high=c(),
                 
                 p_icu=c(),
                 p_icu_low=c(),
                 p_icu_high=c(),
                 
                 p_tested=c(),
                 p_tested_low=c(),
                 p_tested_high=c(),
                 
                 p_positive=c(),
                 p_positive_low=c(),
                 p_positive_high=c(),
                 
                 stringsAsFactors = F)

for (i in 1:length(regions)){
  reg <- regions[i]
  name <- region_names[i]
  cat("Processing", reg, "\n")
  dfaux <- dt[which(dt$iso_3166_2 == reg), ]
  dd <- process_region(dfaux, reg, name, pop=populations[i], dates, num_responses, age)
  
  # smoothed p_cases and CI:
  dd <- smooth_column(df_in = dd, col_s = "p_cases",
                      basis_dim = smooth_param, link_in = "log", monotone = T)
  dd <- smooth_column(df_in = dd, col_s = "p_cases_low",
                      basis_dim = smooth_param, link_in = "log", monotone = T)
  dd <- smooth_column(df_in = dd, col_s = "p_cases_high",
                      basis_dim = smooth_param, link_in = "log", monotone = T)
  # dd$p_cases_smooth <- 
  #   with(dd, ksmooth(date, dd$p_cases, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  # dd$p_cases_low_smooth <- 
  #   with(dd, ksmooth(date, dd$p_cases_low, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  # dd$p_cases_high_smooth <- 
  #   with(dd, ksmooth(date, dd$p_cases_high, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  
  dd$p_cases_low_smooth <- ifelse(dd$p_cases_low_smooth > dd$p_cases_smooth, 
                                        dd$p_cases_smooth, 
                                        dd$p_cases_low_smooth)
  dd$p_cases_high_smooth <- ifelse(dd$p_cases_high_smooth < dd$p_cases_smooth, 
                                         dd$p_cases_smooth, 
                                         dd$p_cases_high_smooth)
  

  # smoothed p_cases and CI:
  # dd <- smooth_column(df_in = dd, col_s = "p_recentcases", 
  #                     basis_dim = smooth_param, link_in = "log")
  # dd <- smooth_column(df_in = dd, col_s = "p_recentcases_low", 
  #                     basis_dim = smooth_param, link_in = "log")
  # dd <- smooth_column(df_in = dd, col_s = "p_recentcases_high", 
  #                     basis_dim = smooth_param, link_in = "log")
  dd$p_recentcases_smooth <- 
    with(dd, ksmooth(date, dd$p_recentcases, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  dd$p_recentcases_low_smooth <- 
    with(dd, ksmooth(date, dd$p_recentcases_low, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  dd$p_recentcases_high_smooth <- 
    with(dd, ksmooth(date, dd$p_recentcases_high, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  
  dd$p_recentcases_low_smooth <- ifelse(dd$p_recentcases_low_smooth > dd$p_recentcases_smooth, 
                                        dd$p_recentcases_smooth, 
                                        dd$p_recentcases_low_smooth)
  dd$p_recentcases_high_smooth <- ifelse(dd$p_recentcases_high_smooth < dd$p_recentcases_smooth, 
                                        dd$p_recentcases_smooth, 
                                        dd$p_recentcases_high_smooth)
  
  
  cat("- Writing estimates for:", reg, "\n")
  write.csv(dd, paste0(estimates_path, reg, "-estimate.csv"), row.names = FALSE)
  dw <- rbind(dw, dd)
}

# for (j in 1:length(dates)){
#   write.csv(dw[dw$date == dates[j], ], 
#             paste0(estimates_path, country_iso, "-", dates_dash[j], "-estimate.csv"), 
#             row.names = FALSE)
# }
write.csv(dw[dw$date == dates[length(dates)], ], 
          paste0(estimates_path, country_iso, "-latest-estimate.csv"), 
          row.names = FALSE)

