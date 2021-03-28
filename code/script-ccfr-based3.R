library(tidyverse)
library(readxl)
library(httr)

source("smooth_greedy_monotone.R")

country_codes_file <- "../data/common-data/wikipedia-iso-country-codes.xlsx"
estimates_path <- "../data/estimates-ccfr-based/PlotData/"

# country_codes_file <- "../coronasurveys/data/common-data/wikipedia-iso-country-codes.xlsx"
# estimates_path <- "./estimates-ccfr-based/PlotData/"

contagious_window <- 12
active_window <- 18
z_mean_hdt <- 13
z_sd_hdt <- 12.7
z_median_hdt <- 9.1
c_cfr_baseline <- 1.38
c_cfr_error <- 0.05
c_cfr_estimate_range <- c(c_cfr_baseline-c_cfr_error, c_cfr_baseline+c_cfr_error)


# - Cases_infected: Population that is or has been infected of COVID-19.
# - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
# - Cases_contagious: Those infected that can transmit the virus on a given day (assumes a case is contagious 12 days after infected)
# - Cases_active: Those infected whose case is still active on a given day (assumes a case is active 18 days after infected)

calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se, error=z*se))
}

hosp_to_death_trunc <- function(x, mu_hdt, sigma_hdt){
  dlnorm(x, mu_hdt, sigma_hdt)
}

scale_cfr <- function(data_1_in, delay_fun, mu_hdt, sigma_hdt){
  case_incidence <- data_1_in$cases
  death_incidence <- data_1_in$deaths
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:length(case_incidence)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  if (sum(death_incidence, na.rm = T) > cumulative_known_t){
    ccfrr <- data.frame(nCFR = 0, cCFR = 0, total_deaths = 0, 
                        cum_known_t = 0, total_cases = sum(case_incidence))
  } else{
    ccfrr <- data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
                        cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
  }
  return(ccfrr)
}

plot_estimates <- function(country_geoid = "ES",
                           dts, 
                           contagious_window,
                           active_window){
  cat("::- script-ccfr-based: Computing ccfr-based estimates for", country_geoid, "::\n")
  mu_hdt = log(z_median_hdt)
  sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt))
  
  # data <- dts %>% 
  #     select(dateRep:popData2019, "Alpha.2.code" )
    # data$geoId <- data$Alpha.2.code 
    # data <- data %>% select(dateRep:popData2019)
    # data <- data[data$geoId == country_geoid,]
    
  dt <- dts %>% select(Date, cases, deaths, CountryCode)
    
    # dt <- as.data.frame(data[rev(1:nrow(data)),])
    ####### fix NAs in cases and deaths #######
    dt$cases[is.na(dt$cases)] <- 0
    dt$deaths[is.na(dt$deaths)] <- 0
    ##########################################
    dt$cum_cases <- cumsum(dt$cases)
    dt$cum_deaths <- cumsum(dt$deaths)
    
    dt$Date <- as.Date(dt$Date, format = "%Y-%m-%d")
    dt$date <- gsub("-", "/", dt$Date)
    
    pop_data <- read.csv("../data/common-data/oxford-umd-country-population.csv", as.is = T)
    dt$population <- pop_data$population[pop_data$CountryCode == dt$CountryCode[1]]
    
    dt$geoId <- pop_data$geo_id[pop_data$CountryCode == dt$CountryCode[1]]
    dt <- dt %>% 
      select(date, geoId, population, cases, deaths, cum_cases, cum_deaths) %>% 
      rename(countrycode = geoId) %>% 
      select(date, countrycode, population, cases, deaths, cum_cases, cum_deaths)
    
    ndt <- nrow(dt)
    est_ccfr <- rep(NA, ndt)
    est_ccfr_low <- rep(NA, ndt)
    est_ccfr_high <- rep(NA, ndt)
    p_ccfr <- rep(NA, ndt)
    p_ccfr_low <- rep(NA, ndt)
    p_ccfr_high <- rep(NA, ndt)
    
    #cat("::- script-ccfr-based: Computing ccfr-based estimates for", country_geoid, "::\n")
    
    for (i in ndt : 1) {
      data2t <- dt[1:i, c("cases", "deaths")]
      ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc,
                        mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
      fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
      sigma_fraction_reported <- (1/ccfr$total_deaths)-(1/ccfr$cum_known_t)+ (1/1023) - (1/74130)
      fraction_reported_high <- fraction_reported * exp(1.96*sigma_fraction_reported)
      fraction_reported_low <- fraction_reported * exp(-(1.96*sigma_fraction_reported))
      est_ccfr_low[i] <- dt$cum_cases[i]*(1/fraction_reported_high)#switch low and high here coz of inverse.
      est_ccfr_high[i] <- dt$cum_cases[i]*(1/fraction_reported_low)
      est_ccfr[i] <- dt$cum_cases[i]*(1/fraction_reported)
      p_ccfr[i] <- est_ccfr[i]/dt$population[1]
      p_ccfr_low[i] <- est_ccfr_low[i]/dt$population[1]
      p_ccfr_high[i] <- est_ccfr_high[i]/dt$population[1]
    }
    
    # Avoid having more than 100% of the population reported infected
    dt$cases_infected <- pmin(est_ccfr, dt$population)
    dt$cases_infected_low <- pmin(est_ccfr_low, dt$population)
    dt$cases_infected_high <- pmin(est_ccfr_high, dt$population)
    dt$p_cases_infected <- pmin(p_ccfr, 1)
    dt$p_cases_infected_low <- pmin(p_ccfr_low, 1)
    dt$p_cases_infected_high <- pmin(p_ccfr_high, 1)
    
    # daily ccfr estimate
    dt$cases_daily <- c(0, diff(smooth_greedy(dt$cases_infected)))
    
    #contagious
    if (nrow(dt) >= contagious_window){
      dt$cases_contagious <- cumsum(c(dt$cases_daily[1:contagious_window],
                                      diff(dt$cases_daily, lag = contagious_window)))
    }
    else {
      dt$cases_contagious <- NA
    }
    
    #cases_active
    if (nrow(dt) >= active_window){
      dt$cases_active <- cumsum(c(dt$cases_daily[1:active_window],
                                diff(dt$cases_daily, lag = active_window)))
    }
    else {
      dt$cases_active <- NA
    }
    
    # - Cases_infected: Population that is or has been infected of COVID-19.
    # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
    # - Cases_contagious: Those infected that can transmit the virus on a given day (assumes a case is contagious 12 days after infected)
    # - Cases_active: Those infected whose case is still active on a given day (assumes a case is active 18 days after infected)

    dt$p_cases_daily <- dt$cases_daily/dt$population
    dt$p_cases_contagious <- dt$cases_contagious/dt$population
    dt$p_cases_active <- dt$cases_active/dt$population
    
    dt_w <- dt %>% 
      select("date", "countrycode", "population", "cases", "deaths", "cum_cases", "cum_deaths", 
             "cases_infected", "cases_infected_low", "cases_infected_high",
             "cases_daily", "cases_contagious", "cases_active", 
             "p_cases_infected", "p_cases_infected_low", "p_cases_infected_high", 
             "p_cases_daily", "p_cases_contagious", "p_cases_active")
    
    dir.create(estimates_path, showWarnings = F)
    # cat("::- script-ccfr-based: Writing data for", country_geoid, "::\n")
    write.csv(dt_w, paste0(estimates_path, country_geoid, "-estimate.csv"), row.names = FALSE)
    
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

# generate_estimates <- function(contagious_window = 12,
#                                active_window = 18){
#   url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
#                Sys.Date(), ".xlsx", sep = "")
#   GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#   cat("::- script-ccfr: Checking the ECDC data for the day ::\n")
#   #try( data_ecdc <- read_excel(tf), silent = T) # ECDC daily excel seems unvailable for now
#   try( data_ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
#                              na.strings = "", fileEncoding = "UTF-8-BOM"), silent = T)
#   
#   if(!exists("data_ecdc")){
#     cat("::- script-ccfr: Seems the ECDC data for the day is not available yet ::\n")
#     cat("::- script-ccfr: Trying to get data for the previous day ::\n")
#     url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
#                  Sys.Date()-1, ".xlsx", sep = "")
#     GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#     try( data_ecdc <- read_excel(tf), silent = T)
#     if(!exists("data_ecdc")){
#       stop("::- script-confirmed: Unfortunately, the ECDC data for yesterday is not availabe neither ::\n")
#     }else{
#       cat("::- script-ccfr: Using ECDC data for previous day ::\n")
#       data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
#       data_country_code <- read_excel(country_codes_file)
#       names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
#                                     "Alpha.3.code", "Numeric.code", "ISO.3166.2")
#       
#       data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
#       
#       all_geo_ids <- unique(data_ecdc$Alpha.2.code)
#       sapply(all_geo_ids, plot_estimates, dts = data_ecdc, 
#              contagious_window = contagious_window, 
#              active_window = active_window)
#     }
#   } else{
#     cat("::- script-ccfr: ECDC data for the day available! ::\n")
#     data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
#     data_country_code <- read_excel(country_codes_file)
#     names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
#                                   "Alpha.3.code", "Numeric.code", "ISO.3166.2")
#     data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
#     all_geo_ids <- unique(data_ecdc$Alpha.2.code)
#     go <- sapply(all_geo_ids, plot_estimates, dts =  data_ecdc, 
#                  contagious_window = contagious_window, 
#                  active_window = active_window)
#   }
#   
# }
# 
# generate_estimates(contagious_window = contagious_window,
#                    active_window = active_window)

#plot_estimates(country_geoid = "ES", dts =  data_ecdc, contagious_window = contagious_window)
