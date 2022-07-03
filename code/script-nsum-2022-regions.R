library(tidyr)
library(dplyr)
# install.packages("xtable")
library("xtable")

responses_path <- "../data/aggregate/"
data_path <- "../data/common-data/regions-tree-population.csv"
estimates_path <- "../data/estimates-nsum-2022/PlotData/regional_data/"

# responses_path <- "../coronasurveys/data/aggregate/"
# data_path <- "../coronasurveys/data/common-data/regions-tree-population.csv"
# estimates_path <- "./estimates-regions/"

start_date <- as.Date("2022-07-01")

countries <- c(
"ES"
,"DE"
,"IT"
,"FR"
,"GB"
,"PT"
,"GR"
,"US"
,"CL"
,"ZA"
,"JP"
)

ci_level <- 0.95
cases_cutoff <- 1 # 3/4 # 1/2 changed on 2022-04-12
fatalities_cutoff <- 3/4 # 1/2
recent_cutoff <- 1 # 3/4 # 1/2 changed on 2022-04-12

max_responses <- 300
max_age <- 30
max_age_recent <- 14
sampling <- 50000 # If the reach is < population/sampling the estimate is NA
sampling_recent <- 100000 # If the reach is < population/sampling_recent the estimate is NA

remove_outliers <- function(dt, ratio_cutoff, fatalities_cutoff) {
  cat("Total responses :", nrow(dt), "\n")
    #remove outliers of reach.
  dt <- dt[!is.na(dt$reach),]
  dt <- dt[dt$reach != 0, ]
  dt <- dt[!is.na(dt$cases),]
  cat("Responses after removing reach=NA or cases=NA or reach=0 :", nrow(dt), "\n")
  
  #Compute cutoffs
  reach_cutoff <- boxplot.stats(dt$reach, coef=1.5)$stats[5] # changed cutoff to upper fence
  
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
  cat("Responses after removing ouliers with reach cutoff", 
      reach_cutoff, ":", nrow(dt), "\n")
  
  dt <- dt[(dt$cases/dt$reach) <= cases_cutoff, ]
  cat("Responses after removing ouliers with cases/reach cutoff", 
      cases_cutoff, ":", nrow(dt), "\n")
  
  dt <- dt %>% filter(is.na(dt$fatalities) | (dt$fatalities/dt$reach) <= fatalities_cutoff)
  cat("Responses after removing ouliers with fatalities/reach cutoff", 
      fatalities_cutoff, ":", nrow(dt), "\n")

  dt <- dt %>% filter(is.na(dt$recentcases) | (dt$recentcases/dt$reach) <= recent_cutoff)
  cat("Responses after removing ouliers with recent/reach cutoff", 
      recent_cutoff, ":", nrow(dt), "\n")
  
  cat("\n")
  return(dt)
}

process_ratio <- function(dt, numerator, denominator, control){
  dta <- dt[!is.na(dt[[numerator]]),]
  dta <- dta[!is.na(dta[[denominator]]),]
  dta <- dta[dta[[numerator]] <= dta[[control]],] # Additional control
  if (nrow(dta)>0){
    p_est <- sum(dta[[numerator]])/sum(dta[[denominator]])
    level <- ci_level
    z <- qnorm(level+(1-level)/2)
    se <- sqrt(p_est*(1-p_est))/sqrt(sum(dta[[denominator]]))
    return(list(val=p_est, low=max(0,p_est-z*se), high=min(1,p_est+z*se), error=z*se, std=se))
  }
  else {
    return(list(val=NA, low=NA, high=NA, error=NA, std=NA))
  }
}


process_region <- function(dt, reg, 
                           # name, 
                           pop, dates, max_responses, max_age, recent_max_age){
  cat("Working with", nrow(dt), "responses\n"  )

  region <- c()
  # regionname <- c()
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
  
  for (j in dates){
    #Keep responses at most "max_age" old
    subcondition <- (as.Date(dt$timestamp) > (as.Date(j)-max_age) & as.Date(dt$timestamp) <= as.Date(j))
    dt_date <- dt[subcondition, ]
    
    #Remove duplicated cookies keeping the most recent response
    dt_date <- dt_date[!duplicated(dt_date$cookie, fromLast=TRUE, incomparables = c("")),]
    
    # #Keep all the responses of the day or at most max_responses
    # nr <- nrow(dt[as.Date(dt_date$timestamp) == as.Date(j), ])
    # dt_date <- tail(dt_date, max(max_responses,nr))
    #Keep at most max_responses
    dt_date <- tail(dt_date, max_responses)
    
    #Keep responses at most "max_age_recent" old for recent computations
    dt_recent <- dt_date
    subcondition <- (as.Date(dt_recent$timestamp) > (as.Date(j)-max_age_recent) & as.Date(dt_recent$timestamp) <= as.Date(j) )
    dt_recent <- dt_recent[subcondition, ]
    
    # cat("Responses for the date", nrow(dt_date), "recent:", nrow(dt_recent), "\n")
    
    region <- c(region, reg)
    # regionname <- c(regionname, name)
    sample_size <- c(sample_size, nrow(dt_date))
    reach <- c(reach, sum(dt_date$reach))
    
    sample_size_recent <- c(sample_size_recent, nrow(dt_recent))
    reach_recent <- c(reach_recent, sum(dt_recent$reach))

    if (sum(dt_date$reach) >= pop/sampling){
      est <- process_ratio(dt_date, "cases", "reach", "reach")
      p_infected <- c(p_infected, est$val)
      #p_infected_error <- c(p_infected_error, est$error)
      p_infected_low <- c(p_infected_low, est$low)
      p_infected_high <- c(p_infected_high, est$high)
      
      est <- process_ratio(dt_date, "fatalities", "reach", "cases")
      p_fatalities <- c(p_fatalities, est$val)
      #p_fatalities_error <- c(p_fatalities_error, est$error)
      p_fatalities_low <- c(p_fatalities_low, est$low)
      p_fatalities_high <- c(p_fatalities_high, est$high)
    }
    else {
      # cat("Low reach\n"  )
      p_infected <- c(p_infected, NA)
      #p_infected_error <- c(p_infected_error, NA)
      p_infected_low <- c(p_infected_low, NA)
      p_infected_high <- c(p_infected_high, NA)
      p_fatalities <- c(p_fatalities, NA)
      #p_fatalities_error <- c(p_fatalities_error, NA)
      p_fatalities_low <- c(p_fatalities_low, NA)
      p_fatalities_high <- c(p_fatalities_high, NA)
    }

    if (sum(dt_recent$reach) >= pop/sampling_recent){
      est <- process_ratio(dt_recent, "recentcases", "reach", "cases")
      p_recent <- c(p_recent, est$val)
      #p_recent_error <- c(p_recent_error, est$error)
      p_recent_low <- c(p_recent_low, est$low)
      p_recent_high <- c(p_recent_high, est$high)
      
      dt_recent$cases_daily <- dt_recent$recentcases / 7
      est <- process_ratio(dt_recent, "cases_daily", "reach", "cases")
      p_daily <- c(p_daily, est$val)
      #p_daily_error <- c(p_daily_error, est$error)
      p_daily_low <- c(p_daily_low, est$low)
      p_daily_high <- c(p_daily_high, est$high)
      
      est <- process_ratio(dt_recent, "stillsick", "reach", "cases")
      p_active <- c(p_active, est$val)
      #p_active_error <- c(p_active_error, est$error)
      p_active_low <- c(p_active_low, est$low)
      p_active_high <- c(p_active_high, est$high)
    }
    else {
      # cat("Low reach_recent\n"  )
      p_recent <- c(p_recent, NA)
      #p_recent_error <- c(p_recent_error, NA)
      p_recent_low <- c(p_recent_low, NA)
      p_recent_high <- c(p_recent_high, NA)
      
      p_daily <- c(p_daily, NA)
      #p_daily_error <- c(p_daily_error, NA)
      p_daily_low <- c(p_daily_low, NA)
      p_daily_high <- c(p_daily_high, NA)
      
      p_active <- c(p_active, NA)
      #p_active_error <- c(p_active_error, NA)
      p_active_low <- c(p_active_low, NA)
      p_active_high <- c(p_active_high, NA)
      
    }
    
    population <- c(population, pop)
  }
  
  dd <- data.frame(date = dates,
                   region,
                   # regionname,
                   population,
                   sample_size,
                   reach,
                   sample_size_recent,
                   reach_recent,
                   
                   p_infected,
                   #p_infected_error,
                   p_infected_low,
                   p_infected_high,

                   p_fatalities,
                   #p_fatalities_error,
                   p_fatalities_low,
                   p_fatalities_high,

                   p_recent,
                   #p_recent_error,
                   p_recent_low,
                   p_recent_high,
                   
                   p_daily,
                   #p_daily_error,
                   p_daily_low,
                   p_daily_high,
                   
                   p_active,
                   #p_active_error,
                   p_active_low,
                   p_active_high,
                   
                   stringsAsFactors = F)
  
  return(dd)
}




################################## Start of main body

for (co in 1:length(countries)){
  
  country_iso <- countries[co]

cat("Country ", country_iso, " region daily script run at ", as.character(Sys.time()), "\n\n")

#list of regions
region_tree <- read.csv(data_path, as.is = T)
names(region_tree) <- tolower(names(region_tree))
region_tree <- region_tree[which(region_tree$countrycode==country_iso),]
region_tree$population <- as.numeric(region_tree$population)
region_tree <- region_tree[which(!is.na(region_tree$population)),]
regions <- unique(region_tree$regioncode)

file_path <- paste0(responses_path, country_iso, "-aggregate.csv")
dt <- read.csv(file_path, as.is = T)
cat("Received ", nrow(dt), " responses\n\n")
names(dt) <- tolower(names(dt))

#list of dates
dates_dash <- as.character(seq.Date(start_date, Sys.Date(), by = "days"))
dates <- gsub("-","/", dates_dash)

dt <- remove_outliers(dt, cases_cutoff, fatalities_cutoff)

dw <- data.frame()

for (i in 1:length(regions)){
  reg <- regions[i]
  cat("Processing", reg, #region_names[i], 
      "\n")
  rt <- region_tree[region_tree$regioncode == reg,]
  pop <- sum(rt$population)
  dd <- process_region(dt[dt$iso.3166.2 == reg, ], reg, # name=region_names[i], 
                       pop, dates, max_responses, max_age, recent_max_age)
  #cat("- Writing estimates for:", reg, region_names[i], "\n")
  dw <- rbind(dw, dd)
}
write.csv(dw, paste0(estimates_path, country_iso, "-estimate.csv"), row.names = FALSE)

dw_latest <- dw[dw$date == dates[length(dates)], ]
rownames(dw_latest) <- NULL
write.csv(dw_latest, paste0(estimates_path, country_iso, "-latest-estimate.csv"), row.names = FALSE)
}

