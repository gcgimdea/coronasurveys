# Rosa's attempt. has problem of double counting.
get_portugal_region_based_rosa <- function(max_ratio = .3, write_file = T){
  cat("generating region based estimate for portugal \n")
  region_pop_portugal <- read.csv("region_pop_portugal.csv", as.is = T)
  dt <- read.csv("../data/aggregate/PT-aggregate.csv", as.is = T)
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
  dt$date <- substr(dt$timestamp, 1, 10)
  n_inital_response <- nrow(dt)
  #############################
  dt <- dt[!is.na(dt$reach),]
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  if(sum(dt$reach > reach_cutoff) > 0 ){
    n_reach_outliers <- sum(dt$reach > reach_cutoff) #number of outliers removed based on reach
    dt <- dt[dt$reach <= reach_cutoff, ]
  }else{
    n_reach_outliers <- 0
  }
  
  # remove outliers based on max ratio of   0.3
  dt$ratio <- dt$cases/dt$reach
  dt2 <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  n_zero_reach_outliers <- sum(!is.finite(dt$ratio)) 
  if(sum(dt2$ratio > max_ratio) > 0 ){
    n_maxratio_outliers <- sum(dt2$ratio > max_ratio) 
    dt2 <- dt2[dt2$ratio <= max_ratio, ]
  }else{
    n_maxratio_outliers <- 0
  }
  
  # set "" to todo el pais
  dt2$region[dt2$region == ""] <- "Todo o país"
  dt2$iso.3166.2[dt2$iso.3166.2 == ""] <- "PT"
  
  # get all the dates
  dates <- unique(dt2$date)
  # create vector of results
  cases_p_reach <- c()
  cases_p_reach_prop <- c()
  # total population
  total_pop <- region_pop_portugal$pop_district[2]
  A11 <- c()
  A12 <- c() 
  A21 <- c()
  A22 <- c()
  n <- c() 
  
  Vp1 <- c()
  Vp2  <- c()
  
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    # get entries before and including date
    dt_date <- dt2[as.Date(dt2$date) <= as.Date(j), ]
    
    # get starting date: date at which we have at least 300 from the past
    date_t <- tail(dt_date$date, 300)[1]
    dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    
    # exclude all country
    regions <- unique(dt2$iso.3166.2)[unique(dt2$iso.3166.2) != "PT"]
    lreg <- length(regions)
    
    # copute the ratio for the regions
    cases_p_reach_t <- c()
    cases_p_reach_prop_t <- c()
    
    # estimated_cases_t <- c()
    # prop_cases_t <- c()
    A11t <- rep(0, lreg)
    A12t <- rep(0, lreg)
    
    A21t <- rep(0, lreg)
    A22t <- rep(0, lreg)
    
    
    ni <- c()
    
    for (i in 1:lreg){
      reg <- regions[i]
      #cat("working on", reg, "\n")
      # get data for all country and the current region
      dt_current <- dt2_r[dt2_r$iso.3166.2 == reg, ]
      ni <- c(ni, nrow(dt_current))
      # get current population
      pop_current <-  region_pop_portugal$pop_district[region_pop_portugal$iso31662 == reg]
      weight_current <- pop_current/total_pop
      
      
      Si_current1 <- sum(((dt_current$cases/dt_current$reach) - 
                            (sum(dt_current$cases, na.rm = T)/sum(dt_current$reach)))^2)/(nrow(dt_current) - 1)
      Si_current2 <- sum(((dt_current$cases/dt_current$reach) - 
                            (mean(dt_current$cases/dt_current$reach, na.rm = T)))^2)/(nrow(dt_current) - 1)
      
      A11t[i] <- weight_current * (Si_current1)
      A12t[i] <- (1 - weight_current) * (Si_current1)
      
      A21t[i] <- weight_current * (Si_current2)
      A22t[i] <- (1 - weight_current) * (Si_current2)        
      
      cases_p_reach_current <- weight_current * 
        sum(dt_current$cases, na.rm = T)/sum(dt_current$reach, na.rm = T)
      cases_p_reach_prop_current <- weight_current * mean(dt_current$cases/dt_current$reach, na.rm = T)
      
      # estimated_cases_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i] * cases_p_reach_current
      # prop_cases_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i] * cases_p_reach_prop_current
      
      cases_p_reach_t <- c(cases_p_reach_t, cases_p_reach_current)
      cases_p_reach_prop_t <- c(cases_p_reach_prop_t, cases_p_reach_prop_current)
      # estimated_cases_t <- c(estimated_cases_t, estimated_cases_current)
      # prop_cases_t <- c(prop_cases_t, prop_cases_current)
    }
    
    A11 <- sum(A11t, na.rm = T)
    A12 <- sum(A12t, na.rm = T)
    
    A21 <- sum(A21t, na.rm = T)
    A22 <- sum(A22t, na.rm = T)
    
    n <- sum(ni)        
    f <- n/total_pop
    varp1 <- ((  (1 - f)/n ) * A11) + (( (1 - f)/(n^2) ) * A21)
    varp2 <- ((  (1 - f)/n ) * A21) + (((1 - f)/(n^2)) * A22)
    Vp1 <- c(Vp1, varp1)
    Vp2 <- c(Vp2, varp2)
    
    cases_p_reach <- c(cases_p_reach, sum(cases_p_reach_t, na.rm = T))
    cases_p_reach_prop <- c(cases_p_reach_prop, sum(cases_p_reach_prop_t, na.rm = T))
    
  }
  #Vp1[!is.finite(Vp1)] <- 0
  #Vp2[!is.finite(Vp2)] <- 0
  
  region_based_estimate2 <- data.frame(date = dates,
                                       cases_p_reach = cases_p_reach,
                                       
                                       cases_p_reach_low = cases_p_reach - (1.96 * sqrt(Vp1)),
                                       cases_p_reach_high = cases_p_reach + (1.96 * sqrt(Vp1)),
                                       #variance_p_reach = Vp1,
                                       #variance_p_reach_prop = Vp2,
                                       cases_p_reach_prop = cases_p_reach_prop, 
                                       #cases_p_reach_prop_low = cases_p_reach_prop - 1.96 * sqrt(Vp2),
                                       #cases_p_reach_prop_high = cases_p_reach_prop + 1.96 * sqrt(Vp2),
                                       estimated_cases = cases_p_reach * total_pop,
                                       estimate_cases_low = (cases_p_reach - (1.96 * sqrt(Vp1))) * total_pop,
                                       estimate_cases_high = (cases_p_reach + (1.96 * sqrt(Vp1))) * total_pop,
                                       prop_cases = cases_p_reach_prop * total_pop,
                                       prop_cases_low = (cases_p_reach_prop - 1.96 * sqrt(Vp2)) * total_pop,
                                       prop_cases_high = (cases_p_reach_prop + 1.96 * sqrt(Vp2)) * total_pop,
                                       stringsAsFactors = F)
  if(write_file == T){
    cat("writing the region based estimate for Portugal..\n")
    write.csv(region_based_estimate2, paste0("../data/PlotData/PT_regional_estimates/region_based_estimates/",
                                             "PT-region-based-estimate_rosa.csv"))
  }
  else{
    return(region_based_estimate2)
  }
  
}

get_portugal_region_based_rosa(write_file = T)
