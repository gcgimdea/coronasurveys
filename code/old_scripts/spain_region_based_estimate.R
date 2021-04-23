
library(stringr)

#Antonio's first attempt
get_spain_region_based_estimate_antonio1 <- function(max_ratio = .3){
  cat("generating region based estimate for spain \n")
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  ccaa_pop <- read.csv("ccaa_population.csv", as.is = T)
  
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
  
  
  dt$date <- substr(dt$timestamp, 1, 10)
  dt$reach[1:102] <- 150 # impute Dunbar number
  n_inital_response <- nrow(dt)
  dt <- dt[!is.na(dt$reach),]
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  if(sum(dt$reach > reach_cutoff) > 0 ){
    # write.table(dt[dt$reach >= reach_cutoff, ],
    #             file = paste0("outliers_removed/", file_name, "_", "outliers_reach.txt"),
    #              append = T) # write out outliers from reach column to the ouliers removed folder
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
  dt2$region[dt2$region == ""] <- "Todo el país"
  dt2$iso.3166.2[dt2$iso.3166.2 == ""] <- "ES"
  
  # get all the dates
  dates <- unique(dt2$date)
  # create vector of results
  estimated_cases <- c()
  prop_cases <- c()
  
  estimate_cases_world <- c()
  estimate_cases_reg <- c()
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    dt_date <- dt2[as.Date(dt2$date) <= as.Date(j), ]
    
    # check for the date which correspond to the last 50 whole country entry
    date_w <- tail(dt_date$date[dt_date$region == "Todo el país"], 50 )[1]
    #check for the date which correspond to the last 100 entry
    date_t <- tail(dt_date$date, 300)[1]
    #dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    if (as.Date(date_w) < as.Date(date_t)){
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    } else{
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_w), ]
    }
    
    m <- sum(dt2_r$region == "Todo el país")
    n <- nrow(dt2_r) - m
    
    # compute the ratios for todo el pais
    dt2_rt <- dt2_r[dt2_r$region == "Todo el país", ]
    cases_p_reach_w <- sum(dt2_rt$cases) / sum(dt2_rt$reach)
    cases_p_reach_prop_w <- mean(dt2_rt$cases/dt2_rt$reach)
    
    estimated_cases_w <- ccaa_pop$population[ccaa_pop$ccaa_survey == "Todo el país"] * cases_p_reach_w * (m/(m+n))
    prop_cases_w <- ccaa_pop$population[ccaa_pop$ccaa_survey == "Todo el país"] * cases_p_reach_prop_w * (m/(m+n))
    
    # copute the ratio for the regions
    cases_p_reach_t <- c()
    cases_p_reach_prop_t <- c()
    
    estimated_cases_t <- c()
    prop_cases_t <- c()
    
    for (i in unique(dt2$iso.3166.2)[unique(dt2$iso.3166.2) != "ES"]){
      # get data for all country and the current region
      dt_current <- dt2_r[dt2_r$iso.3166.2 == i, ]
      
      cases_p_reach_current <- sum(dt_current$cases)/sum(dt_current$reach)
      cases_p_reach_prop_current <- mean(dt_current$cases/dt_current$reach)
      
      estimated_cases_current <- ccaa_pop$population[ccaa_pop$iso31662 == i] * cases_p_reach_current
      prop_cases_current <- ccaa_pop$population[ccaa_pop$iso31662 == i] * cases_p_reach_prop_current
      
      cases_p_reach_t <- c(cases_p_reach_t, cases_p_reach_current)
      cases_p_reach_prop_t <- c(cases_p_reach_prop_t, cases_p_reach_prop_current)
      estimated_cases_t <- c(estimated_cases_t, estimated_cases_current)
      prop_cases_t <- c(prop_cases_t, prop_cases_current)
    }
    
    estimated_cases_t <- sum(estimated_cases_t, na.rm = T) * (n/(m+n))
    prop_cases_t <- sum(prop_cases_t, na.rm = T) * (n/(m+n))
    
    estimated_cases <-c(estimated_cases, estimated_cases_w + estimated_cases_t)
    estimate_cases_world <- c(estimate_cases_world, estimated_cases_w)
    estimate_cases_reg <- c(estimate_cases_reg, estimated_cases_t)
    
    prop_cases <- c(prop_cases, prop_cases_w + prop_cases_t)
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      estimated_cases_region_based = estimated_cases,
                                      prop_cases_region_based = prop_cases, 
                                      estimate_cases_world = estimate_cases_world,
                                      estimate_cases_reg = estimate_cases_reg)
  cat("writing the region based estimate for Spain..\n")
  write.csv(region_based_estimate, paste0("../data/PlotData/ES_regional_estimates/region_based_estimates/",
                                          "ES-region-based-estimate_antonio_first_attempt.csv"))
}

#get_spain_region_based_estimate_antonio1() #replaced by the ones in separate files

#Antonio's first attempt but with  I_r, I_c, r_r, r_c written out
get_spain_region_based_estimate_antonio2 <- function(max_ratio = .3){
  cat("generating region based estimate for spain \n")
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  ccaa_pop <- read.csv("ccaa_population.csv", as.is = T)
  
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
  
  
  dt$date <- substr(dt$timestamp, 1, 10)
  dt$reach[1:102] <- 150 # impute Dunbar number
  n_inital_response <- nrow(dt)
  dt <- dt[!is.na(dt$reach),]
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  if(sum(dt$reach > reach_cutoff) > 0 ){
    # write.table(dt[dt$reach >= reach_cutoff, ],
    #             file = paste0("outliers_removed/", file_name, "_", "outliers_reach.txt"),
    #              append = T) # write out outliers from reach column to the ouliers removed folder
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
  dt2$region[dt2$region == ""] <- "Todo el país"
  dt2$iso.3166.2[dt2$iso.3166.2 == ""] <- "ES"
  
  # get all the dates
  dates <- unique(dt2$date)
  # create vector of results
  estimated_cases <- c()
  prop_cases <- c()
  
  estimate_cases_world <- c()
  estimate_cases_reg <- c()
  
  I_c_est <- c()
  I_c_prop <- c()
  r_c <- c()
  
  I_r_est2 <- c()
  I_r_prop2 <- c()
  r_r2 <- c()
  
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    dt_date <- dt2[as.Date(dt2$date) <= as.Date(j), ]
    
    # check for the date which correspond to the last 30 whole country entry
    date_w <- tail(dt_date$date[dt_date$region == "Todo el país"], 50 )[1]
    #check for the date which correspond to the last 100 entry
    date_t <- tail(dt_date$date, 300)[1]
    #dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    if (as.Date(date_w) < as.Date(date_t)){
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    } else{
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_w), ]
    }
    m <- sum(dt2_r$region == "Todo el país")
    n <- nrow(dt2_r) - m
    
    # compute the ratios for todo el pais   # I_r, I_c, r_r, r_c
    dt2_rt <- dt2_r[dt2_r$region == "Todo el país", ]
    cases_p_reach_w <- sum(dt2_rt$cases) / sum(dt2_rt$reach)
    cases_p_reach_prop_w <- mean(dt2_rt$cases/dt2_rt$reach)
    
    estimated_cases_w <- ccaa_pop$population[ccaa_pop$ccaa_survey == "Todo el país"] * cases_p_reach_w * (m/(m+n))
    prop_cases_w <- ccaa_pop$population[ccaa_pop$ccaa_survey == "Todo el país"] * cases_p_reach_prop_w * (m/(m+n))
    
    I_c_est <- c(I_c_est, ccaa_pop$population[ccaa_pop$ccaa_survey == "Todo el país"] * cases_p_reach_w)
    I_c_prop <- c(I_c_prop, ccaa_pop$population[ccaa_pop$ccaa_survey == "Todo el país"] * cases_p_reach_prop_w)
    
    r_c <- c(r_c, sum(dt2_rt$reach))
    
    # copute the ratio for the regions
    cases_p_reach_t <- c()
    cases_p_reach_prop_t <- c()
    
    estimated_cases_t <- c()
    prop_cases_t <- c()
    
    r_r <- c()
    I_r_est <- c()
    I_r_prop <- c()
    
    for (i in unique(dt2$iso.3166.2)[unique(dt2$iso.3166.2) != "ES"]){
      # get data for all country and the current region
      dt_current <- dt2_r[dt2_r$iso.3166.2 == i, ]
      
      cases_p_reach_current <- sum(dt_current$cases)/sum(dt_current$reach)
      cases_p_reach_prop_current <- mean(dt_current$cases/dt_current$reach)
      r_r <- c(r_r, sum(dt_current$reach))
      
      estimated_cases_current <- ccaa_pop$population[ccaa_pop$iso31662 == i] * cases_p_reach_current
      prop_cases_current <- ccaa_pop$population[ccaa_pop$iso31662 == i] * cases_p_reach_prop_current
      
      I_r_i_est <- ccaa_pop$population[ccaa_pop$iso31662 == i] * cases_p_reach_current
      I_r_i_prop <- ccaa_pop$population[ccaa_pop$iso31662 == i] * cases_p_reach_prop_current
      
      I_r_est <- c(I_r_est, I_r_i_est)
      I_r_prop <- c(I_r_prop, I_r_i_prop )
      
      cases_p_reach_t <- c(cases_p_reach_t, cases_p_reach_current)
      cases_p_reach_prop_t <- c(cases_p_reach_prop_t, cases_p_reach_prop_current)
      estimated_cases_t <- c(estimated_cases_t, estimated_cases_current)
      prop_cases_t <- c(prop_cases_t, prop_cases_current)
    }
    
    estimated_cases_t <- sum(estimated_cases_t, na.rm = T) * (n/(m+n))
    prop_cases_t <- sum(prop_cases_t, na.rm = T) * (n/(m+n))
    
    estimated_cases <-c(estimated_cases, estimated_cases_w + estimated_cases_t)
    estimate_cases_world <- c(estimate_cases_world, estimated_cases_w)
    estimate_cases_reg <- c(estimate_cases_reg, estimated_cases_t)
    
    prop_cases <- c(prop_cases, prop_cases_w + prop_cases_t)
    
    I_r_est2 <- c(I_r_est2, sum(I_r_est, na.rm = T))
    I_r_prop2 <- c(I_r_prop2, sum(I_r_prop, na.rm = T))
    r_r2 <- c(r_r2, sum(r_r, na.rm = T))
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      estimated_cases_region_based = estimated_cases,
                                      prop_cases_region_based = prop_cases, 
                                      estimate_cases_world = estimate_cases_world,
                                      estimate_cases_reg = estimate_cases_reg, 
                                      I_r_est = I_r_est2,
                                      I_c_est =  I_c_est, 
                                      r_r = r_r2, 
                                      r_c = r_c,
                                      I_r_prop = I_r_prop2,
                                      I_c_prop = I_c_prop)
  cat("writing the region based estimate for Spain..\n")
  write.csv(region_based_estimate, paste0("../data/PlotData/ES_regional_estimates/region_based_estimates/",
                                          "ES-region-based-estimate_antonio_first_attempt_with_Is.csv"))
}

#get_spain_region_based_estimate_antonio2()

# Rosa's attempt. has problem of double counting.
get_spain_region_based_rosa <- function(max_ratio = .3, write_file = T){
  cat("generating region based estimate for spain \n")
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  # remove provincia
  dt <- dt[!str_detect((dt$Region), pattern = "Provincia"),]
  # change region Baleares from ESPM to ESIB
  dt$ISO.3166.2[dt$Region == "Baleares"] <- "ESIB" 
  # 
  ccaa_pop <- read.csv("ccaa_population.csv", as.is = T)
  
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
  
  
  dt$date <- substr(dt$timestamp, 1, 10)
  dt$reach[1:102] <- 150 # impute Dunbar number
  n_inital_response <- nrow(dt)
  dt <- dt[!is.na(dt$reach),]
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  if(sum(dt$reach > reach_cutoff) > 0 ){
    # write.table(dt[dt$reach >= reach_cutoff, ],
    #             file = paste0("outliers_removed/", file_name, "_", "outliers_reach.txt"),
    #              append = T) # write out outliers from reach column to the ouliers removed folder
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
  dt2$region[dt2$region == ""] <- "Todo el país"
  dt2$iso.3166.2[dt2$iso.3166.2 == ""] <- "ES"
  
  # get all the dates
  dates <- unique(dt2$date)
  # create vector of results
  cases_p_reach <- c()
  cases_p_reach_prop <- c()
  # total population
  total_pop <- ccaa_pop$population[1]
  A11 <- c()
  A12 <- c() 
  A21 <- c()
  A22 <- c()
  n <- c() 
  
  Vp1 <- c()
  Vp2  <- c()
  
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    dt_date <- dt2[as.Date(dt2$date) <= as.Date(j), ]
    
    date_t <- tail(dt_date$date, 300)[1]
    dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    
    regions <- unique(dt2$iso.3166.2)[unique(dt2$iso.3166.2) != "ES"]
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
      pop_current <- ccaa_pop$population[ccaa_pop$iso31662 == reg]
      weight_current <- pop_current/total_pop
      
      
      Si_current1 <- sum(((dt_current$cases/dt_current$reach) - (sum(dt_current$cases, na.rm = T)/sum(dt_current$reach)))^2)/(nrow(dt_current) - 1)
      Si_current2 <- sum(((dt_current$cases/dt_current$reach) - (mean(dt_current$cases/dt_current$reach, na.rm = T)))^2)/(nrow(dt_current) - 1)
      
      A11t[i] <- weight_current * (Si_current1)
      A12t[i] <- (1 - weight_current) * (Si_current1)
      
      A21t[i] <- weight_current * (Si_current2)
      A22t[i] <- (1 - weight_current) * (Si_current2)        
      
      cases_p_reach_current <- weight_current * sum(dt_current$cases, na.rm = T)/sum(dt_current$reach, na.rm = T)
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
                                       p_w = cases_p_reach,
                                       p_w_low = cases_p_reach - (1.96 * sqrt(Vp1)),
                                       p_w_high = cases_p_reach + (1.96 * sqrt(Vp1)),
                                       p_m = cases_p_reach_prop, 
                                       p_m_low = cases_p_reach_prop - 1.96 * sqrt(Vp2),
                                       p_m_high = cases_p_reach_prop + 1.96 * sqrt(Vp2),
                                       stringsAsFactors = F)
  if(write_file == T){
    cat("writing the region based estimate for Spain..\n")
    write.csv(region_based_estimate2, paste0("../data/PlotData/ES_regional_estimates/region_based_estimates/",
                                             "ES-region-based-estimate_rosa.csv"))
  }
  else{
    return(region_based_estimate2)
  }
  
}

get_spain_region_based_rosa(write_file = T)




