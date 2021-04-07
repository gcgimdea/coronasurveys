# needs review when new data is available.
get_portugal_regional_estimates <- function(batch_size = 30,
                                         batching_method = "antonio",
                                         max_ratio = .3,
                                         correction_factor = 1, 
                                         z_mean_hdt = 13,
                                         z_sd_hdt = 12.7,
                                         z_median_hdt = 9.1,
                                         c_cfr_baseline = 1.38,
                                         c_cfr_estimate_range = c(1.23, 1.53)){
  mu_hdt = log(z_median_hdt)
  sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt))
  
  
  data_source <- read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = F) %>% 
    select("data", "data_dados", "confirmados_arsnorte", "confirmados_arscentro",
           "confirmados_arslvt",  "confirmados_arsalentejo", "confirmados_arsalgarve",
           "confirmados_acores", "confirmados_madeira", "obitos_arsnorte", "obitos_arscentro",
           "obitos_arslvt", "obitos_arsalentejo", "obitos_arsalgarve", "obitos_acores",
           "obitos_madeira")
  
  cases_data_source <- data_source  %>% 
    select("data", "confirmados_arsnorte", "confirmados_arscentro",
           "confirmados_arslvt",  "confirmados_arsalentejo", "confirmados_arsalgarve",
           "confirmados_acores", "confirmados_madeira") %>% 
    gather(key = "region", value = "cum_cases", 2:8) %>% 
    group_by(region) %>% 
    mutate(cases =  c(first(cum_cases), diff(cum_cases)) ) %>% 
    ungroup() %>% 
    mutate(region = substr(region, 13, 23))
  
  cases_data_source$cases[cases_data_source$cases<0] <- 0
  
  obitos_data_source <- data_source  %>% 
    select("data",  "obitos_arsnorte", "obitos_arscentro",
           "obitos_arslvt", "obitos_arsalentejo", "obitos_arsalgarve", "obitos_acores",
           "obitos_madeira") %>% 
    gather(key = "region", value = "cum_deaths", 2:8) %>% 
    group_by(region) %>% 
    mutate(deaths =  c(first(cum_deaths), diff(cum_deaths)) ) %>% 
    ungroup() %>% 
    mutate(region = substr(region, 8, 20))
  
  obitos_data_source$deaths[obitos_data_source$deaths<0] <- 0
  
  dt_ds <- full_join(cases_data_source, obitos_data_source) %>% 
    mutate(cum_deaths_400 = cum_deaths *400) %>%
    rename(date = data)
  
  dt_ds$date <- as.character(as.Date(dt_ds$date, format = "%d-%m-%Y"))
  dt_ds$date <- gsub("-", "/", dt_ds$date)
  
  #dt_ds <- dt_ds %>% left_join(region_pop_portugal, by = "region")

  dt_ds_est <- lapply(unique(dt_ds$region), function(x){
    cat("working on the region", x, "\n")
    dt_ccca <- dt_ds[dt_ds$region == x, ]
    ndt <- nrow(dt_ccca)
    est_ccfr <- rep(NA, ndt)
    est_ccfr_low <- rep(NA, ndt)
    est_ccfr_high <- rep(NA, ndt)
    #cat("computing ccfr estimate for ", country_geoid, "...\n")
    for (i in ndt : 1) {
      data2t <- dt_ccca[1:i, c("cases", "deaths")]
      ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
      fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
      
      sigma_fraction_reported <- (1/ccfr$total_deaths)-(1/ccfr$cum_known_t)+ (1/1023) - (1/74130)
      fraction_reported_high <- fraction_reported * exp(1.96*sigma_fraction_reported)
      fraction_reported_low <- fraction_reported * exp(-(1.96*sigma_fraction_reported))
      est_ccfr_low[i] <- dt_ccca$cum_cases[i]*(1/fraction_reported_high)#swich low and high here coz of inverse.
      est_ccfr_high[i] <- dt_ccca$cum_cases[i]*(1/fraction_reported_low)
      est_ccfr[i] <- dt_ccca$cum_cases[i]*(1/fraction_reported)
    }
    dt_ccca$est_ccfr <- est_ccfr
    dt_ccca$est_ccfr_low <- est_ccfr_low
    dt_ccca$est_ccfr_high <- est_ccfr_high
    #survey_ccaa <- ccaa_pop$ccaa_survey[ccaa_pop$ccaa == x]
    #survey_ccaa_pop <- region_pop_portugal$pop_2014[region_pop_portugal$region == x]
    
    if(x %in% c("all_country", unique(dt_ds$region)) ){
      survey_gforms_estimate <- estimate_cases_aggregate_portugal_regional(region = x,
                                                                        #region_population = survey_ccaa_pop,
                                                                        max_ratio = max_ratio,
                                                                        correction_factor = correction_factor, 
                                                                        method = batching_method,
                                                                        batch = batch_size)$dt_estimates
    }else{
      survey_gforms_estimate <- data.frame(date = dt_ccca$date,
                                           sample_size = NA,
                                           mean_cases = NA,
                                           mean_reach = NA,
                                           dunbar_reach = NA,
                                           cases_p_reach = NA,
                                           cases_p_reach_low = NA,
                                           cases_p_reach_high = NA,
                                           cases_p_reach_error = NA,
                                           cases_p_reach_prop = NA,
                                           cases_p_reach_prop_median = NA,
                                           estimated_cases = NA,
                                           estimate_cases_low = NA,
                                           estimate_cases_high = NA,
                                           estimate_cases_error = NA,
                                           prop_cases = NA,
                                           dunbar_cases = NA,
                                           pop_cases_low = NA,
                                           pop_cases_high = NA,
                                           pop_cases_error = NA,
                                           dunbar_cases_low = NA,
                                           dunbar_cases_high = NA,
                                           dunbar_cases_error = NA,
                                           stringsAsFactors = F)
    }
    
    
    
    dt_ccca <- left_join(dt_ccca, survey_gforms_estimate, by = "date")
    return(dt_ccca)
  })
  

  
  
  dt_ds_est <- do.call(rbind, dt_ds_est)
  lapply(unique(dt_ds_est$region), function(x){
    dt_ccca <- dt_ds_est[dt_ds_est$region == x, ]
    write.csv(dt_ccca, paste0("../data/PlotData/PT_regional_estimates/", x, "-", "estimates.csv"))
  })
}




estimate_cases_aggregate_portugal_regional <- function(region,
                                                   # region_population,
                                                    batch,
                                                    method = "antonio",
                                                    max_ratio,
                                                    correction_factor) {

  dt <- read.csv("../data/aggregate/PT-aggregate.csv", as.is = T)
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
  region_pop_portugal <- read.csv("region_pop_portugal.csv", stringsAsFactors = F)[,-1]
  dt$date <- substr(dt$timestamp, 1, 10)
  n_inital_response <- nrow(dt)
  
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
    #write.table(dt[dt$ratio >= max_ratio, ],
    #            file = paste0("outliers_removed/", file_name, "_", "outliers_max_ratio.txt"),
    #            append = T) # write out outliers based on max_Ratio
    n_maxratio_outliers <- sum(dt2$ratio > max_ratio) 
    dt2 <- dt2[dt2$ratio <= max_ratio, ]
  }else{
    n_maxratio_outliers <- 0
  }
  
  dt2 <- full_join(dt2, region_pop_portugal, by = c("region" = "district"))
  
  dt2_r <- dt2[dt2$region.y == region, ]
  region_population <- region_pop_portugal$pop_2014[region_pop_portugal$region == region][1]

  method <- match.arg(method)
  
  if (method == "antonio"){
    dt_batch <- dt2_r %>%
      group_by(date) %>% 
      summarise(sample_size = n())
    # generate grouping variable: if number of responses in a day is sufficient, then agg that day, if not agg multiple days
    group = 1
    group_factor = c()
    container <- c()
    
    for (i in 1:nrow(dt_batch)) {
      container <- c(container, dt_batch$sample_size[i])
      if(sum(container) < batch){
        group_factor <- c(group_factor, group)
      } else{
        group_factor <- c(group_factor, group)
        container <- c()
        group = group + 1
      }
    }
    
    dt_batch$group_factor <- group_factor
    
    dt_batch_s <- dt_batch %>% 
      group_by(group_factor) %>%
      summarise(n = sum(sample_size)) 
    dt_batch_s$include <- dt_batch_s$n >= batch
    
    dt_batch <- full_join(dt_batch, dt_batch_s, by = "group_factor") %>% 
      filter(include == T)
    
    dt2_r <- full_join(dt2_r, dt_batch[,-c(2, 4,5)], by = "date")
    dt_summary <- dt2_r %>%
      filter(!is.na(group_factor)) %>% 
      group_by(group_factor) %>% 
      summarise(date = last(date),
                sample_size = n(), 
                mean_cases = mean(cases),
                mean_reach = mean(reach),
                dunbar_reach = 150 * n(),
                cases_p_reach = sum(cases)/sum(reach), 
                cases_p_reach_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                 pop_size = sum(reach))$low,
                cases_p_reach_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                  pop_size = sum(reach))$upp,
                cases_p_reach_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$error,
                cases_p_reach_prop = mean(ratio), 
                cases_p_reach_prop_median = median(ratio),
                estimated_cases = region_population * sum(cases)/sum(reach) * correction_factor, 
                estimate_cases_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                  pop_size = sum(reach))$low *  region_population * correction_factor,
                estimate_cases_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$upp *  region_population * correction_factor,
                estimate_cases_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                    pop_size = sum(reach))$error *  region_population * correction_factor,
                
                prop_cases = region_population * mean(ratio) * correction_factor,
                dunbar_cases = region_population * (sum(cases)/dunbar_reach) * correction_factor, 
                pop_cases_low = calculate_ci(p_est = mean(ratio), level = 0.95,
                                             pop_size = sum(reach))$low * region_population * correction_factor,
                pop_cases_high = calculate_ci(p_est = mean(ratio), level = 0.95,
                                              pop_size = sum(reach))$upp * region_population * correction_factor,
                pop_cases_error = calculate_ci(p_est = mean(ratio), level = 0.95,
                                               pop_size = sum(reach))$error * region_population * correction_factor,
                dunbar_cases_low = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                pop_size = dunbar_reach)$low * region_population * correction_factor,
                dunbar_cases_high = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                 pop_size = dunbar_reach)$upp * region_population * correction_factor,
                dunbar_cases_error = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                  pop_size = dunbar_reach)$error * region_population * correction_factor )
    dt_summary <- dt_summary[, -1] # remove group factor variable
    dt_summary$cases_p_reach_low[dt_summary$cases_p_reach_low < 0]   <- 0.000001
    dt_summary$estimate_cases_low[dt_summary$estimate_cases_low < 0] <- 0.000001
    dt_summary$pop_cases_low[dt_summary$pop_cases_low < 0] <- 0.000001
    dt_summary$dunbar_cases_low[dt_summary$dunbar_cases_low < 0] <- 0.000001
    
  }  else{
    stop("method can only be antonio")
  }
  
  return(list(dt_estimates = dt_summary,
              n_inital_response = n_inital_response,
              n_reach_outliers = n_reach_outliers,
              n_maxratio_outliers = n_maxratio_outliers, 
              n_zero_reach_outliers = n_zero_reach_outliers,
              n_final_response = sum(dt_summary$sample_size)))
  
}


