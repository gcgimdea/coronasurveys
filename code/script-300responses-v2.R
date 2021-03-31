library(tidyr)
library(dplyr)

responses_path <- "../data/aggregate/"
data_path <- "../data/common-data/regions-tree-population.csv"
estimates_path <- "../data/estimates-300responses/PlotData/"

# responses_path <- "../coronasurveys/data/aggregate/"
# data_path <- "../coronasurveys/data/common-data/regions-tree-population.csv"
# estimates_path <- "./estimates-300responses/PlotData/"

cases_cutoff <- 1/2

get_countries_with_survey <- function(path = responses_path){
  #get list of countries with surveys
  plotdata_files <- list.files(path)
  plotdata_files <- plotdata_files[plotdata_files != "Twitter-surveys.csv"]
  substr(plotdata_files,start = 1, stop = 2)
}

get_spain_region_based_rosa <- function(country_geoid = "ES",
                                        # max_ratio = .3,
                                        write_file = T, 
                                        survey_countries = get_countries_with_survey()){
  cat("::- script-300responses: Working on ", country_geoid, "::\n")
  
  if(country_geoid %in% survey_countries){
    file_path <- paste0(responses_path, country_geoid, "-aggregate.csv")
    dt <- read.csv(file_path, as.is = T)
    names(dt) <- tolower(names(dt))
    dt <- dt[, c("timestamp","region","reach","cases","recovered","fatalities","recentcases","stillsick", "iso.3166.1.a2", "iso.3166.2")]
    dt$date <- substr(dt$timestamp, 1, 10)
    dates <- as.character(seq.Date(as.Date(dt$date[1]), as.Date(tail(dt$date,1)), by = "days"))
    dates <- gsub("-","/", dates)
    if(country_geoid == "ES"){
      # change province madrid to comunidad madrid
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:Madrid")] <- "ESMD" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:Madrid")] <- "Comunidad Autónoma:Madrid, Comunidad de"
      
      # change provincia navarra
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:Navarra / Nafarroa")] <- "ESNC" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:Navarra / Nafarroa")] <- "Comunidad Autónoma:Navarra, Comunidad Foral de / Nafarroako Foru Komunitatea"
      
      #dt$region[stringr::str_detect((dt$iso.3166.2), pattern = "ESNC")]
      
      # change La rioja
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:La Rioja")] <- "ESRI" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:La Rioja")] <- "Comunidad Autónoma:La Rioja"
      #dt$region[stringr::str_detect((dt$iso.3166.2), pattern = "ESRI")]
      
      # change Baleares
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:Balears")] <- "ESIB" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:Balears")] <- "Comunidad Autónoma:Illes Balears"
      dt$iso.3166.2[dt$region == "Baleares"] <- "ESIB" 
      #dt$region[stringr::str_detect((dt$iso.3166.2), pattern = "ESPM")]
      
      # change murcia
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:Murcia")] <- "ESMC" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:Murcia")] <- "Comunidad Autónoma:Murcia, Región de"
      #dt$region[stringr::str_detect((dt$iso.3166.2), pattern = "ESMU")]
      
      # change cantabria
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:Cantabria")] <- "ESCB" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:Cantabria")] <- "Comunidad Autónoma:Cantabria"
      #dt$region[stringr::str_detect((dt$iso.3166.2), pattern = "ESMU")]
      #unique(dti$region)
      
      # change asturias
      dt$iso.3166.2[stringr::str_detect((dt$region), pattern = "Provincia:Asturias")] <- "ESAS" 
      dt$region[stringr::str_detect((dt$region), pattern = "Provincia:Asturias")] <- "Comunidad Autónoma:Asturias, Principado de"
      #dt$region[stringr::str_detect((dt$iso.3166.2), pattern = "ESMU")]
      
      # remove provincia
      dt <- dt[!stringr::str_detect((dt$region), pattern = "Provincia"),]
      
    }
    
    if (country_geoid == "IT"){
      dt <- dt[!stringr::str_detect((dt$region), pattern = "Province:"),]
    }
    regions_tree <- read.csv(file = data_path, as.is = T) %>% 
      filter(countrycode == country_geoid) %>% 
      group_by(regioncode) %>% 
      summarise(population = sum(population))
    
    if(country_geoid == "ES"){
      dt$reach[1:102] <- 150 # impute Dunbar number
    }
    #remove outlier of reach.
    dt <- dt[!is.na(dt$reach),]
    reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
    dt <- dt[dt$reach <= reach_cutoff, ]
    
    # remove outliers based on max ratio cases/reach
    dt$ratio <- dt$cases/dt$reach
    dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
    dt <- dt[dt$ratio <= cases_cutoff, ]
    
    # set "" to todo el pais"
    dt$region[dt$region == ""] <- "all country"
    dt$iso.3166.2[dt$iso.3166.2 == ""] <- country_geoid
    
    # get all the dates
    #dates <- unique(dt$date)  # creatr a list of consecutive dates
    # create vector of results
    cases_p_reach <- c()
    cases_p_reach_prop <- c()
    
    p_cases_recent <- c()
    p_stillsick <- c()
    p_fatalities <- c()
    p_recovered <- c()
    
    # total population
    total_pop <- sum(regions_tree$population)
    A11 <- c()
    A12 <- c() 
    A21 <- c()
    A22 <- c()
    n <- c() 
    
    Vp1 <- c()
    Vp2  <- c()
    
    for (j in dates){
      # cat("::- script-300responses: Working on date: ", j, "::\n"  )
      dt_date <- dt[as.Date(dt$date) <= as.Date(j), ]
      
      date_t <- tail(dt_date$date, 300)[1]
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
      
      # regions <- unique(dt$iso.3166.2)[unique(dt$iso.3166.2) != country_geoid]
      regions <- regions_tree$regioncode
      lreg <- length(regions)
      
      # compute the ratio for the regions
      cases_p_reach_t <- c()
      cases_p_reach_prop_t <- c()
      
      p_cases_recent_t <- c()
      p_stillsick_t <- c()
      p_fatalities_t <- c()
      p_recovered_t <- c()
      
      # estimated_cases_t <- c()
      # prop_cases_t <- c()
      A11t <- rep(NA, lreg)
      A12t <- rep(NA, lreg)
      
      A21t <- rep(NA, lreg)
      A22t <- rep(NA, lreg)
      
      
      ni <- c()
      
      for (i in 1:lreg){
        reg <- regions[i]
        # cat("::- script-300responses: Working on region: ", reg, "\n")
        # get data for all country and the current region
        dt_current <- dt2_r[dt2_r$iso.3166.2 == reg, ]
        #if(nrow(dt_current) == 0) next()
        ni <- c(ni, nrow(dt_current))
        # get current population
        pop_current <- regions_tree$population[regions_tree$regioncode == reg]
        weight_current <- pop_current/total_pop
        
        
        Si_current1 <- sum(((dt_current$cases/dt_current$reach) - (sum(dt_current$cases, na.rm = T)/sum(dt_current$reach)))^2)/(nrow(dt_current) - 1)
        Si_current2 <- sum(((dt_current$cases/dt_current$reach) - (mean(dt_current$cases/dt_current$reach, na.rm = T)))^2)/(nrow(dt_current) - 1)
        
        # cat("::- script-300responses: Working on region: ", reg, weight_current, Si_current1, "\n")
        
        A11t[i] <- weight_current * (Si_current1)
        A12t[i] <- (1 - weight_current) * (Si_current1)
        
        A21t[i] <- weight_current * (Si_current2)
        A22t[i] <- (1 - weight_current) * (Si_current2)        
        
        cases_p_reach_current <- weight_current * sum(dt_current$cases, na.rm = T)/sum(dt_current$reach, na.rm = T)
        cases_p_reach_prop_current <- weight_current * mean(dt_current$cases/dt_current$reach, na.rm = T)
        
        p_cases_recent_current <- weight_current * sum(dt_current$recentcases, na.rm = T)/sum(dt_current$reach, na.rm = T)
        p_stillsick_current <- weight_current * sum(dt_current$stillsick, na.rm = T)/sum(dt_current$reach, na.rm = T)
        p_fatalities_current <- weight_current * sum(dt_current$fatalities, na.rm = T)/sum(dt_current$reach, na.rm = T)
        p_recovered_current <- weight_current * sum(dt_current$recovered, na.rm = T)/sum(dt_current$reach, na.rm = T)
        
        cases_p_reach_t <- c(cases_p_reach_t, cases_p_reach_current)
        cases_p_reach_prop_t <- c(cases_p_reach_prop_t, cases_p_reach_prop_current)
        # estimated_cases_t <- c(estimated_cases_t, estimated_cases_current)
        # prop_cases_t <- c(prop_cases_t, prop_cases_current)
        
        p_cases_recent_t <- c(p_cases_recent_t, p_cases_recent_current)
        p_stillsick_t <- c(p_stillsick_t, p_stillsick_current)
        p_fatalities_t <- c(p_fatalities_t, p_fatalities_current)
        p_recovered_t <- c(p_recovered_t, p_recovered_current)
        
        
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
      
      p_cases_recent <- c(p_cases_recent, sum(p_cases_recent_t, na.rm = T))
      p_stillsick <- c(p_stillsick, sum(p_stillsick_t, na.rm = T))
      p_fatalities <- c(p_fatalities, sum(p_fatalities_t, na.rm = T))
      p_recovered <- c(p_recovered, sum(p_recovered_t, na.rm = T))
      
    }
    
    region_based_estimate2 <- data.frame(date = dates,
                                         p_cases_infected = cases_p_reach,
                                         p_cases_infected_error = 1.96 * sqrt(Vp1),
                                         # p_m = cases_p_reach_prop, 
                                         # p_m_low = cases_p_reach_prop - 1.96 * sqrt(Vp2),
                                         # p_m_high = cases_p_reach_prop + 1.96 * sqrt(Vp2),
                                         p_fatalities,
                                         p_recovered,
                                         p_cases_recent,
                                         p_cases_daily = p_cases_recent / 7,
                                         p_cases_active = p_stillsick,
                                         stringsAsFactors = F)
    if(write_file == T){
      cat("::- script-300responses: Writing estimates for:", country_geoid, "::\n")
      write.csv(region_based_estimate2,
                paste0(estimates_path, country_geoid, "-estimate.csv"))

    }
    else{
      return(region_based_estimate2)
    }
    
  } else{
    cat("::- script-300responses:", country_geoid, "does not have a survey data yet ::\n")
    return(NULL)
  }
}


# "DE", "FR" , "GB", "EC", 
# CY does not have regional data check
country_interest <- c("ES", "IT",  "PT", "UA", "BR", "US", "DE", "GB", "FR")
# country_interest <- c("GB")

dd <- sapply(country_interest, get_spain_region_based_rosa, write_file = T)

