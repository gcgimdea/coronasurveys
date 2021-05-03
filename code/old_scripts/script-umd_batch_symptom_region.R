## Libraries
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(stringr)

## Load smoothing function ----
source("smooth_column-v2.R")

estimates_path <- "../data/estimates-umd-symptom-survey/"

# estimates_path <- "./estimates-umd-symptom-survey/"

batch_d <- 10000

## Function to extract updated data from UMD api: ----
UMD_api <- function(country, region = F,  type = "daily", date_start = NA, date_end = NA){
  
  country <- str_replace_all(country, " ", "%")
  
  # first date available:
  if (is.na(date_start)) {
    # request <- GET(url = paste0("https://covidmap.umd.edu/api/datesavail?country=",
    #                             country,"%"))
    
    if (!region) {
      request <- GET(url = paste0("https://covidmap.umd.edu/api/datesavail?country=",
                                  country))
    }else {
      request <- GET(url = paste0("https://covidmap.umd.edu/api/datesavail?country=",
                                  country, "&region=all"))
    }
    
    # make sure the content is encoded with 'UTF-8'
    response <- content(request, as = "text", encoding = "UTF-8")
    
    # now we can have a dataframe for use!
    date_start <- fromJSON(response, flatten =  T) %>% data.frame() 
    
    date_start = min(date_start$data.survey_date)
  }
  
  # today:
  if (is.na(date_end)) {
    date_end = format(Sys.time(), "%Y%m%d")
  }
  
  # adding url
  if (!region) {
    path <- paste0("https://covidmap.umd.edu/api/resources?indicator=covid",
                   "&type=", type, 
                   "&country=", country, 
                   "&daterange=", date_start, "-", date_end) 
  }else {
    path <- paste0("https://covidmap.umd.edu/api/resources?indicator=covid",
                   "&type=", type, 
                   "&country=", country, 
                   "&region=all", 
                   "&daterange=", date_start, "-", date_end) 
  }
  
  
  # request data from api
  request <- GET(url = path)
  
  # make sure the content is encoded with 'UTF-8'
  response <- content(request, as = "text", encoding = "UTF-8")
  
  # now we can have a dataframe for use!
  coviddata <- fromJSON(response, flatten = TRUE) %>% data.frame()
  
  return(coviddata)
}

## Batched-estimated-cases Function ----
batch_effect <- function(df_batch_in, denom2try){
  df_out <- data.frame()
  for (denom in denom2try) { # different denominators for batch size
    
    df_temp <- df_batch_in
    
    b_size <- df_temp[1, "population"]/denom
    
    df_temp$batched_pct_cli <- NA
    
    # df_temp <- df_temp %>%  arrange(desc(date) )
    
    df_temp$cum_responses <- cumsum(df_temp$total_responses)
    df_temp$cum_number_cli <- cumsum(df_temp$number_cli)
    
    i_past = 1 # where last batch size was reached
    for (i in 1:(nrow(df_temp)-1)) {
      if (df_temp[i, "cum_responses"] >= b_size) {
        
        df_temp[ceiling((i+i_past)/2), "batched_pct_cli"] <- 
          df_temp[i, "cum_number_cli"]/df_temp[i, "cum_responses"]*100
        
        i_past = i
        
        df_temp[(i+1):nrow(df_temp), "cum_responses"] <- 
          df_temp[(i+1):nrow(df_temp), "cum_responses"] - df_temp[i, "cum_responses"]
        df_temp[(i+1):nrow(df_temp), "cum_number_cli"] <- 
          df_temp[(i+1):nrow(df_temp), "cum_number_cli"] - df_temp[i, "cum_number_cli"]
        
      } # if-cum_responses-greater-b_size
    } # for-rows-df_temp
    
    # smooth "pct_cli" with our method:
    df_temp <- smooth_column(df_in = df_temp, 
                             col_s = "pct_cli", 
                             link_in = "log",
                             basis_dim = min(sum(!is.na(df_temp$pct_cli))-1, 25),
                             monotone = F)
    
    # # fill_pct_cli_smooth_post_batched_smoothing
    # # fill first and last NA's of "batched"-version with "pct_cli_smooth":
    # # 1. non-NA elements in "batched_pct_cli":
    # first_non_NA <- min(which(!is.na(df_temp$batched_pct_cli)))
    # last_non_NA <- max(which(!is.na(df_temp$batched_pct_cli)))
    # # 2.assign the values from "pct_cli_smooth":
    # df_temp[-(first_non_NA:last_non_NA) , "batched_pct_cli"] <- df_temp[-(first_non_NA:last_non_NA) , "pct_cli_smooth"]
    
    # # fill_pct_cli_smooth_pre_batched_smoothing_only_1st_and_last
    # # fill first and last elements of "batched"-version with "pct_cli_smooth":
    # df_temp[c(1, nrow(df_temp)), "batched_pct_cli"] <- df_temp[c(1, nrow(df_temp)), "pct_cli_smooth"]
    
    # fill_pct_cli_pre_batched_smoothing
    # fill first and last NA's of "batched"-version with "pct_cli":
    # 1. non-NA elements in "batched_pct_cli":
    first_non_NA <- min(which(!is.na(df_temp$batched_pct_cli)))
    last_non_NA <- max(which(!is.na(df_temp$batched_pct_cli)))
    # 2.assign the values from "pct_cli_smooth":
    df_temp[-(first_non_NA:last_non_NA) , "batched_pct_cli"] <- 
      df_temp[-(first_non_NA:last_non_NA) , "pct_cli"]
    
    # smooth the  "batched_pct_cli" with our method:
    df_temp <- smooth_column(df_in = df_temp, 
                             col_s = "batched_pct_cli", 
                             link_in = "log",
                             basis_dim = min(sum(!is.na(df_temp$batched_pct_cli))-1, 25), 
                             monotone = F)
    
    # # fill_pct_cli_smooth_post_batched_smoothing
    # # fill first and last NA's of "batched"-version with "pct_cli_smooth":
    # # 1. non-NA elements in "batched_pct_cli":
    # first_non_NA <- min(which(!is.na(df_temp$batched_pct_cli)))
    # last_non_NA <- max(which(!is.na(df_temp$batched_pct_cli)))
    # # 2.assign the values from "pct_cli_smooth":
    # df_temp[-(first_non_NA:last_non_NA) , "batched_pct_cli_smooth"] <- df_temp[-(first_non_NA:last_non_NA) , "pct_cli_smooth"]

    
    df_temp <- df_temp %>% 
      select(date, population, total_responses, pct_cli, 
             pct_cli_smooth, pct_cli_smooth_low, pct_cli_smooth_high,
             number_cli, batched_pct_cli, 
             batched_pct_cli_smooth, batched_pct_cli_smooth_low, batched_pct_cli_smooth_high) %>% 
      mutate(estimate_cli = population*(batched_pct_cli_smooth/100),
             cum_estimate_cli = cumsum(estimate_cli),
             number_cli_smooth = population*(pct_cli_smooth/100),
             cum_number_cli_smooth = cumsum(number_cli_smooth))
    
    df_temp$b_size_denom <- denom 
    
    df_out <- rbind(df_out, df_temp)
  }
  
  return(df_out)
}


# umd_batch_symptom_country <- function(countries_2_try, denom_2_try, d_to_save){
#   for (country in countries_2_try) {
    
    country = "Spain"
    denom_2_try = seq(1500, batch_d, by = 500)
    d_to_save = batch_d

    print(paste0("Batching and smoothing: ", country, "'s UMD data"))
    
    ## Load data 
    dt <- UMD_api(country = country, region = T)
    
    # remove "data." from column names:
    colnames(dt) <- str_replace_all(colnames(dt), "data.", "")
    
    # get a specific region:
    unique(dt$region)
    dt <- dt %>% filter(region == "Comunidad de Madrid")
    
    # set dates:
    dt <- dt %>% mutate(date = paste0( str_sub(survey_date, 1, 4), "-",
                                       str_sub(survey_date, 5, 6), "-",
                                       str_sub(survey_date, 7, 8))) %>% 
      mutate(date = as.Date(date)) %>% 
      select(date, percent_cli, percent_cli_unw) #, sample_size)
    
    # rename columns to use Fb. Challenge scripts:
    colnames(dt) <- c("date", "pct_cli_weighted", "pct_cli") #, "total_responses")
    
    # I THINK pct_cli IS NOW A RATIO IN [0,1] NOT A %:
    summary(dt$pct_cli)
    summary(dt$pct_cli_weighted)
    # transform it to emulate Fb Challenge analysis:
    dt$pct_cli <- dt$pct_cli*100
    dt$pct_cli_weighted <- dt$pct_cli_weighted*100
    
    # number of infected:
    # dt$number_cli <- dt$total_responses*dt$pct_cli/100
    
    # add population:
    # dt$population <- countries[countries$country==country, "population"]
    dt$population <- 6663394
    
    # df_out <- batch_effect(df_batch_in = dt, 
    #                        denom2try = denom_2_try) # denom2try = seq(1000, 5000, by = 500)
    df_out <- dt
    
    # df_out$p_cases_active <- df_out$batched_pct_cli_smooth/100
    # df_out$p_cases_active_high <- df_out$batched_pct_cli_smooth_high/100
    # df_out$p_cases_active_low <- df_out$batched_pct_cli_smooth_low/100
    
    df_out$date <- as.Date(gsub("-", "/", df_out$date))
    # df_out$date <- as.Date(df_out$date)
    
    # select a single batch size:
    # df_save <- df_out %>% filter(b_size_denom == d_to_save)
    df_save <- df_out
    
    country_code <- "ES"
    region_code <- "ESMD"
    
    write.csv(df_save,
              paste0(estimates_path, country_code, "/", region_code, "-estimate.csv"),
              row.names = FALSE)
    
    
    # ## Some plots
    # df_out$d = paste0("d = ", df_out$b_size_denom)
    # 
    # df_out <- df_out %>% ungroup()
    # 
    # p1 <- ggplot(data = df_out, aes(x = date, colour = Legend)) +
    #   facet_wrap( ~ d, scales = "free_y" )+
    #   geom_point(aes(y = batched_pct_cli, colour = "Batched CSDC CLI"), alpha = 0.5, size = 2) +
    #   geom_line(aes(y = batched_pct_cli_smooth, colour = "Batched CSDC CLI (smooth)"), 
    #             linetype = "solid", size =1, alpha = 0.6) +
    #   geom_ribbon(aes(ymin = batched_pct_cli_smooth_low, 
    #                   ymax = batched_pct_cli_smooth_high), 
    #               alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
    #   geom_point(aes(y = pct_cli, colour = "CSDC CLI"), alpha = 0.2, size = 2) +
    #   geom_line(aes(y = pct_cli_smooth, colour = "CSDC CLI (smooth)"), 
    #             linetype = "solid", size = 1, alpha = 0.6) +
    #   geom_ribbon(aes(ymin = pct_cli_smooth_low, 
    #                   ymax = pct_cli_smooth_high), 
    #               alpha = 0.1, color = "red", size = 0.1, fill = "red") +
    #   geom_point(aes(y = pct_cli, colour = "d = population / batch size"), alpha = 0) +
    #   theme_bw() +
    #   scale_colour_manual(values = c("blue", "blue", "red", "red", "black"),
    #                       guide = guide_legend(override.aes = list(
    #                         linetype = c("blank", "solid", "blank", "solid", "blank"),
    #                         shape = c(1, NA, 1, NA, NA)))) +
    #   xlab("Date") + ylab("% symptomatic cases") + ggtitle(country) +
    #   theme(legend.position = "bottom")
    # p1
    # ggsave(plot = p1, 
    #        filename =  paste0(estimates_path, country_code, "/", region_code, "-plots-by-batch.png"), 
    #        width = 9, height = 6)
    
#   } # end-for-countries_2_try
# } #end-function: umd_batch_symptom_country



## List of countries and regions: ----

# ## Function to create csv with available countries ---
# ## It adds populations and iso codes (alpha 2 and 3)
# create_countries_pop_iso <- function(){
#   request <- GET(url = "https://covidmap.umd.edu/api/country")
#   
#   response <- content(GET(url = "https://covidmap.umd.edu/api/country"),
#                       as = "text", encoding = "UTF-8")
#   
#   # available countries:
#   countries <- fromJSON(response, flatten = TRUE) %>% data.frame() 
#   
#   # country data: iso codes and population:
#   countries_pop <- read.csv("../data/common-data/country_population_ecdc.csv", 
#                             header = T) %>% 
#     select(country_territory, countryterritoryCode, geo_id, population)
#   
#   colnames(countries_pop) <- c("country", "iso_alpha3", "iso_alpha2", "population")
#   
#   countries_pop$country <- str_replace_all(countries_pop$country, "_", " ")
#   
#   countries <- left_join(countries, countries_pop, by = "country")
#   
#   countries[countries$country == "C\xf4te d'Ivoire", 2:4] <- 
#     countries_pop[countries_pop$country == "Cote dIvoire", 2:4]
#   
#   countries[countries$country == "Czech Republic", 2:4] <- 
#     countries_pop[countries_pop$country == "Czechia", 2:4]
#   
#   levels(countries$iso_alpha2) <- c(levels(countries$iso_alpha2), "HK")
#   levels(countries$iso_alpha3) <- c(levels(countries$iso_alpha3), "HKG")
#   countries[countries$country == "Hong Kong", "iso_alpha3"] <- "HKG"
#   countries[countries$country == "Hong Kong", "iso_alpha2"] <- "HK"
#   countries[countries$country == "Hong Kong", "population"] <- 7496981
#   
#   countries[countries$country == "Puerto Rico, U.S.", 2:4] <- 
#     countries_pop[countries_pop$country == "Puerto Rico", 2:4]
#   
#   levels(countries$iso_alpha2) <- c(levels(countries$iso_alpha2), "TW")
#   levels(countries$iso_alpha3) <- c(levels(countries$iso_alpha3), "TWN")
#   countries[countries$country == "Taiwan", "iso_alpha3"] <- "TWN"
#   countries[countries$country == "Taiwan", "iso_alpha2"] <- "TW"
#   countries[countries$country == "Taiwan", "population"] <- 23568378
#   
#   countries[countries$country == "Tanzania", 2:4] <- 
#     countries_pop[countries_pop$country == "United Republic of Tanzania", 2:4]
#   
#   write.csv(countries, file = "../data/common-data/country_population_umd.csv")
# }
# 
# ## The csv is already created, uncomment if needed again:
# # create_countries_pop_iso()
# 
# ## Available countries and regions ----
# countries <- read.csv("../data/common-data/country_population_umd.csv", 
#                       header = T)