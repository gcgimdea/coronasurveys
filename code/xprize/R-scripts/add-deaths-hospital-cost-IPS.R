# load library
library(tidyverse)
library(data.table)
# library(readxl)
# library(httr)

args <- commandArgs(trailingOnly = T)

print(args)
cat("usage: command start_date end_date cases_file output_file iplan_file cost_file\n")

start_date <- as.Date(args[1])
end_date <- as.Date(args[2])
cases_file <- args[3]
output_file <- args[4]
iplan_file <- args[5]
cost_file <- args[6]

country_region_list <- "./data/countries_regions.csv"

onset_to_death_window <- 13 # CDC web site
onset_to_hospital <- 6 # CDC web site
cases_in_hospital <- 0.25 # Augusto's study in our draft
hospital_in_icu <- 0.30 # CDC web site <50: 23.8%, 50-64: 36.1%, >64: 35.3%
IFR <- 0.01 # 1% IFR


process_country_region <- function(regiondf, df, iplan, costs) {
  
  country <- regiondf$CountryName[1]
  region <- regiondf$RegionName[1]
  
  # cat("\n working on ", country, region, "\n")
  
  df <- df[(df$CountryName == country) & 
                  (df$RegionName == region),]
  iplan <- iplan[(iplan$CountryName == country) & 
             (iplan$RegionName == region),]
  dfcost <- costs[(costs$CountryName == country) & 
                    (costs$RegionName == region),]
  
  
  df$PredictedDailyNewDeaths <- NA
  df$PredictedDailyNewHospital <- NA
  df$PredictedDailyNewICU <- NA
  
  # Change df$PredictedDailyNewDeaths
  df$PredictedDailyNewDeaths <- shift(df$PredictedDailyNewCases * IFR, 
                                        n = onset_to_death_window, 
                                        fill = NA)
  
  # Hospital cases
  df$PredictedDailyNewHospital <- shift(df$PredictedDailyNewCases * cases_in_hospital, 
                                        n = onset_to_hospital, 
                                        fill = NA)
  
  df$PredictedDailyNewICU <- df$PredictedDailyNewHospital * hospital_in_icu
  
  # Computes the cost of the vectors
  for (i in 1:nrow(df)) {
    df[i, "Cost"] <- as.matrix(iplan[i, c("C1_School.closing","C2_Workplace.closing",
                                       "C3_Cancel.public.events","C4_Restrictions.on.gatherings",
                                       "C5_Close.public.transport","C6_Stay.at.home.requirements",
                                       "C7_Restrictions.on.internal.movement","C8_International.travel.controls",
                                       "H1_Public.information.campaigns","H2_Testing.policy",
                                       "H3_Contact.tracing","H6_Facial.Coverings")]) %*% t(as.matrix(dfcost[1, 3:14]))
  }
  
  df[, c("C1_School.closing","C2_Workplace.closing",
         "C3_Cancel.public.events","C4_Restrictions.on.gatherings",
         "C5_Close.public.transport","C6_Stay.at.home.requirements",
         "C7_Restrictions.on.internal.movement","C8_International.travel.controls",
         "H1_Public.information.campaigns","H2_Testing.policy",
         "H3_Contact.tracing","H6_Facial.Coverings")] <- 
    iplan[, c("C1_School.closing","C2_Workplace.closing",
                                        "C3_Cancel.public.events","C4_Restrictions.on.gatherings",
                                        "C5_Close.public.transport","C6_Stay.at.home.requirements",
                                        "C7_Restrictions.on.internal.movement","C8_International.travel.controls",
                                        "H1_Public.information.campaigns","H2_Testing.policy",
                                        "H3_Contact.tracing","H6_Facial.Coverings")]
  
  return(df)
}

# ---------------------- main 

costs <- read.csv(cost_file, stringsAsFactors=FALSE) #, check.names = FALSE
costs$RegionName[is.na(costs$RegionName)] <- ""

iplan <- read.csv(iplan_file, stringsAsFactors=FALSE) #, check.names = FALSE
iplan$RegionName[is.na(iplan$RegionName)] <- ""
iplan$Date <- as.Date(iplan$Date)
iplan <- iplan[(iplan$Date >= start_date) & (iplan$Date <= end_date),]

dfc <- read.csv(cases_file, stringsAsFactors=FALSE) #, check.names = FALSE)
dfc$RegionName[is.na(dfc$RegionName)] <- ""
dfc$Date <- as.Date(dfc$Date)
dfc <- dfc[(dfc$Date >= start_date) & (dfc$Date <= end_date),]

regiondf <- read.csv(country_region_list, stringsAsFactors=FALSE)
regiondf$RegionName[is.na(regiondf$RegionName)] <- ""
regiondf <- regiondf[,c("CountryName", "RegionName")]
n <- nrow(regiondf)

# print(n)

df2 <- process_country_region(as.data.frame(regiondf[1,]), dfc, iplan, costs)

if (n>1) {
  for (i in 2:n) {
    df2 <- bind_rows(df2, process_country_region(as.data.frame(regiondf[i,]), dfc, iplan, costs))
  }
}

df2 <- df2[order(df2$CountryName,df2$RegionName,df2$Date),]
write.csv(df2, output_file, row.names = F)
