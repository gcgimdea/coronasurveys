# load library
library(tidyverse)
#library(data.table)
# library(readxl)
# library(httr)

args <- commandArgs(trailingOnly = T)

print(args)
cat("usage: command results_file output_file prescription_number\n")

results_file <- args[1]
output_file <- args[2]
prescription_number <- as.integer(args[3])

country_region_list <- "./data/countries_regions.csv"

process_country_region <- function(regiondf, df) {
  
  country <- regiondf$CountryName[1]
  region <- regiondf$RegionName[1]
  
  cat("\n working on ", country, region, "\n")
  
  df <- df[(df$CountryName == country) & 
                  (df$RegionName == region),]

  df$SumCases <- sum(df$PredictedDailyNewCases)
  df$SumCost <- sum(df$Cost)
  df$AvgCases <- mean(df$PredictedDailyNewCases)
  df$AvgCost <- mean(df$Cost)
  
  
  return(as.data.frame(df[1,c("CountryName", "RegionName", "SumCases", "SumCost", "AvgCases", "AvgCost")]))
}

# ---------------------- main 

dfc <- read.csv(results_file, stringsAsFactors=FALSE) #, check.names = FALSE)
dfc$RegionName[is.na(dfc$RegionName)] <- ""
dfc$Date <- as.Date(dfc$Date)

regiondf <- read.csv(country_region_list, stringsAsFactors=FALSE)
regiondf$RegionName[is.na(regiondf$RegionName)] <- ""
regiondf <- regiondf[,c("CountryName", "RegionName")]
n <- nrow(regiondf)

# print(n)

df2 <- process_country_region(as.data.frame(regiondf[1,]), dfc)

if (n>1) {
  for (i in 2:n) {
    df2 <- bind_rows(df2, process_country_region(as.data.frame(regiondf[i,]), dfc))
  }
}

df2$PrescriptionIndex <- prescription_number

df2 <- df2[order(df2$CountryName,df2$RegionName),]  #,df2$PrescriptionIndex,df2$Date),]
write.csv(df2, output_file, row.names = F)
