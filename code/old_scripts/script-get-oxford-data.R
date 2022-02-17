# load library
library(dplyr)
# library(ggplot2)
# library(httr)
# library(jsonlite)
library(stringr)
library(readr)

DATA_URL = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
country_file <- "../data/common-data/country_oxford.csv"
# region_file <- "../data/common-data/region_oxford.csv"
data_file <- "../data/common-data/oxford-umd-country-population.csv"
output_path = "../data/oxford/"

data_ox <- read_csv(DATA_URL) #, as.is = T)
cat("::- script-confirmed: Oxford data available! ::\n")
jurisdictions <- unique(data_ox$Jurisdiction)
if (length(jurisdictions) != 2) {
  cat("Something wrong with jurisdictions", jurisdictions, "\n")
}

data_ox <- data_ox %>% mutate(Date = paste0( str_sub(Date, 1, 4), "-",
                                   str_sub(Date, 5, 6), "-",
                                   str_sub(Date, 7, 8))) %>% mutate(Date = as.Date(Date))

# write.csv(data_ox, paste0(output_path, "whole-data-latest.csv"),
#           row.names = FALSE)


df_country <- data_ox[data_ox$Jurisdiction == "NAT_TOTAL",]
# df_region <- data_ox[data_ox$Jurisdiction == "STATE_TOTAL",]

# Generates files with the list of countries and regions
# df_country <- df_country %>%
#   select(CountryName, CountryCode)  %>%
#   distinct()
write.csv(df_country %>%
            select(CountryName, CountryCode)  %>%
            distinct(),
          file = "../data/common-data/country_oxford.csv",
          row.names = FALSE)

# # df_region <- df_region %>%
# #   select(RegionName, RegionCode)  %>%
# #   distinct()
# write.csv(df_region %>%
#             select(RegionName, RegionCode)  %>%
#             distinct(),
#           file = "../data/common-data/region_oxford.csv",
#           row.names = FALSE)

c_data <- read.csv(data_file, as.is = T)

dir.create(paste0(output_path, "PlotData"), showWarnings = F)
dir.create(paste0(output_path, "region"), showWarnings = F)

country_list <- read.csv(country_file, as.is = T)
all_countries <- country_list$CountryName

for (country in all_countries) {
  cat("Processing", country, "\n")
  df <- df_country[df_country$CountryName == country ,]
  df$cases <- c(0,diff(df$ConfirmedCases))
  df$deaths <- c(0,diff(df$ConfirmedDeaths))
  geoid <- c_data[c_data$CountryName == country,"geo_id"]
  df$population <- c_data[c_data$CountryName == country,"population"]
  df$iso2 <- geoid
  df <- df %>% select(CountryName, CountryCode, Date, ConfirmedCases, ConfirmedDeaths, 
                cases, deaths, population, iso2)
  write.csv(df, paste0(output_path, "PlotData/", geoid, "-estimate.csv"),
            row.names = FALSE)
}

# region_list <- read.csv(region_file, as.is = T)
# all_regions <- region_list$RegionName
# for (region in all_regions) {
#   cat("Processing", region, "\n")
#   df <- df_region[df_region$RegionName == region,]
#   df$cases <- c(NA,diff(df$ConfirmedCases))
#   df$deaths <- c(NA,diff(df$ConfirmedDeaths))
#   region_code <- df$RegionCode[1]
#   df$population <- region_list[region_list$RegionName == region,"Population"]
#   df$iso2 <- region_code
#   df <- df %>% select(CountryName, CountryCode, RegionName, RegionCode, Date, 
#                 ConfirmedCases, ConfirmedDeaths, cases, deaths, iso2)
#   write.csv(df, paste0(output_path, "region/", region_code, "-estimate.csv"),
#             row.names = FALSE)
# }


