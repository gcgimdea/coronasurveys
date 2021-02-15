library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(zoo)

DATA_URL = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
country_region_list <- "https://raw.githubusercontent.com/GCGImdea/xprize/main/data/common-data/countries_regions.csv"
# ips_file <- "https://raw.githubusercontent.com/GCGImdea/xprize/main/work/IPS-latest.csv"
output_file <- "./data/IPS-latest-full.csv"

# start_date <- ymd("2020-01-01")
# end_date <- ymd("2021-05-31")
args <- commandArgs(trailingOnly = T)
start_date <- as.Date(args[1])
end_date <- as.Date(args[2])

process_country_region <- function(regiondf, df) {
  
  country <- regiondf$CountryName[1]
  region <- regiondf$RegionName[1]
  
  # cat("Working on ", country, region, "\n")
  
  dfd <- df[(df$CountryName == country) & (df$RegionName == region),]
  
  dfd$C1_School.closing <- na.locf(dfd$C1_School.closing, na.rm = FALSE)
  dfd$C1_School.closing[is.na(dfd$C1_School.closing)] <- 0
  dfd$C2_Workplace.closing <- na.locf(dfd$C2_Workplace.closing, na.rm = FALSE)
  dfd$C2_Workplace.closing[is.na(dfd$C2_Workplace.closing)] <- 0
  dfd$C3_Cancel.public.events <- na.locf(dfd$C3_Cancel.public.events, na.rm = FALSE)
  dfd$C3_Cancel.public.events[is.na(dfd$C3_Cancel.public.events)] <- 0
  dfd$C4_Restrictions.on.gatherings <- na.locf(dfd$C4_Restrictions.on.gatherings, na.rm = FALSE)
  dfd$C4_Restrictions.on.gatherings[is.na(dfd$C4_Restrictions.on.gatherings)] <- 0
  dfd$C5_Close.public.transport <- na.locf(dfd$C5_Close.public.transport, na.rm = FALSE)
  dfd$C5_Close.public.transport[is.na(dfd$C5_Close.public.transport)] <- 0
  dfd$C6_Stay.at.home.requirements <- na.locf(dfd$C6_Stay.at.home.requirements, na.rm = FALSE)
  dfd$C6_Stay.at.home.requirements[is.na(dfd$C6_Stay.at.home.requirements)] <- 0
  dfd$C7_Restrictions.on.internal.movement <- na.locf(dfd$C7_Restrictions.on.internal.movement, na.rm = FALSE)
  dfd$C7_Restrictions.on.internal.movement[is.na(dfd$C7_Restrictions.on.internal.movement)] <- 0
  dfd$C8_International.travel.controls <- na.locf(dfd$C1_School.closing, na.rm = FALSE)
  dfd$C8_International.travel.controls[is.na(dfd$C8_International.travel.controls)] <- 0
  dfd$H1_Public.information.campaigns <- na.locf(dfd$H1_Public.information.campaigns, na.rm = FALSE)
  dfd$H1_Public.information.campaigns[is.na(dfd$H1_Public.information.campaigns)] <- 0
  dfd$H2_Testing.policy <- na.locf(dfd$H2_Testing.policy, na.rm = FALSE)
  dfd$H2_Testing.policy[is.na(dfd$H2_Testing.policy)] <- 0
  dfd$H3_Contact.tracing <- na.locf(dfd$H3_Contact.tracing, na.rm = FALSE)
  dfd$H3_Contact.tracing[is.na(dfd$H3_Contact.tracing)] <- 0
  dfd$H6_Facial.Coverings <- na.locf(dfd$H6_Facial.Coverings, na.rm = FALSE)
  dfd$H6_Facial.Coverings[is.na(dfd$H6_Facial.Coverings)] <- 0
  
  dfd <- dfd %>% 
    complete(Date = seq.Date(start_date, end_date, by="day")) %>% 
    fill(CountryName, RegionName, 
         C1_School.closing,
         C2_Workplace.closing,
         C3_Cancel.public.events,
         C4_Restrictions.on.gatherings,
         C5_Close.public.transport,
         C6_Stay.at.home.requirements,
         C7_Restrictions.on.internal.movement,
         C8_International.travel.controls,
         H1_Public.information.campaigns,
         H2_Testing.policy,
         H3_Contact.tracing,
         H6_Facial.Coverings)
  
  return(dfd)
}

# ------------ main -----------------

cat("*** Creating IPS file...\n")

data_ox <- read.csv(DATA_URL)
# cat("::- script-confirmed: Oxford data available! ::\n")
jurisdictions <- unique(data_ox$Jurisdiction)
if (length(jurisdictions) != 2) {
  cat("Something wrong with jurisdictions", jurisdictions, "\n")
}

data_ox <- data_ox %>% mutate(Date = paste0( str_sub(Date, 1, 4), "-",
                                             str_sub(Date, 5, 6), "-",
                                             str_sub(Date, 7, 8))) %>% mutate(Date = as.Date(Date))

data_ox <- data_ox %>% 
  select(CountryName, RegionName, Date, C1_School.closing,	C2_Workplace.closing,
         C3_Cancel.public.events,	C4_Restrictions.on.gatherings, C5_Close.public.transport,
         C6_Stay.at.home.requirements,	C7_Restrictions.on.internal.movement,
         C8_International.travel.controls, H1_Public.information.campaigns,
         H2_Testing.policy,	H3_Contact.tracing, H6_Facial.Coverings)

# colnames(data_ox)<- c("CountryName", "RegionName", "Date", "C1_School closing",	"C2_Workplace closing",
#                       "C3_Cancel public events",	"C4_Restrictions on gatherings", "C5_Close public transport",
#                       "C6_Stay at home requirements",	"C7_Restrictions on internal movement",
#                       "C8_International travel controls", "H1_Public information campaigns",
#                       "H2_Testing policy",	"H3_Contact tracing", "H6_Facial Coverings")

df <- data_ox # Instead of renaming I just make a copy

# df <- read.csv(ips_file) #, check.names = FALSE)
df$Date <- as.Date(df$Date)
df$RegionName[is.na(df$RegionName)] <- ""

df <- df[df$Date >= start_date,]

regiondf <- read.csv(country_region_list)
n <- nrow(regiondf)

df2 <- process_country_region(as.data.frame(regiondf[1,]), df)

if (n>1) {
  for (i in 2:n) {
    df2 <- bind_rows(df2, process_country_region(as.data.frame(regiondf[i,]), df))
  }
}

colnames(df2)<- c("Date", "CountryName", "RegionName", "C1_School closing",	"C2_Workplace closing",
                      "C3_Cancel public events",	"C4_Restrictions on gatherings", "C5_Close public transport",
                      "C6_Stay at home requirements",	"C7_Restrictions on internal movement",
                      "C8_International travel controls", "H1_Public information campaigns",
                      "H2_Testing policy",	"H3_Contact tracing", "H6_Facial Coverings")

df2 <- df2[order(df2$CountryName,df2$RegionName,df2$Date),]
write.csv(df2, output_file, row.names = F)
