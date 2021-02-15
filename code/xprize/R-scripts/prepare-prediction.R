# load library
library(tidyverse)
# library(readxl)
# library(httr)

args <- commandArgs(trailingOnly = T)

print(args)
cat("usage: command start_date change_date end_date path_to_ips_file path_to_prescriptions prescription_number output_file_path\n")

start_date <- as.Date(args[1])
change_date <- as.Date(args[2])
end_date <- as.Date(args[3])
path_to_ips_file <- args[4]
path_to_prescriptions <- args[5]
prescription_number <- as.integer(args[6])
output_file_path <- args[7]



df <- read.csv(path_to_ips_file, stringsAsFactors=FALSE) #, check.names = FALSE)
df$Date <- as.Date(df$Date)

df <- df[df$Date >= start_date,]
df <- df[df$Date < change_date,]

dfd <- read.csv(path_to_prescriptions, stringsAsFactors=FALSE) #, check.names = FALSE)
dfd$Date <- as.Date(dfd$Date)

dfd <- dfd[dfd$PrescriptionIndex == prescription_number,]
dfd <- dfd[dfd$Date >= change_date,]
dfd <- dfd[dfd$Date <= end_date,]
dfd <- dfd %>% 
  select(CountryName, RegionName, Date, C1_School.closing,	C2_Workplace.closing,
         C3_Cancel.public.events,	C4_Restrictions.on.gatherings, C5_Close.public.transport,
         C6_Stay.at.home.requirements,	C7_Restrictions.on.internal.movement,
         C8_International.travel.controls, H1_Public.information.campaigns,
         H2_Testing.policy,	H3_Contact.tracing, H6_Facial.Coverings)

df <- bind_rows(df, dfd)

colnames(df)<- c("Date", "CountryName", "RegionName", "C1_School closing",	"C2_Workplace closing",
                  "C3_Cancel public events",	"C4_Restrictions on gatherings", "C5_Close public transport",
                  "C6_Stay at home requirements",	"C7_Restrictions on internal movement",
                  "C8_International travel controls", "H1_Public information campaigns",
                  "H2_Testing policy",	"H3_Contact tracing", "H6_Facial Coverings")

df <- df[order(df$CountryName,df$RegionName,df$Date),]
write.csv(df, output_file_path, row.names = FALSE)
