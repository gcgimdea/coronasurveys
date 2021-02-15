# load library
# library(geometry)
library(tidyverse)
# library(readxl)
# library(httr)

start_time <- Sys.time()

args <- commandArgs(trailingOnly = T)
script_path <- args[6]

# cat("Arguments:", args, "\n")
# 
# if (length(args) < 5) {
#   cat("usage: command start_date end_date ips_file cost_file output_file\n")
#   quit(save="no")
# }

start_date <- as.Date(args[1])
end_date <- as.Date(args[2])
ips_file <- args[3]
cost_file <- args[4]
output_file <- args[5]

country_region_list <- file.path(script_path, "countries_regions.csv")
ratios_file <- file.path(script_path, "ratios.csv")
dance_file <- file.path(script_path, "dance_full.csv")
hammer_length <- 45 # days

process_country_region <- function(regiondf, ratios, dfdance, costs) {
  
  country <- regiondf$CountryName[1]
  region <- regiondf$RegionName[1]
  
  # cat("\n working on ", country, region, "\n")
  
  dfr <- ratios[(ratios$CountryName == country) & 
                  (as.character(ratios$RegionName) == as.character(region)),]
  dfd <- dfdance[(dfdance$CountryName == country) & 
                   (as.character(dfdance$RegionName) == as.character(region)),]
  dfcost <- costs[(costs$CountryName == country) & 
                    (as.character(costs$RegionName) == as.character(region)),]
  
  # Computes the cost of the vectors
  for (i in 1:nrow(dfr)) {
    dfr[i, "Cost"] <- as.matrix(dfr[i, 4:15]) %*% t(as.matrix(dfcost[1, 3:14]))
  }
  
  # cat("\n Cost computation \n")
  

  # Select the vector with lowest ratio, breaking ties by cost
  dfr <- dfr[(dfr$avg_ratio == min(dfr$avg_ratio)),]
  dfr <- dfr[(dfr$Cost == min(dfr$Cost)),]
  
  change_date <- min(start_date + hammer_length - 1, end_date)
  
  dfd$`C1_School closing`[(dfd$Date <= change_date)] <- dfr$`C1_School closing`[1]
  dfd$`C2_Workplace closing`[(dfd$Date <= change_date)] <- dfr$`C2_Workplace closing`[1]
  dfd$`C3_Cancel public events`[(dfd$Date <= change_date)] <- dfr$`C3_Cancel public events`[1]
  dfd$`C4_Restrictions on gatherings`[(dfd$Date <= change_date)] <- dfr$`C4_Restrictions on gatherings`[1]
  dfd$`C5_Close public transport`[(dfd$Date <= change_date)] <- dfr$`C5_Close public transport`[1]
  dfd$`C6_Stay at home requirements`[(dfd$Date <= change_date)] <- dfr$`C6_Stay at home requirements`[1]
  dfd$`C7_Restrictions on internal movement`[(dfd$Date <= change_date)] <- dfr$`C7_Restrictions on internal movement`[1]
  dfd$`C8_International travel controls`[(dfd$Date <= change_date)] <- dfr$`C8_International travel controls`[1]
  dfd$`H1_Public information campaigns`[(dfd$Date <= change_date)] <- dfr$`H1_Public information campaigns`[1]
  dfd$`H2_Testing policy`[(dfd$Date <= change_date)] <- dfr$`H2_Testing policy`[1]
  dfd$`H3_Contact tracing`[(dfd$Date <= change_date)] <- dfr$`H3_Contact tracing`[1]
  dfd$`H6_Facial Coverings`[(dfd$Date <= change_date)] <- dfr$`H6_Facial Coverings`[1]
  
  return(dfd)
}


costs <- read.csv(cost_file, check.names = FALSE, stringsAsFactors=FALSE)
costs$RegionName[is.na(costs$RegionName)] <- ""

dfd <- read.csv(dance_file, check.names = FALSE, stringsAsFactors=FALSE)
dfd$Date <- as.Date(dfd$Date)
dfd$RegionName[is.na(dfd$RegionName)] <- ""

dfdance <- dfd[(dfd$Date >= start_date) & (dfd$Date <= end_date),]

ratios <- read.csv(ratios_file, check.names = FALSE, stringsAsFactors=FALSE)
ratios$RegionName[is.na(ratios$RegionName)] <- ""

# regiondf <- read.csv(country_region_list, stringsAsFactors=FALSE)
regiondf <- read.csv(ips_file, check.names = FALSE, stringsAsFactors=FALSE)
regiondf$RegionName[is.na(regiondf$RegionName)] <- ""
# regiondf <- costs
regiondf <- regiondf[,c("CountryName", "RegionName")]
regiondf <- unique(regiondf)
n <- nrow(regiondf)

# print(n)

df2 <- process_country_region(as.data.frame(regiondf[1,]), ratios, dfdance, costs)

if (n>1) {
  for (i in 2:n) {
    df2 <- bind_rows(df2, process_country_region(as.data.frame(regiondf[i,]), ratios, dfdance, costs))
  }
}

# colnames(df2)<- c("Date", "CountryName", "RegionName", "C1_School closing",	"C2_Workplace closing",
#                   "C3_Cancel public events",	"C4_Restrictions on gatherings", "C5_Close public transport",
#                   "C6_Stay at home requirements",	"C7_Restrictions on internal movement",
#                   "C8_International travel controls", "H1_Public information campaigns",
#                   "H2_Testing policy",	"H3_Contact tracing", "H6_Facial Coverings")

df2 <- df2[order(df2$CountryName,df2$RegionName,df2$Date),]
write.csv(df2, output_file, row.names = F)

end_time <- Sys.time()
cat("Execution time prescribe5: ", end_time - start_time, "\n")

