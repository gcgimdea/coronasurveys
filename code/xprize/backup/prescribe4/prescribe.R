# load library
library(tidyverse)
# library(readxl)
# library(httr)

start_time <- Sys.time()

args <- commandArgs(trailingOnly = T)
script_path <- args[6]

iplan_file <- file.path(script_path, "hammer_full.csv")

# cat("Arguments:", args, "\n")
# 
# if (length(args) < 5) {
#   cat("usage: command start_date end_date ips_file cost_file output_file\n")
#   quit(save="no")
# }

start_date <- as.Date(args[1])
end_date <- as.Date(args[2])
path_to_ips_file <- args[3]
path_to_cost_file <- args[4]
output_file_path <- args[5]

df <- read.csv(iplan_file, check.names = FALSE, stringsAsFactors=FALSE)
df$Date <- as.Date(df$Date)
df <- df[df$Date >= start_date,]
df <- df[df$Date <= end_date,]

df <- df[order(df$CountryName,df$RegionName,df$Date),]
write.csv(df, output_file_path, row.names = FALSE)

end_time <- Sys.time()
cat("Execution time prescribe4: ", end_time - start_time, "\n")

