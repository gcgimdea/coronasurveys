# Merges all files from each country in ./microdata/XXX/* into a large csv file.
# The dates to include are [start_date,end_date]

# library(tidyverse)
library(dplyr)
# library(nnet)
# library(zoo)
# library(countrycode)
library(data.table)
library(parallel)

args <- commandArgs(trailingOnly = T)
cat(args, "\n")

quarter <- args[1]

# start_date <- "2020-01-01"
# end_date <- Sys.Date()

if (quarter == "2020-Q2") {
  start_date <- "2020-04-23"
  end_date <- "2020-07-03"
}

if (quarter == "2020-Q3") {
  start_date <- "2020-06-10"
  end_date <- "2020-10-03"
}

if (quarter == "2020-Q4") {
  start_date <- "2020-09-10"
  end_date <- "2021-01-03"
}

if (quarter == "2021-Q1") {
  start_date <- "2020-12-10"
  end_date <- "2021-04-03"
}

if (quarter == "2021-Q2") {
  start_date <- "2021-03-10"
  end_date <- "2021-07-03"
}

if (quarter == "2021-Q3") {
  start_date <- "2021-06-10"
  end_date <- "2021-10-03"
}

if (quarter == "2021-Q4") {
  start_date <- "2021-09-10"
  end_date <- "2022-01-03"
}

if (quarter == "2022-Q1") {
  start_date <- "2021-12-10"
  end_date <- "2022-04-20"
}

if (quarter == "2022-Q2") {
  start_date <- "2022-03-10"
  end_date <- "2022-07-03"
}

if (quarter == "2022-Q1Q2") {
  start_date <- "2021-12-10"
  end_date <- "2022-07-03"
}

input_path <- paste0("./microdata/")
output_path <- paste0("./", quarter, "/total/")

dir.create(paste0("./", quarter), showWarnings = F)
dir.create(output_path, showWarnings = F)

#-------------------------------Creation data set whole country

process_country <- function(iso2, dates){
  cat(iso2, "\n")
  dfTotal <- data.frame()
  for (d in dates) {
    filename <- paste0(input_path, d, "/", iso2, ".rds")
    if (file.exists(filename)) {
      # cat(d, " ")
      df <- readRDS(file=filename)
      dfTotal <- dplyr::bind_rows(dfTotal,df)
    }
  }
  if (nrow(dfTotal) > 0 ) {
    cat("\n Total:", "dim:", dim(dfTotal), iso2, "\n")
    
    # filename <- paste0(output_path, iso2, ".csv") # ***
    # fwrite(dfTotal, file=filename) # ***
    
    filename <- paste0(output_path, iso2, ".rds")
    saveRDS(dfTotal, file=filename)
  } else {
    cat("*** Country with empty data:", dim(dfTotal), iso2, "\n")
  }
}

#------------------------------Main

cat("*** microdata2total\n")

dir.create(output_path, showWarnings = F)

dates <- as.character(seq(as.Date(start_date), as.Date(end_date), by="days"))

allDirs <- list.files(paste0(input_path), pattern="*", full.names=FALSE, recursive=FALSE)
allDirs <- intersect(allDirs, dates)
files <- c()
for (d in allDirs) {
  allFiles<-list.files(paste0(input_path, d, "/"), pattern="*", full.names=FALSE, recursive=FALSE)
  files <- unique(c(files, allFiles))
}
countries <- unique(substr(files,1,2))

if (quarter == "2022-Q2") {
  countries <- c("AR", "BE", "BY", "CL", "DZ", "FR", "GT", "ID", "JO", "KR", "MX", "NP", "PK", "RO", "SI", "UA",
    "AT", "BG", "CA", "CO", "EG", "GB", "HN", "IN", "JP", "LB", "MZ", "NZ", "PL", "RS", "SK", "UY",
    "AU", "BO", "CH", "CZ", "ES", "GH", "HR", "IQ", "KE", "LY", "NG", "PE", "PT", "RU", "TH", "VN",
    "BD", "BR", "CI", "DE", "ET", "GR", "HU", "IT", "KH", "MM", "NL", "PH", "PY", "SE", "TW", "ZA")
}

cat("Processing ", length(countries), "countries\n")
print(countries)

# kk <- lapply(countries, process_country, dates)
kk <- mclapply(countries, process_country, dates)
