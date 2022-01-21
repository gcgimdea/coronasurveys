
# library(readr)
# library(lubridate)
library(zoo)
# library(tidyr)
library(data.table)
library(tidyverse)

responses_path2020 <- "/gauss_data/coronasurveys/UMD-2020/aggregates/country/"
responses_path2021 <- "/gauss_data/coronasurveys/UMD-2021/aggregates/country/"
# data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/PlotData/"

files2020 <- list.files(path=responses_path2020, pattern="*.csv", full.names=FALSE, recursive=FALSE)
files2021 <- list.files(path=responses_path2021, pattern="*.csv", full.names=FALSE, recursive=FALSE)
files <- unique(c(files2020, files2021))

# cols_to_use <- c("ISO2",	"ISO_3",	"country_agg",	"date",	"first_date",	"count", 
#                  "day_count",	"days_aggregated",
#                  "p_cli",	"p_cli_CI",	"p_cli_weight",	"p_cli_weight_CI",	
#                  "p_cliWHO",	"p_cliWHO_CI",	"p_cliWHO_weight",	"p_cliWHO_weight_CI",	
#                  "p_cli_local",	"p_cli_local_CI", "test_recent", "positive_recent", "B0.1", "B0.2")

character_cols <- c("ISO2",	"ISO_3",	"country_agg")
date_cols <- c("date", "first_date")
numeric_cols <- c("count", "day_count", "days_aggregated", "p_cli",	"p_cli_CI",	"p_cli_weight",	"p_cli_weight_CI",	
                  "p_cliWHO",	"p_cliWHO_CI",	"p_cliWHO_weight",	"p_cliWHO_weight_CI",	"p_cli_local",	"p_cli_local_CI", 
                  "test_recent", "positive_recent", "B0.1", "B0.2")

cols_to_use <- c(character_cols, date_cols, numeric_cols)

smooth_param <- 30


get_slope7 <- function(x) {
  slp <- NA
  if(sum(is.na(x) | is.infinite(x)) == 0) {
    # dt <- data.table(index=seq(1,7), value=x)
    # slp <- lm(value ~ index, data=dt)$coefficients[2]
    slp <- (x[7]-x[1])/6
  }
  return(slp)
}

process_country <- function(file) {
  iso2 <- substr(file, 1, 2)
  cat(iso2, " ")
  f2020 <- paste0(responses_path2020, file)
  f2021 <- paste0(responses_path2021, file)
  
  if (file.exists(f2020)) {
    DT2020 <- fread(f2020)
    DT2020 <- DT2020[,date:=as.Date(date)]
    DT2020 <- DT2020[,first_date:=as.Date(first_date)]
    cols_filter <- intersect(cols_to_use, colnames(DT2020))
    DT2020 <- DT2020[date >= "2020-01-01" & date <= "2020-12-31", ..cols_filter]
  } else {
    DT2020 <- data.table() 
  }
  if (file.exists(f2021)) {
    DT2021 <- fread(f2021)
    DT2021 <- DT2021[,date:=as.Date(date)]
    DT2021 <- DT2021[,first_date:=as.Date(first_date)]
    cols_filter <- intersect(cols_to_use, colnames(DT2021))
    DT2021 <- DT2021[date >= "2021-01-01" & date <= "2021-12-31", ..cols_filter]
  } else {
    DT2021 <- data.table() 
  }
  DT <- rbindlist(list(DT2020, DT2021), use.names=TRUE, fill=TRUE)
  
  DT <- DT[order(date)]
  
  DT <- DT[, p_cli_smooth := 
             with(DT, ksmooth(date, p_cli, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cli_CI_smooth :=
             with(DT, ksmooth(date, p_cli_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cli_weight_smooth :=
             with(DT, ksmooth(date, p_cli_weight, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cli_weight_CI_smooth :=
             with(DT, ksmooth(date, p_cli_weight_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cliWHO_smooth :=
             with(DT, ksmooth(date, p_cliWHO, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cliWHO_CI_smooth :=
             with(DT, ksmooth(date, p_cliWHO_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cliWHO_weight_smooth :=
             with(DT, ksmooth(date, p_cliWHO_weight, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cliWHO_weight_CI_smooth :=
             with(DT, ksmooth(date, p_cliWHO_weight_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cli_local_smooth :=
             with(DT, ksmooth(date, p_cli_local, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  DT <- DT[, p_cli_local_CI_smooth :=
             with(DT, ksmooth(date, p_cli_local_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  
  # First derivative
  # DT <- DT[, p_cli_smooth_slope := rollapply(DT[,p_cli_smooth],7,get_slope7,fill=NA,align="right")]
  # DT <- DT[, p_cli_weight_smooth_slope := rollapply(DT[,p_cli_weight_smooth],7,get_slope7,fill=NA,align="right")]
  # DT <- DT[, p_cliWHO_smooth_slope := rollapply(DT[,p_cliWHO_smooth],7,get_slope7,fill=NA,align="right")]
  # DT <- DT[, p_cliWHO_weight_smooth_slope := rollapply(DT[,p_cliWHO_weight_smooth],7,get_slope7,fill=NA,align="right")]
  # DT <- DT[, p_cli_local_smooth_slope := rollapply(DT[,p_cli_local_smooth],7,get_slope7,fill=NA,align="right")]
  
  # Second derivative
  # DT <- DT[, p_cli_smooth_slope2 := rollapply(DT[,p_cli_smooth_slope],7,get_slope7,fill=NA,align="right")]
  # DT <- DT[, p_cliWHO_smooth_slope2 := rollapply(DT[,p_cliWHO_smooth_slope],7,get_slope7,fill=NA,align="right")]
  
  if ("B0.1" %in% colnames(DT)) {
    DT <- DT[, p_cases_infected := B0.1/(B0.1 + B0.2)]
    DT <- DT[, p_cases_infected_smooth := 
               with(DT, ksmooth(date, p_cases_infected, kernel = "normal", bandwidth = smooth_param, x.points=date))$y]
  }
  DT <- DT[, p_cases_active := p_cli_smooth]
  
  filename <- paste0(estimates_path, iso2, "-estimate.csv")
  fwrite(DT, filename)
}
 
kk <- lapply(files, process_country)
# kk <- mclapply(files, process_country)
