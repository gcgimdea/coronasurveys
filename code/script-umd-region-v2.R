
# library(readr)
# library(lubridate)
library(zoo)
# library(tidyr)
library(data.table)
library(tidyverse)

responses_path2020 <- "/gauss_data/coronasurveys/UMD-2020/aggregates/region/"
responses_path2021 <- "/gauss_data/coronasurveys/UMD-2021/aggregates/region/"
# data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/region/"

files2020 <- list.files(path=responses_path2020, pattern="*.csv", full.names=FALSE, recursive=FALSE)
files2021 <- list.files(path=responses_path2021, pattern="*.csv", full.names=FALSE, recursive=FALSE)
files <- unique(c(files2020, files2021))

cols_to_use <- c("ISO2",	"ISO_3",	"country_agg",	"region_agg", "date", "first_date", "count", "day_count", "days_aggregated",
                 "p_cli",	"p_cli_CI",	"p_cli_weight",	"p_cli_weight_CI",	
                 "p_cliWHO",	"p_cliWHO_CI",	"p_cliWHO_weight",	"p_cliWHO_weight_CI",	
                 "p_cli_local",	"p_cli_local_CI", "TPR", "TPR_CI")

character_cols <- c("ISO2",	"ISO_3",	"country_agg",	"region_agg")
date_cols <- c("date", "first_date")
numeric_cols <- c("count", "day_count", "days_aggregated", "p_cli",	"p_cli_CI",	"p_cli_weight",	"p_cli_weight_CI",	
                  "p_cliWHO",	"p_cliWHO_CI",	"p_cliWHO_weight",	"p_cliWHO_weight_CI",	"p_cli_local",	"p_cli_local_CI", 
                  "TPR", "TPR_CI")

smooth_param <- 15


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
  cat("\n Country:", file, "\n")
  f2020 <- paste0(responses_path2020, file)
  f2021 <- paste0(responses_path2021, file)
  df <- data.frame()
  if (file.exists(f2020)) {
    df <- fread(f2020, data.table = F)
    df <- df %>% 
      dplyr::select(all_of(cols_to_use))
    for (c in character_cols) {
      df[[c]] <- as.character(df[[c]])
    }
    for (c in date_cols) {
      df[[c]] <- as.Date(df[[c]])
    }
    for (c in numeric_cols) {
      df[[c]] <- as.numeric(df[[c]])
    }
    # df$date <- as.Date(df$date)
    df <- df[which((df$date >= as.Date("2020-01-01")) & (df$date <= as.Date("2020-12-31"))),]
    # cat("2020:", dim(df), "\n")
  }
  if (file.exists(f2021)) {
    df2 <- fread(f2021, data.table = F)
    df2 <- df2 %>% 
      dplyr::select(all_of(cols_to_use))
    for (c in character_cols) {
      df2[[c]] <- as.character(df2[[c]])
    }
    for (c in date_cols) {
      df2[[c]] <- as.Date(df2[[c]])
    }
    for (c in numeric_cols) {
      df2[[c]] <- as.numeric(df2[[c]])
    }
    # df2$date <- as.Date(df2$date)
    df2 <- df2[which((df2$date >= as.Date("2021-01-01")) & (df2$date <= as.Date("2021-12-31"))),]
    df <- dplyr::bind_rows(df, df2)
    # cat("2021:", dim(df), "\n")
  }
  
  regions <- unique(df$region_agg)
  regions <- regions[!is.na(regions)]
  
  dfTotal <- NULL
  for (r in regions) {
    dfr <- df[which(df$region_agg == r),]
    cat(r, " ")
    dfr <- dfr[order(dfr$date),]
    # cat("Smoothing...")
    dfr$p_cli_smooth <- 
      with(dfr, ksmooth(date, p_cli, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cli_CI_smooth <- 
      with(dfr, ksmooth(date, p_cli_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cli_weight_smooth <- 
      with(dfr, ksmooth(date, p_cli_weight, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cli_weight_CI_smooth <- 
      with(dfr, ksmooth(date, p_cli_weight_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cliWHO_smooth <- 
      with(dfr, ksmooth(date, p_cliWHO, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cliWHO_CI_smooth <- 
      with(dfr, ksmooth(date, p_cliWHO_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cliWHO_weight_smooth <- 
      with(dfr, ksmooth(date, p_cliWHO_weight, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cliWHO_weight_CI_smooth <- 
      with(dfr, ksmooth(date, p_cliWHO_weight_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cli_local_smooth <- 
      with(dfr, ksmooth(date, p_cli_local, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    dfr$p_cli_local_CI_smooth <- 
      with(dfr, ksmooth(date, p_cli_local_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))$y
    
    dfr$p_cli_smooth_slope <- rollapply(dfr$p_cli_smooth,7,get_slope7,fill=NA,align="right")
    dfr$p_cli_weight_smooth_slope <- rollapply(dfr$p_cli_weight_smooth,7,get_slope7,fill=NA,align="right")
    dfr$p_cliWHO_smooth_slope <- rollapply(dfr$p_cliWHO_smooth,7,get_slope7,fill=NA,align="right")
    dfr$p_cliWHO_weight_smooth_slope <- rollapply(dfr$p_cliWHO_weight_smooth,7,get_slope7,fill=NA,align="right")
    dfr$p_cli_local_smooth_slope <- rollapply(dfr$p_cli_local_smooth,7,get_slope7,fill=NA,align="right")

    if (is.data.frame(dfTotal)) {
      dfTotal <- rbind(dfTotal, dfr)
    } else {
      dfTotal <- dfr
    }
    # cat(dim(dfTotal), "\n")
  }
  if (is.data.frame(dfTotal)) {
    fwrite(dfTotal, paste0(estimates_path, file))
  }
}

kk <- lapply(files, process_country)
# kk <- mclapply(files, process_country)

