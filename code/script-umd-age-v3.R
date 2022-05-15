
library(zoo)
library(data.table)
library(tidyverse)

umd_path <- "../../SymptomSurveyData/data/UMD/"
# data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/age/"

quarter_list <- c("2020-Q2", "2020-Q3", "2020-Q4", "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4", "2022-Q1Q2") #"2022-Q1", "2022-Q2")

character_cols <- c("ISO2",	"ISO_3",	"country_agg",	"age")
date_cols <- c("date",	"first_date")
numeric_cols <- c("day_count", "days_aggregated", "count", "weight",
                  "infected", "not_infected", "pos_RF", "pos_RF_symp", "pos_XGB", "pos_XGB_symp",
                  "cli", "cli_weight", 
                  "cliWHO", "cliWHO_weight", 
                  "cli_local", "reach",
                  "positive_recent", "test_recent")

cols_to_use <- c(character_cols, date_cols, numeric_cols)

smooth_param <- 14
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

smooth_col <- function(x) {
  return(rollsum(x, smooth_param, fill=0, align = "right"))
}

process_ratio <- function(numerator, denominator){
  numerator <- smooth_col(numerator)
  denominator <- smooth_col(denominator)
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

compute_ratios <- function(dfdf) {
  dfdf$date <- as.Date(dfdf$date)
  dfdf <- dfdf[order(dfdf$date),]
  
  est <- process_ratio(dfdf$infected, (dfdf$infected + dfdf$not_infected))
  dfdf$p_infected <- est$val
  # dfdf$p_infected_error <- est$error
  dfdf$p_infected_low <- est$low
  dfdf$p_infected_high <- est$high
  
  est <- process_ratio(dfdf$pos_RF, dfdf$count)
  dfdf$p_rf <- est$val
  # dfdf$p_rf_error <- est$error
  dfdf$p_rf_low <- est$low
  dfdf$p_rf_high <- est$high
  
  est <- process_ratio(dfdf$pos_RF_symp, dfdf$count)
  dfdf$p_rf_symp <- est$val
  # dfdf$p_rf_symp_error <- est$error
  dfdf$p_rf_symp_low <- est$low
  dfdf$p_rf_symp_high <- est$high
  
  est <- process_ratio(dfdf$pos_XGB, dfdf$count)
  dfdf$p_xgb <- est$val
  # dfdf$p_xgb_error <- est$error
  dfdf$p_xgb_low <- est$low
  dfdf$p_xgb_high <- est$high
  
  est <- process_ratio(dfdf$pos_XGB_symp, dfdf$count)
  dfdf$p_xgb_symp <- est$val
  # dfdf$p_xgb_symp_error <- est$error
  dfdf$p_xgb_symp_low <- est$low
  dfdf$p_xgb_symp_high <- est$high
  
  est <- process_ratio(dfdf$cli, dfdf$count)
  dfdf$p_cli <- est$val
  # dfdf$p_cli_error <- est$error
  dfdf$p_cli_low <- est$low
  dfdf$p_cli_high <- est$high
  
  est <- process_ratio(dfdf$cli_weight, dfdf$weight)
  dfdf$p_cli_weight <- est$val
  # dfdf$p_cli_weight_error <- est$error
  dfdf$p_cli_weight_low <- est$low
  dfdf$p_cli_weight_high <- est$high
  
  est <- process_ratio(dfdf$cliWHO, dfdf$count)
  dfdf$p_cliWHO <- est$val
  # dfdf$p_cliWHO_error <- est$error
  dfdf$p_cliWHO_low <- est$low
  dfdf$p_cliWHO_high <- est$high
  
  est <- process_ratio(dfdf$cliWHO_weight, dfdf$weight)
  dfdf$p_cliWHO_weight <- est$val
  # dfdf$p_cliWHO_weight_error <- est$error
  dfdf$p_cliWHO_weight_low <- est$low
  dfdf$p_cliWHO_weight_high <- est$high
  
  est <- process_ratio(dfdf$cli_local, dfdf$reach)
  dfdf$p_cli_local <- est$val
  # dfdf$p_cli_local_error <- est$error
  dfdf$p_cli_local_low <- est$low
  dfdf$p_cli_local_high <- est$high
  
  est <- process_ratio(dfdf$positive_recent, dfdf$test_recent)
  dfdf$TPR <- est$val
  # dfdf$TPR_error <- est$error
  dfdf$TPR_low <- est$low
  dfdf$TPR_high <- est$high
  
  return(dfdf)
}



process_country <- function(iso2) {
  cat("\n Country:", iso2, "\n")
  
  # Read files
  file_short_csv <- paste0(iso2,".csv")
  df <- NULL
  for (quarter in quarter_list) {
    input_path <- paste0(umd_path, quarter, "/aggregates/age/")
    file_input_csv <- paste0(input_path, file_short_csv)
    if (file.exists(file_input_csv)){
      df_aux <- fread(file_input_csv, data.table = FALSE)
      # cat(file_input_csv, dim(df_aux), "\n")
      # Keep only columns of interest
      cols_filter <- intersect(cols_to_use, colnames(df_aux))
      df_aux <- df_aux %>%
        dplyr::select(all_of(cols_filter))
      # fread does not identify well the type of column
      for (c in intersect(character_cols, colnames(df_aux))) {
        df_aux[[c]] <- as.character(df_aux[[c]])
      }
      for (c in intersect(date_cols, colnames(df_aux))) {
        df_aux[[c]] <- as.Date(df_aux[[c]])
      }
      for (c in intersect(numeric_cols, colnames(df_aux))) {
        df_aux[[c]] <- as.numeric(df_aux[[c]])
      }
      # Merge quarter
      df <- dplyr::bind_rows(df, df_aux)
    } 
  }
  cat("Total:", dim(df), "\n")
  
  ages <- unique(df$age)
  ages <- ages[!is.na(ages)]
  
  dfTotal <- NULL
  for (r in ages) {
    dfr <- df[which(df$age == r),]
    cat(r, " ")
    
    dfr <- compute_ratios(dfr)
    
    dfTotal <- dplyr::bind_rows(dfTotal, dfr)
  }
  if (is.data.frame(dfTotal)) {
    dfTotal <- dfTotal %>% dplyr::rename(country=country_agg)
    fwrite(dfTotal, paste0(estimates_path, file_short_csv))
  }
}

# Main --------------------------------

country_list <- c()
for (quarter in quarter_list) {
  cl <- list.files(path=paste0(umd_path, quarter, "/aggregates/country/"), 
                   pattern="*.csv", 
                   full.names=FALSE, 
                   recursive=FALSE)
  
  country_list <- unique(c(country_list, cl))
}
country_list <- substr(country_list, 1, 2)

kk <- lapply(country_list, process_country)
# kk <- mclapply(country_list, process_country)

