library(useful)
library(dplyr)
library(zoo)
library(data.table)

# umd_path <- "/gauss_data/coronasurveys/github/SymptomSurveyData/Omicron/"
umd_path <- "../../SymptomSurveyData/data/UMD/"
country_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/PlotData/"

quarter_list <- c("2020-Q2", "2020-Q3", "2020-Q4", "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4", "2022-Q1")

character_cols <- c("ISO2",	"ISO_3",	"country_agg")
date_cols <- c("date", "first_date")
numeric_cols <- c("count", "day_count", "days_aggregated", 
                  "p_infected", "p_infected_CI", "p_rf", "p_rf_CI",
                  "p_cli",	"p_cli_CI",	"p_cli_weight",	"p_cli_weight_CI", 
                  "p_cliWHO",	"p_cliWHO_CI",	"p_cliWHO_weight",	"p_cliWHO_weight_CI",	
                  "p_cli_local",	"p_cli_local_CI", "test_recent", "positive_recent")

cols_to_use <- c(character_cols, date_cols, numeric_cols)  

smooth_param <- 14

smooth_col <- function(x) {
  return(rollsum(x, smooth_param, fill=NA, align = "right"))
}

process_country <- function(iso2) {
  cat("\n Country:", iso2, "\n")
  
  # Read files
  file_short_csv <- paste0(iso2,".csv")
  df <- NULL
  for (quarter in quarter_list) {
    input_path <- paste0(umd_path, quarter, "/aggregates/country/")
    file_input_csv <- paste0(input_path, file_short_csv)
    if (file.exists(file_input_csv)){
      df_aux <- fread(file_input_csv, data.table = FALSE)
      cols_filter <- intersect(cols_to_use, colnames(df_aux))
      df_aux <- df_aux %>%
        dplyr::select(all_of(cols_filter))
      for (c in intersect(character_cols, colnames(df_aux))) {
        df_aux[[c]] <- as.character(df_aux[[c]])
      }
      for (c in intersect(date_cols, colnames(df_aux))) {
        df_aux[[c]] <- as.Date(df_aux[[c]])
      }
      for (c in intersect(numeric_cols, colnames(df_aux))) {
        df_aux[[c]] <- as.numeric(df_aux[[c]])
      }
      df <- dplyr::bind_rows(df, df_aux)
    } 
  }
  
  # cat("Smoothing...")
  df$p_infected_smooth <- smooth_col(df$p_infected)
  df$p_infected_CI_smooth <- smooth_col(df$p_infected_CI)
  
  df$p_rf_smooth <- smooth_col(df$p_rf)
  df$p_rf_CI_smooth <- smooth_col(df$p_rf_CI)
  
  df$p_cli_smooth <- smooth_col(df$p_cli)
  df$p_cli_CI_smooth <- smooth_col(df$p_cli_CI)
  df$p_cli_weight_smooth <- smooth_col(df$p_cli_weight)
  df$p_cli_weight_CI_smooth <- smooth_col(df$p_cli_weight_CI)
  
  df$p_cliWHO_smooth <- smooth_col(df$p_cliWHO)
  df$p_cliWHO_CI_smooth <- smooth_col(df$p_cliWHO_CI)
  df$p_cliWHO_weight_smooth <- smooth_col(df$p_cliWHO_weight)
  df$p_cliWHO_weight_CI_smooth <- smooth_col(df$p_cliWHO_weight_CI)
  
  df$p_cli_local_smooth <- smooth_col(df$p_cli_local)
  df$p_cli_local_CI_smooth <- smooth_col(df$p_cli_local_CI)

  filename <- paste0(estimates_path, iso2, "-estimate.csv")
  fwrite(df, filename)
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
