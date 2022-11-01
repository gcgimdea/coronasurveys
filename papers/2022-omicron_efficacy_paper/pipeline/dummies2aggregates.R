library(tidyverse)
# library(dplyr)
# library(nnet)
library(zoo)
# library(stringr)
library(parallel)
library(data.table)
library(randomForest)
library(caret)

args <- commandArgs(trailingOnly = T)
cat(args, "\n")

quarter <- args[1]

input_path <- paste0("./", quarter, "/dummies/")
quarter_path <- "../github/SymptomSurveyData/data/UMD/"
output_path <- paste0(quarter_path, quarter, "/aggregates/")
models_rf_path <- paste0("./", quarter, "/models_rf/")
models_rf_symp_path <- paste0("./", quarter, "/models_rf_symp/")
models_XGB_path <- paste0("./", quarter, "/models_XGB/")
models_XGB_symp_path <- paste0("./", quarter, "/models_XGB_symp/")

dir.create(paste0(quarter_path, quarter), showWarnings = F)
dir.create(output_path, showWarnings = F)

if (quarter == "2020-Q2") {
  start_date <- "2020-04-23"
  end_date <- "2020-06-30"
}

if (quarter == "2020-Q3") {
  start_date <- "2020-07-01"
  end_date <- "2020-09-30"
}

if (quarter == "2020-Q4") {
  start_date <- "2020-10-01"
  end_date <- "2020-12-31"
}

if (quarter == "2021-Q1") {
  start_date <- "2021-01-01"
  end_date <- "2021-03-31"
}

if (quarter == "2021-Q2") {
  start_date <- "2021-04-01"
  end_date <- "2021-06-30"
}

if (quarter == "2021-Q3") {
  start_date <- "2021-07-01"
  end_date <- "2021-09-30"
}

if (quarter == "2021-Q4") {
  start_date <- "2021-10-01"
  end_date <- "2021-12-31"
}

if (quarter == "2022-Q1") {
  start_date <- "2022-01-01"
  end_date <- "2022-04-13"
}

if (quarter == "2022-Q2") {
  start_date <- "2022-04-14"
  end_date <- "2022-06-30"
}

if (quarter == "2022-Q1Q2") {
  start_date <- "2022-01-01"
  end_date <- "2022-06-30"
}

# start_date <- as.Date("2021-01-01")
# start_date <- "2020-04-23"
# end_date <- Sys.Date()
# end_date <- "2020-12-31"


min_responses <- 100
# age_period <- 1 # Age values are aggregated at least by this number of days
 
cols_no_aggregate <- c("RecordedDate", "date_from_file")

cols_to_average <- c("B2", "B4", "E5", "E6", "E7")

# --------------------------

smooth_param <- 30

# reach <- 71 #53 # 53 is the global average reach, 71 is the Indian average reach
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

process_umd_file <- function(dfdf, iso2) {

  dfdf$date <- as.Date(dfdf$date)
  # dfdf <- dfdf[order(dfdf$date),]
  
  est <- process_ratio(dfdf$infected, (dfdf$infected + dfdf$not_infected))
  dfdf$p_infected <- est$val
  dfdf$p_infected_CI <- est$error
  
  est <- process_ratio(dfdf$pos_RF, dfdf$count)
  dfdf$p_rf <- est$val
  dfdf$p_rf_CI <- est$error
  
  est <- process_ratio(dfdf$pos_RF_symp, dfdf$count)
  dfdf$p_rf_symp <- est$val
  dfdf$p_rf_symp_CI <- est$error
  
  est <- process_ratio(dfdf$pos_XGB, dfdf$count)
  dfdf$p_XGB <- est$val
  dfdf$p_XGB_CI <- est$error

  est <- process_ratio(dfdf$pos_XGB_symp, dfdf$count)
  dfdf$p_XGB_symp <- est$val
  dfdf$p_XGB_symp_CI <- est$error
  
  est <- process_ratio(dfdf$cli, dfdf$count)
  dfdf$p_cli <- est$val
  dfdf$p_cli_CI <- est$error
  # dfdf$p_cli_low <- est$low
  # dfdf$p_cli_high <- est$high
  
  est <- process_ratio(dfdf$cli_weight, dfdf$weight)
  dfdf$p_cli_weight <- est$val
  dfdf$p_cli_weight_CI <- est$error
  # dfdf$p_cli_weight_low <- est$low
  # dfdf$p_cli_weight_high <- est$high
  
  est <- process_ratio(dfdf$cliWHO, dfdf$count)
  dfdf$p_cliWHO <- est$val
  dfdf$p_cliWHO_CI <- est$error
  # dfdf$p_cliWHO_low <- est$low
  # dfdf$p_cliWHO_high <- est$high

  est <- process_ratio(dfdf$cliWHO_weight, dfdf$weight)
  dfdf$p_cliWHO_weight <- est$val
  dfdf$p_cliWHO_weight_CI <- est$error
  # dfdf$p_cliWHO_weight_low <- est$low
  # dfdf$p_cliWHO_weight_high <- est$high

  # reach <- (sum(dfdf$cli_local) * sum(dfdf$count)) / (sum(dfdf$cli)*sum(dfdf$count_local))
  # cat("\n", iso2, " Reach:", reach, "\n")
  
  est <- process_ratio(dfdf$cli_local, dfdf$reach)
  dfdf$p_cli_local <- est$val
  dfdf$p_cli_local_CI <- est$error
  # dfdf$p_cli_local_low <- est$low
  # dfdf$p_cli_local_high <- est$high

  # est <- process_ratio(dfdf$positive_recent, dfdf$test_recent)
  # dfdf$TPR <- est$val
  # dfdf$TPR_CI <- est$error
  # dfdf$TPR_low <- est$low
  # dfdf$TPR_high <- est$high
  
  return(dfdf)
}

ComputePredicates <- function(df, iso2) {
  
  ml_data = df %>% select(-c(weight, Finished, B2, B4, E5, E6, E7,
                             survey_version, RecordedDate, ISO_3, country_agg, region_agg, 
                             date_from_file, date, ISO2, age), 
                          -contains(".NA"), -contains("B7"), -contains("B8")
                          # , -contains("B0"), -contains("B15"), -contains("V1"), -contains("V2"), -contains("V3"),
                          # -contains("V4"), -contains("V5"), -contains("V6"), -contains("V9")
  )

  # Positive as classified by the Random Forest
  file_short_rds <- paste0(iso2,".rds")
  file_model <- paste0(models_rf_path, file_short_rds)
  
  if (file.exists(file_model)){
    cat("Running RF model for ", iso2, "\n")
    classifier_RF <- readRDS(file=file_model)
    df$pos_RF <- predict(classifier_RF, ml_data)
    df$pos_RF <- as.numeric(levels(df$pos_RF))[df$pos_RF]
  }
  else {
    cat("No RF model for ", iso2, "\n")
    df$pos_RF <- 0
  }
  # Correct the values that are tested in latest 14 days and positive or negative
  # df$pos_RF[which((df$B7.1 == 1) & (df$B8.2 == 1))] <- 0
  # df$pos_RF[which((df$B7.1 == 1) & (df$B8.1 == 1))] <- 1
    
  # df$pos_RF_weight <- 0
  # df$pos_RF_weight[which(df$pos_RF == 1)] <- df$weight[which(df$pos_RF == 1)]
  
  # Positive as classified by the Random Forest with symptoms
  file_short_rds <- paste0(iso2,".rds")
  file_model <- paste0(models_rf_symp_path, file_short_rds)
  if (file.exists(file_model)){
    cat("Running RF_symp model for ", iso2, "\n")
    classifier_RF <- readRDS(file=file_model)
    df$pos_RF_symp <- predict(classifier_RF, ml_data)
    df$pos_RF_symp <- as.numeric(levels(df$pos_RF_symp))[df$pos_RF_symp]
  }
  else {
    cat("No RF_symp model for ", iso2, "\n")
    df$pos_RF_symp <- 0
  }
  
  
  # Positive as classified by the XGBoost
  file_short_rds <- paste0(iso2,".rds")
  file_model <- paste0(models_XGB_path, file_short_rds)
  if (file.exists(file_model)){
    cat("Running XGBoost model for ", iso2, "\n")
    classifier_XGB <- readRDS(file=file_model)
    pred = predict(classifier_XGB, as.matrix(ml_data))
    df$pos_XGB = round(pred)
    #df$pos_XGB <- as.numeric(levels(df$pos_XGB))[df$pos_XGB]
  }
  else {
    cat("No XGBoost model for ", iso2, "\n")
    df$pos_XGB <- 0
  }
  
  # Positive as classified by the XGBoost with symptoms
  file_short_rds <- paste0(iso2,".rds")
  file_model <- paste0(models_XGB_symp_path, file_short_rds)
  if (file.exists(file_model)){
    cat("Running XGBoost model with symptoms for ", iso2, "\n")
    classifier_XGB <- readRDS(file=file_model)
    pred = predict(classifier_XGB, as.matrix(ml_data))
    df$pos_XGB_symp = round(pred)
    # df$pos_XGB_symp = round(predict(classifier_XGB, as.matrix(ml_data)))
    # df$pos_XGB_symp <- as.numeric(levels(df$pos_XGB_symp))[df$pos_XGB_symp]
  }
  else {
    cat("No XGBoost model for ", iso2, "\n")
    df$pos_XGB_symp <- 0
  }
  
  # To count the number of responses
  df$count <- 1
  
  # To count the number of symptomatic responses
  df$symptomatic <- 0
  with_symptoms <- which(df$B1_1.1==1 | df$B1_2.1==1 | df$B1_3.1==1 | df$B1_4.1==1 | 
                      df$B1_5.1==1 | df$B1_6.1==1 | df$B1_7.1==1 | df$B1_8.1==1 | 
                      df$B1_9.1==1 | df$B1_10.1==1 
                      # | df$B1_11.1==1
                      | df$B1_12.1==1 
                      | df$B1_13.1==1 
                      # | df$B1_14.1==1
                      )
  df$symptomatic[with_symptoms] <- 1
  
  # To count infected
  df$infected <- 0
  df$infected[which(df$B0.1 == 1)] <- 1
  df$not_infected <- 0
  df$not_infected[which(df$B0.2 == 1)] <- 1
  
  # To count vaccinated
  # "V1 Have you had a COVID-19 vaccination?"
  df$vaccinated <- 0
  df$vaccinated[which(df$V1.1 == 1)] <- 1
  # df$vaccinated_weight <- 0
  # df$vaccinated_weight[which(df$vaccinated == 1)] <- df$weight[which(df$vaccinated == 1)]
  df$unvaccinated <- 0
  df$unvaccinated[which(df$V1.2 == 1)] <- 1
  df$vac1dose <- 0
  df$vac1dose[which(df$V1.1 == 1 & df$V2.1 == 1)] <- 1
  df$vac2doses <- 0
  df$vac2doses[which(df$V1.1 == 1 & df$V2.2 == 1)] <- 1
  
  # COVID-like illness: fever, along with cough or difficulty breathing (recall that 1=yes, 2=no)
  df$cli <- 0
  df$cli[which((df$B1_1.1 == 1) & ((df$B1_2.1 == 1) | (df$B1_3.1 == 1)))] <- 1
  df$cli_weight <- 0
  df$cli_weight[which(df$cli == 1)] <- df$weight[which(df$cli == 1)]
  df$cli_vaccinated <- 0
  df$cli_vaccinated[which(df$vaccinated == 1 & df$cli == 1)] <- 1
  df$cli_unvaccinated <- 0
  df$cli_unvaccinated[which(df$unvaccinated == 1 & df$cli == 1)] <- 1
  df$cli_vac1dose <- 0
  df$cli_vac1dose[which(df$vac1dose == 1 & df$cli == 1)] <- 1
  df$cli_vac2doses <- 0
  df$cli_vac2doses[which(df$vac2doses == 1 & df$cli == 1)] <- 1
  
  # COVID-like illness for world health organization: fever + cough + fatigue
  df$cliWHO <- 0
  df$cliWHO[which((df$B1_1.1 == 1) & (df$B1_2.1 == 1) & (df$B1_4.1 == 1) )] <- 1
  df$cliWHO_weight <- 0
  df$cliWHO_weight[which(df$cliWHO == 1)] <- df$weight[which(df$cliWHO == 1)]
  
  # Compute the number of CLI cases in the local community from B3 and B4
  df$cli_local <- NA
  df$cli_local[which(df$B3.2 == 1)] <- 0
  # Default is 1 in case that B3=yes and B4 is not available.
  df$cli_local[which(df$B3.1 == 1)] <- pmax(1, df$B4[which(df$B3.1 == 1)])
  
  # To count the number of responses in cli_local_com
  df$count_local <- 1
  df$count_local[which(is.na(df$cli_local))] <- 0
  
  # Compute reach
  reach <- (sum(df$cli_local, na.rm = TRUE) * sum(df$count)) / (sum(df$cli)*sum(df$count_local))
  cat("\n", iso2, " Reach:", reach, "\n")
  df$reach <- 0
  df$reach[which(df$count_local==1)] <- reach
  
  # Stringent CLI: Anosmia and (Fever or Muscle Pain or Cough)
  # df$stringent_cli <- 0
  # df$stringent_cli[which((df$B1_10.1 == 1) & ((df$B1_1.1 == 1) | (df$B1_6.1 == 1) | (df$B1_2.1 == 1)))] <- 1
  # df$stringent_cli_vaccinated <- 0
  # df$stringent_cli_vaccinated[which(df$vaccinated == 1 & df$stringent_cli == 1)] <- 1
  # df$stringent_cli_unvaccinated <- 0
  # df$stringent_cli_unvaccinated[which(df$unvaccinated == 1 & df$stringent_cli == 1)] <- 1
  # df$stringent_cli_vac1dose <- 0
  # df$stringent_cli_vac1dose[which(df$vac1dose == 1 & df$stringent_cli == 1)] <- 1
  # df$stringent_cli_vac2doses <- 0
  # df$stringent_cli_vac2doses[which(df$vac2doses == 1 & df$stringent_cli == 1)] <- 1

  # Classic CLI: Cough and (Fever or Muscle Pain or Anosmia)
  # df$classic_cli <- 0
  # df$classic_cli[which((df$B1_2.1 == 1) & ((df$B1_1.1 == 1) | (df$B1_6.1 == 1) | (df$B1_10.1 == 1)))] <- 1
  # df$classic_cli_vaccinated <- 0
  # df$classic_cli_vaccinated[which(df$vaccinated == 1 & df$classic_cli == 1)] <- 1
  # df$classic_cli_unvaccinated <- 0
  # df$classic_cli_unvaccinated[which(df$unvaccinated == 1 & df$classic_cli == 1)] <- 1
  # df$classic_cli_vac1dose <- 0
  # df$classic_cli_vac1dose[which(df$vac1dose == 1 & df$classic_cli == 1)] <- 1
  # df$classic_cli_vac2doses <- 0
  # df$classic_cli_vac2doses[which(df$vac2doses == 1 & df$classic_cli == 1)] <- 1
  
  # Broad CLI: Muscle Pain and (Fever or Cough or Anosmia)
  # df$broad_cli <- 0
  # df$broad_cli[which((df$B1_6.1 == 1) & ((df$B1_1.1 == 1) | (df$B1_2.1 == 1) | (df$B1_10.1 == 1)))] <- 1
  # df$broad_cli_vaccinated <- 0
  # df$broad_cli_vaccinated[which(df$vaccinated == 1 & df$broad_cli == 1)] <- 1
  # df$broad_cli_unvaccinated <- 0
  # df$broad_cli_unvaccinated[which(df$unvaccinated == 1 & df$broad_cli == 1)] <- 1
  # df$broad_cli_vac1dose <- 0
  # df$broad_cli_vac1dose[which(df$vac1dose == 1 & df$broad_cli == 1)] <- 1
  # df$broad_cli_vac2doses <- 0
  # df$broad_cli_vac2doses[which(df$vac2doses == 1 & df$broad_cli == 1)] <- 1
  
  # To count the number of people tested in latest 14 days
  df$test_recent <- 0
  df$test_recent[which(df$B7.1 == 1 & (df$B8.1 == 1 | df$B8.2 == 1))] <- 1
  
  # To count tested vaccinated
  df$tested_vaccinated <- 0
  df$tested_vaccinated[which(df$vaccinated == 1 & df$test_recent == 1)] <- 1
  df$tested_unvaccinated <- 0
  df$tested_unvaccinated[which(df$unvaccinated == 1 & df$tested_recent == 1)] <- 1
  df$tested_vac1dose <- 0
  df$tested_vac1dose[which(df$vac1dose == 1 & df$tested_recent == 1)] <- 1
  df$tested_vac2doses <- 0
  df$tested_vac2doses[which(df$vac2doses == 1 & df$tested_recent == 1)] <- 1
  
  # To count the number of positive tests in latest 14 days
  df$positive_recent <- 0
  df$positive_recent[which((df$B7.1 == 1) & (df$B8.1 == 1))] <- 1
  
  # To count positive vaccinated
  df$positive_vaccinated <- 0
  df$positive_vaccinated[which(df$vaccinated == 1 & df$positive_recent == 1)] <- 1
  # df$positive_vaccinated_weight <- 0
  # df$positive_vaccinated_weight[which(df$positive_vaccinated == 1)] <- df$weight[which(df$positive_vaccinated == 1)]
  df$positive_unvaccinated <- 0
  df$positive_unvaccinated[which(df$unvaccinated == 1 & df$positive_recent == 1)] <- 1
  df$positive_vac1dose <- 0
  df$positive_vac1dose[which(df$vac1dose == 1 & df$positive_recent == 1)] <- 1
  df$positive_vac2doses <- 0
  df$positive_vac2doses[which(df$vac2doses == 1 & df$positive_recent == 1)] <- 1
  
  # Positive with symptoms
  df$positive_symptomatic <- 0
  df$positive_symptomatic[which(df$symptomatic == 1 & df$positive_recent == 1)] <- 1
  
  # To count pos_RF vaccinated
  # "V1 Have you had a COVID-19 vaccination?"
  df$pos_RF_vaccinated <- 0
  df$pos_RF_vaccinated[which(df$vaccinated == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_unvaccinated <- 0
  df$pos_RF_unvaccinated[which(df$unvaccinated == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_vaccinated_weight <- 0
  # df$pos_RF_vaccinated_weight[which(df$pos_RF_vaccinated == 1)] <- df$weight[which(df$pos_RF_vaccinated == 1)]
  df$pos_RF_vac1dose <- 0
  df$pos_RF_vac1dose[which(df$vac1dose == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_vac2doses <- 0
  df$pos_RF_vac2doses[which(df$vac2doses == 1 & df$pos_RF == 1)] <- 1
  
  # Positive RF with symptoms
  df$pos_RF_symptomatic <- 0
  df$pos_RF_symptomatic[which(df$symptomatic == 1 & df$pos_RF == 1)] <- 1
  
  # Positive with each symptom
  df$positive_Fever_B1_1 <- 0
  df$positive_Fever_B1_1[which(df$B1_1.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Cough_B1_2 <- 0
  df$positive_Cough_B1_2[which(df$B1_2.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Difficulty_breathing_B1_3 <- 0
  df$positive_Difficulty_breathing_B1_3[which(df$B1_3.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Fatigue_B1_4 <- 0
  df$positive_Fatigue_B1_4[which(df$B1_4.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Stuffy_or_runny_nose_B1_5 <- 0
  df$positive_Stuffy_or_runny_nose_B1_5[which(df$B1_5.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Aches_or_muscle_pain_B1_6 <- 0
  df$positive_Aches_or_muscle_pain_B1_6[which(df$B1_6.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Sore_throat_B1_7 <- 0
  df$positive_Sore_throat_B1_7[which(df$B1_7.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Chest_pain_B1_8 <- 0
  df$positive_Chest_pain_B1_8[which(df$B1_8.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Nausea_B1_9 <- 0
  df$positive_Nausea_B1_9[which(df$B1_9.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Loss_of_smell_or_taste_B1_10 <- 0
  df$positive_Loss_of_smell_or_taste_B1_10[which(df$B1_10.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Headache_B1_12 <- 0
  df$positive_Headache_B1_12[which(df$B1_12.1 == 1 & df$positive_recent == 1)] <- 1
  df$positive_Chills_B1_13 <- 0
  df$positive_Chills_B1_13[which(df$B1_13.1 == 1 & df$positive_recent == 1)] <- 1
  
  # Positive vaccinated with each symptom
  df$pos_vaccinated_Fever_B1_1 <- 0
  df$pos_vaccinated_Fever_B1_1[which(df$B1_1.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Cough_B1_2 <- 0
  df$pos_vaccinated_Cough_B1_2[which(df$B1_2.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Difficulty_breathing_B1_3 <- 0
  df$pos_vaccinated_Difficulty_breathing_B1_3[which(df$B1_3.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Fatigue_B1_4 <- 0
  df$pos_vaccinated_Fatigue_B1_4[which(df$B1_4.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Stuffy_or_runny_nose_B1_5 <- 0
  df$pos_vaccinated_Stuffy_or_runny_nose_B1_5[which(df$B1_5.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Aches_or_muscle_pain_B1_6 <- 0
  df$pos_vaccinated_Aches_or_muscle_pain_B1_6[which(df$B1_6.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Sore_throat_B1_7 <- 0
  df$pos_vaccinated_Sore_throat_B1_7[which(df$B1_7.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Chest_pain_B1_8 <- 0
  df$pos_vaccinated_Chest_pain_B1_8[which(df$B1_8.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Nausea_B1_9 <- 0
  df$pos_vaccinated_Nausea_B1_9[which(df$B1_9.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Loss_of_smell_or_taste_B1_10 <- 0
  df$pos_vaccinated_Loss_of_smell_or_taste_B1_10[which(df$B1_10.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Headache_B1_12 <- 0
  df$pos_vaccinated_Headache_B1_12[which(df$B1_12.1 == 1 & df$positive_vaccinated == 1)] <- 1
  df$pos_vaccinated_Chills_B1_13 <- 0
  df$pos_vaccinated_Chills_B1_13[which(df$B1_13.1 == 1 & df$positive_vaccinated == 1)] <- 1
  
  # Positive RF with each symptom
  df$pos_RF_Fever_B1_1 <- 0
  df$pos_RF_Fever_B1_1[which(df$B1_1.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Cough_B1_2 <- 0
  df$pos_RF_Cough_B1_2[which(df$B1_2.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Difficulty_breathing_B1_3 <- 0
  df$pos_RF_Difficulty_breathing_B1_3[which(df$B1_3.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Fatigue_B1_4 <- 0
  df$pos_RF_Fatigue_B1_4[which(df$B1_4.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Stuffy_or_runny_nose_B1_5 <- 0
  df$pos_RF_Stuffy_or_runny_nose_B1_5[which(df$B1_5.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Aches_or_muscle_pain_B1_6 <- 0
  df$pos_RF_Aches_or_muscle_pain_B1_6[which(df$B1_6.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Sore_throat_B1_7 <- 0
  df$pos_RF_Sore_throat_B1_7[which(df$B1_7.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Chest_pain_B1_8 <- 0
  df$pos_RF_Chest_pain_B1_8[which(df$B1_8.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Nausea_B1_9 <- 0
  df$pos_RF_Nausea_B1_9[which(df$B1_9.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Loss_of_smell_or_taste_B1_10 <- 0
  df$pos_RF_Loss_of_smell_or_taste_B1_10[which(df$B1_10.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Headache_B1_12 <- 0
  df$pos_RF_Headache_B1_12[which(df$B1_12.1 == 1 & df$pos_RF == 1)] <- 1
  df$pos_RF_Chills_B1_13 <- 0
  df$pos_RF_Chills_B1_13[which(df$B1_13.1 == 1 & df$pos_RF == 1)] <- 1
  
  # Positive RF vaccinated with each symptom
  df$pos_RF_vaccinated_Fever_B1_1 <- 0
  df$pos_RF_vaccinated_Fever_B1_1[which(df$B1_1.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Cough_B1_2 <- 0
  df$pos_RF_vaccinated_Cough_B1_2[which(df$B1_2.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Difficulty_breathing_B1_3 <- 0
  df$pos_RF_vaccinated_Difficulty_breathing_B1_3[which(df$B1_3.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Fatigue_B1_4 <- 0
  df$pos_RF_vaccinated_Fatigue_B1_4[which(df$B1_4.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Stuffy_or_runny_nose_B1_5 <- 0
  df$pos_RF_vaccinated_Stuffy_or_runny_nose_B1_5[which(df$B1_5.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Aches_or_muscle_pain_B1_6 <- 0
  df$pos_RF_vaccinated_Aches_or_muscle_pain_B1_6[which(df$B1_6.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Sore_throat_B1_7 <- 0
  df$pos_RF_vaccinated_Sore_throat_B1_7[which(df$B1_7.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Chest_pain_B1_8 <- 0
  df$pos_RF_vaccinated_Chest_pain_B1_8[which(df$B1_8.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Nausea_B1_9 <- 0
  df$pos_RF_vaccinated_Nausea_B1_9[which(df$B1_9.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Loss_of_smell_or_taste_B1_10 <- 0
  df$pos_RF_vaccinated_Loss_of_smell_or_taste_B1_10[which(df$B1_10.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Headache_B1_12 <- 0
  df$pos_RF_vaccinated_Headache_B1_12[which(df$B1_12.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  df$pos_RF_vaccinated_Chills_B1_13 <- 0
  df$pos_RF_vaccinated_Chills_B1_13[which(df$B1_13.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  
  
  # Reorder the columns to place the new ones first
  #df <- subset(df, select=c("count", "symptomatic", "cli", "cli_weight", "cliWHO", "cliWHO_weight", "cli_local", "count_local",
  #                          "test_recent", "positive_recent", "positive_vaccinated", "positive_symptomatic",
  #                          colnames(df)[-c(count, symptomatic, cli, cli_weight, cliWHO, cliWHO_weight, cli_local, count_local,
  #                                          test_recent, positive_recent, positive_vaccinated, positive_symptomatic)]))
  return(df)
}

Aggregate_Country <- function(df) {
  df <- subset(df, select = -c(region_agg, age))
  df$date <- as.Date(df$date)
  
  df <- df %>% group_by(ISO2,ISO_3,country_agg,date) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    arrange(desc(date))
  # df <- df %>% group_by(ISO2,ISO_3,country_agg) %>% 
  #   slice(seq_len(which.max(cumsum(count) >= 100))) %>%
  #   summarise_if(is.numeric, sum, na.rm = TRUE)
  
  df$first_date <- df$date
  df$day_count <- df$count
  df$days_aggregated <- 1
  
  dfTotal <- df[which(df$count >= min_responses),]

  rows_to_aggregate <- which(df$count < min_responses)
  if (length(rows_to_aggregate) > 0) {
    # Aggregate the dates that have less than 100 responses in dfTotal
    nr <- nrow(df)
    for (i in rows_to_aggregate) {
      dfa <- df[i:nr,]
      dfa <- dfa %>% 
        # slice(i:nr) %>% 
        slice(seq_len(which.max(cumsum(count) >= min_responses))) # dfa has the slice that add 100 starting at pos i
      dfa$first_date <- dfa$date[nrow(dfa)]
      dfa$day_count <- dfa$count[1]
      dfa$date <- dfa$date[1]
      dfTotal <- rbind(dfTotal,dfa)
    }
    dfTotal <- dfTotal %>% group_by(ISO2,ISO_3,country_agg,date,first_date,day_count) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      filter(count>=min_responses)
  }
  dfTotal <- dfTotal %>%
    arrange(ISO2,ISO_3,country_agg,date)
  return(dfTotal)
}

Aggregate_Age <- function(df) {
  df <- subset (df, select = -c(region_agg))
  df$date <- as.Date(df$date)
  df <- df[which(!is.na(df$age)),]
  
  df <- df %>% group_by(ISO2,ISO_3,country_agg,age,date) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    arrange(desc(date))

  df$first_date <- df$date
  df$day_count <- df$count
  df$days_aggregated <- 1
  
  dfTotal <- df[which(df$count >= min_responses),]
  if (nrow(df) > nrow(dfTotal)) { # There are rows to aggregate
    ages <- unique(df$age)
    for (a in ages) {
      df_age <- df[(df$age == a),]
      rows_to_aggregate <- which(df_age$count < min_responses)
      if (length(rows_to_aggregate) > 0) {
        # Aggregate the dates that have less than 100 responses in dfTotal
        nr <- nrow(df_age)
        for (i in rows_to_aggregate) {
          dfa <- df_age[i:nr,]
          dfa <- dfa %>% 
            # slice(i:nr) %>%
            slice(seq_len(which.max(cumsum(count) >= min_responses))) # dfa has the slice that add 100 starting at pos i
          dfa$first_date <- dfa$date[nrow(dfa)]
          dfa$day_count <- dfa$count[1]
          dfa$date <- dfa$date[1]
          dfTotal <- rbind(dfTotal,dfa)
        }
      }
    }
    dfTotal <- dfTotal %>% group_by(ISO2,ISO_3,country_agg,age,date,first_date,day_count) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      filter(count>=min_responses)
  }
  dfTotal <- dfTotal %>%
    arrange(ISO2,ISO_3,country_agg,age,date)
  return(dfTotal)
}

Aggregate_Region <- function(df) {
  df <- subset (df, select = -c(age))
  df$date <- as.Date(df$date)
  df <- df[which(!is.na(df$region_agg)),]
  
  df <- df %>% group_by(ISO2,ISO_3,country_agg,region_agg,date) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    arrange(desc(date))
  
  df$first_date <- df$date
  df$day_count <- df$count
  df$days_aggregated <- 1
  
  dfTotal <- df[df$count >= min_responses,]
  if (nrow(df) > nrow(dfTotal)) { # There are rows to aggregate
    regions <- unique(df$region_agg)
    for (r in regions) {
      cat(r, " ")
      df_reg <- df[(df$region_agg == r),]
      rows_to_aggregate <- which(df_reg$count < min_responses)
      if (length(rows_to_aggregate) > 0) {
        # Aggregate the dates that have less than 100 responses in dfTotal
        nr <- nrow(df_reg)
        for (i in rows_to_aggregate) {
          dfa <- df_reg[i:nr,]
          dfa <- dfa %>% 
            # slice(i:nr) %>%
            slice(seq_len(which.max(cumsum(count) >= min_responses))) # dfa has the slice that add 100 starting at pos i
          dfa$first_date <- dfa$date[nrow(dfa)]
          dfa$day_count <- dfa$count[1]
          dfa$date <- dfa$date[1]
          dfTotal <- rbind(dfTotal,dfa)
        }
      }
    }
    dfTotal <- dfTotal %>% group_by(ISO2,ISO_3,country_agg,region_agg,date,first_date,day_count) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      filter(count>=min_responses)
  }
  dfTotal <- dfTotal %>%
    arrange(ISO2,ISO_3,country_agg,region_agg,date)
  return(dfTotal)
}

process_country <- function(iso2, allFiles) {
  cat("Country:", iso2, "\n")
  
  # allFilesVersion <- allFiles[grep(iso2, allFiles)]
  # allFilesVersion<-sort(unique(as.numeric(str_extract_all(allFilesVersion, "[0-9]+"))))
  
  # for (ver in allFilesVersion) {
  #   cat("Version:", ver, "\n")
    file_short_csv <- paste0(iso2,".csv")
    file_short_rds <- paste0(iso2,".rds")
    file_input <- paste0(input_path, file_short_rds)
    if (file.exists(file_input)){
      df <- readRDS(file=file_input)
      cat("Total:", dim(df), iso2, "\n")
      
      # try(df <- RemoveOutliersBeforeDummification(df), silent = F)
      # cat("After first outlier removal:", dim(df),  unique(df$ISO2),"\n")
      
      # try(df <-RemoveBinaryOutliers(df, cols_to_dummify), silent = F)
      # cat("After removing binary outliers:", dim(df), unique(df$ISO2), "\n")
      
      if (nrow(df)>0) {
        df <- ComputePredicates(df, iso2)
        cat("After predicates:", dim(df), iso2, "\n")
        
        # Sort by the time the survey was filled
        df <- df[order(df$RecordedDate),]
        # Remove columns that are not likely to be used and hard to aggregate
        cols_to_remove <- intersect(cols_no_aggregate, colnames(df))
        df <- df %>% dplyr::select(-all_of(cols_to_remove))
        # Remove dummified columns
        df <- df %>% select(-contains(".") | starts_with("C0") | starts_with("I6") | 
                              starts_with("D1") | starts_with("D2") | starts_with("D4") | starts_with("D5"))
        
        # cat("\n", iso2, " Colnames:", colnames(df), "\n")
        
        # Aggregation at the country level
        df_aux <- Aggregate_Country(df)
        if (is.data.frame(df_aux)) {
          df_aux <- process_umd_file(df_aux, iso2)
          df_aux <- df_aux[which((df_aux$date >= start_date) & (df_aux$date <= end_date)),]
          fwrite(df_aux, file=paste0(output_path, "country/", file_short_csv))
          # saveRDS(df_aux, file=paste0(output_path, "country/", file_short_rds))
        }
        cat("* After country:", dim(df_aux), iso2, "\n")

        # Aggregation per age group
        df_aux <- Aggregate_Age(df)
        if (is.data.frame(df_aux)) {
          df_aux <- process_umd_file(df_aux, iso2)
          df_aux <- df_aux[which((df_aux$date >= start_date) & (df_aux$date <= end_date)),]
          fwrite(df_aux, file=paste0(output_path, "age/", file_short_csv))
          # saveRDS(df_aux, file=paste0(output_path, "age/", file_short_rds))
        }
        cat("* After age:", dim(df_aux), iso2, "\n")

        # Aggregation at the region level
        df_aux <- Aggregate_Region(df)
        if (is.data.frame(df_aux)) {
          df_aux <- process_umd_file(df_aux, iso2)
          df_aux <- df_aux[which((df_aux$date >= start_date) & (df_aux$date <= end_date)),]
          fwrite(df_aux, file=paste0(output_path, "region/", file_short_csv))
          # saveRDS(df_aux, file=paste0(output_path, "region/", file_short_rds))
        }
        cat("* After region:", dim(df_aux), iso2, "\n")
      }
    }
}

#--------------------------------main

dir.create(output_path, showWarnings = F)
dir.create(paste0(output_path, "country/"), showWarnings = F)
dir.create(paste0(output_path, "region/"), showWarnings = F)
dir.create(paste0(output_path, "age/"), showWarnings = F)

#Read all the files and extract the versions that we have in that folder
allFiles<-list.files(input_path)
countries <- unique(substr(allFiles,1,2))
# countries <- c("GB") # GR", "IN", "CA", "NP", "BD", "PK", "LK", "AR", "BR", "CL", "PY", "UY", "CO", "PE", 
#                "EC", "VE", "CR", "GY", "SR", "PA", "BO", "SE", "BE", "ES", "PT", "NL", "BT", "MY", "OM", 
#                "AE", "BW", "LY", "TN")

# kk <- lapply(countries, process_country, allFiles)
kk <- mclapply(countries, process_country, allFiles)
