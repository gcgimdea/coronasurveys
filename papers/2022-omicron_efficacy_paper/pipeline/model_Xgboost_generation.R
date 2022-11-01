# library(caTools)
# library(glmnet)
library(xgboost)
library(caret)
library(dplyr)
library(parallel)
library(tidyverse)

# setwd("../Data/Data_ZA_22-02-2022")

args <- commandArgs(trailingOnly = T)
cat(args, "\n")

quarter <- args[1]
# quarter <- "2022-Q1"

input_path <- paste0("./", quarter, "/dummies/")
output_path <- paste0("./", quarter, "/models_XGB/")

# input_path <- paste0("../Data/Data_ZA_22-02-2022/", quarter, "/dummies/")
# output_path <- paste0("../Data/Data_ZA_22-02-2022/", quarter, "/models_XGB/")


create_model <- function(train, test) {
  # dtrain = xgb.DMatrix(as.matrix(train %>% select(-positive)), label=as.matrix(train$positive))
  cat("Unique",unique(train$positive),"\n")
  # classifier_Xgboost = xgboost(dtrain,
  #                              # eta = 0.1,
  #                              # max_depth = 15,
  #                              nround=15,
  #                              # subsample = 0.5,
  #                              # colsample_bytree = 0.5,
  #                              # seed = 1,
  #                              # eval_metric = "merror",
  #                              objective = "binary:logistic",
  #                              # num_class = 12,
  #                              # nthread = 3
  # )
  classifier_Xgboost = xgboost(data = as.matrix(train %>% select(-positive)),
                               label = train$positive,
                               # eta = 0.1,
                               # max_depth = 15,
                               nround=15,
                               # subsample = 0.5,
                               # colsample_bytree = 0.5,
                               # seed = 1,
                               eval_metric = "logloss",
                               objective = "binary:logistic",
                               # num_class = 12,
                               # nthread = 3
  )
  
#  print(classifier_RF)
  # -- Predicting the Test set results
  y_pred = predict(classifier_Xgboost, as.matrix(test %>% select(-positive)))
  y_pred = round(y_pred)
  # -- Confusion Matrix

  # print(confusionMatrix(as.factor(test$positive), as.factor(y_pred), mode = "everything", positive="1"))
  # -- Plotting model
  # plot(classifier_RF)
  # -- Importance plot
#  print(importance(classifier_RF))
  # -- Variable importance plot
  # varImpPlot(classifier_RF)
  
  return(classifier_Xgboost)
}


process_country <- function(iso2) {
  cat("Country:", iso2, "\n")
  
  # file_short_csv <- paste0(iso2,".csv")
  file_short_rds <- paste0(iso2,".rds")
  file_input <- paste0(input_path, file_short_rds)
  
  if (file.exists(file_input)){
    df <- readRDS(file=file_input)
    cat("Total:", dim(df), iso2, "\n")
    
    golden <- which(df$B7.1==1 & (df$B8.1==1 | df$B8.2==1))
    df_golden <-df[golden,]
    
    df_golden$positive <- df_golden$B8.1 #factor(df_golden$B8.1) #
    
    df_golden <- df_golden %>%
      select(-c(weight, Finished, B2, B4, E5, E6, E7, survey_version, RecordedDate, ISO_3, country_agg, region_agg, 
                date_from_file, date, ISO2, age), 
             -contains(".NA"), -contains("B7"), -contains("B8")
             # , -contains("B0"), -contains("B15"), -contains("V1"), -contains("V2"), -contains("V3"),
             # -contains("V4"), -contains("V5"), -contains("V6"), -contains("V9")
      )
    
    cat("Total tested:", dim(df_golden), iso2, "\n")
    
    if (nrow(df_golden)>0 & length(unique(df_golden$positive))>1) {
      train <- df_golden
      test <- df_golden
      classifier_Xgboost <- create_model(train, test)
      
      file_output <- paste0(output_path, file_short_rds)
      saveRDS(classifier_Xgboost, file_output)
    }
    
  }
}


#--------------------------------main

dir.create(output_path, showWarnings = F)

#Read all the files and extract the versions that we have in that folder
allFiles<-list.files(input_path)
countries <- unique(substr(allFiles,1,2))

# kk <- lapply(countries, process_country)
kk <- mclapply(countries, process_country)