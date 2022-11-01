# library(caTools)
# library(glmnet)
library(randomForest)
library(caret)
library(dplyr)
library(parallel)

args <- commandArgs(trailingOnly = T)
cat(args, "\n")

quarter <- args[1]

input_path <- paste0("./", quarter, "/dummies/")
output_path <- paste0("./", quarter, "/models_rf_symp/")

create_model <- function(train, test) {
  classifier_RF = randomForest(x = train %>% select(-positive),
                               y = train$positive, 
                               ntree = 100
                               # , importance = TRUE 
  )
#  print(classifier_RF)
  # -- Predicting the Test set results
  y_pred = predict(classifier_RF, newdata = test %>% select(-positive))
  # -- Confusion Matrix
  print(confusionMatrix(test$positive, y_pred, mode = "everything", positive="1"))
  # -- Plotting model
  # plot(classifier_RF)
  # -- Importance plot
#  print(importance(classifier_RF))
  # -- Variable importance plot
  # varImpPlot(classifier_RF)
  
  return(classifier_RF)
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
    
    if (nrow(df_golden)>0) {
      df_golden$positive <- 0
      positive_with_symptoms <- which((df_golden$B8.1==1) & 
                                        (df_golden$B1_1.1==1 | df_golden$B1_2.1==1 | df_golden$B1_3.1==1 | 
                                           df_golden$B1_4.1==1 | df_golden$B1_5.1==1 | df_golden$B1_6.1==1 | 
                                           df_golden$B1_7.1==1 | df_golden$B1_8.1==1 | df_golden$B1_9.1==1 | 
                                           df_golden$B1_10.1==1 | # df_golden$B1_11.1==1 |
                                           df_golden$B1_12.1==1 | df_golden$B1_13.1==1 # | df_golden$B1_14.1==1
                                        ))
      cat("Number of pos with symptoms:", length(positive_with_symptoms), "\n")
      df_golden$positive[positive_with_symptoms] <- 1
      
      df_golden$positive <- factor(df_golden$positive)
      df_golden <- df_golden %>%
        select(-c(weight, Finished, B2, B4, E5, E6, E7, survey_version, RecordedDate, ISO_3, country_agg, region_agg, 
                  date_from_file, date, ISO2, age), 
               -contains(".NA"), -contains("B7"), -contains("B8")
               # , -contains("B0"), -contains("B15"), -contains("V1"), -contains("V2"), -contains("V3"),
               # -contains("V4"), -contains("V5"), -contains("V6"), -contains("V9")
        )
      
      cat("Total tested:", dim(df_golden), iso2, "\n")
      
      if (length(unique(df_golden$positive))>1) {
        train <- df_golden
        test <- df_golden
        classifier_RF <- create_model(train, test)
        
        file_output <- paste0(output_path, file_short_rds)
        saveRDS(classifier_RF, file_output)
      }
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
