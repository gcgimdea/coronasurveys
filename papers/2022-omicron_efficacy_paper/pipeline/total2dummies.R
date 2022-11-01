# library(tidyverse)
# library(dplyr)
library(nnet)
# library(zoo)
# library(stringr)
library(parallel)
library(data.table)

source("RemoveOutliers.R")

args <- commandArgs(trailingOnly = T)
cat(args, "\n")

quarter <- args[1]

input_path <- paste0("./", quarter, "/total/")
output_path <- paste0("./", quarter, "/dummies/")

dir.create(paste0("./", quarter), showWarnings = F)
dir.create(output_path, showWarnings = F)
  
cols_to_dummify <- c(
  "B0", #"B0a", # These two will be merged as B0
  "B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6", "B1_7",
  "B1_8", "B1_9", "B1_10", "B1_11", "B1_12", "B1_13", "B1_14",
  "B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
  "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11", "B1b_x12", "B1b_x13", "B1b_x14",
  # "B2", #"B2b", # These two will be merged as B2
  "B3", 
  # "B4",
  "B5", "B6",
  "B7", #"B7c", # These two will be merged as B7
  "B8", #"B8a", # These two will be merged as B8
  "B9", "B10", "B11",
  "B12_1", "B12_2", "B12_3", "B12_4", "B12_5", "B12_6",
  "B13_1", "B13_2", "B13_3", "B13_4", "B13_5", "B13_6", "B13_7",
  "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
  "B15_1","B15_2","B15_3","B15_4","B15_5","B15_6","B15_7",
  "C0_1", #"C0a_1", # These two will be merged as C0_1
  "C0_2", #"C0a_2", # These two will be merged as C0_2
  "C0_3", #"C0a_3", # These two will be merged as C0_3
  "C0_4", #"C0a_4", # These two will be merged as C0_4
  "C0_5", #"C0a_5", # These two will be merged as C0_5
  "C0_6", #"C0a_6", # These two will be merged as C0_6
  "C0a_7", # New option in C0a
  "C1_m", "C2", "C3", "C5", "C6",
  "C7", "C8", "C9", "C9a", "C12",
  "C13_1", #"C13a_1", # These two will be merged as C13_1
  "C13_2", #"C13a_2", # These two will be merged as C13_2
  "C13_3", #"C13a_3", # These two will be merged as C13_3
  "C13_4", #"C13a_4", # These two will be merged as C13_4
  "C13_5", #"C13a_5", # These two will be merged as C13_5
  "C13_6", #"C13a_6", # These two will be merged as C13_6
  "C13a_7", # New option in C13a
  "C14", "C14a", # Not merged because options differ
  "D1", "D2", "D3", "D4",
  "D5", "D6_1", "D6_2", "D6_3",
  "D7", #"D7a", # These two will be merged as D7
  "D8", "D9", 
  "D10", #"D10a", # These two will be merged as D10
  "E2", "E3", "E4", 
  # "E5", "E6",
  # "E7", #"E7a", # These two will be merged as E7
  "H1", "H2", "H3",
  "I6_1","I6_2","I6_3","I6_4","I6_5","I6_6","I6_7","I6_8",
  "J6",
  "V1", #"V1alt_A_1", # These two will be merged as V1
  "V2", "V2a", # Not merged. Note that V2a changed from V10 to V13
  "V2b", "V2c",
  "V3", #"V3a", # These two will be merged as V3
  "V4_1", "V4_2", "V4_3", "V4_4", "V4_5", "V4_6",
  "V5a_1", "V5a_2", "V5a_3", "V5a_4", "V5a_5", "V5a_6", "V5a_7", "V5a_8", "V5a_9", "V5a_10", "V5a_11", "V5a_12",
  "V5b_1", "V5b_2", "V5b_3", "V5b_4", "V5b_5", "V5b_6", "V5b_7", "V5b_8", "V5b_9", "V5b_10", "V5b_11", "V5b_12",
  "V5c_1", "V5c_2", "V5c_3", "V5c_4", "V5c_5", "V5c_6", "V5c_7", "V5c_8", "V5c_9", "V5c_10", "V5c_11", "V5c_12",
  "V5d_1", "V5d_2", "V5d_3", "V5d_4", "V5d_5", "V5d_6", "V5d_7", "V5d_8", "V5d_9", "V5d_10", "V5d_11", "V5d_12",
  "V6_1", "V6_2", "V6_3", "V6_4", "V6_5", "V6_6", "V6_7",
  "V9",
  "V10_1", "V10_2", "V10_3", "V10_4", "V10_5", "V10_6", "V10_7", "V10_8", "V10_9", "V10_10",
  "V11", "V12",
  "V13", 
  "V15", #"V15a", # These two will be merged as V15
  "V16"  #"V16a" # These two will be merged as V16
)

# #------------------------------- Creating dummies 

Create_Dummies <- function(df, cols_to_dummify, iso2) {
  # Copy the age group to preserve it in another colum (age)
  df$age<-df$E4
  #Transform categorical variables into binaries, then remove the categorical columns(the original ones).
  for (i in cols_to_dummify){
    # cat("* Column:", i, iso2, "\n")
    # if(i %in% colnames(df) & (sum(is.na(df[[i]])) == 0) ){
    df[[i]][which(is.na(df[[i]]))] <- "NA"
    if(length(unique(df[[i]])) <= 20){ # In case the column takes many values
      dfcol<- class.ind(as.factor(df[[i]]))
      colnames(dfcol)<-paste0(i,".",colnames(dfcol))
      df <- cbind(df,dfcol)
      df[[i]]<-NULL
    } else {
      cat("*** Column has more than 10 values:", i, iso2, "\n")
    }
  }
  return(df)
}

process_country <- function(iso2, cols_to_dummify) {
  cat("Country:", iso2, "\n")
  
  filein_short <- paste0(iso2, ".rds")
  filein <- paste0(input_path, filein_short)
  if (file.exists(filein)){
    df <- readRDS(file=filein)
    cat("Total:", dim(df), iso2, "\n")
    
    cols_to_dummify <- intersect(cols_to_dummify, colnames(df))
    cat("Columns to dummify:", cols_to_dummify, iso2, "\n")
    
    try(df <- RemoveOutliersBeforeDummification(df), silent = F)
    cat("After first outlier removal:", dim(df),  iso2,"\n")
    
    df <- Create_Dummies(df, cols_to_dummify, iso2)
    cat("After create dummies:", dim(df), iso2, "\n")
    
    # try(df <-RemoveBinaryOutliers(df, cols_to_dummify), silent = F)
    # cat("After removing binary outliers:", dim(df), iso2, "\n")
    
    # filename_csv <- paste0(output_path, iso2, ".csv") # ***
    # fwrite(df, file=filename_csv) # ***
    
    filename_rds <- paste0(output_path, iso2, ".rds")
    saveRDS(df, file=filename_rds)
  }
}

#--------------------------------main

cat("*** total2dummies\n")

#Read all the files and extract the versions that we have in that folder
allFiles<-list.files(input_path, pattern="*", full.names=FALSE, recursive=FALSE)
countries <- unique(substr(allFiles,1,2))
# countries <- c("PT", "GR", "IN") #, "BR") # ***

dir.create(output_path, showWarnings = F)

# load("col_min.R")
# cols_to_dummify <- intersect(cols_to_dummify, col_min)

# kk <- lapply(countries, process_country, cols_to_dummify)
kk <- mclapply(countries, process_country, cols_to_dummify)
