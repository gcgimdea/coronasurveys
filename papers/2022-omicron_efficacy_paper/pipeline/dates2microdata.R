library(tidyverse)
# library(dplyr)
# library(zoo)
# library(lubridate)
library(data.table)
library(parallel)

args <- commandArgs(trailingOnly = T)
cat(args, "\n")

quarter <- args[1]

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
  start_date <- "2022-04-14"
  end_date <- "2022-07-03"
}

if (quarter == "2022-Q1Q2") {
  start_date <- "2021-12-10"
  end_date <- "2022-07-03"
}

dates_path <- "./umd-dates/"
microdata_path <- paste0("./microdata/")
country_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"

dir.create(paste0("./", quarter), showWarnings = F)
dir.create(microdata_path, showWarnings = F)

# 2020
# start_date <- "2020-04-23"
# end_date <- "2021-01-03"

# start_date <- "2020-11-13"
# end_date <- "2020-12-25"

# 2021
# start_date <- "2020-12-01"
# start_date <- "2021-09-01"
# end_date <- Sys.Date()

# cols_no_aggregate <- c("survey_region", "survey_version", "intro1", "intro2",
#                        "A1", "A2_2_1", "A2_2_2", "Q_Language", "Q_TotalDuration", "GID_0", "GID_1", "NAME_0",
#                        "NAME_1", "country_region_numeric", "1w_0unw")

numeric_cols <- c("intro1", "intro2", "A1", "weight", "Finished",
                  "B0", "B0a", # These two will be merged as B0
                  "B1_1", "B1_2", "B1_3", "B1_4", "B1_5", "B1_6", "B1_7",
                  "B1_8", "B1_9", "B1_10", "B1_11", "B1_12", "B1_13", "B1_14",
                  "B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                  "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11", "B1b_x12", "B1b_x13", "B1b_x14",
                  "B2", "B2b", # These two will be merged as B2
                  "B3", "B4",
                  "B5", "B6",
                  "B7", "B7c", # These two will be merged as B7
                  "B8", "B8a", # These two will be merged as B8
                  "B9", "B10", "B11",
                  "B12_1", "B12_2", "B12_3", "B12_4", "B12_5", "B12_6",
                  "B13_1", "B13_2", "B13_3", "B13_4", "B13_5", "B13_6", "B13_7",
                  "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
                  "B15_1","B15_2","B15_3","B15_4","B15_5","B15_6","B15_7",
                  "C0_1", "C0a_1", # These two will be merged as C0_1
                  "C0_2", "C0a_2", # These two will be merged as C0_2
                  "C0_3", "C0a_3", # These two will be merged as C0_3
                  "C0_4", "C0a_4", # These two will be merged as C0_4
                  "C0_5", "C0a_5", # These two will be merged as C0_5
                  "C0_6", "C0a_6", # These two will be merged as C0_6
                  "C0a_7", # New option in C0a
                  "C1_m", "C2", "C3", "C5", "C6",
                  "C7", "C8", "C9", "C9a", "C12",
                  "C13_1", "C13a_1", # These two will be merged as C13_1
                  "C13_2", "C13a_2", # These two will be merged as C13_2
                  "C13_3", "C13a_3", # These two will be merged as C13_3
                  "C13_4", "C13a_4", # These two will be merged as C13_4
                  "C13_5", "C13a_5", # These two will be merged as C13_5
                  "C13_6", "C13a_6", # These two will be merged as C13_6
                  "C13a_7", # New option in C13a
                  "C14", "C14a", # Not merged because options differ
                  "D1", "D2", "D3", "D4",
                  "D5", "D6_1", "D6_2", "D6_3",
                  "D7", "D7a", # These two will be merged as D7
                  "D8", "D9", 
                  "D10", "D10a", # These two will be merged as D10
                  "E2", "E3", "E4", "E5", "E6",
                  "E7", "E7a", # These two will be merged as E7
                  "H1", "H2", "H3",
                  "I6_1","I6_2","I6_3","I6_4","I6_5","I6_6","I6_7","I6_8",
                  "J6",
                  "V1", "V1alt_A_1", # These two will be merged as V1
                  "V2", "V2a", # Not merged. Note that V2a changed from V10 to V13
                  "V2b", "V2c",
                  "V3", "V3a", # These two will be merged as V3
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
                  "V15", "V15a", # These two will be merged as V15
                  "V16", "V16a" # These two will be merged as V16
                  )

character_cols <- c("survey_version", "RecordedDate", "ISO_3", "country_agg", "region_agg")

col_min <- c(numeric_cols, character_cols)
# all_cols <- c(cols_no_aggregate, col_min)

# --------- Functions

merge_cols <- function(df, col1, col2) {
  df[col1] <- pmax(df[col1], df[col2])
  df[col2] <- NULL
  return(df)
}

process_day <- function(d) {
  cat(d, "\n")
  filen <- paste0(dates_path, d, "_full.csv")
  date_dir <- paste0(microdata_path, d, "/")

  if (file.exists(filen) & !file.exists(date_dir)) {
    cat("processing ", filen, "\n")
    
    dir.create(date_dir, showWarnings = F)
    
    df <- fread(filen, data.table = F, header=T)
    # Unused options:
    # select=list(character=character_cols, numeric=numeric_cols), # Gives the columns to keep
    # check.names=T) # Checks that the column names are OK

    # The following fixes the issue that some files have the first column (row counter) 
    # without name, and fread assigns it the name V1
    if (colnames(df)[1]=="V1") {
      cat("*** V1 removed", d, "\n")
      df[1] <- NULL
    }

    # cat("Total:", "dim:", dim(df), "\n")
    
    # Create cols that donot exist in the data frame
    for (col in col_min) {
      if (!(col %in% colnames(df))){
        df[[col]] <- "-99"
      }
    }

    # Keep only col_min columns
    df <- df %>% 
      dplyr::select(all_of(col_min))
    
    # fread is messing up the column types, so I am forcing the class of those used
    for (c in character_cols) {
      df[[c]] <- as.character(df[[c]])
    }
    for (c in numeric_cols) {
      df[[c]] <- as.numeric(df[[c]])
    }
    
    # Merge of columns
    df <- merge_cols(df, "B0", "B0a") # These two will be merged as B0
    df <- merge_cols(df, "B2", "B2b") # These two will be merged as B2
    df <- merge_cols(df, "B7", "B7c") # These two will be merged as B7
    df <- merge_cols(df, "B8", "B8a") # These two will be merged as B8
    df <- merge_cols(df, "C0_1", "C0a_1") # These two will be merged as C0_1
    df <- merge_cols(df, "C0_2", "C0a_2") # These two will be merged as C0_2
    df <- merge_cols(df, "C0_3", "C0a_3") # These two will be merged as C0_3
    df <- merge_cols(df, "C0_4", "C0a_4") # These two will be merged as C0_4
    df <- merge_cols(df, "C0_5", "C0a_5") # These two will be merged as C0_5
    df <- merge_cols(df, "C0_6", "C0a_6") # These two will be merged as C0_6
    df <- merge_cols(df, "C13_1", "C13a_1") # These two will be merged as C13_1
    df <- merge_cols(df, "C13_2", "C13a_2") # These two will be merged as C13_2
    df <- merge_cols(df, "C13_3", "C13a_3") # These two will be merged as C13_3
    df <- merge_cols(df, "C13_4", "C13a_4") # These two will be merged as C13_4
    df <- merge_cols(df, "C13_5", "C13a_5") # These two will be merged as C13_5
    df <- merge_cols(df, "C13_6", "C13a_6") # These two will be merged as C13_6
    df <- merge_cols(df, "D7", "D7a") # These two will be merged as D7
    df <- merge_cols(df, "D10", "D10a") # These two will be merged as D10
    df <- merge_cols(df, "E7", "E7a") # These two will be merged as E7
    df <- merge_cols(df, "V1", "V1alt_A_1") # These two will be merged as V1
    df <- merge_cols(df, "V3", "V3a") # These two will be merged as V3
    df <- merge_cols(df, "V15", "V15a") # These two will be merged as V15
    df <- merge_cols(df, "V16", "V16a") # These two will be merged as V16
    
    # replace -99 and -77 and -88 and -66 with NA
    df <- df %>%
      dplyr::na_if(-99) %>%
      dplyr::na_if(-77) %>%
      dplyr::na_if(-88) %>%
      dplyr::na_if(-66) %>%
      dplyr::na_if("-99") %>%
      dplyr::na_if("-77") %>%
      dplyr::na_if("-88") %>%
      dplyr::na_if("-66") %>%
      dplyr::na_if("-99.0") %>%
      dplyr::na_if("-77.0") %>%
      dplyr::na_if("-66") %>%
      dplyr::na_if("-88.0")
    # cat("After removing cols and adding NA:", dim(df), "\n")
    
    # Remove responses that do not consent (intro1 and intro2) or are not above 18 (A1)
    df <- df[which((df$intro1==1) & (df$intro2==1) & (df$A1==1)),]
    df$intro1 <- NULL
    df$intro2 <- NULL
    df$A1 <- NULL
    # cat("After removing non-consent responses:", "dim:", dim(df), "\n")
    
    # cat("versions:", unique(df$survey_version),"\n")
    
    # Store the date from the file
    df$date_from_file <- d
    # Create a column with the date the response was provided
    df$date <- substr(df$RecordedDate, start = 1, stop = 10)
    
    # Remove rows with undefined country
    df <- df[which(!is.na(df$ISO_3)), ]
    # cat("After removing NA countries:", dim(df), d, "\n")
    
    cat("Final:", dim(df), d, "\n")
    cat("columns:", colnames(df), d, "\n")
    
    iso_list <- unique(df$ISO_3)
    
    for (iso3 in iso_list) {
      # cat(iso3, " ")
      
      iso2 <- codes$ISO2[which(codes$ISO3 == iso3)]
      dfb <- df[which(df$ISO_3 == iso3),]
      dfb$ISO2 <- iso2
      
      # country_file <- paste0(date_dir, iso2, ".csv") # ***
      # fwrite(dfb, file=country_file) #, row.names = FALSE) # ***
      
      country_file <- paste0(date_dir, iso2, ".rds")
      saveRDS(dfb, file=country_file)
    }
    cat("\n")
  }
}

# Main ------------------------

codes <- fread(country_file, data.table = F)

dir.create(microdata_path, showWarnings = F)

dates <- seq(as.Date(start_date), as.Date(end_date), by="days")

# kk <- lapply(as.character(as.Date(dates)), process_day)
kk <- mclapply(as.character(as.Date(dates)), process_day)

