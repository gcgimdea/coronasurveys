# library(lubridate)
library(useful)
library(dplyr)
# library(readr)
# library(cowplot)
library(zoo)
library(data.table)
# library(DescTools)

# umd_path <- "/gauss_data/coronasurveys/github/SymptomSurveyData/Omicron/"
umd_path <- "../../SymptomSurveyData/Omicron/"
country_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/PlotData/"

quarter_list <- c("2020-Q2", "2020-Q3", "2020-Q4", "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4", "2022-Q1")

smooth_param <- 14
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  numerator <- rollsum(numerator, smooth_param, fill=NA, align = "right")
  denominator <- rollsum(denominator, smooth_param, fill=NA, align = "right")
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

cnames <- fread(country_file, data.table = FALSE)

country_name_iso3 <- function(iso2) {
  name_line <- cnames[which(cnames$ISO2==iso2),]
  if (nrow(name_line)>0) {
    name <- name_line$country[1]
    iso3 <- name_line$ISO3[1]
  } else {
    name <- NA
    iso3 <- NA
  }
  return(list(name=name, iso3=iso3))
}

process_country <- function(iso2) {
  cat(iso2, " ")
  
  # Read files
  file_short_csv <- paste0(iso2,".csv")
  file_short_rds <- paste0(iso2,".rds")
  umd <- NULL
  for (quarter in quarter_list) {
    input_path <- paste0(umd_path, quarter, "/aggregates/country/")
    # file_input_csv <- paste0(input_path, file_short_csv)
    file_input_rds <- paste0(input_path, file_short_rds)
    if (file.exists(file_input_rds)){
      umd_aux <- readRDS(file=file_input_rds)
      cat(iso2, quarter, dim(umd_aux), "\n")
      umd <- rbind(umd, umd_aux)  
      cat(iso2, quarter, "Total:", dim(umd), "\n")
    }
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

stop()




args <- commandArgs(trailingOnly = T)
# args <- c("ES", "2021-07-01", "2021-11-30")
cat(args, "\n")
if (length(args)<1) {
  stop("Arguments: country start_date end_date\n")
}

iso2 <- args[1]
cat("** Country:", iso2, "\n")

# Read files
file_short_csv <- paste0(iso2,".csv")
file_short_rds <- paste0(iso2,".rds")
umd <- NULL
for (quarter in quarter_list) {
  input_path <- paste0(umd_path, quarter, "/aggregates/country/")
  file_input_csv <- paste0(input_path, file_short_csv)
  file_input_rds <- paste0(input_path, file_short_rds)
  # if (file.exists(file_input)){
    umd_aux <- readRDS(file=file_input_rds)
    # umd_aux <- fread(file_input_csv, data.table = FALSE)
    cat(iso2, quarter, dim(umd_aux), "\n")
    # umd_aux <- umd_aux %>% 
    #   select(-contains("."))
    # cat(colnames(umd_aux), "\n")
    umd <- rbind(umd, umd_aux)  
    cat(iso2, quarter, "Total:", dim(umd), "\n")
  # }
}

confirmed_file <- paste0(confirmed_path, iso2, "-estimate.csv")
confirmed <- fread(confirmed_file, data.table = FALSE)

variants <- fread(variants_file, data.table = FALSE)
variants$date <- as.Date(variants$date)
variants <- variants[which(variants$ISO2==iso2),]

# Build data frame
umd$date <- as.Date(umd$date)
df <- data.frame(date = umd$date)

est <- process_ratio(umd$positive_recent,umd$test_recent)	
df$p_tested <- est$val
df$p_tested_error <- est$error

est <- process_ratio(umd$cli,umd$count)
df$p_cli <- est$val
df$p_cli_error <- est$error
est <- process_ratio(umd$cli_vaccinated,umd$vaccinated)
df$PV_cli <- est$val
df$PV_cli_error <- est$error
est <- process_ratio(umd$cli_unvaccinated,umd$unvaccinated)
df$PU_cli <- est$val
df$PU_cli_error <- est$error
est <- process_ratio(umd$cli_vac1dose,umd$vac1dose)
df$PV1D_cli <- est$val
df$PV1D_cli_error <- est$error
est <- process_ratio(umd$cli_vac2doses,umd$vac2doses)
df$PV2D_cli <- est$val
df$PV2D_cli_error <- est$error

est <- process_ratio(umd$stringent_cli,umd$count)
df$p_stringent_cli <- est$val
df$p_stringent_cli_error <- est$error
est <- process_ratio(umd$stringent_cli_vaccinated,umd$vaccinated)
df$PV_stringent_cli <- est$val
df$PV_stringent_cli_error <- est$error
est <- process_ratio(umd$stringent_cli_unvaccinated,umd$unvaccinated)
df$PU_stringent_cli <- est$val
df$PU_stringent_cli_error <- est$error
est <- process_ratio(umd$stringent_cli_vac1dose,umd$vac1dose)
df$PV1D_stringent_cli <- est$val
df$PV1D_stringent_cli_error <- est$error
est <- process_ratio(umd$stringent_cli_vac2doses,umd$vac2doses)
df$PV2D_stringent_cli <- est$val
df$PV2D_stringent_cli_error <- est$error

est <- process_ratio(umd$classic_cli,umd$count)
df$p_classic_cli <- est$val
df$p_classic_cli_error <- est$error
est <- process_ratio(umd$classic_cli_vaccinated,umd$vaccinated)
df$PV_classic_cli <- est$val
df$PV_classic_cli_error <- est$error
est <- process_ratio(umd$classic_cli_unvaccinated,umd$unvaccinated)
df$PU_classic_cli <- est$val
df$PU_classic_cli_error <- est$error
est <- process_ratio(umd$classic_cli_vac1dose,umd$vac1dose)
df$PV1D_classic_cli <- est$val
df$PV1D_classic_cli_error <- est$error
est <- process_ratio(umd$classic_cli_vac2doses,umd$vac2doses)
df$PV2D_classic_cli <- est$val
df$PV2D_classic_cli_error <- est$error

est <- process_ratio(umd$broad_cli,umd$count)
df$p_broad_cli <- est$val
df$p_broad_cli_error <- est$error
est <- process_ratio(umd$broad_cli_vaccinated,umd$vaccinated)
df$PV_broad_cli <- est$val
df$PV_broad_cli_error <- est$error
est <- process_ratio(umd$broad_cli_unvaccinated,umd$unvaccinated)
df$PU_broad_cli <- est$val
df$PU_broad_cli_error <- est$error
est <- process_ratio(umd$broad_cli_vac1dose,umd$vac1dose)
df$PV1D_broad_cli <- est$val
df$PV1D_broad_cli_error <- est$error
est <- process_ratio(umd$broad_cli_vac2doses,umd$vac2doses)
df$PV2D_broad_cli <- est$val
df$PV2D_broad_cli_error <- est$error

est <- process_ratio(umd$pos_RF,umd$count)
df$p_pos_RF <- est$val
df$p_pos_RF_error <- est$error
est <- process_ratio(umd$pos_RF_vaccinated,umd$vaccinated)
df$PV_pos_RF <- est$val
df$PV_pos_RF_error <- est$error
est <- process_ratio(umd$pos_RF_unvaccinated,umd$unvaccinated)
df$PU_pos_RF <- est$val
df$PU_pos_RF_error <- est$error
est <- process_ratio(umd$pos_RF_vac1dose,umd$vac1dose)
df$PV1D_pos_RF <- est$val
df$PV1D_pos_RF_error <- est$error
est <- process_ratio(umd$pos_RF_vac2doses,umd$vac2doses)
df$PV2D_pos_RF <- est$val
df$PV2D_pos_RF_error <- est$error

est <- process_ratio_proportions(umd$pos_RF_vaccinated, umd$vaccinated, umd$pos_RF_unvaccinated,umd$unvaccinated)
df$RR_pos_RF <- est$val
df$RR_pos_RF_lower <- est$low
df$RR_pos_RF_upper <- est$high
est <- process_ratio_proportions(umd$pos_RF_vac1dose,umd$vac1dose, umd$pos_RF_unvaccinated,umd$unvaccinated)
df$RR1D_pos_RF <- est$val
df$RR1D_pos_RF_lower <- est$low
df$RR1D_pos_RF_upper <- est$high
est <- process_ratio_proportions(umd$pos_RF_vac2doses,umd$vac2doses, umd$pos_RF_unvaccinated,umd$unvaccinated)
df$RR2D_pos_RF <- est$val
df$RR2D_pos_RF_lower <- est$low
df$RR2D_pos_RF_upper <- est$high

df$efficacy_RF <- 1 - df$RR_pos_RF
df$efficacy_RF_lower <- 1 - df$RR_pos_RF_upper
df$efficacy_RF_upper <- 1 - df$RR_pos_RF_lower
df$efficacy1D_RF <- 1 - df$RR1D_pos_RF
df$efficacy1D_RF_lower <- 1 - df$RR1D_pos_RF_upper
df$efficacy1D_RF_upper <- 1 - df$RR1D_pos_RF_lower
df$efficacy2D_RF <- 1 - df$RR2D_pos_RF
df$efficacy2D_RF_lower <- 1 - df$RR2D_pos_RF_upper
df$efficacy2D_RF_upper <- 1 - df$RR2D_pos_RF_lower

confirmed$date <- as.Date(confirmed$date)
confirmed$p_cases_smooth <- rollmean(confirmed$p_cases_active, smooth_param, fill=NA, align = "right")
confirmed <- confirmed %>% select(date, p_confirmed=p_cases_smooth)

df <- merge(df, confirmed, on='date')

fwrite(df, file=paste0(data_path, file_short_csv))

# cat(iso2, dim(df), colnames(df), "\n")

# -- Select dates
start_date <- as.Date("2021-01-01")
end_date <- max(df$date)
if (length(args) > 1) {
  start_date <- as.Date(args[2])
}
cat("Start date:", as.character(start_date), "\n")
if (length(args) > 2) {
  end_date <- as.Date(args[3])
}
cat("End date:", as.character(end_date), "\n")

df <- df[which((df$date >= start_date) & (df$date <= end_date)),]

# Variants
variants <- variants[which((variants$date >= start_date) & (variants$date <= end_date)),]

var_delta <- variants[which((variants$variant=="Delta")),]
cat("Delta dates (date, perc, num_seq, RR, RR_low, RR_high, PV, PV_error):\n")
for (i in seq(1,nrow(var_delta))) {
  d <- var_delta[i,"date"]
  cat(iso2, "& Delta &", as.character(d), "&", var_delta[i,"perc_sequences"], "&", var_delta[i,"num_sequences_total"], "& ")
  j <- which(df$date==d)
  cat(df[j,"RR_pos_RF"], "(", df[j,"RR_pos_RF_lower"], ",", df[j,"RR_pos_RF_upper"], ") & ")
  cat(df[j,"PV_pos_RF"], "(", df[j,"PV_pos_RF_error"], ")\n")
}
date_delta <- var_delta[1,"date"]

var_omicron <- variants[which((variants$variant=="Omicron")),]
cat("Omicron dates:\n")
for (i in seq(1,nrow(var_omicron))) {
  d <- var_omicron[i,"date"]
  cat(iso2, "& Omicron &", as.character(d), "&", var_omicron[i,"perc_sequences"], "&", var_omicron[i,"num_sequences_total"], "& ")
  j <- which(df$date==d)
  cat(df[j,"RR_pos_RF"], "(", df[j,"RR_pos_RF_lower"], ",", df[j,"RR_pos_RF_upper"], ") & ")
  cat(df[j,"PV_pos_RF"], "(", df[j,"PV_pos_RF_error"], ")\n")
}

cat("Today:\n")
j <- which(df$date==end_date)
cat(iso2, "& Omicron &", as.character(end_date), "&", 
    df[j,"RR_pos_RF"], "(", df[j,"RR_pos_RF_lower"], ",",  df[j,"RR_pos_RF_upper"], ") & ",
    df[j,"PV_pos_RF"], "(", df[j,"PV_pos_RF_error"], ")\n")

# Correlation
ret <- country_name_iso3(iso2)

cat("Correlation:\n")
cat("Confirmed | Tested:\n")
cat("CLI & str-CLI & cla-CLI & bro-CLI & RF & Tested & CLI & str-CLI & cla-CLI & bro-CLI & RF \n")
cat(ret$name, "& ")
cat(format(round(cor(df$p_confirmed, df$p_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_confirmed, df$p_stringent_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_confirmed, df$p_classic_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_confirmed, df$p_broad_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_confirmed, df$p_pos_RF), 2), nsmall = 2), "& ")

cat(format(round(cor(df$p_tested, df$p_confirmed), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_tested, df$p_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_tested, df$p_stringent_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_tested, df$p_classic_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_tested, df$p_broad_cli), 2), nsmall = 2), "& ")
cat(format(round(cor(df$p_tested, df$p_pos_RF), 2), nsmall = 2), "\\\\ \n")
