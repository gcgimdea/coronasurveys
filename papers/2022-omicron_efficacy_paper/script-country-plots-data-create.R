# library(lubridate)
library(useful, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
# library(readr)
# library(cowplot)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)
# library(DescTools)

umd_path <- "./"
confirmed_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-confirmed/PlotData/"
country_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
# variants_file <- "./data/variants.csv"
plots_path <- "./plots/"
data_path <- "./data/"

quarter_list <- c("2021-Q2", "2021-Q3", "2021-Q4")

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

process_ratio_proportions <- function(x1, n1, x2, n2){
  x1 <- rollsum(x1, smooth_param, fill=NA, align = "right")
  n1 <- rollsum(n1, smooth_param, fill=NA, align = "right")
  x2 <- rollsum(x2, smooth_param, fill=NA, align = "right")
  n2 <- rollsum(n2, smooth_param, fill=NA, align = "right")
  pi1 <- x1/n1
  pi2 <- x2/n2
  p_est <- pmin(1,pi1/pi2)
  se <- sqrt((1-pi1)/(pi1*n1)+(1-pi2)/(pi2*n2))
  return(list(val=p_est, low=pmax(0,p_est*exp(-z*se)), high=pmin(1,p_est*exp(+z*se)), std=se))
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
  # file_input_rds <- paste0(input_path, file_short_rds)
  # if (file.exists(file_input)){
    # umd_aux <- readRDS(file=file_input_rds)
    umd_aux <- fread(file_input_csv, data.table = FALSE)
    # cat(iso2, quarter, dim(umd_aux), "\n")
    # umd_aux <- umd_aux %>% 
    #   select(-contains("."))
    # cat(colnames(umd_aux), "\n")
    umd <- dplyr::bind_rows(umd, umd_aux) 
    # cat(iso2, quarter, "Total:", dim(umd), "\n")
  # }
}

confirmed_file <- paste0(confirmed_path, iso2, "-estimate.csv")
confirmed <- fread(confirmed_file, data.table = FALSE)

# variants <- fread(variants_file, data.table = FALSE)
# variants$date <- as.Date(variants$date)
# variants <- variants[which(variants$ISO2==iso2),]

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

fwrite(df, file=paste0(data_path, file_short_csv))
