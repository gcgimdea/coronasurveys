# library(lubridate)
library(useful, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
# library(readr)
# library(cowplot)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)
# library(DescTools)

umd_path <- "./"
country_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
vaccines_file <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
# variants_file <- "./data/variants.csv"
variants_file <- "./data/variants-monthly.csv"
# plots_path <- "./plots/"
data_path <- "./data/"
efficacy_file <- "./data/efficacy-data.csv"

min_sequences <- 30
min_samples <- 1000
min_prev <- 0.01

start_date1 <- as.Date("2021-10-01")
end_date1 <- as.Date("2021-10-31")
start_date2 <- as.Date("2021-12-01")
# start_date2 <- as.Date("2021-12-15")
end_date2 <- as.Date("2021-12-31")
# end_date2 <- as.Date("2022-01-10")
# end_date2 <- Sys.Date()

cat("Dates 1:", as.character(start_date1), as.character(end_date1), "\n")
cat("Dates 2:", as.character(start_date2), as.character(end_date2), "\n")

countries <- c(
  "BR", "MX", "JP", "DE", "IT", "FR", "IN", "UA", "SE", "CA",
  "GB", "VN", "AR", "RU", "HU", "RO", "PL", "TH", "AU", "ES",
  "CO", "PH", "NL", "ID", "TW", "TR", "EG", "PT", "CZ", "GR",
  "DK", "AT", "PE", "CL", "ZA", "NO", "BE", "CH", "VE", "BG",
  "NZ", "EC", "IQ", "IL", "BD", "SK", "NG", "MY", "RS", "KR"
  # "DZ", "FI", "PK", "HR", "GT", "PR", "KE", "IE", "BO", "CR"
  # "SV", "NI", "MA", "UY", "JO", "TN", "SI", "BY", "MM", "SA"
  # "NP", "DO", "HN", "LY", "PY", "AE", "SD", "HK", "BA", "KZ",
  # "PA", "LB", "SG", "GH", "LK", "MD", "CI", "PS", "ET", "AL",
  # "UZ", "AZ", "CM", "AO", "YE", "AF", "TZ", "AM", "MZ", "KW",
  # "KH", "KG", "QA", "LA", "CD", "HT", "ML", "BJ", "SN", "BF",
  # "MG", "OM", "GN", "AD", "AS", "MR", "ZW", "LU", "CY", "BH",
  # "GE", "BS", "ME", "EE", "BZ", "UG", "MT", "LT", "ZM", "SS",
  # "LV", "GA", "DM", "CG", "BW", "BM", "IS", "TL"
)

# Other definitions
quarter_list <- c("2021-Q2", "2021-Q3", "2021-Q4")

smooth_param <- 14
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

# Computes ratio of two values. If outside [0,1] returns NA. Confidence intervals are in [0,1]
process_ratio <- function(numerator, denominator){
  p_est <- numerator/denominator
  if (!is.na(p_est)) {
    if ((p_est>1) | (p_est<0)) {
      p_est <- NA
    }
  }
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

# Computes ratio of proportios. If outside [0,1] returns NA. Confidence intervals are in [0,1]
process_ratio_proportions <- function(x1, n1, x2, n2){
  pi1 <- x1/n1
  pi2 <- x2/n2
  p_est <- pi1/pi2
  if ((n1 < min_samples) | (n2 < min_samples)) p_est <- NA
  if ((pi1 < min_prev) | (pi2 < min_prev)) p_est <- NA
  if (!is.na(p_est)) {
    if ((p_est>1) | (p_est<0)) p_est <- NA
  }
  se <- sqrt((1-pi1)/(pi1*n1)+(1-pi2)/(pi2*n2))
  return(list(val=p_est, low=pmax(0,p_est*exp(-z*se)), high=pmin(1,p_est*exp(+z*se)), 
              gap=p_est*exp(+z*se)-p_est*exp(-z*se), std=se))
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

vacc <- fread(vaccines_file, data.table = FALSE)
vacc$date <- as.Date(vacc$date)
vacc <- vacc[which(vacc$date <= end_date2),]

variants <- fread(variants_file, data.table = FALSE)
# variants$date <- as.Date(variants$date)

df <- data.frame(ISO2=countries)

for (iso2 in countries) {
  
  pos_iso2 <- which(df$ISO2==iso2)
  
  # Read files
  file_short_csv <- paste0(iso2,".csv")
  file_short_rds <- paste0(iso2,".rds")
  umd <- NULL
  for (quarter in quarter_list) {
    input_path <- paste0(umd_path, quarter, "/aggregates/country/")
    file_input_csv <- paste0(input_path, file_short_csv)
    file_input_rds <- paste0(input_path, file_short_rds)
    # umd_aux <- readRDS(file=file_input_rds)
    umd_aux <- fread(file_input_csv, data.table = FALSE)
    # umd <- rbind(umd, umd_aux)  
    umd <- dplyr::bind_rows(umd, umd_aux)
  }
  
  ret <- country_name_iso3(iso2)
  df$country_name[pos_iso2] <- ret$name
  df$ISO3[pos_iso2] <- ret$iso3
  
  # Vaccines
  vac_df <- vacc[which(vacc$iso_code==df$ISO3[pos_iso2]),]
  df$total_vaccinations_per_hundred[pos_iso2] <- max(vac_df$total_vaccinations_per_hundred, na.rm = TRUE)
  df$people_vaccinated_per_hundred[pos_iso2] <- max(vac_df$people_vaccinated_per_hundred, na.rm = TRUE)
  df$people_fully_vaccinated_per_hundred[pos_iso2] <- max(vac_df$people_fully_vaccinated_per_hundred, na.rm = TRUE)

  vac_df$total_boosters_per_hundred[is.na(vac_df$total_boosters_per_hundred)] <- 0
  df$total_boosters_per_hundred[pos_iso2] <- max(vac_df$total_boosters_per_hundred, na.rm = TRUE)

  
  vac_df <- vac_df[which(vac_df$total_vaccinations>0),]
  df$start_vacc[pos_iso2] <- min(vac_df$date)
  
  # # Prevalence of Omicron

  df_var <- variants[which(variants$ISO2==iso2),]
  if (nrow(df_var)>0) {
    df$prev_omicron[pos_iso2] <- df_var[1,"omicron_prevalence"]
    df$prev_omicron_error[pos_iso2] <- df_var[1,"omicron_prevalence_error"]
    df$prev_omicron_low[pos_iso2] <- df_var[1,"omicron_prevalence_low"]
    df$prev_omicron_high[pos_iso2] <- df_var[1,"omicron_prevalence_high"]
    df$num_sequences_total[pos_iso2] <- df_var[1,"total_seq"]
  } else {
    df$prev_omicron[pos_iso2] <- 0
    df$prev_omicron_error[pos_iso2] <- 0
    df$prev_omicron_low[pos_iso2] <- 0
    df$prev_omicron_high[pos_iso2] <- 0
    df$num_sequences_total[pos_iso2] <- 0
  }
  
  # Filter by dates
  umd$date <- as.Date(umd$date)
  df1 <- umd[which((umd$date >= start_date1) & (umd$date <= end_date1)),]
  df2 <- umd[which((umd$date >= start_date2) & (umd$date <= end_date2)),]
  
  # Sample sizes
  df$count_1[pos_iso2] <- sum(df1$count)
  df$vaccinated_1[pos_iso2] <- sum(df1$vaccinated)
  df$unvaccinated_1[pos_iso2] <- sum(df1$unvaccinated)
  df$vac1dose_1[pos_iso2] <- sum(df1$vac1dose)
  df$vac2doses_1[pos_iso2] <- sum(df1$vac2doses)
  
  df$count_2[pos_iso2] <- sum(df2$count)
  df$vaccinated_2[pos_iso2] <- sum(df2$vaccinated)
  df$unvaccinated_2[pos_iso2] <- sum(df2$unvaccinated)
  df$vac1dose_2[pos_iso2] <- sum(df2$vac1dose)
  df$vac2doses_2[pos_iso2] <- sum(df2$vac2doses)
  
  # Positive cases with classifiers
  
  df$pos_RF_1[pos_iso2] <- sum(df1$pos_RF)
  df$pos_RF_vaccinated_1[pos_iso2] <- sum(df1$pos_RF_vaccinated)
  df$pos_RF_unvaccinated_1[pos_iso2] <- sum(df1$pos_RF_unvaccinated)
  df$pos_RF_vac1dose_1[pos_iso2] <- sum(df1$pos_RF_vac1dose)
  df$pos_RF_vac2doses_1[pos_iso2] <- sum(df1$pos_RF_vac2doses)
  
  df$pos_RF_2[pos_iso2] <- sum(df2$pos_RF)
  df$pos_RF_vaccinated_2[pos_iso2] <- sum(df2$pos_RF_vaccinated)
  df$pos_RF_unvaccinated_2[pos_iso2] <- sum(df2$pos_RF_unvaccinated)
  df$pos_RF_vac1dose_2[pos_iso2] <- sum(df2$pos_RF_vac1dose)
  df$pos_RF_vac2doses_2[pos_iso2] <- sum(df2$pos_RF_vac2doses)
  
  # Prevalence
  est <- process_ratio(sum(df1$pos_RF), sum(df1$count))
  df$p_pos_RF_1[pos_iso2] <- est$val
  df$p_pos_RF_1_error[pos_iso2] <- est$error
  est <- process_ratio(sum(df2$pos_RF), sum(df2$count))
  df$p_pos_RF_2[pos_iso2] <- est$val
  df$p_pos_RF_2_error[pos_iso2] <- est$error
  
  # Prevalence vaccinated, unvaccinated, 1D, 2D
  est <- process_ratio(sum(df1$pos_RF_vaccinated),sum(df1$vaccinated))
  df$PV_pos_RF_1[pos_iso2] <- est$val
  df$PV_pos_RF_1_error[pos_iso2] <- est$error
  est <- process_ratio(sum(df2$pos_RF_vaccinated),sum(df2$vaccinated))
  df$PV_pos_RF_2[pos_iso2] <- est$val
  df$PV_pos_RF_2_error[pos_iso2] <- est$error
  
  est <- process_ratio(sum(df1$pos_RF_unvaccinated),sum(df1$unvaccinated))
  df$PU_pos_RF_1[pos_iso2] <- est$val
  df$PU_pos_RF_1_error[pos_iso2] <- est$error
  est <- process_ratio(sum(df2$pos_RF_unvaccinated),sum(df2$unvaccinated))
  df$PU_pos_RF_2[pos_iso2] <- est$val
  df$PU_pos_RF_2_error[pos_iso2] <- est$error
  
  est <- process_ratio(sum(df1$pos_RF_vac1dose),sum(df1$vac1dose))
  df$PV1D_pos_RF_1[pos_iso2] <- est$val
  df$PV1D_pos_RF_1_error[pos_iso2] <- est$error
  est <- process_ratio(sum(df2$pos_RF_vac1dose),sum(df2$vac1dose))
  df$PV1D_pos_RF_2[pos_iso2] <- est$val
  df$PV1D_pos_RF_2_error[pos_iso2] <- est$error
  
  est <- process_ratio(sum(df1$pos_RF_vac2doses),sum(df1$vac2doses))
  df$PV2D_pos_RF_1[pos_iso2] <- est$val
  df$PV2D_pos_RF_1_error[pos_iso2] <- est$error
  est <- process_ratio(sum(df2$pos_RF_vac2doses),sum(df2$vac2doses))
  df$PV2D_pos_RF_2[pos_iso2] <- est$val
  df$PV2D_pos_RF_2_error[pos_iso2] <- est$error
  
  # # Efficacy vaccinated
  
  # # RR Random Forest
  est <- process_ratio_proportions(sum(df1$pos_RF_vaccinated), sum(df1$vaccinated),
                                    sum(df1$pos_RF_unvaccinated), sum(df1$unvaccinated))
  df$RR_pos_RF_1[pos_iso2] <- est$val
  df$RR_pos_RF_1_low[pos_iso2] <- est$low
  df$RR_pos_RF_1_high[pos_iso2] <- est$high
  est <- process_ratio_proportions(sum(df2$pos_RF_vaccinated), sum(df2$vaccinated),
                                    sum(df2$pos_RF_unvaccinated), sum(df2$unvaccinated))
  df$RR_pos_RF_2[pos_iso2] <- est$val
  df$RR_pos_RF_2_low[pos_iso2] <- est$low
  df$RR_pos_RF_2_high[pos_iso2] <- est$high
  
  est <- process_ratio_proportions(sum(df1$pos_RF_vac1dose), sum(df1$vac1dose),
                                   sum(df1$pos_RF_unvaccinated), sum(df1$unvaccinated))
  df$RR_pos_RF_vac1dose_1[pos_iso2] <- est$val
  df$RR_pos_RF_vac1dose_1_low[pos_iso2] <- est$low
  df$RR_pos_RF_vac1dose_1_high[pos_iso2] <- est$high
  est <- process_ratio_proportions(sum(df2$pos_RF_vac1dose), sum(df2$vac1dose),
                                   sum(df2$pos_RF_unvaccinated), sum(df2$unvaccinated))
  df$RR_pos_RF_vac1dose_2[pos_iso2] <- est$val
  df$RR_pos_RF_vac1dose_2_low[pos_iso2] <- est$low
  df$RR_pos_RF_vac1dose_2_high[pos_iso2] <- est$high
  
  est <- process_ratio_proportions(sum(df1$pos_RF_vac2doses), sum(df1$vac2doses),
                                   sum(df1$pos_RF_unvaccinated), sum(df1$unvaccinated))
  df$RR_pos_RF_vac2doses_1[pos_iso2] <- est$val
  df$RR_pos_RF_vac2doses_1_low[pos_iso2] <- est$low
  df$RR_pos_RF_vac2doses_1_high[pos_iso2] <- est$high
  est <- process_ratio_proportions(sum(df2$pos_RF_vac2doses), sum(df2$vac2doses),
                                   sum(df2$pos_RF_unvaccinated), sum(df2$unvaccinated))
  df$RR_pos_RF_vac2doses_2[pos_iso2] <- est$val
  df$RR_pos_RF_vac2doses_2_low[pos_iso2] <- est$low
  df$RR_pos_RF_vac2doses_2_high[pos_iso2] <- est$high
  
}

# RF

df$efficacy_RF_1 <- 1-df$RR_pos_RF_1
df$efficacy_RF_1_low <- 1-df$RR_pos_RF_1_high
df$efficacy_RF_1_high <- 1-df$RR_pos_RF_1_low
df$efficacy_RF_2 <- 1-df$RR_pos_RF_2
df$efficacy_RF_2_low <- 1-df$RR_pos_RF_2_high
df$efficacy_RF_2_high <- 1-df$RR_pos_RF_2_low

df$efficacy_RF_vac1dose_1 <- 1-df$RR_pos_RF_vac1dose_1
df$efficacy_RF_vac1dose_1_low <- 1-df$RR_pos_RF_vac1dose_1_high
df$efficacy_RF_vac1dose_1_high <- 1-df$RR_pos_RF_vac1dose_1_low
df$efficacy_RF_vac1dose_2 <- 1-df$RR_pos_RF_vac1dose_2
df$efficacy_RF_vac1dose_2_low <- 1-df$RR_pos_RF_vac1dose_2_high
df$efficacy_RF_vac1dose_2_high <- 1-df$RR_pos_RF_vac1dose_2_low

df$efficacy_RF_vac2doses_1 <- 1-df$RR_pos_RF_vac2doses_1
df$efficacy_RF_vac2doses_1_low <- 1-df$RR_pos_RF_vac2doses_1_high
df$efficacy_RF_vac2doses_1_high <- 1-df$RR_pos_RF_vac2doses_1_low
df$efficacy_RF_vac2doses_2 <- 1-df$RR_pos_RF_vac2doses_2
df$efficacy_RF_vac2doses_2_low <- 1-df$RR_pos_RF_vac2doses_2_high
df$efficacy_RF_vac2doses_2_high <- 1-df$RR_pos_RF_vac2doses_2_low

# Filter by number of sequences and presence of Omicron
df <- df[which(df$num_sequences_total>=min_sequences),]
df <- df[which(df$prev_omicron>0),]

# Filter if no efficacy available in December
df <- df[which(!is.na(df$efficacy_RF_2) | 
                 !is.na(df$efficacy_RF_vac1dose_2) | 
                 !is.na(df$efficacy_RF_vac2doses_2)),]

# Write file
df <- df[order(df$country_name),]
fwrite(df, file=efficacy_file)
