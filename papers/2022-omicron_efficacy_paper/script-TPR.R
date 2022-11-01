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
owid_file <-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
tpr_table_file <- "./plots/table_tpr.tex"

countries <- c(
  "BR", "MX", "JP", "DE", "IT", "FR", "IN", "UA", "SE", "CA",
  "GB", "VN", "AR", "RU", "HU", "RO", "PL", "TH", "AU", "ES",
  # "CO", "PH", "NL", "ID", "TW", "TR", "EG", "PT", "CZ", "GR",
  # "DK", "AT", "PE", "CL", 
  "ZA"
  # "NO", "BE", "CH", "VE", "BG",
  # "NZ", "EC", "IQ", "IL", "BD", "SK", "NG", "MY", "RS", "KR",
  # "DZ", "FI", "PK", "HR", "GT", "PR", "KE", "IE", "BO", "CR",
  # "SV", "NI", "MA", "UY", "JO", "TN", "SI", "BY", "MM", "SA",
  # "NP", "DO", "HN", "LY", "PY", "AE", "SD", "HK", "BA", "KZ",
  # "PA", "LB", "SG", "GH", "LK", "MD", "CI", "PS", "ET", "AL",
  # "UZ", "AZ", "CM", "AO", "YE", "AF", "TZ", "AM", "MZ", "KW",
  # "KH", "KG", "QA", "LA", "CD", "HT", "ML", "BJ", "SN", "BF",
  # "MG", "OM", "GN", "AD", "AS", "MR", "ZW", "LU", "CY", "BH",
  # "GE", "BS", "ME", "EE", "BZ", "UG", "MT", "LT", "ZM", "SS",
  # "LV", "GA", "DM", "CG", "BW", "BM", "IS", "TL"
)

quarter_list <- c("2021-Q2", "2021-Q3", "2021-Q4")

start_date <- as.Date("2021-06-18")
end_date <- as.Date("2021-12-31")
# end_date <- Sys.Date()

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


owid <- fread(owid_file, data.table = FALSE)
owid$date <- as.Date(owid$date)
owid <- owid %>%
  select(iso_code, date, positive_rate)

dfo <- data.frame(ISO2=countries,
                 country=NA,
                 ISO3=NA,
                 owid_tpr=NA,
                 ctis_tpr=NA,
                 owid_cor=NA,
                 ctis_cor=NA,
                 owid_ctis_cor=NA,
                 RF_cor=NA,
                 cli_cor=NA,
                 scli_cor=NA,
                 ccli_cor=NA,
                 bcli_cor=NA
)

for (iso2 in countries) {
  
  # cat("** Country:", iso2, "\n")
  pos_iso2 <- which(dfo$ISO2==iso2)
  
  # Read files
  file_short_csv <- paste0(iso2,".csv")
  file_short_rds <- paste0(iso2,".rds")
  umd <- NULL
  for (quarter in quarter_list) {
    input_path <- paste0(umd_path, quarter, "/aggregates/country/")
    file_input_csv <- paste0(input_path, file_short_csv)
    file_input_rds <- paste0(input_path, file_short_rds)
    # if (file.exists(file_input)){
    # umd_aux <- readRDS(file=file_input_rds)
    umd_aux <- fread(file_input_csv, data.table = FALSE)
    # cat(iso2, quarter, dim(umd_aux), "\n")
    # umd_aux <- umd_aux %>% 
    #   select(-contains("."))
    # cat(colnames(umd_aux), "\n")
    # umd <- rbind(umd, umd_aux)  
    umd <- dplyr::bind_rows(umd, umd_aux) 
    # cat(iso2, quarter, "Total:", dim(umd), "\n")
    # }
  }
  
  confirmed_file <- paste0(confirmed_path, iso2, "-estimate.csv")
  confirmed <- fread(confirmed_file, data.table = FALSE)
  
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
  
  ret <- country_name_iso3(iso2)
  # cat("iso3", ret$iso3, "\n")
  owid_aux <- owid[which(owid$iso_code==ret$iso3),]
  df <- merge(df, owid_aux, on='date')
  
  # df$positive_rate[which(is.na(df$positive_rate))] <- 0
  # df$positive_rate_smooth <- rollmean(df$positive_rate, smooth_param, fill=NA, align = "right")
  
  df <- df[which((df$date >= start_date) & (df$date <= end_date)),]
  
  dfo$country[pos_iso2] <- ret$name
  dfo$ISO3[pos_iso2] <- ret$iso3
  
  dfo$owid_tpr[pos_iso2] <- mean(df$positive_rate, na.rm = T)
  dfo$ctis_tpr[pos_iso2] <- mean(df$p_tested, na.rm = T)
  dfo$owid_cor[pos_iso2] <- cor(df$positive_rate, df$p_confirmed)
  # if (is.na(dfo$owid_cor[pos_iso2])) {dfo$owid_cor[pos_iso2] <- 0}
  dfo$ctis_cor[pos_iso2] <- cor(df$p_tested, df$p_confirmed)
  dfo$owid_ctis_cor[pos_iso2] <- cor(df$positive_rate, df$p_tested)
  # if (is.na(dfo$owid_ctis_cor[pos_iso2])) {dfo$owid_ctis_cor[pos_iso2] <- 0}
  dfo$sqerror[pos_iso2] <- sum((df$positive_rate - df$p_tested)^2)
  
  dfo$RF_cor[pos_iso2] <- cor(df$p_confirmed, df$p_pos_RF)
  dfo$cli_cor[pos_iso2] <- cor(df$p_confirmed, df$p_cli)
  dfo$scli_cor[pos_iso2] <- cor(df$p_confirmed, df$p_stringent_cli)
  dfo$ccli_cor[pos_iso2] <- cor(df$p_confirmed, df$p_classic_cli)
  dfo$bcli_cor[pos_iso2] <- cor(df$p_confirmed, df$p_broad_cli)
  
}

cat("\n \n")

bold_if_large <- function(x) {
  # if (!is.numeric(x)) return(x)
  if (is.nan(x)) return("--")
  if (as.numeric(x)>=0.9) return(paste0("\\textbf{",x,"}"))
  return (x)
}

bold_if_small <- function(x) {
  # if (!is.numeric(x)) return(x)
  # if (is.na(x)) return("NA")
  if (x=="NaN") return("--")
  if (as.numeric(x)<=0.1) return(paste0("\\textbf{",x,"}"))
  return (x)
}

row_color <- function(x,y) {
  if (x=="NaN") return("")
  if (as.numeric(x)<=0.1 & as.numeric(y)<=0.1) {
    return("\\rowcolor[gray]{0.7} \n")
  }
  if (as.numeric(x)<=0.1 | as.numeric(y)<=0.1) {
    return("\\rowcolor[gray]{0.9} \n")
  }
  return("")
}


dfo <- dfo[order(dfo$country, dfo$ctis_tpr,	dfo$owid_tpr),]

# cat("Country & OWID_TPR & CTIS_TPR & RF & CLI & str-CLI & cla-CLI & bro-CLI \n")
sink(tpr_table_file)
cat("& & & \\multicolumn{5}{c}{Pearson correlation with \\textsf{Confirmed}} \\\\ \\hline \n")
cat("& OWID & CTIS & \\textsf{Random} & \\textsf{UMD} & \\textsf{Stringent} & \\textsf{Classic} & \\textsf{Broad} \\\\ \n")
cat("Country & TPR & TPR & \\textsf{Forest} & \\textsf{CLI} & \\textsf{CLI} & \\textsf{CLI} & \\textsf{CLI} \\\\ \\hline \n")
for (i in seq(1,nrow(dfo))) {
  
  cat(row_color(dfo$owid_tpr[i],dfo$ctis_tpr[i]))
  cat(dfo$country[i], "& ")
  
  cat(bold_if_small(format(round(dfo$owid_tpr[i], 2), nsmall = 2)), "& ")
  cat(bold_if_small(format(round(dfo$ctis_tpr[i], 2), nsmall = 2)), "& ")
  # cat(format(round(dfo$owid_cor[i], 2), nsmall = 2), "& ")
  # cat(format(round(dfo$ctis_cor[i], 2), nsmall = 2), "& ")
  # cat(format(round(dfo$owid_ctis_cor[i], 2), nsmall = 2), "& ")
  # cat(bold_if_small(format(round(dfo$sqerror[i], 2), nsmall = 2)), "& ")
  
  cat(bold_if_large(format(round(dfo$RF_cor[i], 2), nsmall = 2)), "& ")
  cat(bold_if_large(format(round(dfo$cli_cor[i], 2), nsmall = 2)), "& ")
  cat(bold_if_large(format(round(dfo$scli_cor[i], 2), nsmall = 2)), "& ")
  cat(bold_if_large(format(round(dfo$ccli_cor[i], 2), nsmall = 2)), "& ")
  cat(bold_if_large(format(round(dfo$bcli_cor[i], 2), nsmall = 2)))
  
  if (i != nrow(dfo)) cat("\\\\ \n")
  else cat("\n")
}
sink()
