# library(lubridate)
library(useful, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
# library(readr)
# library(cowplot)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)

variant_file <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/variants/covid-variants.csv"
country_file <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
data_file <- "./data/variants-monthly.csv"

start_date <- as.Date("2021-12-15")
# end_date <- as.Date("2021-12-31")
end_date <- Sys.Date()

ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

cnames <- fread(country_file, data.table = FALSE)
var <- fread(variant_file, data.table = FALSE)
var <- var[which(var$perc_sequences>0),]
var$date <- as.Date(var$date)
var <- var[which(var$date>=start_date & var$date<=end_date),]

country_list <- unique(var$location)
cat(country_list)

country <- c()
ISO2 <- c()
total_seq <- c()
omicron_seq <- c()

for (co in country_list) {
  cat(co," ")
  df_country <- var[which(var$location==co),]
  name_line <- cnames[which(cnames$country==co),]
  if (nrow(name_line)>0) {
    iso2 <- name_line$ISO2[1]
  } else {
    iso2 <- NA
  }
  country <- c(country, co)
  ISO2 <- c(ISO2, iso2)
  total_seq <- c(total_seq, sum(df_country$num_sequences))
  df_country <- df_country[which(df_country$variant=="Omicron"),]
  omicron_seq <- c(omicron_seq, sum(df_country$num_sequences))
}

df <- data.frame(ISO2=ISO2,country=country,total_seq=total_seq,omicron_seq=omicron_seq)

est <- process_ratio(df$omicron_seq, df$total_seq)
df$omicron_prevalence <- est$val
df$omicron_prevalence_error <- est$error
df$omicron_prevalence_low <- est$low
df$omicron_prevalence_high <- est$high

# df <- NULL

# df <- df[order(df$variant, df$perc_sequences,	df$num_sequences_total),]
# df <- df[order(df$variant, df$perc_sequences, decreasing = TRUE),]
df <- df[order(df$omicron_prevalence, df$total_seq, decreasing = TRUE),]
fwrite(df, file=data_file)

# for (i in seq(1,nrow(df))) {
#   cat(df[i,"country"], "&", df[i,"ISO2"], "&", df[i,"omicron_prevalence"], "&", df[i,"omicron_seq"], "&", df[i,"total_seq"], "\\\\ \n")
# }

