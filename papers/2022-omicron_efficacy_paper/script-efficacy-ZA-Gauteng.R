library(useful, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)

umd_path <- "./"
confirmed_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-confirmed/PlotData/"
# variants_file <- "./data/variants.csv"
efficacy_gauteng_table <- "./plots/table_efficacy_gauteng.tex"
data_path <- "./data/"

quarter_list <- c("2021-Q2", "2021-Q3", "2021-Q4")

iso2 <- "ZA"
region <- "Gauteng"
cat("** Country:", iso2, "\n")
cat("** region:", region, "\n")

smooth_param <- 14
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  # numerator <- rollsum(numerator, smooth_param, fill=NA, align = "right")
  # denominator <- rollsum(denominator, smooth_param, fill=NA, align = "right")
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

process_ratio_proportions <- function(x1, n1, x2, n2){
  # x1 <- rollsum(x1, smooth_param, fill=NA, align = "right")
  # n1 <- rollsum(n1, smooth_param, fill=NA, align = "right")
  # x2 <- rollsum(x2, smooth_param, fill=NA, align = "right")
  # n2 <- rollsum(n2, smooth_param, fill=NA, align = "right")
  pi1 <- x1/n1
  pi2 <- x2/n2
  p_est <- pmin(1,pi1/pi2)
  se <- sqrt((1-pi1)/(pi1*n1)+(1-pi2)/(pi2*n2))
  return(list(val=p_est, low=pmax(0,p_est*exp(-z*se)), high=pmin(1,p_est*exp(+z*se)), std=se))
}

print_line <- function(method, est1, est2, est3) {
  cat(paste0("\\textsf{", method, "} & "))
  cat(paste0(format(round(1-est1$val, 2), nsmall = 2), " [", 
             format(round(1-est1$high, 2), nsmall = 2), ",", 
             format(round(1-est1$low, 2), nsmall = 2), "] & "))
  cat(paste0(format(round(1-est2$val, 2), nsmall = 2), " [", 
             format(round(1-est2$high, 2), nsmall = 2), ",", 
             format(round(1-est2$low, 2), nsmall = 2), "] & "))
  cat(paste0(format(round(1-est3$val, 2), nsmall = 2), " [", 
             format(round(1-est3$high, 2), nsmall = 2), ",", 
             format(round(1-est3$low, 2), nsmall = 2), "] \\\\ \n"))
}

print_last_line <- function(method, est1, est2, est3) {
  cat(paste0("\\textsf{", method, "} & "))
  cat(paste0(format(round(1-est1$val, 2), nsmall = 2), " [", 
             format(round(1-est1$high, 2), nsmall = 2), ",", 
             format(round(1-est1$low, 2), nsmall = 2), "] & "))
  cat(paste0(format(round(1-est2$val, 2), nsmall = 2), " [", 
             format(round(1-est2$high, 2), nsmall = 2), ",", 
             format(round(1-est2$low, 2), nsmall = 2), "] & "))
  cat(paste0(format(round(1-est3$val, 2), nsmall = 2), " [", 
             format(round(1-est3$high, 2), nsmall = 2), ",", 
             format(round(1-est3$low, 2), nsmall = 2), "] \n"))
}

# Read files
file_short_csv <- paste0(iso2,".csv")
file_short_rds <- paste0(iso2,".rds")
umd <- NULL
for (quarter in quarter_list) {
  input_path <- paste0(umd_path, quarter, "/aggregates/region/")
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

umd <- umd[which(umd$region_agg==region),]

umd$date <- as.Date(umd$date)

  start_date1 <- as.Date("2021-06-18")
  start_date2 <- as.Date("2021-08-09")
  start_date3 <- as.Date("2021-12-01")
  end_date1 <- as.Date("2021-07-18")
  end_date2 <- as.Date("2021-09-06")
  # end_date3 <- max(umd$date)
  end_date3 <- as.Date("2021-12-31")

df1 <- umd[which((umd$date >= start_date1) & (umd$date <= end_date1)),]
df2 <- umd[which((umd$date >= start_date2) & (umd$date <= end_date2)),]
df3 <- umd[which((umd$date >= start_date3) & (umd$date <= end_date3)),]

sink(efficacy_gauteng_table)

cat("& Jun-Jul & Aug-Sep & Dec \\\\
Method & Efficacy [95\\%CI] & Efficacy [95\\%CI] & Efficacy [95\\%CI] \\\\ \n")

# Vaccinated 
cat("\\hline & \\multicolumn{3}{c}{Vaccinated} \\\\ \\hline \n")

# Random Forest
est1 <- process_ratio_proportions(sum(df1$pos_RF_vaccinated), sum(df1$vaccinated), 
                                  sum(df1$pos_RF_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$pos_RF_vaccinated), sum(df2$vaccinated), 
                                  sum(df2$pos_RF_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$pos_RF_vaccinated), sum(df3$vaccinated), 
                                  sum(df3$pos_RF_unvaccinated), sum(df3$unvaccinated))
print_line("Random Forest", est1, est2, est3)

# UMD CLI
est1 <- process_ratio_proportions(sum(df1$cli_vaccinated), sum(df1$vaccinated), 
                                  sum(df1$cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$cli_vaccinated), sum(df2$vaccinated), 
                                  sum(df2$cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$cli_vaccinated), sum(df3$vaccinated), 
                                  sum(df3$cli_unvaccinated), sum(df3$unvaccinated))
print_line("UMD CLI", est1, est2, est3)

# Stringent CLI
est1 <- process_ratio_proportions(sum(df1$stringent_cli_vaccinated), sum(df1$vaccinated), 
                                  sum(df1$stringent_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$stringent_cli_vaccinated), sum(df2$vaccinated), 
                                  sum(df2$stringent_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$stringent_cli_vaccinated), sum(df3$vaccinated), 
                                  sum(df3$stringent_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Stringent CLI", est1, est2, est3)

# Classic CLI
est1 <- process_ratio_proportions(sum(df1$classic_cli_vaccinated), sum(df1$vaccinated), 
                                  sum(df1$classic_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$classic_cli_vaccinated), sum(df2$vaccinated), 
                                  sum(df2$classic_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$classic_cli_vaccinated), sum(df3$vaccinated), 
                                  sum(df3$classic_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Classic CLI", est1, est2, est3)

# Broad CLI
est1 <- process_ratio_proportions(sum(df1$broad_cli_vaccinated), sum(df1$vaccinated), 
                                  sum(df1$broad_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$broad_cli_vaccinated), sum(df2$vaccinated), 
                                  sum(df2$broad_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$broad_cli_vaccinated), sum(df3$vaccinated), 
                                  sum(df3$broad_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Broad CLI", est1, est2, est3)

# Vaccinated 1 dose
cat("\\hline & \\multicolumn{3}{c}{Vaccinated with one dose} \\\\ \\hline \n")

# Random Forest
est1 <- process_ratio_proportions(sum(df1$pos_RF_vac1dose), sum(df1$vac1dose), 
                                  sum(df1$pos_RF_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$pos_RF_vac1dose), sum(df2$vac1dose), 
                                  sum(df2$pos_RF_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$pos_RF_vac1dose), sum(df3$vac1dose), 
                                  sum(df3$pos_RF_unvaccinated), sum(df3$unvaccinated))
print_line("Random Forest", est1, est2, est3)

# UMD CLI
est1 <- process_ratio_proportions(sum(df1$cli_vac1dose), sum(df1$vac1dose), 
                                  sum(df1$cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$cli_vac1dose), sum(df2$vac1dose), 
                                  sum(df2$cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$cli_vac1dose), sum(df3$vac1dose), 
                                  sum(df3$cli_unvaccinated), sum(df3$unvaccinated))
print_line("UMD CLI", est1, est2, est3)

# Stringent CLI
est1 <- process_ratio_proportions(sum(df1$stringent_cli_vac1dose), sum(df1$vac1dose), 
                                  sum(df1$stringent_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$stringent_cli_vac1dose), sum(df2$vac1dose), 
                                  sum(df2$stringent_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$stringent_cli_vac1dose), sum(df3$vac1dose), 
                                  sum(df3$stringent_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Stringent CLI", est1, est2, est3)

# Classic CLI
est1 <- process_ratio_proportions(sum(df1$classic_cli_vac1dose), sum(df1$vac1dose), 
                                  sum(df1$classic_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$classic_cli_vac1dose), sum(df2$vac1dose), 
                                  sum(df2$classic_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$classic_cli_vac1dose), sum(df3$vac1dose), 
                                  sum(df3$classic_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Classic CLI", est1, est2, est3)

# Broad CLI
est1 <- process_ratio_proportions(sum(df1$broad_cli_vac1dose), sum(df1$vac1dose), 
                                  sum(df1$broad_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$broad_cli_vac1dose), sum(df2$vac1dose), 
                                  sum(df2$broad_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$broad_cli_vac1dose), sum(df3$vac1dose), 
                                  sum(df3$broad_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Broad CLI", est1, est2, est3)

# Vaccinated 2 doses
cat("\\hline & \\multicolumn{3}{c}{Vaccinated with two doses} \\\\ \\hline \n")

# Random Forest
est1 <- process_ratio_proportions(sum(df1$pos_RF_vac2doses), sum(df1$vac2doses), 
                                  sum(df1$pos_RF_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$pos_RF_vac2doses), sum(df2$vac2doses), 
                                  sum(df2$pos_RF_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$pos_RF_vac2doses), sum(df3$vac2doses), 
                                  sum(df3$pos_RF_unvaccinated), sum(df3$unvaccinated))
print_line("Random Forest", est1, est2, est3)

# UMD CLI
est1 <- process_ratio_proportions(sum(df1$cli_vac2doses), sum(df1$vac2doses), 
                                  sum(df1$cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$cli_vac2doses), sum(df2$vac2doses), 
                                  sum(df2$cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$cli_vac2doses), sum(df3$vac2doses), 
                                  sum(df3$cli_unvaccinated), sum(df3$unvaccinated))
print_line("UMD CLI", est1, est2, est3)

# Stringent CLI
est1 <- process_ratio_proportions(sum(df1$stringent_cli_vac2doses), sum(df1$vac2doses), 
                                  sum(df1$stringent_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$stringent_cli_vac2doses), sum(df2$vac2doses), 
                                  sum(df2$stringent_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$stringent_cli_vac2doses), sum(df3$vac2doses), 
                                  sum(df3$stringent_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Stringent CLI", est1, est2, est3)

# Classic CLI
est1 <- process_ratio_proportions(sum(df1$classic_cli_vac2doses), sum(df1$vac2doses), 
                                  sum(df1$classic_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$classic_cli_vac2doses), sum(df2$vac2doses), 
                                  sum(df2$classic_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$classic_cli_vac2doses), sum(df3$vac2doses), 
                                  sum(df3$classic_cli_unvaccinated), sum(df3$unvaccinated))
print_line("Classic CLI", est1, est2, est3)

# Broad CLI
est1 <- process_ratio_proportions(sum(df1$broad_cli_vac2doses), sum(df1$vac2doses), 
                                  sum(df1$broad_cli_unvaccinated), sum(df1$unvaccinated))
est2 <- process_ratio_proportions(sum(df2$broad_cli_vac2doses), sum(df2$vac2doses), 
                                  sum(df2$broad_cli_unvaccinated), sum(df2$unvaccinated))
est3 <- process_ratio_proportions(sum(df3$broad_cli_vac2doses), sum(df3$vac2doses), 
                                  sum(df3$broad_cli_unvaccinated), sum(df3$unvaccinated))
print_last_line("Broad CLI", est1, est2, est3)

sink()