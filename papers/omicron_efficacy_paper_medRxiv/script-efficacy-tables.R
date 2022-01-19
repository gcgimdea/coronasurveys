# library(lubridate)
library(useful, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
# library(readr)
# library(cowplot)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)
# library(DescTools)


efficacy_file <- "./data/efficacy-data.csv"
table_omicron_prev_vacc <- "./plots/table_omicron_prev_vacc.tex"
table_omicron_1D_2D <- "./plots/table_omicron_1D_2D.tex"
table_total_samples <- "./plots/table_total_samples.tex"
table_positives <- "./plots/table_positives.tex"
table_vaccination <- "./plots/table_vaccination.tex"
table_correlations <- "./plots/table_correlations.tex"
table_means <- "./plots/table_means.tex"


df <- read.csv(file=efficacy_file)
df$start_vacc <- as.Date(df$start_vacc)

# Print data for tables

print_value <- function(x) {
  if (is.na(x)) {
    return("--")
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

print_value_corr <- function(x) {
  if (is.na(x)) {
    return("--")
  } else {
    return(format(round(x, 6), nsmall = 6))
  }
}

# print_two_values_CI <- function(x, xl, xh, y, yl, yh) {
#   return (paste0(format(round(x, 2), nsmall = 2), " [",
#                  format(round((xl), 2), nsmall = 2), ",",
#                  format(round((xh), 2), nsmall = 2), "] & ",
#                  format(round(y, 2), nsmall = 2), " [",
#                  format(round((yl), 2), nsmall = 2), ",",
#                  format(round((yh), 2), nsmall = 2), "]"
#                  )
#   )
# }

print_two_values_CI <- function(x, xl, xh, y, yl, yh) {
  val <- print_value(x)
  if (!is.na(x)) val <- paste0(val, " [", print_value(xl), ",", print_value(xh), "]")
  val <- paste0(val, " & ", print_value(y))
  if (!is.na(y)) val <- paste0(val, " [", print_value(yl), ",", print_value(yh), "]")
  return (val)
}

find_est <- function(x){
  avg <- mean(x, na.rm = T)
  se <- sd(x, na.rm = T)
  n <- length(which(!is.na(x)))
  err_ci <- qnorm(0.975)*se/sqrt(n)
  return(list(val=avg, low=avg-err_ci, high=avg+err_ci, error=err_ci, std=se))
}

# Basic statistics

sink(table_means)

cat("Vaccination status & October & December & October & December \\\\ \\hline \n")

est1 <- find_est(df$PV2D_pos_RF_1)
est2 <- find_est(df$PV2D_pos_RF_2)
cat("Vaccinated 2 doses &", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high))
est1 <- find_est(df$efficacy_RF_vac2doses_1)
est2 <- find_est(df$efficacy_RF_vac2doses_2)
cat("&", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high), "\\\\ \n")

est1 <- find_est(df$PV_pos_RF_1)
est2 <- find_est(df$PV_pos_RF_2)
cat("Vaccinated &", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high))
est1 <- find_est(df$efficacy_RF_1)
est2 <- find_est(df$efficacy_RF_2)
cat("&", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high), "\\\\ \n")

est1 <- find_est(df$PV1D_pos_RF_1)
est2 <- find_est(df$PV1D_pos_RF_2)
cat("Vaccinated 1 dose &", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high))
est1 <- find_est(df$efficacy_RF_vac1dose_1)
est2 <- find_est(df$efficacy_RF_vac1dose_2)
cat("&", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high), "\\\\ \n")

est1 <- find_est(df$PU_pos_RF_1)
est2 <- find_est(df$PU_pos_RF_2)
cat("Unvaccinated &", print_two_values_CI(est1$val, est1$low, est1$high, est2$val, est2$low, est2$high))
cat("& -- & -- \n")

sink()

# Correlation Table

# lmp <- function (modelobject) {
#   if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")  
#   f <- summary(modelobject)$fstatistic  
#   p <- pf(f[1],f[2],f[3],lower.tail=F)
#   attributes(p) <- NULL
#   return(p)
# }

sink(table_correlations)

# cat(" & Correlation & Regression & \\\\ \n")
# cat(" & coefficient & slope & P-value \\\\ \\hline \n") 
cat(" & Correlation &  \\\\ \n")
cat(" & coefficient & P-value\\\\ \\hline \n") 

cat("Prevalence omicron vs vaccination efficacy & ")
res <- cor.test(df$prev_omicron, df$efficacy_RF_2, method = "pearson")
cat(print_value_corr(res$estimate), " & ", print_value_corr(res$p.value), "\\\\ \n")

# fit.lm <- lm(df$efficacy_RF_2 ~ df$prev_omicron)
# cat(print_value_corr(coef(fit.lm)[2])," & ", print_value_corr(lmp(fit.lm))," \\\\ \n")

cat("Prevalence omicron vs vacc. efficacy 1 dose & ")
res <- cor.test(df$prev_omicron, df$efficacy_RF_vac1dose_2, method = "pearson")
cat(print_value_corr(res$estimate), " & ", print_value_corr(res$p.value), "\\\\ \n")

# fit.lm <- lm(df$efficacy_RF_vac1dose_2 ~ df$prev_omicron)
# cat(print_value_corr(coef(fit.lm)[2])," & ", print_value_corr(lmp(fit.lm))," \\\\ \n")

cat("Prevalence omicron vs vacc. efficacy 2 doses & ")
res <- cor.test(df$prev_omicron, df$efficacy_RF_vac2doses_2, method = "pearson")
cat(print_value_corr(res$estimate), " & ", print_value_corr(res$p.value), "\n")

# fit.lm <- lm(df$efficacy_RF_vac2doses_2 ~ df$prev_omicron)
# cat(print_value_corr(coef(fit.lm)[2])," & ", print_value_corr(lmp(fit.lm)),"\n")

sink()


# cat("Country & % omicron & Sample size & Prevalence Oct & Prevalence Dec & Vac efficacy Oct & Vac efficacy Dec \n")

sink(table_omicron_prev_vacc)

for (i in seq(1,nrow(df))) {
  
  cat(df$country_name[i], "& ")
  
  cat(paste0(format(round(df$prev_omicron[i], 2), nsmall = 2), " [",
             format(round((df$prev_omicron_low[i]), 2), nsmall = 2), ",",
             format(round((df$prev_omicron_high[i]), 2), nsmall = 2), "] & "))

  cat( print_two_values_CI(df$p_pos_RF_1[i], df$p_pos_RF_1[i]-df$p_pos_RF_1_error[i], df$p_pos_RF_1[i]+df$p_pos_RF_1_error[i],
                          df$p_pos_RF_2[i], df$p_pos_RF_2[i]-df$p_pos_RF_2_error[i], df$p_pos_RF_2[i]+df$p_pos_RF_2_error[i]))
  cat(" & ")
  
  cat( print_two_values_CI(df$efficacy_RF_1[i], df$efficacy_RF_1_low[i], df$efficacy_RF_1_high[i],
                          df$efficacy_RF_2[i], df$efficacy_RF_2_low[i], df$efficacy_RF_2_high[i]))
  
  if (i != nrow(df)) cat("\\\\ \n")
  else cat("\n")
}

sink()

# cat("\n \n")
# cat("Country")
# cat("& % omicron")
# cat("& Vac efficacy 1 dose Oct & Vac efficacy 1 dose Dec ")
# cat("& Vac efficacy Oct 2 doses & Vac efficacy Dec 2 doses ")
# cat("\n")

sink(table_omicron_1D_2D)

for (i in seq(1,nrow(df))) {
  
  cat(df$country_name[i], "& ")
  
  cat(paste0(format(round(df$prev_omicron[i], 2), nsmall = 2), " [",
             format(round((df$prev_omicron_low[i]), 2), nsmall = 2), ",",
             format(round((df$prev_omicron_high[i]), 2), nsmall = 2), "] & "))
 
  cat( print_two_values_CI(df$efficacy_RF_vac1dose_1[i], df$efficacy_RF_vac1dose_1_low[i], df$efficacy_RF_vac1dose_1_high[i],
                         df$efficacy_RF_vac1dose_2[i], df$efficacy_RF_vac1dose_2_low[i], df$efficacy_RF_vac1dose_2_high[i]))
  cat(" & ")
 
  cat( print_two_values_CI(df$efficacy_RF_vac2doses_1[i], df$efficacy_RF_vac2doses_1_low[i], df$efficacy_RF_vac2doses_1_high[i],
                         df$efficacy_RF_vac2doses_2[i], df$efficacy_RF_vac2doses_2_low[i], df$efficacy_RF_vac2doses_2_high[i]))
  
  if (i != nrow(df)) cat("\\\\ \n")
  else cat("\n")
}

sink()

# cat("\n \n")
# cat("Country ")
# cat("& Samples Oct & Samples Dec ")
# cat("& Unvac Oct & Unvac Dec ")
# cat("& Vac Oct & Vac Dec ")
# cat("& Vac 1D Oct & Vac 1D Dec ")
# cat("& Vac 2D Oct & Vac 2D Dec ")
# cat("\\\n")

sink(table_total_samples)

for (i in seq(1,nrow(df))) {
  
  cat(df$country_name[i], "& ")
  
  cat(df$count_1[i], "& ")
  cat(df$count_2[i], "& ")
  cat(df$unvaccinated_1[i], "& ")
  cat(df$unvaccinated_2[i], "& ")
  cat(df$vaccinated_1[i], "& ")
  cat(df$vaccinated_2[i], "& ")
  cat(df$vac1dose_1[i], "& ")
  cat(df$vac1dose_2[i], "& ")
  cat(df$vac2doses_1[i], "& ")
  cat(df$vac2doses_2[i])
  
  if (i != nrow(df)) cat("\\\\ \n")
  else cat("\n")
}

sink()

# cat("\n \n")
# cat("Country ")
# cat("& Pos Oct & Pos Dec ")
# cat("& Pos unvac Oct & Pos unvac Dec ")
# cat("& Pos vac Oct & Pos vac Dec ")
# cat("& Pos 1D Oct & Pos 1D Dec ")
# cat("& Pos 2D Oct & Pos 2D Dec ")
# cat("\n")

sink(table_positives)

for (i in seq(1,nrow(df))) {
  cat(df$country_name[i], "& ")
  
  cat(df$pos_RF_1[i], "& ")
  cat(df$pos_RF_2[i], "& ")
  
  cat(df$pos_RF_unvaccinated_1[i], "& ")
  cat(df$pos_RF_unvaccinated_2[i], "& ")
  cat(df$pos_RF_vaccinated_1[i], "& ")
  cat(df$pos_RF_vaccinated_2[i], "& ")
  cat(df$pos_RF_vac1dose_1[i], "& ")
  cat(df$pos_RF_vac1dose_2[i], "& ")
  cat(df$pos_RF_vac2doses_1[i], "& ")
  cat(df$pos_RF_vac2doses_2[i])
  
  if (i != nrow(df)) cat("\\\\ \n")
  else cat("\n")
}

sink()

# cat("\n \n")
# cat("Country & % vaccination & % vaccinated & % fully vaccinated & % boosters & Vac start \n")

sink(table_vaccination)

for (i in seq(1,nrow(df))) {
  
  cat(df$country_name[i], "& ")
  
  cat(format(round(df$total_vaccinations_per_hundred[i], 2), nsmall = 2), "& ")
  cat(format(round(df$people_vaccinated_per_hundred[i], 2), nsmall = 2), "& ")
  cat(format(round(df$people_fully_vaccinated_per_hundred[i], 2), nsmall = 2), "& ")
  cat(format(round(df$total_boosters_per_hundred[i], 2), nsmall = 2), "& ")
  cat(as.character(df$start_vacc[i]))
  
  if (i != nrow(df)) cat("\\\\ \n")
  else cat("\n")
  
}

sink()
