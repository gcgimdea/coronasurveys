library(lubridate)
library(useful)
library(dplyr)
# smoothed p_cases and CI:
source("smooth_column-v2.R")

# population <- 6663394

estimates_path <- "../data/estimates-provinces/"
estimates_umd_path <- "../data/estimates-umd-symptom-survey/"
plots_path <- "../data/estimates-provinces/plots/"
ccfr_path <- "../data/estimates-ccfr-based/ES/"

# estimates_path <- "./estimates-provinces/"
# estimates_umd_path <- "./estimates-umd-symptom-survey/"
# plots_path <- "./estimates-provinces/plots/"
# datadista_file <- "../estimates-datadista/smooth_cases_fatalities.csv"


smooth_param <- 15
age_recent <- 7
#start_date <- "2020-10-10"
start_date <- "2021-01-01"
#end_date <- "2020-12-10"
end_date <- Sys.Date()
cum_window <- 14


## Load data ----
df_cs <- read.csv(paste0(estimates_path, "ES/ESM-estimate.csv"))
df_cs <- df_cs %>% select(date, p_recentcases, p_recentcases_error, p_cases_daily, p_cases_daily_error,
                    p_cases, p_cases_error, p_stillsick, p_stillsick_error)
df_cs$date <- as.Date(df_cs$date)

# Shift CoronaSurveys data by 1/2 of the aggregation interval
df_cs$date <- df_cs$date - (age_recent-1)/2

# df <- tail(df, n=(nrow(df)-211))
df_cs <- df_cs[df_cs$date >= ymd(start_date),]
df_cs <- df_cs[df_cs$date <= ymd(end_date),]


#shift cases daily
df_cs$daily_shifted <- df_cs$p_cases_daily
df_cs$daily_error_shifted <- df_cs$p_cases_daily_error

len <- nrow(df_cs)
df_cs$daily_shifted[1:(len-3)] <- df_cs$daily_shifted[4:len]
df_cs$daily_error_shifted[1:(len-3)] <- df_cs$daily_error_shifted[4:len]
df_cs$daily_shifted[(len-2):len] <- 
  df_cs$daily_error_shifted[(len-2):len] <- NA

## Non-monotonic ----
cat("Smoothing p_recentcases\n")
df_cs <- smooth_column(df_in = df_cs, 
                    col_s = "p_recentcases", 
                    basis_dim = smooth_param,
                    link_in = "log")

cat("Smoothing daily_shifted\n")
df_cs <- smooth_column(df_in = df_cs, 
                    col_s = "daily_shifted", 
                    basis_dim = smooth_param,
                    link_in = "log")

cat("Smoothing p_stillsick\n")
df_cs <- smooth_column(df_in = df_cs, 
                    col_s = "p_stillsick", 
                    basis_dim = smooth_param,
                    link_in = "log")




# Read cCFR-based data
df_ccfr <- read.csv(paste0(ccfr_path, "ESMD-estimate.csv"))
df_ccfr <- df_ccfr %>% 
  select(date, cases, p_cases_daily, p_cases_active, population)
df_ccfr$date <- as.Date(df_ccfr$date)

df_ccfr <- df_ccfr[df_ccfr$date >= ymd(start_date),]

# Computing 7-day cumulative incidence
df_ccfr$p_confirmed <- df_ccfr$cases / df_ccfr$population

df_ccfr$p_cum_confirmed <- NA
df_ccfr$p_cum_daily <- NA
if (nrow(df_ccfr) >= cum_window){
  df_ccfr$p_cum_confirmed <- cumsum(c(df_ccfr$p_confirmed[1:cum_window],
                                  diff(df_ccfr$p_confirmed, 
                                       lag = cum_window)))
  df_ccfr$p_cum_daily <- cumsum(c(df_ccfr$p_cases_daily[1:cum_window],
                                    diff(df_ccfr$p_cases_daily, 
                                         lag = cum_window)))
}

# Smoothing
cat("Smoothing p_cum_confirmed\n")
df_ccfr <- smooth_column(df_in = df_ccfr,
                       col_s = "p_cum_confirmed", 
                       basis_dim = smooth_param,
                       link_in = "log")

# cat("Smoothing p_cum_daily\n")
# df_ccfr <- smooth_column(df_in = df_ccfr,
#                        col_s = "p_cum_daily",
#                        basis_dim = smooth_param,
#                        link_in = "log")

# cat("Smoothing p_cases_active\n")
# df_ccfr <- smooth_column(df_in = df_ccfr,
#                        col_s = "p_cases_active", 
#                        basis_dim = smooth_param,
#                        link_in = "log")




# Read UMD data
df_umd <- read.csv(paste0(estimates_umd_path, "ES/ESMD-estimate.csv"))
df_umd <- df_umd %>% select(date, pct_cli_weighted) #, batched_pct_cli)
df_umd$date <- as.Date(df_umd$date)

# df <- tail(df, n=(nrow(df)-211))
df_umd <- df_umd[df_umd$date >= ymd(start_date),]

cat("Smoothing pct_cli_weighted\n")
df_umd <- smooth_column(df_in = df_umd, 
                    col_s = "pct_cli_weighted", 
                    basis_dim = smooth_param,
                    link_in = "log")

# cat("Smoothing batched_pct_cli\n")
# df_umd <- smooth_column(df_in = df_umd,
#                         col_s = "batched_pct_cli",
#                         basis_dim = smooth_param,
#                         link_in = "log")

# colors <- c("Nuevos casos" = "red", "recent_c" = "red", "sick_c" = "blue", "Sintomáticos" = "blue")
p1 <- ggplot(data = df_umd, aes(x = date, color = ""))  +
  # geom_rect(xmin = ymd("2020-09-19"), xmax = ymd("2020-09-20"),
  #               ymin = 0, ymax = Inf, 
  #             alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-09-26"), xmax = ymd("2020-09-27"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-03"), xmax = ymd("2020-10-04"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-10"), xmax = ymd("2020-10-12"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-17"), xmax = ymd("2020-10-18"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-24"), xmax = ymd("2020-10-25"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-31"), xmax = ymd("2020-11-02"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-7"), xmax = ymd("2020-11-09"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-14"), xmax = ymd("2020-11-15"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-21"), xmax = ymd("2020-11-22"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-28"), xmax = ymd("2020-11-29"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-12-05"), xmax = ymd("2020-12-08"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-12-12"), xmax = ymd("2020-12-13"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # 
  geom_point(aes(y = pct_cli_weighted*1000, color = "U. Maryland Covidmap"), 
             alpha = 0.5, size = 2) +
  # geom_point(aes(y = batched_pct_cli*1000, color = "UMD-B"), 
  #            alpha = 0.5, size = 2) +
  geom_line(aes(y = pct_cli_weighted_smooth*1000, color = "U. Maryland Covidmap"),
            linetype = "solid", size = 1, alpha = 0.6) +
  #
  # geom_ribbon(aes(ymin = pct_cli_weighted_smooth_low*1000, 
  #                 ymax = pct_cli_weighted_smooth_high*1000), 
  #             alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  # geom_point(aes(y = recent_shifted*100000, color = "Nuevos casos (7 días)"),
  #            alpha = 0.5, size = 2) +
  # geom_line(aes(y = recent_shifted_smooth*100000, color = "Nuevos casos (7 días)"),
  #           linetype = "solid", size = 1, alpha = 0.6) +
  # geom_ribbon(aes(ymin = (recent_shifted_smooth-recent_error_shifted)*100000, 
  #                 ymax = (recent_shifted_smooth+recent_error_shifted)*100000), 
  #             alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  geom_point(data=df_cs, aes(y = p_stillsick*100000, color = "Enfermos CoronaSurveys"), alpha = 0.5, size = 2) +
  geom_line(data=df_cs, aes(y = p_stillsick_smooth*100000, color = "Enfermos CoronaSurveys"), 
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(data=df_cs, aes(ymin = (p_stillsick_smooth-p_stillsick_error)*100000, 
                  ymax = (p_stillsick_smooth+p_stillsick_error)*100000), 
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  #
  # geom_point(data = df_ccfr, aes(y = p_cases_active*100000, color = "cCFR-based"),
  #            alpha = 0.5, size = 2) +
  # geom_line(data = df_ccfr, aes(y = p_cases_active_smooth*100000, color = "cCFR-based"),
  #           linetype = "solid", size = 1, alpha = 0.6) +
  #
    labs(x = "Fecha", y =  "Casos por 100.000 habitantes") +
  # ylim(0, 3000)+
  theme_bw() + 
  ggtitle("Casos activos en Madrid") +
  scale_colour_manual(values = c("blue", "red", "green", "magenta"),
                      name="",
                      guide = guide_legend(override.aes = list(
                        linetype = c(#"dotted", 
                                     # "dotted", "blank", "solid", 
                          "solid", 
                          "solid"),
                        shape = c(#NA, 
                                  # NA, 1, NA, 1, 
                                  1, 1)))) +
  theme(legend.position = "bottom")
#p1

ggsave(plot = p1, 
       filename =  paste0(plots_path, "ESM-active-plot.jpg"), 
       width = 9, height = 6)


p1 <- ggplot(data = df_cs, aes(x = date, color = ""))  +
  # geom_rect(xmin = ymd("2020-09-19"), xmax = ymd("2020-09-20"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-09-26"), xmax = ymd("2020-09-27"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-03"), xmax = ymd("2020-10-04"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-10"), xmax = ymd("2020-10-12"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-17"), xmax = ymd("2020-10-18"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-24"), xmax = ymd("2020-10-25"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-10-31"), xmax = ymd("2020-11-02"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-07"), xmax = ymd("2020-11-09"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-14"), xmax = ymd("2020-11-15"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-21"), xmax = ymd("2020-11-22"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-11-28"), xmax = ymd("2020-11-29"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-12-05"), xmax = ymd("2020-12-08"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  # geom_rect(xmin = ymd("2020-12-12"), xmax = ymd("2020-12-13"),
  #           ymin = 0, ymax = Inf, 
  #           alpha = 0.01, color = "orange", size = 0.1, fill = "yellow") +
  #
  # geom_point(aes(y = daily_shifted*100000, color = "Nuevos casos CoronaSurveys"),
  #            alpha = 0.5, size = 2) +
  # geom_line(aes(y = daily_shifted_smooth*100000, color = "Nuevos casos CoronaSurveys"),
  #           linetype = "solid", size = 1, alpha = 0.6) +
  # geom_ribbon(aes(ymin = (daily_shifted_smooth - daily_error_shifted)*100000,
  #                 ymax = (daily_shifted_smooth + daily_error_shifted)*100000),
  #             alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  #
  geom_point(aes(y = p_recentcases*100000, color = "Nuevos casos CoronaSurveys"),
             alpha = 0.5, size = 2) +
  geom_line(aes(y = p_recentcases_smooth*100000, color = "Nuevos casos CoronaSurveys"),
            linetype = "solid", size = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = (p_recentcases_smooth - p_recentcases_error)*100000,
                  ymax = (p_recentcases_smooth + p_recentcases_error)*100000),
              alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  #
  geom_point(data = df_ccfr, aes(y = p_cum_confirmed*100000, color = "Confirmados"),
             alpha = 0.5, size = 2) +
  geom_line(data = df_ccfr, aes(y = p_cum_confirmed_smooth*100000, color = "Confirmados"),
            linetype = "solid", size = 1, alpha = 0.6) +
  #
  # geom_point(data = df_ccfr, aes(y = p_cum_daily*100000, color = "cCFR-based"),
  #            alpha = 0.5, size = 2) +
  # # geom_line(data = df_ccfr, aes(y = p_cum_daily_smooth*100000, color = "cCFR-based"),
  #           linetype = "solid", size = 1, alpha = 0.6) +
  #
  labs(x = "Fecha", y =  "Casos por 100.000 habitantes") +
  # ylim(0, 3000)+
  theme_bw() + 
  ggtitle("Incidencia acumulada (14 días) en Madrid") +
  scale_colour_manual(values = c("red", "blue", "magenta"),
                      name="",
                      guide = guide_legend(override.aes = list(
                        linetype = c(#"dotted", 
                          # "dotted", "blank", 
                          # "solid", 
                          # "solid", 
                          "solid"),
                        shape = c(#NA, 
                          # NA, 1, NA, 
                          # 1,
                          # 1, 
                          1)))) +
  theme(legend.position = "bottom")
#p1

ggsave(plot = p1, 
       filename =  paste0(plots_path, "ESM-recent-plot.jpg"), 
       width = 9, height = 6)

