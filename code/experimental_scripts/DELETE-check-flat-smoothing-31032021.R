library(tidyverse)

# ----
# source("smooth_column-v2.R")
smooth_param <- 30

start_date <- as.Date("2021-01-01")
end_date <- Sys.Date()-2

# dt <- read_csv("../data/estimates-W/PlotData/US-estimate.csv")
dt <- read_csv("../data/estimates-symptom-survey/PlotData/IN-estimate.csv")
dt$date <- as.Date(dt$date)
dt <- dt[dt$date>=start_date & dt$date<=end_date,]

dt2 <- read_csv("../data/estimates-confirmed/PlotData/IN-estimate.csv")
dt2$date <- as.Date(dt2$date)
dt2 <- dt2[dt2$date>=start_date & dt2$date<=end_date,]


# dt <- read_csv("../data/estimates-300responses/PlotData/ES-estimate.csv")
# dt <- read_csv("../data/estimates-300responses/PlotData/PT-estimate.csv")
# dt <- read_csv("../data/estimates-300responses/PlotData/IT-estimate.csv")


# glimpse(dt)

normalized <- function(x) {
  x[is.na(x)] <- 0
  return ((x-min(x))/(max(x)-min(x)))
}
# ----

dt$y1 <- normalized(dt$p_cli_smooth)
dt$y2 <- normalized(dt2$p_cases_active_smooth_slope)
dt$y3 <- normalized(dt2$p_cases_active_smooth_slope2)
dt$y4 <- normalized(dt2$p_cases_active_smooth)

dt$y5 <- normalized(
  with(dt, ksmooth(date, dt$positive_recent / dt$test_recent, 
                   kernel = "normal", bandwidth = smooth_param, x.points=date))$y
  )

p_test <- ggplot(dt, aes(x = date)) +
  # geom_point(aes(y = p_cases_infected), 
  #            size = 1, color = "red", alpha = 0.5) +
  geom_line(aes(y = dt$y1),
            color = "black", alpha = 0.5, size = 1) +
  geom_line(aes(y = dt$y2),
            color = "red", alpha = 0.5, size = 1) +
  geom_line(aes(y = dt$y3),
            color = "blue", alpha = 0.5, size = 1) +
  geom_line(aes(y = dt$y4),
            color = "magenta", alpha = 0.5, size = 1) +
  geom_line(aes(y = dt$y5),
            color = "green", alpha = 0.5, size = 1) +
  # geom_ribbon(aes(ymin = p_cases_infected_smooth_low,
  #                 ymax = p_cases_infected_smooth_high), 
  #             alpha=0.3, fill = "blue") +
  labs(title = paste0("Active"), 
       x = "Date", y = "Ratio") +
  theme_bw()
p_test

# ----

# dt_short <- dt %>% 
#   select(date, p_cases_infected)
# 
# glimpse(dt_short)
# 

# 
# dt_smooth <- smooth_column(dt_short, "p_cases_infected", 
#               smooth_param, link_in = "log", monotone = T)
# 
# glimpse(dt_smooth)
# 
# p_new <- ggplot(dt_smooth, aes(x = date)) +
#   geom_point(aes(y = p_cases_infected), 
#              size = 1, color = "red", alpha = 0.5) +
#   geom_line(aes(y = p_cases_infected_smooth),
#             color = "blue", alpha = 0.5, size = 1) +
#   geom_ribbon(aes(ymin = p_cases_infected_smooth_low,
#                   ymax = p_cases_infected_smooth_high), 
#               alpha=0.3, fill = "blue") + 
#   labs(title = "Smooth data recomputed") +
#   theme_bw()
# p_new
# 
