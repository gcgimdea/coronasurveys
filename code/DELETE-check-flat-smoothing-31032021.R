library(tidyverse)

# ----

# dt <- read_csv("../data/estimates-W/PlotData/US-estimate.csv")
dt <- read_csv("../data/estimates-W/PlotData/ES-estimate.csv")
# dt <- read_csv("../data/estimates-W/PlotData/UA-estimate.csv")
# dt <- read_csv("../data/estimates-300responses/PlotData/ES-estimate.csv")
# dt <- read_csv("../data/estimates-300responses/PlotData/PT-estimate.csv")
# dt <- read_csv("../data/estimates-300responses/PlotData/IT-estimate.csv")

glimpse(dt)

# ----

p_test <- ggplot(dt, aes(x = date)) +
  geom_point(aes(y = p_cases_infected), 
             size = 1, color = "red", alpha = 0.5) +
  geom_line(aes(y = p_cases_infected_smooth),
            color = "blue", alpha = 0.5, size = 1) +
  geom_ribbon(aes(ymin = p_cases_infected_smooth_low,
                  ymax = p_cases_infected_smooth_high), 
              alpha=0.3, fill = "blue") +
  labs(title = "Smooth data from csv") +
  theme_bw()
p_test

# ----

dt_short <- dt %>% 
  select(date, p_cases_infected)

glimpse(dt_short)

source("smooth_column-v2.R")
smooth_param <- 40

dt_smooth <- smooth_column(dt_short, "p_cases_infected", 
              smooth_param, link_in = "log", monotone = T)

glimpse(dt_smooth)

p_new <- ggplot(dt_smooth, aes(x = date)) +
  geom_point(aes(y = p_cases_infected), 
             size = 1, color = "red", alpha = 0.5) +
  geom_line(aes(y = p_cases_infected_smooth),
            color = "blue", alpha = 0.5, size = 1) +
  geom_ribbon(aes(ymin = p_cases_infected_smooth_low,
                  ymax = p_cases_infected_smooth_high), 
              alpha=0.3, fill = "blue") + 
  labs(title = "Smooth data recomputed") +
  theme_bw()
p_new

