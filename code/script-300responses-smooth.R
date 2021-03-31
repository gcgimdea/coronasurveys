## OPTION 1 (if packs are installed) ----
library(dplyr)
library(plotly)
library(scam)

# smoothed p_cases_infected and CI:
source("smooth_column-v2.R")
smooth_param <- 40

estimates_path <- "../data/estimates-300responses/PlotData/"

# estimates_path <- "./estimates-300responses/PlotData/"


## OPTION 2 (if packs are NOT installed) ----
## List of packages
packages = c("dplyr", "plotly", "scam")
# 
# ## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## Write TRUE for plots:
to_plot = F

## get each dataset in "estimates-300responses" ----

all.files <- list.files(estimates_path)

for (k in 1:length(all.files)) {
  print(paste("smoothing", all.files[k]))
  data300 <- read.csv(paste0(estimates_path, all.files[k]))
  
  if (sum(data300$p_cases_infected != 0) > smooth_param) {
    data300 <- smooth_column(data300, "p_cases_infected", 
                                           smooth_param, link_in = "log", monotone = T)
    # data300 <- smooth_column(data300, "p_cases_infected_error", 
    #                                        smooth_param, link_in = "log", monotone = F)
    data300$p_cases_infected_smooth_low <- data300$p_cases_infected_smooth - data300$p_cases_infected_error
    data300$p_cases_infected_smooth_high <- data300$p_cases_infected_smooth + data300$p_cases_infected_error
  }
  write.csv(data300,
            paste0(estimates_path, all.files[k]), row.names = F)
  
  ## Plots ----
  if (to_plot) {
    p <- to.smooth %>% 
      plot_ly(x = ~date, y = ~p_cases_infected, type = 'scatter', mode = 'markers', 
              name = 'Estimated p') %>% 
      add_trace(x = ~date, y = ~p_cases_infected_smooth, type = 'scatter', mode = 'lines', 
                name = 'Smooth p') %>% 
      add_trace( x = ~date, y = ~p_cases_infected_smooth_high, type = "scatter" , mode = "lines",
                 line = list(color = 'transparent'),
                 showlegend = FALSE, name = 'High')  %>%
      add_trace(x = ~date, y = ~p_cases_infected_smooth_low, type = 'scatter', mode = 'lines',
                fill = 'tonexty', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Low') %>% 
      layout(title = substr(all.files[k], 1, 2))
    print(p)
  } # end-if-plot-smoothed-data
  
} # end-for

