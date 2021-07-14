library(tidyverse)

do_check_plot <- function(signal_to_plot, data_df, all_df_forecasted){
  
  p <- 
  ggplot(data = all_df_forecasted, aes(x = date, group = region) ) +
    geom_line(aes_string(y = paste0("yhat_", signal_to_plot)), 
              color = "blue", alpha = 0.8) +
    geom_point(aes_string(y = signal_to_plot), 
               color = "red", alpha = 0.3) +
    geom_point(data = data_df, aes_string(x = "date", 
                                          y = signal_to_plot, 
                                          group = "region"), 
               color = "yellow", alpha = 0.2) +
    geom_ribbon(aes_string(ymin = paste0("yhat_lower_", signal_to_plot), 
                           ymax = paste0("yhat_upper_", signal_to_plot)),
                fill = "blue", alpha = 0.1) +
    facet_wrap(~region) +
    ylab(signal_to_plot) + xlab("") +
    labs(title = unique(all_df_forecasted$country)) + 
    theme_bw()
  
  print(p)
  
}


in_path_region <- "../data/estimates-symptom-survey/PlotData/regional_data/"
out_path_region <- "../data/estimates-symptom-survey/prophet/region/" # name: XX.csv
path_hiperp_region <- "../data/estimates-symptom-survey/prophet/hiperp_region/"

countries <- list.files(out_path_region)

country_in <- countries[1]

data_df <- read_csv(file = paste0(in_path_region,
                                  country_in)) %>% 
  mutate(date = as.POSIXct(date))
class(data_df$date)

all_df_forecasted <- read_csv(paste0(out_path_region, country_in))

to_try_extra <- c(
  "p_cli_smooth",
  "p_cli_CI_smooth",
  "p_cli_weight_smooth",
  "p_cli_weight_CI_smooth",
  "p_cliWHO_smooth",
  "p_cliWHO_CI_smooth",
  "p_cliWHO_weight_smooth",
  "p_cliWHO_weight_CI_smooth",
  "p_cli_local_CI_smooth",
  "p_cli_local_smooth"
) 

do_check_plot(to_try_extra[1], data_df, all_df_forecasted)
do_check_plot(to_try_extra[2], data_df, all_df_forecasted)
do_check_plot(to_try_extra[3], data_df, all_df_forecasted)
do_check_plot(to_try_extra[4], data_df, all_df_forecasted)
do_check_plot(to_try_extra[5], data_df, all_df_forecasted)
