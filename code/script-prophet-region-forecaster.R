library(tidyverse)
library(prophet)
library(doParallel)
library(tictoc)


# Prophet wrapper ----
propheting_signal <- function(to_try_extra,
                              data_df, 
                              region_in,
                              grwth = 'linear', 
                              to_plot = F,
                              best_hiperparam = NULL
){
  ## propheting_signal:
  ## Input:
  ## to_try_extra:    array of strings with all the signal to be forecasted
  ## data_df:         input data frame (needs a column of dates)
  ## region_in:       region to be analyzed
  ## grwth:           trend growth: linear (default) or logistic
  ## best_hiperparam: best from grid.seasonality, 
  ##                            grid.prior.scale, 
  ##                            grid.fourier.order
  ##
  ## Output:
  ## df_out:          a matrix with the forecasted signals and CIs (yhat, yhat_lower/upper),
  ##                  and a new column indicating the name of the signal.
  
  df_out <- data.frame()
  
  for (var_to_select in to_try_extra) {
    
    print(paste0("Forecasting ",  region_in, "'s ", var_to_select))
    
    df_in <- data_df %>%
      filter(region == region_in) %>% 
      select(date, all_of(var_to_select))
    
    # renaming variables:
    colnames(df_in) <- c("ds", "y")
    
    if (grwth == "logistic") {
      # Saturating max and min:
      df_in$floor <- 0
      df_in$cap <- 1
    }
    
    
    # custom seosonality
    if (!is.null(best_hiperparam) & 
        var_to_select %in% unique(best_hiperparam$signal) ) {
      
      hiper_temp <- best_hiperparam %>% 
        filter(signal == var_to_select) 
      
      m <- prophet(changepoint.range = 0.99,
                   changepoint.prior.scale = hiper_temp[["chg.point.prior"]],
                   growth = grwth,
                   yearly.seasonality = F,
                   weekly.seasonality = F,
                   daily.seasonality = F)
      
      # add custom seasonality (period = 0 means no seasonal component)
      if (hiper_temp[["period"]] != 0) {
        m <- add_seasonality(m,
                             name = 'custom_season', 
                             period = hiper_temp[["period"]], 
                             fourier.order = hiper_temp[["fourier.order"]],
                             prior.scale = hiper_temp[["prior.scale"]])
      }
      
      
      m <- fit.prophet(m, df_in)
    }else {
      
      # fit model with fixed defaults:
      
      m <- prophet(df_in,
                   changepoint.range = 0.99,
                   changepoint.prior.scale = 0.95,
                   growth = grwth,
                   yearly.seasonality = F,
                   weekly.seasonality = T,
                   daily.seasonality = F)
      
    }
    
    # build future:
    future <- make_future_dataframe(m, periods = 14)
    
    if (grwth == "logistic"){
      # Saturating max and min:
      future$floor <- 0
      future$cap <- 1
    }
    
    # forecasting:
    forecast <- predict(m, future)
    
    # # plots:
    if (to_plot) {
      p1 <- plot(m, forecast) + 
        add_changepoints_to_plot(m) + 
        theme_minimal() + 
        labs(title = paste0( region_in, 
                             "-forecasting: ", 
                             var_to_select), 
             x = "Date", y = "% symptomatic") +
        ylim(0, max(df_in$y))
      print(p1)
      
      # TODO: update where to put these plots
      # ggsave(plot = p1,
      #        filename =  paste0(out_path_forecast_extra, 
      #                           "plots/", 
      #                           region_in, 
      #                           "-", 
      #                           var_to_select,
      #                           "-forecast.png"),
      #        width = 7, height = 5)
    }
    
    
    # savings
    forecast <- forecast %>% select(ds, 
                                    yhat, 
                                    yhat_lower, 
                                    yhat_upper,
                                    trend,
                                    trend_lower,
                                    trend_upper 
                                    # custom_season,
                                    # custom_season_lower,
                                    # custom_season_upper
                                    ) %>% 
      mutate(signal_name = var_to_select)
    
    
    df_out <- rbind(df_out, forecast)
    
  } # end for-var_to_select
  
  colnames(df_out)[str_detect(colnames(df_out), "ds")] <- "date"
  
  return(df_out)
} # end-prophet-function


# CV-parameter-tuning ----

propheting_cv_tune <- function(to_try_extra,
                               data_df, 
                               region_in,
                               grwth = 'linear', 
                               all_grid = NULL
                               ){
  ## propheting_cv_tune:
  ## Input:
  ## to_try_extra:    array of strings with all the signal to be forecasted
  ## data_df:         input data frame (needs a column of dates)
  ## region_in:       region to be analyzed
  ## grwth:           trend growth: linear (default) or logistic
  ## all_grid   ----- is a tibble/data frame with the folowing columns :
  ## chg.point.prior: strength of the sparse prior impossed to trend. Higher values
  ##                  result in more flexible trends.
  ## period:          in days (0 is no seasonal component)
  ## fourier.order:   number of bases to represent the seasonality
  ## prior.scale:     strength of the sparse prior impossed to seasonality
  ##
  ## Output:
  ## df_out:          ??? a matrix with the forecasted signals and CIs (yhat, yhat_lower/upper),
  ##                  and a new column indicating the name of the signal. ???
  
  if (is.null(all_grid)) {
    
    all_grid <- tribble(
      ~chg.point.prior, ~period,  ~fourier.order, ~prior.scale,
                  0.99,       0,               0,           0, # no-seasonality
                  0.99,       7,               3,           5, # weekly
                  0.99,      15,               4,           5, # bi-weekly
                  0.99,    30.5,               5,           5, # monthly
                  0.99,   182.5,               8,           5, # half-yearly
                  0.99,     365,              10,           5 # yearly
    )
    
  }
  
  rmses <- data.frame()
  best_param <- data.frame()
  
  for (var_to_select in to_try_extra) {
    
    cat("Hiperparamenter Tuning:",  
        region_in, 
        "'s", 
        var_to_select,
        "(", 
        nrow(all_grid), 
        "combs.) \n")
    
    
    nodes=detectCores()
    cl=makeCluster(nodes-1)
    registerDoParallel(cl)

    tic()
    f = foreach(param_in = 1:nrow(all_grid),
                .combine = 'rbind',
                .packages= c('foreach','doParallel','dplyr', 'prophet'))%dopar%
      {
    
    # f = data.frame()
    # for (param_in in 1:nrow(all_grid)) {
        
        
        df_in <- data_df %>%
          filter(region == region_in) %>% 
          select(date, all_of(var_to_select))
        
        # renaming variables:
        colnames(df_in) <- c("ds", "y")
        
        if (grwth == "logistic") {
          # Saturating max and min:
          df_in$floor <- 0
          df_in$cap <- 1
        }
        
        m <- prophet(changepoint.range = 0.98,
                     changepoint.prior.scale = all_grid[["chg.point.prior"]][param_in], #all_grid[param_in, "chg.point.prior"],
                     growth = grwth,
                     yearly.seasonality = F,
                     weekly.seasonality = F,
                     daily.seasonality = F)
        
        # add custom seasonality (period = 0 means no seasonal component)
        if (all_grid[["period"]][param_in] != 0) {
          m <- add_seasonality(m, 
                               name = 'custom', 
                               period = all_grid[["period"]][param_in], 
                               fourier.order = all_grid[["fourier.order"]][param_in],
                               prior.scale = all_grid[["prior.scale"]][param_in])
          
        }
        
        tryCatch(
          expr = {
            
            m <- fit.prophet(m, df_in)
            
            df.cv <- cross_validation(m, 
                                      initial = round(nrow(df_in)*0.9), 
                                      # period = 1, 
                                      horizon = 7, 
                                      units = 'days')
            
            df.p <- performance_metrics(df.cv, rolling_window = 1) %>% 
              select(horizon, rmse) %>% 
              mutate(
                chg.point.prior = all_grid[["chg.point.prior"]][param_in],
                period = all_grid[["period"]][param_in], 
                fourier.order = all_grid[["fourier.order"]][param_in],
                prior.scale = all_grid[["prior.scale"]][param_in],
                signal = var_to_select
              )
            
          }, # end to try
          error = function(e){ 
            
            # (Optional)
            cat("-> Error with combination: \n")
            print(all_grid[param_in, ])
            print(e)
            
            df.p <- data.frame(
              horizon = NA,
              rmse = NA,
              chg.point.prior = all_grid[["chg.point.prior"]][param_in],
              period = all_grid[["period"]][param_in], 
              fourier.order = all_grid[["fourier.order"]][param_in],
              prior.scale = all_grid[["prior.scale"]][param_in],
              signal = var_to_select
            )
            
          }# end if error
        ) # end tryCatch
        
        # f <- rbind(f, df.p)
        
      } # end-for-all_grid
    
    toc()
    stopCluster(cl)
    
    f <- f %>% filter(rmse == min(rmse, na.rm = T))
    
    best_param <- rbind(best_param, f) 
    
  } # end for-var_to_select
  
  return(best_param)
} # end-prophet-function



# APPLY REGIONAL ----

do_tuning_n_forcasting <- function(to_try_extra,
                                   available_country_reg,
                                   optimize_country_reg,
                                   all_grid = NULL,
                                   grwth = 'linear'){
  
  
  ## Estimate the best hiperparameters ----
  ## (just a few countries)
  
  for (country_in in optimize_country_reg) {
    
    cat("----> Tuning", country_in, "at regional level \n")
    
    data_df <- read_csv(file = paste0(in_path_region, 
                                      country_in), 
                        col_types = cols())
    
    
    all_best_hiper <- data.frame()    # best hiperparameters
    
    
    for (region_in in unique(data_df$region)) {
      
      tryCatch(
        expr = {
          
          best_hiperparam <- propheting_cv_tune(to_try_extra,
                                                data_df, 
                                                region_in,
                                                grwth = grwth, 
                                                all_grid = NULL)
          
          cat("Best Hiperparameters for", region_in, "\n")
          print(best_hiperparam)
          
          best_hiperparam$region <- region_in
          
          all_best_hiper <- rbind(all_best_hiper, best_hiperparam)
          
          
        },
        error = function(e){ 
          # (Optional)
          cat("Error while tuning region", region_in)
          print(e)
        }
      )
      
      
    } # end-region_in-loop
    
    # savings
    write_csv(all_best_hiper, 
              file = paste0(path_hiperp_region, country_in))
    
  } # end-loop-country_in
  
  
  
  ## Forecasting ----
  count_country = 1
  
  for (country_in in available_country_reg) {
    
    cat("\n ----> Forecasting", country_in, "at regional level (", 
        count_country, "/", length(available_country_reg), ")",
        "\n")
    count_country = count_country + 1
    
    data_df <- read_csv(file = paste0(in_path_region, 
                                      country_in), 
                        col_types = cols())
    
    
    all_df_forecasted <- data.frame() # initialize for all the forecastings
    
    
    ### Get best hiperparameters if they exist:
    check_hiperp_file = paste0(path_hiperp_region, country_in)
    
    if (file.exists(check_hiperp_file)) { # assign if it exists
      
      all_best_hiper <- read_csv(check_hiperp_file, col_types = cols())
      
    }else { # null if it doesn't
      
      all_best_hiper <- NULL
      
    }
    
    for (region_in in unique(data_df$region)) {
      
      
      ### Check if we have the hiperparameters for that region:
      if (region_in %in% unique(all_best_hiper$region) ) { # assign if the region exists
        
        best_hiperparam <- all_best_hiper %>% 
          filter(region == region_in)
        
      }else { # null if it doesn't
        
        best_hiperparam <- NULL
        
      }
      
      tryCatch(
        expr = {
          
          ### Forcasting with the best hiperparameters 
          df_test <- propheting_signal(to_try_extra = to_try_extra,
                                       data_df = data_df,
                                       region_in = region_in,
                                       grwth = grwth,
                                       to_plot = F,
                                       best_hiperparam = best_hiperparam)
          
          df_test$region <- region_in
          
          all_df_forecasted <- rbind(all_df_forecasted, df_test)
          
          
        },
        error = function(e){ 
          # (Optional)
          cat("Error while forecasting", region_in)
          print(e)
        }
      )
      
      
    } # end-loop-region_in
    
    all_df_forecasted <- all_df_forecasted %>% 
      pivot_wider(names_from = signal_name, 
                  values_from = 
                    c("yhat",
                      "yhat_lower",
                      "yhat_upper",
                      "trend",
                      "trend_lower",
                      "trend_upper")) %>% 
      full_join(data_df, by = c("date", "region"))
    
    # savings:
    write_csv(all_df_forecasted, 
              file = paste0(out_path_region, country_in))
    
  } # end-loop-country_in
  
  
} # end-do_tuning_n_forcasting
  
  
### Paths ----

in_path_region <- "../data/estimates-symptom-survey/PlotData/regional_data/"
out_path_region <- "../data/estimates-symptom-survey/prophet/region/" # name: XX.csv
path_hiperp_region <- "../data/estimates-symptom-survey/prophet/hiperp_region/"


### Signals to forecast ----
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

### All available countries with regional data 
### NOTE: all the countries + tuning India takes 9 HOURS!!!
available_country_reg <- list.files(in_path_region)

### Just some to optimize
optimize_country_reg <- c(
  "IN.csv"
  # "ES.csv"
)

# Grid of options to optimize (there's a default in propheting_cv_tuning):
all_grid <- tribble(
  ~chg.point.prior, ~period,  ~fourier.order, ~prior.scale,
  0.99,       0,               0,           0, # no-seasonality
  0.99,       7,               3,           5, # weekly
  0.99,      15,               4,           5, # bi-weekly
  0.99,    30.5,               5,           5, # monthly
  0.99,   182.5,               8,           5, # half-yearly
  0.99,     365,              10,           5 # yearly
)

grwth = 'linear'


## Call the tuning and forecasting ----

do_tuning_n_forcasting(to_try_extra = to_try_extra,
                       available_country_reg = available_country_reg,
                       optimize_country_reg = optimize_country_reg,
                       all_grid = all_grid,
                       grwth = 'linear')
