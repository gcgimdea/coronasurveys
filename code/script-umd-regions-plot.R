library(lubridate)
library(useful)
library(dplyr)
# smoothed p_cases and CI:
# source("smooth_column-v2.R")

estimates_umd_path <- "../data/estimates-symptom-survey/region/"
plots_path <- "../data/estimates-symptom-survey/plots/"

smooth_param <- 15

cli_factor <- 1
cliWHO_factor <- 1
cli_local_factor <- 1
anosmia_factor <- 1

# start_date <- "2020-04-10"
start_date <- "2021-01-01"
#end_date <- "2020-12-10"
end_date <- Sys.Date()


plot_region <- function(df_umd, country = "IN", region = "Rajasthan")
{
  region_us <- gsub(" ", "_", region)
  
  # cat("Smoothing p_cli\n")
  # fit <- with(df_umd, 
  #             ksmooth(date, p_cli, kernel = "normal", bandwidth = smooth_param, x.points=date))
  # df_umd$p_cli_smooth <- fit$y
  # fit <- with(df_umd, 
  #             ksmooth(date, p_cli_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))
  # df_umd$p_cli_CI_smooth <- fit$y
  # cat("Smoothing p_cliWHO\n")
  # fit <- with(df_umd, 
  #             ksmooth(date, p_cliWHO, kernel = "normal", bandwidth = smooth_param, x.points=date))
  # df_umd$p_cliWHO_smooth <- fit$y
  # fit <- with(df_umd, 
  #             ksmooth(date, p_cliWHO_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))
  # df_umd$p_cliWHO_CI_smooth <- fit$y
  # cat("Smoothing p_cli_local\n")
  # fit <- with(df_umd, 
  #             ksmooth(date, p_cli_local, kernel = "normal", bandwidth = smooth_param, x.points=date))
  # df_umd$p_cli_local_smooth <- fit$y
  # fit <- with(df_umd, 
  #             ksmooth(date, p_cli_local_CI, kernel = "normal", bandwidth = smooth_param, x.points=date))
  # df_umd$p_cli_local_CI_smooth <- fit$y

  # colors <- c("Nuevos casos" = "red", "recent_c" = "red", "sick_c" = "blue", "Sintomáticos" = "blue")
  p1 <- ggplot(data = df_umd, aes(x = date, color = ""))  +
    # geom_point(aes(y = p_cli*100000, color = "UMD CLI"), alpha = 0.5, size = 2) +
    geom_line(aes(y = cli_factor * p_cli_smooth*100000, color = "UMD CLI"),
              linetype = "solid", size = 1, alpha = 0.6) +
    geom_ribbon(aes(ymin = cli_factor * (p_cli_smooth-p_cli_CI_smooth)*100000,
                    ymax = cli_factor * (p_cli_smooth+p_cli_CI_smooth)*100000),
                alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
    # geom_point(aes(y = p_cliWHO*100000, color = "UMD CLI WHO"), alpha = 0.5, size = 2) +
    geom_line(aes(y = cliWHO_factor * p_cliWHO_smooth*100000, color = "UMD CLI WHO"),
              linetype = "solid", size = 1, alpha = 0.6) +
    geom_ribbon(aes(ymin = cliWHO_factor * (p_cliWHO_smooth-p_cliWHO_CI_smooth)*100000,
                    ymax = cliWHO_factor * (p_cliWHO_smooth+p_cliWHO_CI_smooth)*100000),
                alpha = 0.1, color = "magenta", size = 0.1, fill = "magenta") +
    # geom_point(aes(y = p_cli_local*100000, color = "UMD CLI Indirect"), alpha = 0.5, size = 2) +
    geom_line(aes(y = cli_local_factor * p_cli_local_smooth*100000, color = "UMD CLI Indirect"),
              linetype = "solid", size = 1, alpha = 0.6) +
    geom_ribbon(aes(ymin = cli_local_factor * (p_cli_local_smooth-p_cli_local_CI_smooth)*100000,
                    ymax = cli_local_factor * (p_cli_local_smooth+p_cli_local_CI_smooth)*100000),
                alpha = 0.1, color = "red", size = 0.1, fill = "red") +
    # geom_point(aes(y = p_anosmia*100000, color = "UMD anosmia"), alpha = 0.5, size = 2) +
    # geom_line(aes(y = anosmia_factor * p_anosmia_14days*100000, color = "UMD anosmia"),
    #           linetype = "solid", size = 1, alpha = 0.6) +
    # geom_ribbon(aes(ymin = anosmia_factor * p_anosmia_14days_low*100000,
    #                 ymax = anosmia_factor * p_anosmia_14days_high*100000),
    #             alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
    labs(x = "Date", y =  "Cases per 100,000 people") +
    # ylim(0, 3000)+
    theme_bw() + 
    ggtitle(paste0("Active cases in ", region)) +
    scale_colour_manual(values = c("blue", "red", "magenta", "green"),
                        name="",
                        guide = guide_legend(override.aes = list(
                          linetype = c(#"dotted", 
                            # "dotted", "blank", 
                            "solid", 
                            "solid", 
                            "solid"),
                          shape = c(#NA, 
                            # NA, 1, NA, 
                            1, 
                            1, 1)))) +
    theme(legend.position = "bottom")
  #p1
  ggsave(plot = p1, 
         filename =  paste0(plots_path, country, "/", country, "-", region_us, ".jpg"), 
         width = 9, height = 6)
  
  # p2 <- ggplot(data = df_umd, aes(x = date, color = ""))  +
  #   # geom_point(aes(y = p_cli*100000, color = "UMD CLI"), alpha = 0.5, size = 2) +
  #   geom_line(aes(y = p_cli_smooth*100000, color = "UMD CLI"),
  #             linetype = "solid", size = 1, alpha = 0.6) +
  #   geom_ribbon(aes(ymin = p_cli_smooth_low*100000,
  #                   ymax = p_cli_smooth_high*100000),
  #               alpha = 0.1, color = "green", size = 0.1, fill = "green") +
  #   # geom_point(aes(y = p_cli_local*100000, color = "UMD CLI Indirect"), alpha = 0.5, size = 2) +
  #   geom_line(aes(y = p_cli_local_smooth*100000, color = "UMD CLI Indirect"),
  #             linetype = "solid", size = 1, alpha = 0.6) +
  #   geom_ribbon(aes(ymin = p_cli_local_smooth_low*100000,
  #                   ymax = p_cli_local_smooth_high*100000),
  #               alpha = 0.1, color = "red", size = 0.1, fill = "red") +
  #   # geom_point(aes(y = p_anosmia*100000, color = "UMD anosmia"), alpha = 0.5, size = 2) +
  #   geom_line(aes(y = p_anosmia_smooth*100000, color = "UMD anosmia"),
  #             linetype = "solid", size = 1, alpha = 0.6) +
  #   geom_ribbon(aes(ymin = p_anosmia_smooth_low*100000,
  #                   ymax = p_anosmia_smooth_high*100000),
  #               alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
  #   labs(x = "Date", y =  "Cases per 100,000 people") +
  #   # ylim(0, 3000)+
  #   theme_bw() + 
  #   ggtitle(paste0("Active cases in ", region, " (smoothed)")) +
  #   scale_colour_manual(values = c("blue", "green", "red", "magenta"),
  #                       name="",
  #                       guide = guide_legend(override.aes = list(
  #                         linetype = c(#"dotted", 
  #                           # "dotted", "blank", 
  #                           "solid", 
  #                           "solid", 
  #                           "solid"),
  #                         shape = c(#NA, 
  #                           # NA, 1, NA, 
  #                           1, 1, 1)))) +
  #   theme(legend.position = "bottom")
  # #p1
  # 
  # ggsave(plot = p2, 
  #        filename =  paste0(plots_path, country, "-", region_us, "-smooth.jpg"), 
  #        width = 9, height = 6)
}


#----------

countries <- c("BR", "EC", "ES", "IN", "NG")

for (country in countries) {
  cat(country, "\n")
  # Read UMD data
  df_umd <- read.csv(paste0(estimates_umd_path, country, ".csv"))
  df_umd$date <- as.Date(df_umd$date)
  df_umd <- df_umd[which((df_umd$date >= start_date) & (df_umd$date <= end_date)),]
  
  df_umd <- df_umd[which(!is.na(df_umd$region_agg)),]
  regions <- unique(df_umd$region_agg)

  dir.create(paste0(plots_path, country, "/"), showWarnings = F)
  for (r in regions) {
    cat(r, " ")
    df <- df_umd[df_umd$region_agg == r,]
    if (nrow(df) >= smooth_param) {
      df <- plot_region(df, country, region = r)
    }
  }
  cat("\n")
}
