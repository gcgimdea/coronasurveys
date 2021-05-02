library(lubridate)
library(useful)
library(dplyr)
# smoothed p_cases and CI:
source("smooth_column-v2.R")

estimates_umd_path <- "../data/estimates-symptom-survey/region/"
plots_path <- "../data/estimates-symptom-survey/plots/"

smooth_param <- 40

cli_factor <- 1
cli_local_factor <- 1
anosmia_factor <- 1

# start_date <- "2020-04-10"
start_date <- "2021-01-01"
#end_date <- "2020-12-10"
end_date <- Sys.Date()


plot_region <- function(country = "IN", region = "Rajasthan")
{
  region_us <- gsub(" ", "_", region)
  
  # Read UMD data
  df_umd <- read.csv(paste0(estimates_umd_path, country, "-estimate.csv"))
  df_umd <- df_umd[df_umd$region == region,]
  
  df_umd$date <- as.Date(df_umd$date)
  
  # # cat("Smoothing p_cli\n")
  # df_umd <- smooth_column(df_in = df_umd,
  #                         col_s = "p_cli",
  #                         basis_dim = smooth_param,
  #                         link_in = "log")
  # 
  # # cat("Smoothing p_cli_local\n")
  # df_umd <- smooth_column(df_in = df_umd,
  #                         col_s = "p_cli_local",
  #                         basis_dim = smooth_param,
  #                         link_in = "log")
  # 
  # # cat("Smoothing p_anosmia\n")
  # df_umd <- smooth_column(df_in = df_umd,
  #                         col_s = "p_anosmia",
  #                         basis_dim = smooth_param,
  #                         link_in = "log")
  
  df_umd <- df_umd[df_umd$date >= ymd(start_date),]
  
  # colors <- c("Nuevos casos" = "red", "recent_c" = "red", "sick_c" = "blue", "Sintomáticos" = "blue")
  p1 <- ggplot(data = df_umd, aes(x = date, color = ""))  +
    # geom_point(aes(y = p_cli*100000, color = "UMD CLI"), alpha = 0.5, size = 2) +
    geom_line(aes(y = cli_factor * p_cli_14days*100000, color = "UMD CLI"),
              linetype = "solid", size = 1, alpha = 0.6) +
    geom_ribbon(aes(ymin = cli_factor * p_cli_14days_low*100000,
                    ymax = cli_factor * p_cli_14days_high*100000),
                alpha = 0.1, color = "green", size = 0.1, fill = "green") +
    # geom_point(aes(y = p_cli_local*100000, color = "UMD CLI Indirect"), alpha = 0.5, size = 2) +
    geom_line(aes(y = cli_local_factor * p_cli_local_14days*100000, color = "UMD CLI Indirect"),
              linetype = "solid", size = 1, alpha = 0.6) +
    geom_ribbon(aes(ymin = cli_local_factor * p_cli_local_14days_low*100000,
                    ymax = cli_local_factor * p_cli_local_14days_high*100000),
                alpha = 0.1, color = "red", size = 0.1, fill = "red") +
    # geom_point(aes(y = p_anosmia*100000, color = "UMD anosmia"), alpha = 0.5, size = 2) +
    geom_line(aes(y = anosmia_factor * p_anosmia_14days*100000, color = "UMD anosmia"),
              linetype = "solid", size = 1, alpha = 0.6) +
    geom_ribbon(aes(ymin = anosmia_factor * p_anosmia_14days_low*100000,
                    ymax = anosmia_factor * p_anosmia_14days_high*100000),
                alpha = 0.1, color = "blue", size = 0.1, fill = "blue") +
    labs(x = "Date", y =  "Cases per 100,000 people") +
    # ylim(0, 3000)+
    theme_bw() + 
    ggtitle(paste0("Active cases in ", region, " (14 days moving average)")) +
    scale_colour_manual(values = c("blue", "green", "red", "magenta"),
                        name="",
                        guide = guide_legend(override.aes = list(
                          linetype = c(#"dotted", 
                            # "dotted", "blank", 
                            "solid", 
                            "solid", 
                            "solid"),
                          shape = c(#NA, 
                            # NA, 1, NA, 
                            1, 1, 1)))) +
    theme(legend.position = "bottom")
  #p1
  ggsave(plot = p1, 
         filename =  paste0(plots_path, country, "-", region_us, "-14days.jpg"), 
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

regions_india <- c(
"Andaman and Nicobar",
"Andhra Pradesh",
"Arunachal Pradesh",
"Assam",
"Bihar",
"Chandigarh",
"Chhattisgarh",
"Dadra and Nagar Haveli",
"Daman and Diu",
"Goa",
"Gujarat",
"Haryana",
"Himachal Pradesh",
"Jammu and Kashmir",
"Jharkhand",
"Karnataka",
"Kerala",
"Ladakh",
"Lakshadweep",
"Madhya Pradesh",
"Maharashtra",
"Manipur",
"Meghalaya",
"Mizoram",
"Nagaland",
"NCT of Delhi",
"Odisha",
"Puducherry",
"Punjab",
"Rajasthan",
"Sikkim",
"Tamil Nadu",
"Telangana",
"Tripura",
"Uttar Pradesh",
"Uttarakhand",
"West Bengal"
)

for (r in regions_india) {
  plot_region(country = "IN", region = r)
}

regions_brazil <- c(
  "Acre",
  "Alagoas",
  "Amapá",
  "Amazonas",
  "Bahia",
  "Ceará",
  "Distrito Federal",
  "Espírito Santo",
  "Goiás",
  "Maranhão",
  "Mato Grosso",
  "Mato Grosso do Sul",
  "Minas Gerais",
  "Pará",
  "Paraíba",
  "Paraná",
  "Pernambuco",
  "Piauí",
  "Rio de Janeiro",
  "Rio Grande do Norte",
  "Rio Grande do Sul",
  "Rondônia",
  "Roraima",
  "Santa Catarina",
  "São Paulo",
  "Sergipe",
  "Tocantins"
)

for (r in regions_brazil) {
  plot_region(country = "BR", region = r)
}
