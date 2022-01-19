library(dplyr, warn.conflicts = F, quietly = T)
library(tidyverse, warn.conflicts = F, quietly = T)
library(ggrepel, warn.conflicts = F, quietly = T)


input_file <- "./data/efficacy-data.csv"
output_path <- "./plots/"

ymin = 0
ymax = 0.75

# Scatter plot for the second period

Scatter_plot_vaccines_efficacy <- function() {
  df_eff <- read.csv(file=input_file)
  
  cat("Total:", dim(df_eff), "\n")
  
  cat(df_eff$country_name, "\n")

  df_to_plot <- df_eff %>% ungroup()
  
  plot_2 <- ggplot(df_to_plot, aes(x= prev_omicron, y= efficacy_RF_2)) +
    geom_point(colour="black") +
    geom_errorbarh(data = df_to_plot,
                   aes(xmin = prev_omicron_low,
                       xmax = prev_omicron_high,
                       y = efficacy_RF_2)) +
    geom_errorbar(data = df_to_plot,
                  aes(ymin = efficacy_RF_2_low,
                      ymax = efficacy_RF_2_high,
                      x = prev_omicron)) +
    geom_label_repel(aes(label = country_name),
                                               box.padding   = 0.5, 
                                               point.padding = 0.75,
                                               segment.color = 'red',
                                               size = 6) +
    labs(x = "Prevalence Omicron", y = "Vaccination efficacy") +
    # stat_summary(fun.df_to_plot=mean_cl_normal) + 
    geom_smooth(method='loess', formula=y~x,se = FALSE) + 
    theme(axis.text=element_text(size=16),
                                     axis.title=element_text(size=20)) + ylim(ymin,ymax)
  
  df_to_plot <- df_eff %>% ungroup()
  
  plot_3 <- ggplot(df_to_plot, aes(x= prev_omicron, y= efficacy_RF_vac1dose_2)) +
    geom_point(colour="black") +
    geom_errorbarh(data = df_to_plot,
                   aes(xmin = prev_omicron_low,
                       xmax = prev_omicron_high,
                       y = efficacy_RF_vac1dose_2)) +
    geom_errorbar(data = df_to_plot,
                  aes(ymin = efficacy_RF_vac1dose_2_low,
                      ymax = efficacy_RF_vac1dose_2_high,
                      x = prev_omicron)) +
    geom_label_repel(aes(label = country_name),
                                                box.padding   = 0.5, 
                                                point.padding = 0.75,
                                                segment.color = 'red',
                                                size = 6) +
    labs(x = "Prevalence Omicron", y = "Vaccination efficacy 1 dose") +
    # stat_summary(fun.df_to_plot=mean_cl_normal) + 
    geom_smooth(method='loess', formula=y~x,se = FALSE) + 
    theme(axis.text=element_text(size=16),
                                     axis.title=element_text(size=20)) + ylim(ymin,ymax)
  
  df_to_plot <- df_eff %>% ungroup()
  
  plot_4 <- ggplot(df_to_plot, aes(x= prev_omicron, y= efficacy_RF_vac2doses_2)) +
    geom_point(colour="black") +
    geom_errorbarh(data = df_to_plot,
                   aes(xmin = prev_omicron_low,
                       xmax = prev_omicron_high,
                       y = efficacy_RF_vac2doses_2)) +
    geom_errorbar(data = df_to_plot,
                  aes(ymin = efficacy_RF_vac2doses_2_low,
                      ymax = efficacy_RF_vac2doses_2_high,
                      x = prev_omicron)) +
    geom_label_repel(aes(label = country_name),
                                                 box.padding   = 0.5, 
                                                 point.padding = 0.75,
                                                 segment.color = 'red',
                                                 size = 6) +
    labs(x = "Prevalence Omicron", y = "Vaccination efficacy 2 doses") +
    # stat_summary(fun.df_to_plot=mean_cl_normal) + 
    geom_smooth(method='loess', formula=y~x,se = FALSE) +
    theme(axis.text=element_text(size=16),
                                     axis.title=element_text(size=20)) + ylim(ymin,ymax)
  
  ggsave(
    plot = plot_2,
    filename =  paste0(output_path, "Prevalence_Omicrom_vs_Vac_efficacy.jpg")
    , width = 12
    , height = 10
  )

  ggsave(
    plot = plot_3,
    filename =  paste0(output_path, "Prevalence_Omicrom_vs_Eff_1_dose.jpg")
    , width = 12
    , height = 10
  )

  ggsave(
    plot = plot_4,
    filename =  paste0(output_path, "Prevalence_Omicrom_vs_Eff_2_doses.jpg")
    , width = 12
    , height = 10
  )
}


box_plot_vaccines_efficacy <- function() {
  df_eff <- read.csv(file=input_file)
  cat("Total:", dim(df_eff), "\n")
  
  df_eff_agg_1 <- df_eff %>%
    select(c(ISO2,
             "3-VE Oct" = efficacy_RF_1 ,
             "4-VE Dec" = efficacy_RF_2 , 
             "5-VE 1D Oct" = efficacy_RF_vac1dose_1,
             "6-VE 1D Dec" = efficacy_RF_vac1dose_2, 
             "1-VE 2D Oct" = efficacy_RF_vac2doses_1,
             "2-VE 2D Dec" = efficacy_RF_vac2doses_2))
  
  
  df_to_plot <- df_eff_agg_1 %>% ungroup()
  
  # print(
  plot_1 <- df_to_plot %>% pivot_longer(-ISO2) %>%
    ggplot(aes(x=name,y=value,fill=name))+
    geom_boxplot(show.legend = FALSE) + 
    stat_boxplot(geom ='errorbar') +
    labs(x = "", y = "Vaccination efficacy") +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=20)) + ylim(ymin,ymax)
  # )
  
  ggsave(
    plot = plot_1,
    filename =  paste0(output_path, "Box_plot_vaccination_efficacy.jpg")
    , width = 12
    , height = 10
  )
}

box_plot_vaccines_efficacy()
Scatter_plot_vaccines_efficacy()