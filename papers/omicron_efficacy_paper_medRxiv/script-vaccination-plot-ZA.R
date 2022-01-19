library(tidyverse, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)


umd_path <- "./"
input_path_1 <- "/aggregates/country/"
output_path <- "./plots/"

quarter_list <- c("2021-Q2", "2021-Q3", "2021-Q4")

start_date <- as.Date("2021-06-18")
end_date <- as.Date("2021-12-31")
# end_date <- Sys.Date()

smooth_param <- 14
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  numerator <- rollsum(numerator, smooth_param, fill=NA, align = "right")
  denominator <- rollsum(denominator, smooth_param, fill=NA, align = "right")
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se)) }

plot_positive_vaccinated_percentage_by_dose <- function(iso2, equilibrate) {
  cat("Country:", iso2, "\n")
  
  # Read files
  file_short_csv <- paste0(iso2,".csv")
  file_short_rds <- paste0(iso2,".rds")
  umd <- NULL
  for (quarter in quarter_list) {
    input_path <- paste0(umd_path, quarter, input_path_1)
    file_input_csv <- paste0(input_path, file_short_csv)
    # file_input_rds <- paste0(input_path, file_short_rds)
    # if (file.exists(file_input)){
    # umd_aux <- readRDS(file=file_input_rds)
    umd_aux <- fread(file_input_csv, data.table = FALSE)
    # cat(iso2, quarter, dim(umd_aux), "\n")
    # umd_aux <- umd_aux %>% 
    #   select(-contains("."))
    # cat(colnames(umd_aux), "\n")
    umd <- dplyr::bind_rows(umd, umd_aux) 
    # cat(iso2, quarter, "Total:", dim(umd), "\n")
    # }
  }
  
  df_golden <- umd
  df_golden$date <-as.Date(df_golden$date) 
  
  
  cat("Total:", dim(df_golden), iso2, "\n")
  # df_golden_agg_1 <- df_golden %>%
  #   group_by(week_date = floor_date(date, "week")) %>%
  #   # summarize(positive = sum(positive_recent), positive_Fever_B1_1 = sum(positive_Fever_B1_1),positive_Cough_B1_2 = sum(positive_Cough_B1_2),positive_Difficulty_breathing_B1_3 = sum(positive_Difficulty_breathing_B1_3),positive_Fatigue_B1_4 = sum(positive_Fatigue_B1_4),positive_Stuffy_or_runny_nose_B1_5 = sum(positive_Stuffy_or_runny_nose_B1_5),positive_Aches_or_muscle_pain_B1_6 = sum(positive_Aches_or_muscle_pain_B1_6),positive_Sore_throat_B1_7 = sum(positive_Sore_throat_B1_7),positive_Chest_pain_B1_8 = sum(positive_Chest_pain_B1_8),positive_Nausea_B1_9 = sum(positive_Nausea_B1_9),positive_Loss_of_smell_or_taste_B1_10 = sum(positive_Loss_of_smell_or_taste_B1_10),positive_Headache_B1_12 = sum(positive_Headache_B1_12),positive_Chills_B1_13 = sum(positive_Chills_B1_13))
  #   # summarize(positive_vaccinated = sum(positive_vaccinated), positive_unvaccinated = sum(positive_unvaccinated),unvaccinated = sum(unvaccinated),vaccinated = sum(vaccinated))
  #   summarize(day_count = sum(day_count), vaccinated = sum(vaccinated), unvaccinated = sum(unvaccinated))
  

  df_golden_agg_1 <- df_golden %>%
    select(c(date,vac1dose,vac2doses,count,vaccinated,unvaccinated ))
  df_golden_agg_1$total <- df_golden_agg_1$vaccinated + df_golden_agg_1$unvaccinated

  est <- process_ratio(df_golden_agg_1$vac1dose,df_golden_agg_1$total)
  est_1 <- process_ratio(df_golden_agg_1$vac2doses,df_golden_agg_1$total)
  # est_2 <- process_ratio(df_golden_agg_1$vaccinated - df_golden_agg_1$vac1dose - df_golden_agg_1$vac2doses,df_golden_agg_1$total)
  est_3 <- process_ratio(df_golden_agg_1$unvaccinated,df_golden_agg_1$total)
  # est_4 <- process_ratio(df_golden_agg_1$unvaccinated + df_golden_agg_1$vaccinated,df_golden_agg_1$total)
  

  df_golden_agg_1$per_vaccinated_1_dose <- est$val
  df_golden_agg_1$per_vaccinated_2_dose <- est_1$val
  # df_golden_agg_1$per_vaccinated_NA_doses <- est_2$val
  df_golden_agg_1$per_unvaccinated <- est_3$val
  # df_golden_agg_1$per_Non_reported <- 1 - est_4$val
  
  df_golden_agg_1 <- df_golden_agg_1 %>% ungroup()
  
  df_to_plot <- df_golden_agg_1 %>%
    select(c(date,"Unvaccinated" = per_unvaccinated,
             # "Vaccinated" = per_vaccinated_NA_doses,
             "Vacc 1 dose" = per_vaccinated_1_dose,
             "Vacc 2 doses" = per_vaccinated_2_dose))  #,per_Non_reported 
  
  # colnames(df_to_plot)
  # 
  # df_to_plot[is.na(df_to_plot)] <- 0
  
  df_to_plot <- df_to_plot[which(df_to_plot$date >= start_date & df_to_plot$date <= end_date),]
  
  d_1 <- reshape2::melt(df_to_plot, id.vars="date")
  
  # View(d_1)
  
  # Everything on the same plot
  plot_1 <- ggplot(d_1, aes(date,
                            value,
                            fill = variable,
                            col=variable, alpha = 0.5)) + #
          geom_area() +
          # stat_smooth() +
          scale_fill_manual(values=c("Brown","yellow","red")) + 
          scale_color_manual(values=c("Brown","yellow","red")) +
          labs(x = "date", y = "Ratio") + 
    theme(legend.title = element_text(size = 0),
          legend.text = element_text(size = 16),
          # legend.position = "bottom",
          axis.text=element_text(size=16),
          axis.title=element_text(size=20))+ scale_alpha(guide = 'none')
  
  ggsave(
    plot = plot_1,
    filename =  paste0(output_path, "Vaccination_ratios_area_plot.jpg")
    , width = 12
    , height = 10
  )
  
  # print(ggplot(d_1, aes(date,value, fill = variable,col=variable, alpha = 0.5)) +
  #         geom_point() +
  #         stat_smooth() +
  #         labs(x = "date", y = "Ratio"))

}
plot_positive_vaccinated_percentage_by_dose("ZA",0)
