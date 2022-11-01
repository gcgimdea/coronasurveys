# library(lubridate)
library(useful, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
# library(readr)
# library(cowplot)
library(zoo, warn.conflicts = F, quietly = T)
library(data.table, warn.conflicts = F, quietly = T)
# library(DescTools)

plots_path <- "./plots/"
data_path <- "./data/"

miny <- 0
maxy <- 0.21

args <- commandArgs(trailingOnly = T)
# args <- c("ES", "2021-07-01", "2021-11-30")
cat(args, "\n")
if (length(args)<1) {
  stop("Arguments: country start_date end_date\n")
}

iso2 <- args[1]
cat("** Country:", iso2, "\n")

# Read files
file_short_csv <- paste0(iso2,".csv")
# file_short_rds <- paste0(iso2,".rds")
file_input_csv <- paste0(data_path, file_short_csv)
df <- fread(file_input_csv, data.table = FALSE)

# -- plotting
pal = c("red",
        "orange",
        "brown",
        "blue",
        "magenta",
        "darkgreen",
        "black",
        "#f9c74f")
# "#f9c74f",
# "#f3722c",
# "#43aa8b",
# "#90be6d",
# "#f8961e",
# "#577590")

# All curves

plot_all <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  # geom_line(aes(y = p_tested, color = "TPR"), linetype = "solid", size = 1, alpha = 0.5) +
  # geom_ribbon(aes(ymin = p_tested-p_tested_error, ymax = p_tested+p_tested_error, 
  #                 color = "TPR"
  #                 # , fill = "TPR"
  # ), alpha = 0.1, size = 0.1) +  
  geom_line(aes(y = p_cli, color = "2-UMD CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = p_cli-p_cli_error, ymax = p_cli+p_cli_error, 
                  color = "2-UMD CLI"
                  , fill = "2-UMD CLI"
                  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = p_stringent_cli, color = "3-Stringent CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = p_stringent_cli-p_stringent_cli_error, ymax = p_stringent_cli+p_stringent_cli_error, 
                  color = "3-Stringent CLI"
                  , fill = "3-Stringent CLI"
                  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = p_classic_cli, color = "4-Classic CLI", fill = "4-Classic CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = p_classic_cli-p_classic_cli_error, ymax = p_classic_cli+p_classic_cli_error, 
                  color = "4-Classic CLI"
                  , fill = "4-Classic CLI"
                  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = p_broad_cli, color = "5-Broad CLI", fill = "5-Broad CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = p_broad_cli-p_broad_cli_error, ymax = p_broad_cli+p_broad_cli_error, 
                  color = "5-Broad CLI"
                  , fill = "5-Broad CLI"
                  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = p_pos_RF, color = "1-Random Forest", fill = "1-Random Forest"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = p_pos_RF-p_pos_RF_error, ymax = p_pos_RF+p_pos_RF_error, 
                  color = "1-Random Forest"
                  , fill = "1-Random Forest"
                  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = p_confirmed, color = "6-Confirmed"), linetype = "solid", size = 1, alpha = 0.5) +
  labs(x = "Date", y = "Prevalence") +
  # ylim(lims[1], lims[2]) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) + 
  scale_y_log10() +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_all,
  filename =  paste0(plots_path, "all-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# -- Only curves scaled

plot_scaled_confirmed <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  # geom_line(aes(y = (p_tested - min(p_tested, na.rm = TRUE)) /  (max(p_tested, na.rm = TRUE)-min(p_tested, na.rm = TRUE)), 
  #               color = "TPR"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(aes(y = (p_cli - min(p_cli, na.rm = TRUE)) /  (max(p_cli, na.rm = TRUE)-min(p_cli, na.rm = TRUE)), 
                color = "2-UMD CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(aes(y = (p_stringent_cli - min(p_stringent_cli, na.rm = TRUE)) /
                  (max(p_stringent_cli, na.rm = TRUE)-min(p_stringent_cli, na.rm = TRUE)),
                color = "3-Stringent CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(aes(y = (p_classic_cli - min(p_classic_cli, na.rm = TRUE)) /  
                  (max(p_classic_cli, na.rm = TRUE)-min(p_classic_cli, na.rm = TRUE)),
                color = "4-Classic CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(aes(y = (p_broad_cli - min(p_broad_cli, na.rm = TRUE)) /  
                  (max(p_broad_cli, na.rm = TRUE)-min(p_broad_cli, na.rm = TRUE)),
                color = "5-Broad CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(aes(y = (p_pos_RF - min(p_pos_RF, na.rm = TRUE)) /  
                  (max(p_pos_RF, na.rm = TRUE)-min(p_pos_RF, na.rm = TRUE)), 
                color = "1-Random Forest"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(aes(y = (p_confirmed - min(p_confirmed, na.rm = TRUE)) /  
                  (max(p_confirmed, na.rm = TRUE)-min(p_confirmed, na.rm = TRUE)),
                color = "6-Confirmed"), linetype = "solid", size = 1, alpha = 0.5) +
  labs(x = "Date", y = "Prevalence") +
  # ylim(lims[1], lims[2]) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ggsave(
  plot = plot_scaled_confirmed,
  filename =  paste0(plots_path, "scaled-all-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# Plots PV

plot_PV <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  geom_line(aes(y = PV_cli, color = "2-UMD CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV_cli-PV_cli_error, ymax = PV_cli+PV_cli_error,
                  color = "2-UMD CLI"
                  , fill = "2-UMD CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV_stringent_cli, color = "3-Stringent CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV_stringent_cli-PV_stringent_cli_error, ymax = PV_stringent_cli+PV_stringent_cli_error, 
                  color = "3-Stringent CLI"
                  , fill = "3-Stringent CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV_classic_cli, color = "4-Classic CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV_classic_cli-PV_classic_cli_error, ymax = PV_classic_cli+PV_classic_cli_error,
                  color = "4-Classic CLI"
                  , fill = "4-Classic CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV_broad_cli, color = "5-Broad CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV_broad_cli-PV_broad_cli_error, ymax = PV_broad_cli+PV_broad_cli_error,
                  color = "5-Broad CLI"
                  , fill = "5-Broad CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV_pos_RF, color = "1-Random Forest"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV_pos_RF-PV_pos_RF_error, ymax = PV_pos_RF+PV_pos_RF_error, 
                  color = "1-Random Forest"
                  , fill = "1-Random Forest"
  ), alpha = 0.1, size = 0.1) +
  labs(x = "Date", y = "Prevalence") +
  ylim(miny, maxy) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_PV,
  filename =  paste0(plots_path, "PV-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# Plots PU

plot_PU <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  geom_line(aes(y = PU_cli, color = "2-UMD CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PU_cli-PU_cli_error, ymax = PU_cli+PU_cli_error,
                  color = "2-UMD CLI"
                  , fill = "2-UMD CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PU_stringent_cli, color = "3-Stringent CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PU_stringent_cli-PU_stringent_cli_error, ymax = PU_stringent_cli+PU_stringent_cli_error, 
                  color = "3-Stringent CLI"
                  , fill = "3-Stringent CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PU_classic_cli, color = "4-Classic CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PU_classic_cli-PU_classic_cli_error, ymax = PU_classic_cli+PU_classic_cli_error,
                  color = "4-Classic CLI"
                  , fill = "4-Classic CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PU_broad_cli, color = "5-Broad CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PU_broad_cli-PU_broad_cli_error, ymax = PU_broad_cli+PU_broad_cli_error,
                  color = "5-Broad CLI"
                  , fill = "5-Broad CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PU_pos_RF, color = "1-Random Forest"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PU_pos_RF-PU_pos_RF_error, ymax = PU_pos_RF+PU_pos_RF_error, 
                  color = "1-Random Forest"
                  , fill = "1-Random Forest"
  ), alpha = 0.1, size = 0.1) +
  labs(x = "Date", y = "Prevalence") +
  ylim(miny, maxy) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_PU,
  filename =  paste0(plots_path, "PU-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# Plots PV1D

plot_PV1D <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  geom_line(aes(y = PV1D_cli, color = "2-UMD CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV1D_cli-PV1D_cli_error, ymax = PV1D_cli+PV1D_cli_error,
                  color = "2-UMD CLI"
                  , fill = "2-UMD CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV1D_stringent_cli, color = "3-Stringent CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV1D_stringent_cli-PV1D_stringent_cli_error, ymax = PV1D_stringent_cli+PV1D_stringent_cli_error, 
                  color = "3-Stringent CLI"
                  , fill = "3-Stringent CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV1D_classic_cli, color = "4-Classic CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV1D_classic_cli-PV1D_classic_cli_error, ymax = PV1D_classic_cli+PV1D_classic_cli_error,
                  color = "4-Classic CLI"
                  , fill = "4-Classic CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV1D_broad_cli, color = "5-Broad CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV1D_broad_cli-PV1D_broad_cli_error, ymax = PV1D_broad_cli+PV1D_broad_cli_error,
                  color = "5-Broad CLI"
                  , fill = "5-Broad CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV1D_pos_RF, color = "1-Random Forest"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV1D_pos_RF-PV1D_pos_RF_error, ymax = PV1D_pos_RF+PV1D_pos_RF_error, 
                  color = "1-Random Forest"
                  , fill = "1-Random Forest"
  ), alpha = 0.1, size = 0.1) +
  labs(x = "Date", y = "Prevalence") +
  ylim(miny, maxy) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_PV1D,
  filename =  paste0(plots_path, "PV1D-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# Plots PV2D

plot_PV2D <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  geom_line(aes(y = PV2D_cli, color = "2-UMD CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV2D_cli-PV2D_cli_error, ymax = PV2D_cli+PV2D_cli_error,
                  color = "2-UMD CLI"
                  , fill = "2-UMD CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV2D_stringent_cli, color = "3-Stringent CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV2D_stringent_cli-PV2D_stringent_cli_error, ymax = PV2D_stringent_cli+PV2D_stringent_cli_error, 
                  color = "3-Stringent CLI"
                  , fill = "3-Stringent CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV2D_classic_cli, color = "4-Classic CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV2D_classic_cli-PV2D_classic_cli_error, ymax = PV2D_classic_cli+PV2D_classic_cli_error,
                  color = "4-Classic CLI"
                  , fill = "4-Classic CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV2D_broad_cli, color = "5-Broad CLI"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV2D_broad_cli-PV2D_broad_cli_error, ymax = PV2D_broad_cli+PV2D_broad_cli_error,
                  color = "5-Broad CLI"
                  , fill = "5-Broad CLI"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV2D_pos_RF, color = "1-Random Forest"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV2D_pos_RF-PV2D_pos_RF_error, ymax = PV2D_pos_RF+PV2D_pos_RF_error, 
                  color = "1-Random Forest"
                  , fill = "1-Random Forest"
  ), alpha = 0.1, size = 0.1) +
  labs(x = "Date", y = "Prevalence") +
  ylim(miny, maxy) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_PV2D,
  filename =  paste0(plots_path, "PV2D-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# Plots RF

plot_RF <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  geom_line(aes(y = PV_pos_RF, color = "3-Vaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV_pos_RF-PV_pos_RF_error, ymax = PV_pos_RF+PV_pos_RF_error, 
                  color = "3-Vaccinated"
                  , fill = "3-Vaccinated"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PU_pos_RF, color = "4-Unvaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PU_pos_RF-PU_pos_RF_error, ymax = PU_pos_RF+PU_pos_RF_error, 
                  color = "4-Unvaccinated"
                  , fill = "4-Unvaccinated"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV1D_pos_RF, color = "1-Vacc 1 dose"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV1D_pos_RF-PV1D_pos_RF_error, ymax = PV1D_pos_RF+PV1D_pos_RF_error, 
                  color = "1-Vacc 1 dose"
                  , fill = "1-Vacc 1 dose"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = PV2D_pos_RF, color = "2-Vacc 2 doses"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = PV2D_pos_RF-PV2D_pos_RF_error, ymax = PV2D_pos_RF+PV2D_pos_RF_error, 
                  color = "2-Vacc 2 doses"
                  , fill = "2-Vacc 2 doses"
  ), alpha = 0.1, size = 0.1) +
  labs(x = "Date", y = "Prevalence") +
  ylim(miny, maxy) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_RF,
  filename =  paste0(plots_path, "RF-", iso2, ".jpg")
  , width = 12
  , height = 10
)


# Plots Efficacy

cat("Plots Efficacy\n")

plot_Efficacy <- ggplot(data = df, aes(x = date, color = ""))  +
  # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
  # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
  geom_line(aes(y = efficacy_RF, color = "3-Vaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = efficacy_RF_lower, ymax = efficacy_RF_upper, 
                  color = "3-Vaccinated"
                  , fill = "3-Vaccinated"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = efficacy1D_RF, color = "1-Vacc 1 dose"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = efficacy1D_RF_lower, ymax = efficacy1D_RF_upper,
                  color = "1-Vacc 1 dose"
                  , fill = "1-Vacc 1 dose"
  ), alpha = 0.1, size = 0.1) +
  geom_line(aes(y = efficacy2D_RF, color = "2-Vacc 2 doses"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = efficacy2D_RF_lower, ymax = efficacy2D_RF_upper,
                  color = "2-Vacc 2 doses"
                  , fill = "2-Vacc 2 doses"
  ), alpha = 0.1, size = 0.1) +
  labs(x = "Date", y = "Efficacy") +
  # ylim(lims[1], lims[2]) +
  theme_bw() +
  # ggtitle(paste0("Active cases in ", iso2)) +
  scale_colour_manual(
    values = pal,
    name = "", # "Estimates",
    # guide = guide_legend(
    #   override.aes = list(
    #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
    #     shape = c(1, 1, 1, 1, 1, 1)
    # ))
  ) +
  theme(legend.position = "bottom") + theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(
    fill=FALSE,
    colour = guide_legend(override.aes = list(fill = NA))
  )

ggsave(
  plot = plot_Efficacy,
  filename =  paste0(plots_path, "Efficacy-RF-", iso2, ".jpg")
  , width = 12
  , height = 10
)

# Plots RF relative to var_delta
# delta_pos <- which(df$date==date_delta)
# 
# plot_RF_relative <- ggplot(data = df, aes(x = date, color = ""))  +
#   # geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
#   # geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
#   geom_line(aes(y = PV_pos_RF / PV_pos_RF[delta_pos], color = "3-Vaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = (PV_pos_RF-PV_pos_RF_error) / PV_pos_RF[delta_pos],
#                   ymax = (PV_pos_RF+PV_pos_RF_error) / PV_pos_RF[delta_pos],
#                   color = "3-Vaccinated"
#                   # , fill = "3-Vaccinated"
#   ), alpha = 0.1, size = 0.1) +
#   geom_line(aes(y = PU_pos_RF / PU_pos_RF[delta_pos], color = "4-Unvaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = (PU_pos_RF-PU_pos_RF_error) / PU_pos_RF[delta_pos],
#                   ymax = (PU_pos_RF+PU_pos_RF_error) / PU_pos_RF[delta_pos],
#                   color = "4-Unvaccinated"
#                   # , fill = "4-Unvaccinated"
#   ), alpha = 0.1, size = 0.1) +
#   geom_line(aes(y = PV1D_pos_RF / PV1D_pos_RF[delta_pos], color = "1-Vacc 1 dose"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = (PV1D_pos_RF-PV1D_pos_RF_error) / PV1D_pos_RF[delta_pos],
#                   ymax = (PV1D_pos_RF+PV1D_pos_RF_error) / PV1D_pos_RF[delta_pos],
#                   color = "1-Vacc 1 dose"
#                   # , fill = "1-Vacc 1 dose"
#   ), alpha = 0.1, size = 0.1) +
#   geom_line(aes(y = PV2D_pos_RF / PV2D_pos_RF[delta_pos], color = "2-Vacc 2 doses"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = (PV2D_pos_RF-PV2D_pos_RF_error) / PV2D_pos_RF[delta_pos],
#                   ymax = (PV2D_pos_RF+PV2D_pos_RF_error) / PV2D_pos_RF[delta_pos],
#                   color = "2-Vacc 2 doses"
#                   # , fill = "2-Vacc 2 doses"
#   ), alpha = 0.1, size = 0.1) +
#   labs(x = "Date", y = "Ratio of cases") +
#   # ylim(lims[1], lims[2]) +
#   theme_bw() +
#   # ggtitle(paste0("Active cases in ", iso2)) +
#   scale_colour_manual(
#     values = pal,
#     name = "", # "Estimates",
#     # guide = guide_legend(
#     #   override.aes = list(
#     #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
#     #     shape = c(1, 1, 1, 1, 1, 1)
#     # ))
#   ) +
#   theme(legend.position = "bottom") + theme(
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20)
#   )
# 
# ggsave(
#   plot = plot_RF_relative,
#   filename =  paste0(plots_path, "RF-relative-", iso2, ".jpg")
#   , width = 12
#   , height = 10
# )

# # Plots stringent
# 
# plot_stringent <- ggplot(data = df, aes(x = date, color = ""))  +
#   geom_vline(xintercept = var_delta$date, linetype="solid", color = "blue", size=var_delta$perc_sequences/100) +
#   geom_vline(xintercept = var_omicron$date, linetype="solid", color = "red", size=var_omicron$perc_sequences/100) +
#   geom_line(aes(y = PV_stringent_cli, color = "3-Vaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = PV_stringent_cli-PV_stringent_cli_error, ymax = PV_stringent_cli+PV_stringent_cli_error, 
#                   color = "3-Vaccinated"
#                   # , fill = "3-Vaccinated"
#   ), alpha = 0.1, size = 0.1) +
#   geom_line(aes(y = PU_stringent_cli, color = "4-Unvaccinated"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = PU_stringent_cli-PU_stringent_cli_error, ymax = PU_stringent_cli+PU_stringent_cli_error, 
#                   color = "4-Unvaccinated"
#                   # , fill = "4-Unvaccinated"
#   ), alpha = 0.1, size = 0.1) +
#   geom_line(aes(y = PV1D_stringent_cli, color = "1-Vacc 1 dose"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = PV1D_stringent_cli-PV1D_stringent_cli_error, ymax = PV1D_stringent_cli+PV1D_stringent_cli_error, 
#                   color = "1-Vacc 1 dose"
#                   # , fill = "1-Vacc 1 dose"
#   ), alpha = 0.1, size = 0.1) +
#   geom_line(aes(y = PV2D_stringent_cli, color = "2-Vacc 2 doses"), linetype = "solid", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = PV2D_stringent_cli-PV2D_stringent_cli_error, ymax = PV2D_stringent_cli+PV2D_stringent_cli_error, 
#                   color = "2-Vacc 2 doses"
#                   # , fill = "2-Vacc 2 doses"
#   ), alpha = 0.1, size = 0.1) +
#   labs(x = "Date", y = "Ratio of cases") +
#   # ylim(lims[1], lims[2]) +
#   theme_bw() +
#   # ggtitle(paste0("Active cases in ", iso2)) +
#   scale_colour_manual(
#     values = pal,
#     name = "", # "Estimates",
#     # guide = guide_legend(
#     #   override.aes = list(
#     #     linetype = c( "solid", "solid", "solid", "solid", "solid", "solid"),
#     #     shape = c(1, 1, 1, 1, 1, 1)
#     # ))
#   ) +
#   theme(legend.position = "bottom") + theme(
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20)
#   )
# 
# ggsave(
#   plot = plot_stringent,
#   filename =  paste0(plots_path, "stringent-", iso2, ".jpg")
#   , width = 12
#   , height = 10
# )
