library(readr)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(scales)
library(ggplot2)
library(ggrepel)
library(png)
library(KraljicMatrix)

#input_path <- "IMDEA/CoronaSurvey/Other Experiments/Plot/"
input_path <- "~/IMDEA/comparing_our_model/Plots/"

resize.win <- function(Width=6, Height=6)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

#Tomamos los nombres de las oficinas
#Precision_vs_Sensitivity <- read.csv(paste0(input_path,"Precision_vs_Sensitivity.csv"))
lista <- c("BR_2021","CA_2021","DE_2021","JP_2021","ZA_2021")#"BR_2020","CA_2020","DE_2020","JP_2020","ZA_2020",
for(i in lista){
  SensitivityVsSpecificity <- read.csv(paste0(input_path,"df_to_plot_",i,".csv"))
  
  #Comparing paper for 2021
  SensitivityVsSpecificity <- SensitivityVsSpecificity[c(1:17,dim(SensitivityVsSpecificity)[1]),]
  SensitivityVsSpecificity <- SensitivityVsSpecificity[-c(which(SensitivityVsSpecificity$X == "Smith_2")),]
  SensitivityVsSpecificity[c(1,2,6,12,13),"X"] <- c("Salomon","WHO","Astley","Smith","Roland")
  # resize.win(100,100)
  plot_1 = ggplot(SensitivityVsSpecificity) + theme_light() + geom_frontier(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5]),size = 2) +  
                        labs(y="Precision", x = "Sensitivity") + theme(legend.position='none', axis.title.x=element_text(size=30, face = "plain", colour = "grey30"),axis.title.y=element_text(size=30, face = "plain", colour = "grey30"), axis.text.x=element_text(size=36),axis.text.y=element_text(size=36)) +
                        geom_point(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5],colour = factor(SensitivityVsSpecificity[,1])), size=5) + #, size=1.25
                        geom_label_repel(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5], label = SensitivityVsSpecificity[,1], colour = factor(SensitivityVsSpecificity[,1])), size = 10, max.overlaps = getOption("ggrepel.max.overlaps", default = 50)) + 
                        theme(legend.position="none") #  + ggtitle(paste0("Sensitivity_vs_Precision_",i)) #geom_step(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5]))
  plot_2 = ggplot(SensitivityVsSpecificity) + theme_light() + geom_frontier(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,4]),size = 2) + 
    labs(y="Specificity", x = "Sensitivity")+ theme(legend.position='none', axis.title.x=element_text(size=30, face = "plain", colour = "grey30"),axis.title.y=element_text(size=30, face = "plain", colour = "grey30"), axis.text.x=element_text(size=36),axis.text.y=element_text(size=36)) +
    geom_point(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,4],colour = factor(SensitivityVsSpecificity[,1])), size=5) + #, size=1.25
    geom_label_repel(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,4], label = SensitivityVsSpecificity[,1], colour = factor(SensitivityVsSpecificity[,1])), size = 10, max.overlaps = getOption("ggrepel.max.overlaps", default = 50)) +
    theme(legend.position="none") # + ggtitle(paste0("Sensitivity_vs_Specificity_",i))#geom_step(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5]))
  
  ggsave(
    plot = plot_1,
    filename =  paste0(input_path, "Images_sensitivity_vs_precision/Sensitivity_vs_precision_", i, "_compare_paper.png")
    , width = 12
    , height = 10
  )
  ggsave(
    plot = plot_2,
    filename =  paste0(input_path, "Images_sensitivity_vs_specificity/Sensitivity_vs_specificity_", i, "_compare_paper.png")
    , width = 12
    , height = 10
  )
}

lista <- c("BR_2020","CA_2020","DE_2020","JP_2020","ZA_2020")#"BR_2020","CA_2020","DE_2020","JP_2020","ZA_2020",
for(i in lista){
  SensitivityVsSpecificity <- read.csv(paste0(input_path,"df_to_plot_",i,".csv"))
  
  #Comparing paper for 2021
  SensitivityVsSpecificity <- SensitivityVsSpecificity[c(1:15,(dim(SensitivityVsSpecificity)[1]-2):dim(SensitivityVsSpecificity)[1]),]
  SensitivityVsSpecificity <- SensitivityVsSpecificity[-c(which(SensitivityVsSpecificity$X == "Smith_2")),]
  SensitivityVsSpecificity[c(1,2,4,10,11),"X"] <- c("Salomon","WHO","Astley","Smith","Roland")
  # resize.win(100,100)
  plot_1 = ggplot(SensitivityVsSpecificity) + theme_light() + geom_frontier(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5]),size = 2) + 
    labs(y="Precision", x = "Sensitivity",size=12) + theme(legend.position='none', axis.title.x=element_text(size=30, face = "plain", colour = "grey30"),axis.title.y=element_text(size=30, face = "plain", colour = "grey30"), axis.text.x=element_text(size=36),axis.text.y=element_text(size=36)) +
    geom_point(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5],colour = factor(SensitivityVsSpecificity[,1])), size=5) + #, size=1.25
    geom_label_repel(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5], label = SensitivityVsSpecificity[,1], colour = factor(SensitivityVsSpecificity[,1])), size = 10, max.overlaps = getOption("ggrepel.max.overlaps", default = 50)) + 
    theme(legend.position="none")  #+ ggtitle(paste0("Sensitivity_vs_Precision_",i))#geom_step(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5]))
  plot_2 = ggplot(SensitivityVsSpecificity) + theme_light() + geom_frontier(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,4]),size = 2) + 
    labs(y="Specificity", x = "Sensitivity",size=12) + theme(legend.position='none', axis.title.x=element_text(size=30, face = "plain", colour = "grey30"),axis.title.y=element_text(size=30, face = "plain", colour = "grey30"), axis.text.x=element_text(size=36),axis.text.y=element_text(size=36)) +
    geom_point(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,4],colour = factor(SensitivityVsSpecificity[,1])), size=5) + #, size=1.25
    geom_label_repel(aes(x = SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,4], label = SensitivityVsSpecificity[,1], colour = factor(SensitivityVsSpecificity[,1])), size = 10, max.overlaps = getOption("ggrepel.max.overlaps", default = 50)) +
    theme(legend.position="none") #+ ggtitle(paste0("Sensitivity_vs_Precision_",i))#geom_step(aes(SensitivityVsSpecificity[,3], y= SensitivityVsSpecificity[,5]))
  
  ggsave(
    plot = plot_1,
    filename =  paste0(input_path, "Images_sensitivity_vs_precision/Sensitivity_vs_precision_", i, "_compare_paper.png")
    , width = 12
    , height = 10
  )
  ggsave(
    plot = plot_2,
    filename =  paste0(input_path, "Images_sensitivity_vs_specificity/Sensitivity_vs_specificity_", i, "_compare_paper.png")
    , width = 12
    , height = 10
  )
}

#Arrange photos
# Sensitivity_vs_Specificity_BR_2020 <- readPNG(paste0(input_path,"Images_sensitivity_vs_specificity/Sensitivity_vs_Specificity_BR_2020_last.png"))
# Sensitivity_vs_Specificity_CA_2020 <- readPNG(paste0(input_path,"Images_sensitivity_vs_specificity/Sensitivity_vs_Specificity_CA_2020_last.png"))
# Sensitivity_vs_Specificity_DE_2020 <- readPNG(paste0(input_path,"Images_sensitivity_vs_specificity/Sensitivity_vs_Specificity_DE_2020_last.png"))
# Sensitivity_vs_Specificity_JP_2020 <- readPNG(paste0(input_path,"Images_sensitivity_vs_specificity/Sensitivity_vs_Specificity_JP_2020_last.png"))
# Sensitivity_vs_Specificity_ZA_2020 <- readPNG(paste0(input_path,"Images_sensitivity_vs_specificity/Sensitivity_vs_Specificity_ZA_2020_last.png"))
# rl <- list(Sensitivity_vs_Specificity_BR_2020,Sensitivity_vs_Specificity_CA_2020,Sensitivity_vs_Specificity_DE_2020,
#            Sensitivity_vs_Specificity_JP_2020,Sensitivity_vs_Specificity_ZA_2020)
# gl <- lapply(rl, grid::rasterGrob)
# do.call(gridExtra::grid.arrange, gl)
# 
# par(mfrow=c(2,3))
# plot(Sensitivity_vs_Specificity_BR_2020)
# plot(Sensitivity_vs_Specificity_CA_2020)
# plot(Sensitivity_vs_Specificity_DE_2020)
# plot(Sensitivity_vs_Specificity_JP_2020)
# plot(Sensitivity_vs_Specificity_ZA_2020)

# plot(NA, xlim = c(0, 200), ylim = c(0, 600), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# rasterImage(Sensitivity_vs_Specificity_BR_2020, 0, 0, 100, 200)
# rasterImage(Sensitivity_vs_Specificity_CA_2020, 0, 200, 100, 400)
# rasterImage(Sensitivity_vs_Specificity_DE_2020, 100, 0, 200, 100)
# rasterImage(Sensitivity_vs_Specificity_JP_2020, 100, 200, 200, 400)
# rasterImage(Sensitivity_vs_Specificity_ZA_2020, 50, 400, 150, 600)
