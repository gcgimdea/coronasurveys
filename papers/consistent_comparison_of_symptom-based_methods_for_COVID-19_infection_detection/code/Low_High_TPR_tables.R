input_path <- "~/IMDEA/comparing_our_model/Plots/"

resize.win <- function(Width=6, Height=6)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

#Tomamos los nombres de las oficinas
#Precision_vs_Sensitivity <- read.csv(paste0(input_path,"Precision_vs_Sensitivity.csv"))
lista <- c("BR_2020","CA_2020","DE_2020","JP_2020","ZA_2020")#"BR_2020","CA_2020","DE_2020","JP_2020","ZA_2020",

for (i in lista) {
  SensitivityVsSpecificity <- read.csv(paste0(input_path,"df_to_plot_",i,".csv"))
  SensitivityVsSpecificity <- SensitivityVsSpecificity[c(1:15,(dim(SensitivityVsSpecificity)[1]-2):dim(SensitivityVsSpecificity)[1]),]
  SensitivityVsSpecificity <- SensitivityVsSpecificity[-c(which(SensitivityVsSpecificity$X == "Smith_2")),]
  SensitivityVsSpecificity[c(1,2,4,10,11),"X"] <- c("Salomon","WHO","Astley","Smith","Roland")
  if(i == lista[1]){
    SensitivityVsSpecificity_BR <- SensitivityVsSpecificity
  }
  else if(i == lista[2]){
    SensitivityVsSpecificity_CA <- SensitivityVsSpecificity
  }
  else if(i == lista[3]){
    SensitivityVsSpecificity_DE <- SensitivityVsSpecificity
  }
  else if(i == lista[4]){
    SensitivityVsSpecificity_JP <- SensitivityVsSpecificity
  }
  else{
    SensitivityVsSpecificity_ZA <- SensitivityVsSpecificity
  }
}


df_tot_2020 <- (SensitivityVsSpecificity_BR$F1)*100 + (SensitivityVsSpecificity_CA$F1)*100 + (SensitivityVsSpecificity_DE$F1)*100 + (SensitivityVsSpecificity_JP$F1)*100 + (SensitivityVsSpecificity_ZA$F1)*100
df_tot_high_2020 <- (SensitivityVsSpecificity_BR$F1)*100 + (SensitivityVsSpecificity_ZA$F1)*100
df_tot_low_2020 <- (SensitivityVsSpecificity_CA$F1)*100 + (SensitivityVsSpecificity_DE$F1)*100 + (SensitivityVsSpecificity_JP$F1)*100
df_tot_2020 <- cbind(SensitivityVsSpecificity_BR$X,df_tot_2020/5)
df_tot_high_2020 <- cbind(SensitivityVsSpecificity_BR$X,df_tot_high_2020/2)
df_tot_low_2020 <- cbind(SensitivityVsSpecificity_BR$X,df_tot_low_2020/3)


lista <- c("BR_2021","CA_2021","DE_2021","JP_2021","ZA_2021")#"BR_2020","CA_2020","DE_2020","JP_2020","ZA_2020",

for (i in lista) {
  SensitivityVsSpecificity <- read.csv(paste0(input_path,"df_to_plot_",i,".csv"))
  SensitivityVsSpecificity <- SensitivityVsSpecificity[c(1:17,dim(SensitivityVsSpecificity)[1]),]
  SensitivityVsSpecificity <- SensitivityVsSpecificity[-c(which(SensitivityVsSpecificity$X == "Smith_2")),]
  SensitivityVsSpecificity[c(1,2,6,12,13),"X"] <- c("Salomon","WHO","Astley","Smith","Roland")
  if(i == lista[1]){
    SensitivityVsSpecificity_BR <- SensitivityVsSpecificity
  }
  else if(i == lista[2]){
    SensitivityVsSpecificity_CA <- SensitivityVsSpecificity
  }
  else if(i == lista[3]){
    SensitivityVsSpecificity_DE <- SensitivityVsSpecificity
  }
  else if(i == lista[4]){
    SensitivityVsSpecificity_JP <- SensitivityVsSpecificity
  }
  else{
    SensitivityVsSpecificity_ZA <- SensitivityVsSpecificity
  }
}
df_tot_2021 <- (SensitivityVsSpecificity_BR$F1)*100 + (SensitivityVsSpecificity_CA$F1)*100 + (SensitivityVsSpecificity_DE$F1)*100 + (SensitivityVsSpecificity_JP$F1)*100 + (SensitivityVsSpecificity_ZA$F1)*100
df_tot_high_2021 <- (SensitivityVsSpecificity_BR$F1)*100 + (SensitivityVsSpecificity_ZA$F1)*100
df_tot_low_2021 <- (SensitivityVsSpecificity_CA$F1)*100 + (SensitivityVsSpecificity_DE$F1)*100 + (SensitivityVsSpecificity_JP$F1)*100
df_tot_2021 <- cbind(SensitivityVsSpecificity_BR$X,df_tot_2021/5)
df_tot_high_2021 <- cbind(SensitivityVsSpecificity_BR$X,df_tot_high_2021/2)
df_tot_low_2021 <- cbind(SensitivityVsSpecificity_BR$X,df_tot_low_2021/3)

#Reagrupar para poder sumar
df_tot_2020_n <- rbind(df_tot_2020[1:2,],df_tot_2020[15:16,],df_tot_2020[c(3:14,17),])
df_tot_high_2020_n <- rbind(df_tot_high_2020[1:2,],df_tot_high_2020[15:16,],df_tot_high_2020[c(3:14,17),])
df_tot_low_2020_n <- rbind(df_tot_low_2020[1:2,],df_tot_low_2020[15:16,],df_tot_low_2020[c(3:14,17),])



df_tot <- cbind(df_tot_2020_n[,1],(as.numeric(df_tot_2020_n[,2]) + as.numeric(df_tot_2021[,2]))/2)
df_tot_high <- cbind(df_tot_high_2020_n[,1],(as.numeric(df_tot_high_2020_n[,2]) + as.numeric(df_tot_high_2021[,2]))/2)
df_tot_low <- cbind(df_tot_low_2020_n[,1],(as.numeric(df_tot_low_2020_n[,2]) + as.numeric(df_tot_low_2021[,2]))/2)
