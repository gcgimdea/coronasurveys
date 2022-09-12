library(dplyr)
library(xtable)
sumfun <- function(x) {
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(length(x))
  c(mean=m,sd=s,lwr=m-1.96*se,upr=m+1.96*se)
}

df_F1_2021 <- c()
df_sensitivity_2021 <- c()
df_specificity_2021 <- c()
df_precision_2021 <- c()
df_F1_2020 <- c()
df_sensitivity_2020 <- c()
df_specificity_2020 <- c()
df_precision_2020 <- c()

for (k in c("BR","CA","DE","JP","ZA")) {

  df_2021 <- read.csv(paste0("./IMDEA/comparing_our_model/Last_result_tables/df_",k,"_other_Metrics_CI_Symp_fin.csv"))
  df_2020 <- read.csv(paste0("./IMDEA/comparing_our_model/Last_result_tables/df_",k,"_other_Metrics_CI_Symp_fin_2020.csv"))
  
  for (i in c(1,2,3,4)) {
    
    df_2021_now <- df_2021[which(df_2021$X == i),]
    df_2020_now <- df_2020[which(df_2020$X == i),]
    df_fin_1 <- c()
    df_fin_2 <- c()
    
    for(j in colnames(df_2021)[2:ncol(df_2021)]){
      
      vect_now <- sumfun(df_2021_now[[j]])
      if(k == "ZA"){
        vect_now$name <- j
      }
      vect_now$sd <- NULL
      df_fin_1 <- rbind(df_fin_1,vect_now)
      
    }
    for(j in colnames(df_2020)[2:ncol(df_2020)]){
      
      vect_now <- sumfun(df_2020_now[[j]])
      if(i == 1){
        vect_now <- vect_now*100
      }
      if(k == "ZA"){
        vect_now$name <- j
      }
      vect_now$sd <- NULL
      df_fin_2 <- rbind(df_fin_2,vect_now)
      
    }
    if(i == 1){
      
      df_F1_2021 <-cbind(df_F1_2021,df_fin_1)
      df_F1_2020 <-cbind(df_F1_2020,df_fin_2)
       
    }
    else if(i == 2){
      
      df_specificity_2021 <-cbind(df_specificity_2021,df_fin_1)
      df_specificity_2020 <-cbind(df_specificity_2020,df_fin_2)
      
    }
    else if(i == 3){
      
      df_sensitivity_2021 <-cbind(df_sensitivity_2021,df_fin_1)
      df_sensitivity_2020 <-cbind(df_sensitivity_2020,df_fin_2)
      
    }
    else if(i == 4){
      
      df_precision_2021 <-cbind(df_precision_2021,df_fin_1)
      df_precision_2020 <-cbind(df_precision_2020,df_fin_2)
      
    }
  }
}

xtable(df_precision_2020)

Zoabi_2021 <- c()
Zoabi_2020 <- c()
for (k in c("BR","CA","DE","JP","ZA")) {
  print(k)
  df_2021 <- read.csv(paste0("./IMDEA/comparing_our_model/Last_result_tables/Zoabi_B5_B15_2/df_",k,"_Zoabi_Metrics_CI_Symp.csv"))
  df_2020 <- read.csv(paste0("./IMDEA/comparing_our_model/Last_result_tables/Zoabi_B5_B15_2/df_",k,"_Zoabi_Metrics_CI_Symp_2020.csv"))
  
  Zoabi_2021_now <- c()
  Zoabi_2020_now <- c()
  for (i in c("F1_score","Sensitivity","Specificity","Precision")) { #"AUC",
    
    df_2021_now <- df_2021[which(df_2021$X == i),]
    df_2020_now <- df_2020[which(df_2020$X == i),]
    
    for( j in colnames(df_2020)[2:3]){
      # cat("Country ",k," ",i," ",j,"\n")
      Zoabi_2021_now <- cbind(Zoabi_2021_now,sumfun(df_2021_now[[j]])[c(1,3,4)]*100)
      Zoabi_2020_now <- cbind(Zoabi_2020_now,sumfun(df_2020_now[[j]])[c(1,3,4)]*100)
    }
  }
  Zoabi_2020 <- rbind(Zoabi_2020,Zoabi_2020_now)
  Zoabi_2021 <- rbind(Zoabi_2021,Zoabi_2021_now)
  
}

colnames(Zoabi_2020) <- c("F1_55","F1_65","Sensitivity_55","Sensitivity_65","Specificity_55","Specificity_65","Precision_55","Precision_65")
colnames(Zoabi_2021) <- c("F1_55","F1_65","Sensitivity_55","Sensitivity_65","Specificity_55","Specificity_65","Precision_55","Precision_65")
