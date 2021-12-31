library(data.table)
library(tidyverse)

input_path <- "../data/aggregate/"
output_path <- "../data/common-data/"

# input_path <- "../coronasurveys/data/aggregate/"
# output_path <- "./data/"

calculate_reach_by_country <- function(country,input_path,Table_Reach_By_Countries){
  
  #Read the file
  Country_now<- fread(paste0(input_path,country,"-aggregate.csv"), data.table=F, integer64 = "double")

  #Clean the column that we are going to use
  # Country_cols_now <- Country_now %>% select(c("Reach","ISO-3166-2","Cookie"))
  Reach_now <-as.double(na.omit(Country_now$Reach))
  # View(Reach_now,"\n")
  cutoff_now <- boxplot.stats(Reach_now, coef=1.5)$stats[5]
  # cat(cutoff_now,"\n")
  Reach_now_Filtered <- Reach_now[which(Reach_now <= cutoff_now)]
  
  #Get the stats
  Number_of_reach <- length(Reach_now_Filtered)
  Reach_now_mean <- mean(Reach_now_Filtered)
  Reach_now_var <- var(Reach_now_Filtered)
  
  #Add country to the data frame
  Table_Reach_By_Countries<- rbind(Table_Reach_By_Countries,c(i,Reach_now_mean,Reach_now_var,Number_of_reach))
  colnames(Table_Reach_By_Countries) <- c("Country","Reach_Mean","Reach_Var","Count")
  return(Table_Reach_By_Countries)
}

calculate_reach_by_country_region <- function(country,input_path,Table_Reach_By_Countries){
  
  #Read the file
  Country_now<- fread(paste0(input_path,country,"-aggregate.csv"), data.table=F, integer64 = "double")
  
  #Clean the column that we are going to use (reach) and select the columns interesting for us ("ISO-3166-2","Cookie")
  Country_cols_now <- Country_now %>% select(c("Reach","ISO-3166-2","Cookie"))
  Country_cols_now$Reach<- as.double(Country_cols_now$Reach)
  Country_cols_now <- Country_cols_now[!is.na(Country_cols_now$Reach),]
  cutoff_now <- boxplot.stats(Country_cols_now$Reach, coef=1.5)$stats[5]
  Country_cols_now <- Country_cols_now[which(Country_cols_now$Reach <= cutoff_now),]
  
  #Get the data were we have the same cookie for multiple responses
  Country_cookies <- table(Country_cols_now$Cookie)
  Country_cookies <- Country_cookies[Country_cookies> 1]
  # View(Country_cookies)
  for( k in names(Country_cookies)){
    if(!is.na(k)){ #names(k) != "" 
      Row_To_save<-Country_cols_now[which(Country_cols_now$`Cookie` == k),]
      Country_cols_now <- Country_cols_now[-which(Country_cols_now$`Cookie` == k),]
      Country_cols_now <- rbind(Country_cols_now,Row_To_save[nrow(Row_To_save),])
    }
  }
  
  #All of region with nothing or NA in the region set as 0
  ISO <-Country_cols_now$`ISO-3166-2`
  ISO[is.na(ISO)] <- country
  ISO[ISO == ""] <- country
  Country_cols_now$`ISO-3166-2` <- ISO
  # View(Country_cols_now)
  
  #Get the stats and put it into the table for each region of the country
  for(j in unique(Country_cols_now$`ISO-3166-2`)){
    Country_cols_now_Reg <- Country_cols_now[which(Country_cols_now$`ISO-3166-2` == j),] 
    # View(Country_cols_now_Reg)
    
    Number_of_reach <- nrow(Country_cols_now_Reg)
    Reach_now_mean <- mean(Country_cols_now_Reg$Reach)
    Reach_now_var <- var(Country_cols_now_Reg$Reach)
    
    #Add country to the data frame
    Table_Reach_By_Countries_region<- rbind(Table_Reach_By_Countries_region,c(i,j,Reach_now_mean,Reach_now_var,Number_of_reach))
    colnames(Table_Reach_By_Countries_region) <- c("Country","Region","Reach_Mean","Reach_Var","Count")
  }
  
  return(Table_Reach_By_Countries_region)
}

#MAIN
#Path and get all the names of the countries in that folder
countries<-substr(list.files(input_path, pattern = ".csv"),1,2)

#Call the function for each country
Table_Reach_By_Countries<-c()
Table_Reach_By_Countries_region<-c()
for (i in countries){
  Table_Reach_By_Countries <- calculate_reach_by_country(i,input_path,Table_Reach_By_Countries)
  Table_Reach_By_Countries_region <- calculate_reach_by_country_region(i,input_path,Table_Reach_By_Countries)
}

#write the table
write.csv(Table_Reach_By_Countries, paste0(output_path, "reach_per_country.csv"))
write.csv(Table_Reach_By_Countries_region, paste0(output_path, "reach_per_region.csv"))
