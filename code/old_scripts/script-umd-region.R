# importo librerias
library(readr)
library(lubridate)
library(zoo)
library(dplyr)
library(tidyverse)

responses_path <- "../data/estimates-symptom-survey/aggregated-data/region/"
data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/region/"

# leo los ficheros de interes
files <- list.files(path=responses_path, pattern="*.csv", full.names=TRUE, recursive=FALSE)

paises <- read.csv(data_path, as.is = T, na.string = "NaN")

# fechas <-seq(start,hoy,by=1)
fechas <- list.files(path=responses_path, pattern="*.csv", full.names=FALSE, recursive=FALSE)
fechas <- substring(fechas,1,10)
start <- as.Date(fechas[1])
hoy <- as.Date(fechas[length(fechas)])

# creo unas fechas limites para "jugar" con los ficheros
# start <- as.Date("2020-04-23")

#cambiar la siguiente linea para hacerlo automatico y poner Sys.Date()
# hoy <- Sys.Date()

reach <- 53
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

lista_cli <- c()
lista_count <- c()
lista_anosmia <- c()
lista_cli_local <- c()
lista_count_local <- c()
lista_pais <- c()
lista_fecha <- c()
lista_iso2 <- c()
lista_iso3 <- c()
lista_population <- c()
lista_region <- c()

# fechas <-seq(start,hoy,by=1)
dw <- data.frame()

for (i in 1:length(files)){
  csv <- read.csv(files[i])
  csv_merge <- merge (csv, paises, by = "ISO3", all.x=TRUE)
  fecha_hoy <- as.Date(fechas[i]) 
  
  for (j in 1:dim(csv_merge)[1]){
    pais <- csv_merge$country.x[j]
    region <- csv_merge$region[j]
    iso2 <- csv_merge$ISO2[j]
    iso3 <- csv_merge$ISO3[j]
    cli <- csv_merge$cli[j]
    count <- csv_merge$count[j]
    anosmia <- csv_merge$anosmia[j]
    cli_local <- csv_merge$cli_local_com[j]
    count_local <- csv_merge$count_local_com[j]
    population <- csv_merge$population[j]
    lista_pais[[length(lista_pais)+1]] <- pais
    lista_region[[length(lista_region)+1]] <- region
    lista_population[[length(lista_population)+1]] <- population
    lista_iso2[[length(lista_iso2)+1]] <- iso2
    lista_iso3[[length(lista_iso3)+1]] <- iso3
    lista_fecha[[length(lista_fecha)+1]] <- as.Date(fecha_hoy, origin="1970-01-01")
    lista_cli[[length(lista_cli)+1]] <- cli
    lista_count[[length(lista_count)+1]] <- count
    lista_cli_local[[length(lista_cli_local)+1]] <- cli_local
    lista_count_local[[length(lista_count_local)+1]] <- count_local
    lista_anosmia[[length(lista_anosmia)+1]] <- anosmia
  }
}

df_aux <- data.frame(
      "country" = as.vector(unlist(lista_pais)),
      "region" = as.vector(unlist(lista_region)),
      "ISO2" = as.vector(unlist(lista_iso2)),
      "ISO3" = as.vector(unlist(lista_iso3)),
      "date" = as.Date(as.vector(unlist(lista_fecha)), origin="1970-01-01"),
      "cli" = as.vector(unlist(lista_cli)),
      "anosmia" = as.vector(unlist(lista_anosmia)),
      "count" = as.vector(unlist(lista_count)),
      "cli_local" = as.vector(unlist(lista_cli_local)),
      "count_local" = as.vector(unlist(lista_count_local))
)
      
# te selecciona todas los paisess que salgan en los datos sin repetir
df_isoo <- df_aux[!duplicated(df_aux$ISO2),c("country","region","ISO2","ISO3")] 
# te selecciona todas las regiones que salgan en los datos sin repetir
df_region <- df_aux[!duplicated(df_aux$region),c("country","region","ISO2","ISO3")] 

dw <- data.frame()

for (h in 1:dim(df_isoo)[1]){
  cat(df_isoo$ISO2[h],"\n")
  df_country1 <- df_aux[df_aux$ISO2 == df_isoo$ISO2[h], ]
  df_region_country <- df_country1[!duplicated(df_country1$region), c("country","region","ISO2","ISO3")]
  df_region_country<- na.exclude(df_region_country)
  
  for (i in 1:dim(df_region_country)[1]){
    df_country <- df_aux[df_aux$ISO2 == df_isoo$ISO2[h] & df_aux$region == df_region_country$region[i], ]
    df_country_nan<- na.exclude(df_country)
    df_country_ok<- df_country_nan[!duplicated(df_country_nan),]
    dfdf <- df_country_ok %>%
      complete(date = seq.Date(start, hoy, by="day"), region =df_region_country$region[i], country=df_isoo$country[h], ISO2=df_isoo$ISO2[h], ISO3=df_isoo$ISO3[h] )
      
    # dfdf$cli_7days <- rollapply(dfdf$cli,7,sum,fill=NA,na.rm = TRUE,align="right")
    # dfdf$count_7days <- rollapply(dfdf$count,7,sum,fill=NA,na.rm = TRUE,align="right")
    # dfdf$cli_local_7days <- rollapply(dfdf$cli_local,7,sum,fill=NA,na.rm = TRUE,align="right")
    # dfdf$count_local_7days <- rollapply(dfdf$count_local,7,sum,fill=NA,na.rm = TRUE,align="right")
    
    dfdf$cli_14days <- rollapply(dfdf$cli,14,sum,fill=NA,na.rm = TRUE,align="right")
    dfdf$anosmia_14days <- rollapply(dfdf$anosmia,14,sum,fill=NA,na.rm = TRUE,align="right")
    dfdf$count_14days <- rollapply(dfdf$count,14,sum,fill=NA,na.rm = TRUE,align="right")
    dfdf$cli_local_14days <- rollapply(dfdf$cli_local,14,sum,fill=NA,na.rm = TRUE,align="right")
    dfdf$count_local_14days <- rollapply(dfdf$count_local,14,sum,fill=NA,na.rm = TRUE,align="right")

    est <- process_ratio(dfdf$cli, dfdf$count)
    dfdf$p_cli <- est$val
    dfdf$p_cli_low <- est$low
    dfdf$p_cli_high <- est$high
    
    est <- process_ratio(dfdf$anosmia, dfdf$count)
    dfdf$p_anosmia <- est$val
    dfdf$p_anosmia_low <- est$low
    dfdf$p_anosmia_high <- est$high
    
    est <- process_ratio(dfdf$cli_local, reach * dfdf$count_local)
    dfdf$p_cli_local <- est$val
    dfdf$p_cli_local_low <- est$low
    dfdf$p_cli_local_high <- est$high
    
    # est <- process_ratio(dfdf$cli_7days, dfdf$count_7days)
    # dfdf$p_cli_7days <- est$val
    # dfdf$p_cli_7days_low <- est$low
    # dfdf$p_cli_7days_high <- est$high
  
    est <- process_ratio(dfdf$cli_14days, dfdf$count_14days)
    dfdf$p_cli_14days <- est$val
    dfdf$p_cli_14days_low <- est$low
    dfdf$p_cli_14days_high <- est$high
  
    est <- process_ratio(dfdf$anosmia_14days, dfdf$count_14days)
    dfdf$p_anosmia_14days <- est$val
    dfdf$p_anosmia_14days_low <- est$low
    dfdf$p_anosmia_14days_high <- est$high
    
    # est <- process_ratio(dfdf$cli_local_7days, reach * dfdf$count_local_7days)
    # dfdf$p_cli_local_7days <- est$val
    # dfdf$p_cli_local_7days_low <- est$low
    # dfdf$p_cli_local_7days_high <- est$high

    est <- process_ratio(dfdf$cli_local_14days, reach * dfdf$count_local_14days)
    dfdf$p_cli_local_14days <- est$val
    dfdf$p_cli_local_14days_low <- est$low
    dfdf$p_cli_local_14days_high <- est$high
    dw <- rbind(dw,dfdf)
  }
  
  dw <- dw[order(dw$date,dw$region),]
  write.csv(dw, paste0(estimates_path,df_isoo$ISO2[h],"-estimate.csv"), row.names = FALSE)
  dw <- data.frame()
} 
