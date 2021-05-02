# <!-- Copyright {{ 2021 }} {{ IMDEA Networks Institute }} -->
# <!-- Author {{ Javier Álvarez Benito }} {{https://www.linkedin.com/in/javieralvarezbenito}} -->
# <!-- Licensed under the Apache License, Version 2.0 (the "License"); -->
# <!-- you may not use this file except in compliance with the License. -->
# <!-- You may obtain a copy of the License at -->
# 
# <!-- http://www.apache.org/licenses/LICENSE-2.0 -->
# 
# <!-- Unless required by applicable law or agreed to in writing, software -->
# <!-- distributed under the License is distributed on an "AS IS" BASIS, -->
# <!-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express -->
# <!-- or implied. See the License for the specific language governing -->
# <!-- permissions and limitations under the License. -->

# importo librerias
library(readr)
library(lubridate)
library(zoo)
library(dplyr)
library(tidyverse)

# leo los archivos de datos
responses_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
# estimates_path <- "./jhu/"

estimates_path <- "../data/jhu/"

paises <- read.csv(data_path, as.is = T, na.string = "NaN")


# defino el espacio temporal sobre el que se calcularan los datos
start <- as.Date("2020-01-22")
#cambiar la siguiente linea para hacerlo automatico y poner Sys.Date()
# hoy <- as.Date("2021-04-29")
hoy <- Sys.Date()

fechas <-seq(start,hoy,by=1)
# files <- paste0(responses_path, as.character.Date(fechas, format="%m-%d-%Y"), ".csv")


# ETAPA DE PRE-PROCESADO DE DATOS
# debido a los diversos formatos que hay de datos, columnas, dimensiones, etc
# creo distintos dataframe para guardar los datos
# despues seran juntados en uno solo
dw <- data.frame()
dw_special <- data.frame()
dw_special8 <- data.frame()
dw_2020 <- data.frame()
dw_special_2020 <- data.frame()

# t <- proc.time()

no_date <- c() # To collect the dates that are not avaiable
for (i in 1:length(fechas)){
  
  # cat(as.character.Date(fechas[i]), "\n")
  
  date_error <- FALSE
  tryCatch( { csv <- read_csv(paste0(responses_path, as.character.Date(fechas[i], format="%m-%d-%Y"), ".csv"), 
                              col_types = cols()) }
            , error = function(e) {date_error <<- TRUE})

  if (!date_error){ # If date not available, read_csv failed
    csv$date <- fechas[i]
    if("Country/Region" %in% colnames(csv)){
      if(dim(csv)[2]==9){
        dw_special8 <- rbind(dw_special8,csv)
      }
      else{
        dw_special <- rbind(dw_special, csv)
      }
    }
    else if("Case-Fatality_Ratio" %in% colnames(csv)){
      dw_special_2020 <- rbind(dw_special_2020,csv)
    }
    else{
      if(dim(csv)[2]==13){
        dw_2020 <- rbind(dw_2020,csv)
      }
      else{
        dw <- rbind(dw,csv) 
      }
    }
  }
  else { # read_csv failed
    no_date <- c(no_date, fechas[i])
  }
} 
# t - proc.time()

# cat(as.character.Date(no_date), "\n")

fechas <- fechas[!fechas %in% no_date]

# cat(as.character.Date(fechas))

# # Realizo diversas acciones para trabajar en un formato de fecha adecuado
# dw$date<- as.Date(dw$Last_Update, format = "%m/%d/%y")
# dw_2020$date <- as.Date(dw_2020$Last_Update, format = "%m/%d/%y")
# dw_special$date <- as.Date(dw_special$'Last Update', format = "%m/%d/%y")
# dw_special_2020$date <- as.Date(dw_special_2020$Last_Update, format = "%m/%d/%y")
# dw_special8$date <- as.Date(dw_special8$'Last Update', format = "%m/%d/%y")
# 
# # for (i in 572:dim(dw_special)[1]){
# #   dw_special$date[i] <- as.POSIXct(as.numeric(dw_special$'Last Update'[i]), origin="1970-01-01", tz="GMT")
# # }
# dw_special$date[572:dim(dw_special)[1]] <- 
#   as.POSIXct(as.numeric(dw_special$'Last Update'[572:dim(dw_special)[1]]), origin="1970-01-01", tz="GMT")
# 
# # for (i in 3422:dim(dw_2020)[1]){
# #   dw_2020$date[i] <- as.POSIXct(as.numeric(dw_2020$Last_Update[i]), origin="1970-01-01", tz="GMT")
# # }
# dw_2020$date[3422:dim(dw_2020)[1]] <- 
#   as.POSIXct(as.numeric(dw_2020$Last_Update[3422:dim(dw_2020)[1]]), origin="1970-01-01", tz="GMT")

# El orden de los dataframe por fechas es:
# 1 -> dw_special
# 2 -> dw_special8
# 3 -> dw_2020
# 4 -> dw_special_2020
# 5 -> dw


# selecciono las columnas que quiero para reducir dimensiones
dw_reduce <- dw[ , c("Country_Region", "Province_State", "date", "Confirmed", "Deaths", "Recovered", "Active")]
dw_2020_reduce <- dw_2020[ , c("Country_Region", "Province_State", "date", "Confirmed", "Deaths", "Recovered", "Active")]
dw_special_reduce <- dw_special[ , c("Country/Region", "Province/State", "date", "Confirmed", "Deaths", "Recovered")]
dw_special_2020_reduce <- dw_special_2020[ , c("Country_Region", "Province_State", "date", "Confirmed", "Deaths", "Recovered", "Active")]
dw_special8_reduce <- dw_special8[ , c("Country/Region", "Province/State", "date", "Confirmed", "Deaths", "Recovered")]

# añado una columna para poder unir los dataframe
dw_special_reduce$Active <- NA
dw_special8_reduce$Active <- NA

# renombro ciertas columnas de algunos dataframe para poder trabajar perfectamente
dw_ok <- rename(dw_reduce, country = Country_Region, region = Province_State)
dw_2020_ok <- rename(dw_2020_reduce, country = Country_Region, region = Province_State)
dw_special_ok <- rename(dw_special_reduce, country = 'Country/Region', region = 'Province/State')
dw_special_2020_ok <- rename(dw_special_2020_reduce, country = Country_Region, region = Province_State)
dw_special8_ok <- rename(dw_special8_reduce, country = 'Country/Region', region = 'Province/State')

# junto los dataframe en un solo dataframe, df_total
df_total <- rbind(dw_ok, dw_2020_ok)
df_total <- rbind(df_total, dw_special_ok)
df_total <- rbind(df_total, dw_special_2020_ok)
df_total <- rbind(df_total, dw_special8_ok)

# ordeno el dataframe por paises y fechas, y hago un merge con la base de datos de los distintos paises
df_total_ok <- df_total[order(df_total$country, df_total$date), ]
df_total_merge <- merge (df_total_ok, paises, by = "country", all.x=TRUE)
df_total_merge_ok <- df_total_merge[order(df_total_merge$country, df_total_merge$date), ]

# dataframe con todos los paises que recorrer
df_pais <- df_total_merge_ok[!duplicated(df_total_merge_ok$country),c("country","ISO2","ISO3","population")] 
df_pais_nan<- na.exclude(df_pais)

# bucle que recorre cada pais por todas las fechas
# por cada fecha suma todos los datos disponibles de confirmed, active, deaths y recovered
# se guardan en las correspondientes listas y se crea un dataframe por cada pais que se exportara como csv
for (i in 1:dim(df_pais_nan)[1]){
  # cat(df_pais_nan$country[i],"\n")
  lista_confirmed <- c()
  lista_deaths <- c()
  lista_active <- c()
  lista_recuperados <- c()
  lista_pais <- c()
  lista_fecha <- c()
  lista_iso2 <- c()
  lista_iso3 <- c()
  lista_population <- c()
  df_aux <- data.frame()
  for (j in 1:length(fechas)){
    df_country1 <- df_total_merge_ok[df_total_merge_ok$country == df_pais_nan$country[i], ]
    df_country1_fecha <- df_country1[df_country1$date == fechas[j], ]
  
    confirmados <- sum(df_country1_fecha$Confirmed,na.rm=TRUE)
    activos <- sum(df_country1_fecha$Active,na.rm=TRUE)
    muerto <- sum(df_country1_fecha$Deaths,na.rm=TRUE)
    recuperados <- sum(df_country1_fecha$Recovered,na.rm=TRUE)
  
    lista_pais[[length(lista_pais)+1]] <- df_pais_nan$country[i]
    lista_iso2[[length(lista_iso2)+1]] <- df_pais_nan$ISO2[i]
    lista_iso3[[length(lista_iso3)+1]] <- df_pais_nan$ISO3[i]
    lista_population[[length(lista_pais)+1]] <- df_pais_nan$population[i]
    lista_fecha[[length(lista_fecha)+1]] <- fechas[j]
    lista_confirmed[[length(lista_confirmed)+1]] <- confirmados
    lista_deaths[[length(lista_deaths)+1]] <- muerto
    lista_active[[length(lista_active)+1]] <- activos
    lista_recuperados[[length(lista_recuperados)+1]] <- recuperados
  }
  df_aux <- data.frame(
    "country" = as.vector(unlist(lista_pais)),
    "ISO2" = as.vector(unlist(lista_iso2)),
    "ISO3" = as.vector(unlist(lista_iso3)),
    "population" = as.vector(unlist(lista_population)),
    "date" = as.Date(as.vector(unlist(lista_fecha)), origin="1970-01-01"),
    "confirmed" = as.vector(unlist(lista_confirmed)),
    "deaths" = as.vector(unlist(lista_deaths)),
    "active" = as.vector(unlist(lista_active)),
    "recovered" = as.vector(unlist(lista_recuperados)))
    
  write.csv(df_aux, paste0(estimates_path,df_pais_nan$ISO2[i],"-data.csv"), row.names = FALSE)
}


