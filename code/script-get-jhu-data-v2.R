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

updateTotal <- function(total, toAdd) {
  if (nrow(toAdd) > 0) {
    if (nrow(total) > 0) {
      total <- rbind(df_total, toAdd)
    } else {
      total <-  toAdd
    }
  }
  return( total)
}
# leo los archivos de datos
responses_path <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
  # "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/oxford-umd-country-population.csv"
# estimates_path <- "./jhu/"

estimates_path_region <- "../data/jhu/region/"
estimates_path_country <- "../data/jhu/PlotData/"

paises <- read.csv(data_path, as.is = T, na.string = "NaN")
paises <- dplyr::select(paises, country, ISO2, ISO3) # When reading unified-country-list.csv
# paises <- dplyr::select(paises, country=CountryName, ISO2=geo_id, ISO3=CountryCode)

# defino el espacio temporal sobre el que se calcularan los datos
start <- as.Date("2020-01-22")
#cambiar la siguiente linea para hacerlo automatico y poner Sys.Date()
# hoy <- as.Date("2021-04-29")
hoy <- Sys.Date()

fechas <- seq(start, hoy, by = 1)
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
for (i in 1:length(fechas)) {
  cat(paste("processing date ",as.character.Date(fechas[i]), "\n"))
  
  date_error <- FALSE
  tryCatch({
    csv <-
      read_csv(paste0(
        responses_path,
        as.character.Date(fechas[i], format = "%m-%d-%Y"),
        ".csv"
      ),
      col_types = cols())
  }
  , error = function(e) {
    date_error <<- TRUE
  })
  
  if (!date_error) {
    # If date not available, read_csv failed
    csv$date <- fechas[i]
    if ("Country/Region" %in% colnames(csv)) {
      if (dim(csv)[2] == 9) {
        dw_special8 <- rbind(dw_special8, csv)
      }
      else{
        dw_special <- rbind(dw_special, csv)
      }
    }
    else if ("Case-Fatality_Ratio" %in% colnames(csv)) {
      dw_special_2020 <- rbind(dw_special_2020, csv)
    }
    else{
      if (dim(csv)[2] == 13) {
        dw_2020 <- rbind(dw_2020, csv)
      }
      else{
        dw <- rbind(dw, csv)
      }
    }
  }
  else {
    # read_csv failed
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
if (nrow(dw) > 0) {
  dw <-
    dw[, c(
      "Country_Region",
      "Province_State",
      "date",
      "Confirmed",
      "Deaths",
      "Recovered",
      "Active"
    )]
  dw <-
    rename(dw, country = Country_Region, region = Province_State)
}
if (nrow(dw_2020) > 0) {
  dw_2020 <-
    dw_2020[, c(
      "Country_Region",
      "Province_State",
      "date",
      "Confirmed",
      "Deaths",
      "Recovered",
      "Active"
    )]
  dw_2020 <-
    rename(dw_2020, country = Country_Region, region = Province_State)
}
if (nrow(dw_2020) > 0) {
  dw_special <-
    dw_special[, c("Country/Region",
                   "Province/State",
                   "date",
                   "Confirmed",
                   "Deaths",
                   "Recovered")]
  dw_special$Active <- NA
  dw_special <-
    rename(dw_special, country = 'Country/Region', region = 'Province/State')
}
if (nrow(dw_special_2020) > 0) {
  dw_special_2020 <-
    dw_special_2020[, c(
      "Country_Region",
      "Province_State",
      "date",
      "Confirmed",
      "Deaths",
      "Recovered",
      "Active"
    )]
  dw_special_2020 <-
    rename(dw_special_2020,
           country = Country_Region,
           region = Province_State)
}
if (nrow(dw_special8)) {
  dw_special8 <-
    dw_special8[, c("Country/Region",
                    "Province/State",
                    "date",
                    "Confirmed",
                    "Deaths",
                    "Recovered")]
  dw_special8$Active <- NA
  dw_special8 <-
    rename(dw_special8, country = 'Country/Region', region = 'Province/State')
}
df_total <- data.frame()
df_total<-updateTotal(df_total, dw)
df_total<-updateTotal(df_total, dw_2020)
df_total<-updateTotal(df_total, dw_special)
df_total<-updateTotal(df_total, dw_special_2020)
df_total<-updateTotal(df_total, dw_special8)

# ordeno el dataframe por paises y fechas, y hago un merge con la base de datos de los distintos paises
df_total_ok <- df_total[order(df_total$country, df_total$date),]
df_total_merge <-
  merge (df_total_ok, paises, by = "country", all.x = TRUE)
df_total_merge_ok <-
  df_total_merge[order(df_total_merge$country, df_total_merge$date),]

# dataframe con todos los paises que recorrer
df_pais <-
  df_total_merge_ok
iso_list <- unique(df_total_merge_ok$ISO2) 
# bucle que recorre cada pais por todas las fechas
# por cada fecha suma todos los datos disponibles de confirmed, active, deaths y recovered
# se guardan en las correspondientes listas y se crea un dataframe por cada pais que se exportara como csv
for (mycountry in iso_list) {
   cat(paste("filtering",mycountry,"\n"))
 
  df_aux <- df_pais %>% filter(ISO2==mycountry)
  if (nrow(df_aux)>0 ){
  write.csv(df_aux,
            paste0(estimates_path_region, mycountry, ".csv"),
            row.names = FALSE)
   # df_aux %>% dplyr::group_by(date, country) %>% dplyr::summarize(across(everything()), sum)
   # df_aux %>% dplyr::group_by(date, country) %>% dplyr::summarize(across(everything()), by=sum)
    df_aux<-df_aux %>% dplyr::group_by(date, ISO2, ISO3, country)%>% dplyr::select(-region) %>% dplyr::summarize_each(list(sum))
    write.csv(df_aux,
              paste0(estimates_path_country, mycountry, ".csv"),
              row.names = FALSE)
  } else {
    cat("empty\n")
  }
}
