# importo librerias
library(readr)
library(lubridate)
library(zoo)
library(tidyr)

# leo los ficheros de interes
responses_path <- "../data/estimates-symptom-survey/aggregated-data/age/"
data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/age/"

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

reach <- 53 # Use the average reach from https://coronasurveys.org/participation/
ci_level <- 0.95
z <- qnorm(ci_level+(1-ci_level)/2)

process_ratio <- function(numerator, denominator){
  p_est <- pmin(1, numerator/denominator)
  se <- sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

# creo unas listas de apoyo
lista_cli <- c()
lista_count <- c()
lista_cli_local <- c()
lista_count_local <- c()
lista_pais <- c()
lista_fecha <- c()
lista_iso2 <- c()
lista_iso3 <- c()
lista_population <- c()
lista_edad <- c()

# fechas <-seq(start,hoy,by=1)

# bucle que recorre todos los ficheros
# leo cada fichero y hago un merge para poder tener el pais con sus codigos iso2 e iso3
# almaceno en esas listas el nombre del pais, fecha, y codigos iso2 e iso3
# calculo cli/count y lo guardo en la lista correspondiente
# creo un dataframe auxiliar con las listas guardas (60762x5) country, iso2, iso3, date, population, cli, count, p_cli

for (i in 1:length(files)){
  csv <- read.csv(files[i])
  csv_merge <- merge (csv, paises, by = "ISO3", all.x=TRUE)
  fecha_hoy <- as.Date(fechas[i]) 
  
  for (j in 1:dim(csv_merge)[1]){
    pais <- csv_merge$country.x[j]
    iso2 <- csv_merge$ISO2[j]
    iso3 <- csv_merge$ISO3[j]
    cli <- csv_merge$cli[j]
    count <- csv_merge$count[j]
    cli_local <- csv_merge$cli_local_com[j]
    count_local <- csv_merge$count_local_com[j]
    population <- csv_merge$population[j]
    edad <- csv_merge$age_E4[j]
    lista_pais[[length(lista_pais)+1]] <- pais
    lista_population[[length(lista_population)+1]] <- population
    lista_edad[[length(lista_edad)+1]] <- edad
    lista_iso2[[length(lista_iso2)+1]] <- iso2
    lista_iso3[[length(lista_iso3)+1]] <- iso3
    lista_fecha[[length(lista_fecha)+1]] <- as.Date(fecha_hoy, origin="1970-01-01")
    lista_cli[[length(lista_cli)+1]] <- cli
    lista_count[[length(lista_count)+1]] <- count
    lista_cli_local[[length(lista_cli_local)+1]] <- cli_local
    lista_count_local[[length(lista_count_local)+1]] <- count_local
  }
}

# dataframe auxiliar donde tengo todo lo anterior guardado
df_aux_age <- data.frame(
      "country" = as.vector(unlist(lista_pais)),
      "ISO2" = as.vector(unlist(lista_iso2)),
      "ISO3" = as.vector(unlist(lista_iso3)),
      "age" = as.vector(unlist(lista_edad)),
      "date" = as.Date(as.vector(unlist(lista_fecha)), origin="1970-01-01"),
      "population" = as.vector(unlist(lista_population)),
      "cli" = as.vector(unlist(lista_cli)),
      "count" = as.vector(unlist(lista_count)),
      "cli_local" = as.vector(unlist(lista_cli_local)),
      "count_local" = as.vector(unlist(lista_count_local))
      )
      
# te selecciona todos los paises que salgan en los datos sin repetir
df_isoo <- df_aux_age[!duplicated(df_aux_age$ISO2),c("country","age","ISO2","ISO3","population")]

# te selecciona todos los grupos de edades que salgan en los datos sin repetir
df_age <- df_aux_age[!duplicated(df_aux_age$age),c("country","age","ISO2","ISO3","population")] 
    
dw <- data.frame()

# recorro el dataframe creado previamente pais por pais y aplico el algoritmo diseñado
for (i in 1:dim(df_isoo)[1]){
  for (j in 1:dim(df_age)[1]){
    df_country <- df_aux_age[df_aux_age$ISO2 == df_isoo$ISO2[i] & df_aux_age$age == df_age$age[j], ]
    df_country_nan<- na.exclude(df_country)
    df_country_ok<- df_country_nan[!duplicated(df_country_nan),]
    dfdf_age <- df_country_ok %>%
      complete(date = seq.Date(start, hoy, by="day"), age =df_age$age[j], ISO2=df_isoo$ISO2[i], 
               country=df_isoo$country[i], ISO3=df_isoo$ISO3[i], population=df_isoo$population[i] )
  
  dfdf_age$cli_7days <- rollapply(dfdf_age$cli,7,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$count_7days <- rollapply(dfdf_age$count,7,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$cli_local_7days <- rollapply(dfdf_age$cli_local,7,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$count_local_7days <- rollapply(dfdf_age$count_local,7,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$cli_14days <- rollapply(dfdf_age$cli,14,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$count_14days <- rollapply(dfdf_age$count,14,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$cli_local_14days <- rollapply(dfdf_age$cli_local,14,sum,fill=NA,na.rm = TRUE,align="right")
  dfdf_age$count_local_14days <- rollapply(dfdf_age$count_local,14,sum,fill=NA,na.rm = TRUE,align="right")
  
  est <- process_ratio(dfdf_age$cli, dfdf_age$count)
  dfdf_age$p_cli <- est$val
  dfdf_age$p_cli_low <- est$low
  dfdf_age$p_cli_high <- est$high
  
  est <- process_ratio(dfdf_age$cli_7days, dfdf_age$count_7days)
  dfdf_age$p_cli_7days <- est$val
  dfdf_age$p_cli_7days_low <- est$low
  dfdf_age$p_cli_7days_high <- est$high
  
  est <- process_ratio(dfdf_age$cli_14days, dfdf_age$count_14days)
  dfdf_age$p_cli_14days <- est$val
  dfdf_age$p_cli_14days_low <- est$low
  dfdf_age$p_cli_14days_high <- est$high
  
  est <- process_ratio(dfdf_age$cli_local, reach * dfdf_age$count_local)
  dfdf_age$p_cli_local <- est$val
  dfdf_age$p_cli_local_low <- est$low
  dfdf_age$p_cli_local_high <- est$high
  
  est <- process_ratio(dfdf_age$cli_local_7days, reach * dfdf_age$count_local_7days)
  dfdf_age$p_cli_local_7days <- est$val
  dfdf_age$p_cli_local_7days_low <- est$low
  dfdf_age$p_cli_local_7days_high <- est$high
  
  est <- process_ratio(dfdf_age$cli_local_14days, reach * dfdf_age$count_local_14days)
  dfdf_age$p_cli_local_14days <- est$val
  dfdf_age$p_cli_local_14days_low <- est$low
  dfdf_age$p_cli_local_14days_high <- est$high
  dw <- rbind(dw,dfdf_age)
  }
  dw <- dw[order(dw$date,dw$age),]
  write.csv(dw, paste0(estimates_path,df_isoo$ISO2[i],"-estimate.csv"), row.names = FALSE)
  dw <- data.frame()
}
