# importo librerias

library(readr)
library(lubridate)
library(zoo)
library(tidyr)

responses_path <- "../data/estimates-symptom-survey/aggregated-data/country/"
data_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/common-data/unified-country-list.csv"
estimates_path <- "../data/estimates-symptom-survey/PlotData/"

reach <- 53 # Use the average reach from https://coronasurveys.org/participation/

# leo los ficheros de interes

files <- list.files(path=responses_path, pattern="*.csv", full.names=TRUE, recursive=FALSE)

paises <- read.csv(data_path, as.is = T) #, na.string = "NaN")

# fechas <-seq(start,hoy,by=1)
fechas <- list.files(path=responses_path, pattern="*.csv", full.names=FALSE, recursive=FALSE)
fechas <- substring(fechas,1,10)
start <- as.Date(fechas[1])
hoy <- as.Date(fechas[length(fechas)])

# creo unas fechas limites para "jugar" con los ficheros

# start <- as.Date("2020-04-23")

#cambiar la siguiente linea para hacerlo automatico y poner Sys.Date()
# hoy <- as.Date("2021-04-04")
# hoy <- Sys.Date()

# calcular ratio e intervalo de confianza

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
# division <- c()
# division_local <- c()
lista_pais <- c()
lista_fecha <- c()
lista_iso2 <- c()
lista_iso3 <- c()
lista_population <- c()

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
    # interes <- csv_merge$cli[j]/csv_merge$count[j]
    # interes_local <- csv_merge$cli_local_com[j] / (reach*csv_merge$count_local_com[j])
    pais <- csv_merge$country.x[j]
    iso2 <- csv_merge$ISO2[j]
    iso3 <- csv_merge$ISO3[j]
    cli <- csv_merge$cli[j]
    count <- csv_merge$count[j]
    cli_local <- csv_merge$cli_local_com[j]
    count_local <- csv_merge$count_local_com[j]
    population <- csv_merge$population[j]
    lista_pais[[length(lista_pais)+1]] <- pais
    lista_population[[length(lista_population)+1]] <- population
    lista_iso2[[length(lista_iso2)+1]] <- iso2
    lista_iso3[[length(lista_iso3)+1]] <- iso3
    lista_fecha[[length(lista_fecha)+1]] <- as.Date(fecha_hoy, origin="1970-01-01")
    lista_cli[[length(lista_cli)+1]] <- cli
    lista_count[[length(lista_count)+1]] <- count
    lista_cli_local[[length(lista_cli_local)+1]] <- cli_local
    lista_count_local[[length(lista_count_local)+1]] <- count_local
    # division[[length(division)+1]] <- interes
    # division_local[[length(division_local)+1]] <- interes_local
  }
}
# dataframe auxiliar donde tengo todo lo anterior guardado
df_total_aux <- data.frame(
      "country" = as.vector(unlist(lista_pais)),
      "ISO2" = as.vector(unlist(lista_iso2)),
      "ISO3" = as.vector(unlist(lista_iso3)),
      "date" = as.Date(as.vector(unlist(lista_fecha)), origin="1970-01-01"),
      "population" = as.vector(unlist(lista_population)),
      "cli" = as.vector(unlist(lista_cli)),
      "count" = as.vector(unlist(lista_count)),
      # "p_cli" = as.vector(unlist(division)),
      "cli_local" = as.vector(unlist(lista_cli_local)),
      "count_local" = as.vector(unlist(lista_count_local))
      # "p_cli_local" = as.vector(unlist(division_local))
      )
      
# elimino la fila cuyo pais es -99 ya que el script falla si se introduce tambien
df_total_aux = df_total_aux[df_total_aux$country !="-99", ]

# dataframe con todos los codigos ISO de todos los paises que se han agregado como datos de UMD
df_iso <- df_total_aux[!duplicated(df_total_aux$ISO2),c("country","ISO2","ISO3")]     

# recorro el dataframe creado previamente pais por pais y aplico el algoritmo diseñado
for (i in 1:dim(df_iso)[1]){
  # cat("procesing ", df_iso$ISO2[i], "\n")
  df_country <- df_total_aux[df_total_aux$ISO2 == df_iso$ISO2[i], ]
  df_country_nan<- na.exclude(df_country)
  df_country_ok<- df_country_nan[!duplicated(df_country_nan),]
  df_country_ok <- df_country_ok %>%
      complete(date = seq.Date(start, hoy, by="day"), ISO2 =df_iso$ISO2[i] )
  # complete(date = seq.Date(start, hoy, by="day"), ISO2 =df_iso$ISO2[i] )
  
  
  df_country_ok$cli_7days <- rollapply(df_country_ok$cli,7,sum,fill=NA,align="right")
  df_country_ok$count_7days <- rollapply(df_country_ok$count,7,sum,fill=NA,align="right")
  
  df_country_ok$cli_local_7days <- rollapply(df_country_ok$cli_local,7,sum,fill=NA,align="right")
  df_country_ok$count_local_7days <- rollapply(df_country_ok$count_local,7,sum,fill=NA,align="right")
  
  df_country_ok$cli_14days <- rollapply(df_country_ok$cli,14,sum,fill=NA,align="right")
  df_country_ok$count_14days <- rollapply(df_country_ok$count,14,sum,fill=NA,align="right")
  
  df_country_ok$cli_local_14days <- rollapply(df_country_ok$cli_local,14,sum,fill=NA,align="right")
  df_country_ok$count_local_14days <- rollapply(df_country_ok$count_local,14,sum,fill=NA,align="right")
  
  est <- process_ratio(df_country_ok$cli, df_country_ok$count)
  df_country_ok$p_cli <- est$val
  df_country_ok$p_cli_low <- est$low
  df_country_ok$p_cli_high <- est$high
  
  est <- process_ratio(df_country_ok$cli_7days, df_country_ok$count_7days)
  df_country_ok$p_cli_7days <- est$val
  df_country_ok$p_cli_7days_low <- est$low
  df_country_ok$p_cli_7days_high <- est$high
  
  est <- process_ratio(df_country_ok$cli_14days, df_country_ok$count_14days)
  df_country_ok$p_cli_14days <- est$val
  df_country_ok$p_cli_14days_low <- est$low
  df_country_ok$p_cli_14days_high <- est$high
  
  est <- process_ratio(df_country_ok$cli_local, reach * df_country_ok$count_local)
  df_country_ok$p_cli_local <- est$val
  df_country_ok$p_cli_local_low <- est$low
  df_country_ok$p_cli_local_high <- est$high
  
  est <- process_ratio(df_country_ok$cli_local_7days, reach * df_country_ok$count_local_7days)
  df_country_ok$p_cli_local_7days <- est$val
  df_country_ok$p_cli_local_7days_low <- est$low
  df_country_ok$p_cli_local_7days_high <- est$high
  
  est <- process_ratio(df_country_ok$cli_local_14days, reach * df_country_ok$count_local_14days)
  df_country_ok$p_cli_local_14days <- est$val
  df_country_ok$p_cli_local_14days_low <- est$low
  df_country_ok$p_cli_local_14days_high <- est$high
  
  write.csv(df_country_ok, paste0(estimates_path,df_iso$ISO2[i],"-estimate.csv"), row.names = FALSE)
}

