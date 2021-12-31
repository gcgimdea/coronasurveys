# Fuertemente inspirado en 
# https://es.stackoverflow.com/questions/231828/como-hacer-con-leaflet-un-mapa-de-espa%C3%B1a-por-provincias-coloreadas

library(sf)
library(tidyverse) # La vamos a usar más adelante
library(lubridate)

responses_path <- "../data/aggregate/"
data_path <- "../data/common-data/"
estimates_path <- "../data/estimates-nsum/provinces/"

# responses_path <- "../coronasurveys/data/aggregate/"
# data_path <- "./common-data/"
# estimates_path <- "./estimates-provinces/"


carto_base <- sf::read_sf(paste0(data_path, "Provincias_ETRS89_30N/")) # descragado de https://www.arcgis.com/home/item.html?id=83d81d9336c745fd839465beab885ab7
iso_texto <- read.csv(paste0(data_path, "iso-texto-provincias.csv"), as.is = T)
carto_base <- left_join(carto_base, iso_texto)

#carto_base <- carto_base[, c("region","geometry")]

estimates <- read.csv(paste0(estimates_path, "ES/ES-latest-estimate.csv"), as.is = T)

estimates$reach_ratio <- 100000*estimates$reach / estimates$population
estimates$reach_recent_ratio <- 100000*estimates$reach_recent / estimates$population
estimates$cases_ratio <- 100000*estimates$p_cases
estimates$cases_recent_ratio <- 100000*estimates$p_recentcases

carto_base <- left_join(carto_base, estimates)

carto_base %>% 
  mutate(reach_ratio = ifelse(reach_ratio > 100,  100, reach_ratio)) %>%  
  # mutate(reach_ratio = ifelse(reach_ratio == 0,  NA, reach_ratio)) %>%
  # filter(CCAA != "Canarias") %>% 
  ggplot() +
  geom_sf(aes(fill = reach_ratio))  +
  theme_minimal() + 
  # coord_sf(datum = NA) + 
  scale_fill_continuous(name="Cobertura", low = "white", high = "darkred", na.value = "yellow") +
  labs (title = "Cobertura de la población en personas por 100.000 habitantes", 
        caption = "Coordenadas: arcgis.com. Código adaptado de mpaladino. https://coronasurveys.org") -> reach_plot

jpeg(paste0(estimates_path, "ES/ES-latest-reach-map.jpg"), width = 800, height = 800)
print(reach_plot)
dev.off() 

carto_base %>% 
  # mutate(cases_ratio = ifelse(is.na(cases_ratio),  0, cases_ratio)) %>% 
  # filter(CCAA != "Canarias") %>% 
  ggplot() +
  geom_sf(aes(fill = cases_ratio))  +
  theme_minimal() + 
  # coord_sf(datum = NA) + 
  scale_fill_continuous(name="Casos", low = "white", high = "darkred", na.value = "yellow") +
  labs (title = "Incidencia total estimada por 100.000 habitantes", 
        caption = "Coordenadas: arcgis.com. Código adaptado de mpaladino. https://coronasurveys.org") -> cases_plot

jpeg(paste0(estimates_path, "ES/ES-latest-cases-map.jpg"), width = 800, height = 800)
print(cases_plot)
dev.off() 

carto_base %>% 
  mutate(reach_recent_ratio = ifelse(reach_recent_ratio > 100,  100, reach_recent_ratio)) %>%  
  # mutate(reach_recent_ratio = ifelse(reach_recent_ratio == 0,  NA, reach_recent_ratio)) %>%
  # filter(CCAA != "Canarias") %>% 
  ggplot() +
  geom_sf(aes(fill = reach_recent_ratio))  +
  theme_minimal() + 
  # coord_sf(datum = NA) + 
  scale_fill_continuous(name="Cobertura", low = "white", high = "darkred", na.value = "yellow") +
  labs (title = "Cobertura reciente de la población en personas por 100.000 habitantes\n", 
        caption = "Coordenadas: arcgis.com. Código adaptado de mpaladino. https://coronasurveys.org") -> reach_recent_plot

jpeg(paste0(estimates_path, "ES/ES-latest-reach-recent-map.jpg"), width = 800, height = 800)
print(reach_recent_plot)
dev.off() 

carto_base %>% 
  # mutate(cases_recent_ratio = ifelse(is.na(cases_recent_ratio),  0, cases_recent_ratio)) %>% 
  # filter(CCAA != "Canarias") %>% 
  ggplot() +
  geom_sf(aes(fill = cases_recent_ratio))  +
  theme_minimal() + 
  # coord_sf(datum = NA) + 
  scale_fill_continuous(name="Casos", low = "white", high = "darkred", na.value = "yellow") +
  labs (title = "Incidencia estimada útimos 14 días por 100.000 habitantes", 
        caption = "Coordenadas: arcgis.com. Código adaptado de mpaladino. https://coronasurveys.org") -> cases_recent_plot

jpeg(paste0(estimates_path, "ES/ES-latest-cases-recent-map.jpg"), width = 800, height = 800)
print(cases_recent_plot)
dev.off()

