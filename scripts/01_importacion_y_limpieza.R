# ============================================================
# 01_importacion_y_limpieza.R
# Importamos los datos crudos del Banco Mundial, seleccionamos columnas,
# filtramos los años 2015, 2019 y 2023 y guardamos los datos limpios.
# ============================================================

library(tidyr)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)

options(scipen = 999)
#Rutas para los datasets:
ruta_raw <- "data/raw/"
ruta_clean <- "data/clean/"

anios_filtrar <- c(2000,2010,2023) #anios seleccionados para el análisis

#funcion para limpiar datos

limpiar_wb <- function(nombre_archivo, nombre_variable_salida){
  
  df <- read_excel(paste0(ruta_raw, nombre_archivo), skip = 3) %>% 
    clean_names() %>% 
    select(country_name, country_code, matches("^x[0-9]{4}$")) %>% 
    pivot_longer(cols = matches("^x[0-9]{4}$"),
                 names_to = "anio",
                 values_to = nombre_variable_salida) %>%
    mutate(anio = sub("x","",anio), anio = as.numeric(anio)) %>%
    filter(anio %in% anios_filtrar)
  
  return(df)
}

inflacion_deflactor <- limpiar_wb("API_NY.GDP.DEFL.KD.ZG_DS2 - Inflación deflactor del PIB % anual.xls", "tasa de inflacion")
PBI_crecimiento <- limpiar_wb("API_NY.GDP.MKTP.KD.ZG_DS2 - Crecimiento del PIB.xls", "tasa de crecimiento PBI")
desempleo <- limpiar_wb("API_SL.UEM.TOTL.ZS_DS2 - Desempleo total (% de la fuerza laboral, estimacion modelada de la OIT).xls", "tasa de desempleo")
gasto_publico <- limpiar_wb("API_NE.CON.GOVT.KD.ZG_DS2 - Gasto de consumo final del gobierno general (% del crecimiento anual).xls", "tasa de gasto publico")


#limpiamos la base de tipo de cambio aparte
tasa_de_cambio_raw <- read_excel(
  paste0(ruta_raw, "API_PA.NUS.FCRF_DS2 - Tasa de cambio oficial (UMN por US$, promedio para un período).xls"),
  skip = 3
) %>% 
  clean_names() %>%
  select(country_name, country_code, matches("^x[0-9]{4}$")) %>% 
  pivot_longer(
    cols = matches("^x[0-9]{4}$"),
    names_to = "anio",
    values_to = "tasa_de_cambio_oficial"
  ) %>%
  mutate(
    anio = as.numeric(sub("x", "", anio)),tasa_de_cambio_oficial = gsub(",", ".", tasa_de_cambio_oficial),
    tasa_de_cambio_oficial = as.numeric(tasa_de_cambio_oficial)
  ) %>%
  filter(anio %in% anios_filtrar)

#guardamos outputs limpios

write.csv(inflacion_deflactor,"data/clean/inflacion_deflactor.csv", row.names = FALSE)
write.csv(PBI_crecimiento, "data/clean/PBI_crecimiento.csv", row.names = FALSE)
write.csv(desempleo,"data/clean/desempleo.csv", row.names = FALSE)
write.csv(gasto_publico,"data/clean/gasto_publico.csv",row.names = FALSE)
write.csv(tasa_de_cambio_raw,"data/clean/tasa_de_cambio_raw.csv", row.names = FALSE)
