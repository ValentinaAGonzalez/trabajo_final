
# ==========================
# Outliers y Datos Faltantes
# ==========================


library(dplyr)
library(ggplot2)
library(psych)

#Input: cargamos los datos limpios 

inflacion_deflactor <- read.csv("data/clean/inflacion_deflactor.csv")
PBI_crecimiento <- read.csv("data/clean/PBI_crecimiento.csv")
desempleo <- read.csv("data/clean/desempleo.csv")
gasto_publico <- read.csv("data/clean/gasto_publico.csv")
tasa_de_cambio <- read.csv("data/clean/tasa_de_cambio_raw.csv")

bases_juntas <- list(
  inflacion_deflactor = inflacion_deflactor,
  PBI_crecimiento = PBI_crecimiento,
  desempleo = desempleo,
  gasto_publico = gasto_publico,
  tasa_de_cambio = tasa_de_cambio
) #listamos en un elemento todas las bases para poder analizarlas con una sola formula y simplificar el código

#---------------------
#Analizamos las bases
#---------------------

#variables relevantes para el analisis: inflacion,crecimiento del PBI, desempleo, gasto público, tasa de cambio oficial, anio. 


lapply(bases_juntas,glimpse) #vemos tipo de dato, titulos de columnas, primeras observaciones, estructura del dataset (row, columns)

#datos faltantes (patrón de NA)
lapply(bases_juntas, function(df){
  colSums(is.na(df))
})
 
#porcentaje de NA
lapply(bases_juntas, function(df){
  colMeans(is.na(df))*100
})

#analizamos la cantidad de NA por año para ver concentración de datos faltantes
gasto_publico %>% 
  filter(is.na(tasa.de.gasto.publico)) %>% 
  count(anio)

inflacion_deflactor %>% 
  filter(is.na(tasa.de.inflacion)) %>% 
  count(anio)

PBI_crecimiento %>% 
  filter(is.na(tasa.de.crecimiento.PBI)) %>% 
  count(anio)

desempleo %>% 
  filter(is.na(tasa.de.desempleo)) %>% 
  count(anio)

tasa_de_cambio %>% 
  filter(is.na(tasa_de_cambio_oficial)) %>% 
  count(anio)

#Analizamos la cantidad de NA por pais para ver concentracion de datos faltantes

gasto_publico %>% 
  filter(is.na(tasa.de.gasto.publico)) %>% 
  count(country_name, sort = TRUE)

inflacion_deflactor %>% 
  filter(is.na(tasa.de.inflacion)) %>% 
  count(country_name, sort = TRUE)

PBI_crecimiento %>% 
  filter(is.na(tasa.de.crecimiento.PBI)) %>% 
  count(country_name, sort = TRUE)

desempleo %>% 
  filter(is.na(tasa.de.desempleo)) %>% 
  count(country_name, sort = TRUE)

tasa_de_cambio %>% 
  filter(is.na(tasa_de_cambio_oficial)) %>% 
  count(country_name, sort = TRUE)


#===========================================================
#Estadistica Descriptiva - base sin limpiar outliers o datos
#===========================================================

lapply(bases_juntas,function(df){
  df %>% select(where(is.numeric)) %>% psych::describe()
})  #con psych abarcamos más que con summary: n, media, desvío estándar, mediana, asimetría, curtosis, y otros datos

#calculamos moda: 
moda <- function(x) {
  x <- x[!is.na(x)]             #eliminamos valores NA
  ux <- unique(x)               #extraemos los valores únicos
  ux[which.max(                 #elegimos el valor más frecuente
    tabulate(match(x, ux))      #contamos cuántas veces aparece cada uno
  )]
}

moda_gasto <- moda(gasto_publico$tasa.de.gasto.publico)
moda_PBI <- moda(PBI_crecimiento$tasa.de.crecimiento.PBI)
moda_inflacion <- moda(inflacion_deflactor$tasa.de.inflacion)
moda_desempleo <- moda(desempleo$tasa.de.desempleo)
moda_tasa_de_cambio <- moda(tasa_de_cambio$tasa_de_cambio_oficial)
modas <- list(
  moda_gasto_publico     = moda(gasto_publico$tasa.de.gasto.publico),
  moda_PBI_crecimiento   = moda(PBI_crecimiento$tasa.de.crecimiento.PBI),
  moda_inflacion         = moda(inflacion_deflactor$tasa.de.inflacion),
  moda_desempleo         = moda(desempleo$tasa.de.desempleo),
  moda_tasa_de_cambio    = moda(tasa_de_cambio$tasa_de_cambio_oficial)
) #las listamos en un solo elemento

#calculamos IQR
rango_gasto <- IQR(gasto_publico$tasa.de.gasto.publico, na.rm = TRUE)
rango_PBI <- IQR(PBI_crecimiento$tasa.de.crecimiento.PBI, na.rm = TRUE)
rango_inflacion <- IQR(inflacion_deflactor$tasa.de.inflacion, na.rm =TRUE)
rango_desempleo <- IQR(desempleo$tasa.de.desempleo, na.rm = TRUE)
rango_tasa_de_cambio <- IQR(tasa_de_cambio$tasa_de_cambio_oficial, na.rm = TRUE)

rangos_IQR <- list(
  IQR_gasto_publico     = IQR(gasto_publico$tasa.de.gasto.publico, na.rm = TRUE),
  IQR_PBI_crecimiento   = IQR(PBI_crecimiento$tasa.de.crecimiento.PBI, na.rm = TRUE),
  IQR_inflacion         = IQR(inflacion_deflactor$tasa.de.inflacion, na.rm = TRUE),
  IQR_desempleo         = IQR(desempleo$tasa.de.desempleo, na.rm = TRUE),
  IQR_tasa_de_cambio    = IQR(tasa_de_cambio$tasa_de_cambio_oficial, na.rm = TRUE)
) #los listamos en un solo elemento

#Distribucion de frecuencias por país (identificamos países que aportan indfo completa o incompleta)
freq_gasto <- gasto_publico |> count(country_name, sort = TRUE)
freq_PBI <- PBI_crecimiento |> count(country_name, sort = TRUE)
freq_inflacion <- inflacion_deflactor |> count(country_name, sort = TRUE)
freq_desempleo <- desempleo |> count(country_name, sort = TRUE)
freq_tasa_cambio <- tasa_de_cambio |> count(country_name, sort = TRUE)
frecuencias <- list(
  gasto_publico = freq_gasto,
  PBI_crecimiento = freq_PBI,
  inflacion = freq_inflacion,
  desempleo = freq_desempleo,
  tasa_de_cambio = freq_tasa_cambio
)

#Histogramas (los valores outliers generan mala visualización, lo corregiremos más adelante)

#Histograma inflacion:
ggplot(inflacion_deflactor, aes(x = tasa.de.inflacion)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Histograma - Inflación (deflactor del PIB)",
       x = "Tasa de inflación", y = "Frecuencia")

#Histograma PBI
ggplot(PBI_crecimiento, aes(x = tasa.de.crecimiento.PBI)) +
  geom_histogram(bins = 100, fill = "darkgreen", color = "white") +
  theme_minimal() +
  labs(title = "Histograma - Crecimiento del PBI",
       x = "Tasa de crecimiento PBI (%)", y = "Frecuencia")

#Histograma desempleo
ggplot(desempleo, aes(x = tasa.de.desempleo)) +
  geom_histogram(bins = 100, fill = "orange", color = "white") +
  theme_minimal() +
  labs(title = "Histograma - Desempleo",
       x = "Tasa de desempleo (%)", y = "Frecuencia")

#Histograma gasto público
ggplot(gasto_publico, aes(x = tasa.de.gasto.publico)) +
  geom_histogram(bins = 100, fill = "purple", color = "white") +
  theme_minimal() +
  labs(title = "Histograma - Gasto público (% del PBI)",
       x = "Tasa de crecimiento gasto público", y = "Frecuencia")

#Histograma tasa de cambio
ggplot(tasa_de_cambio, aes(x = tasa_de_cambio_oficial)) +
  geom_histogram(bins = 100, fill = "red", color = "white") +
  theme_minimal() +
  labs(title = "Histograma - Tasa de Cambio Oficial",
       x = "Unidades monetarias locales por U$S", y = "Frecuencia")

#Boxplots (detectamos valores extremos/outliers)

# Boxplot inflación
ggplot(inflacion_deflactor, aes(y = tasa.de.inflacion)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Boxplot - Inflación")

# Boxplot PBI
ggplot(PBI_crecimiento, aes(y = tasa.de.crecimiento.PBI)) +
  geom_boxplot(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Boxplot - Crecimiento PBI")

# Boxplot desempleo
ggplot(desempleo, aes(y = tasa.de.desempleo)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Boxplot - Desempleo")

# Boxplot gasto público
ggplot(gasto_publico, aes(y = tasa.de.gasto.publico)) +
  geom_boxplot(fill = "purple") +
  theme_minimal() +
  labs(title = "Boxplot - Gasto Público (% del PBI)")

# Boxplot tasa de cambio
ggplot(tasa_de_cambio, aes(y = tasa_de_cambio_oficial)) +
  geom_boxplot(fill = "red") +
  theme_minimal() +
  labs(title = "Boxplot - Tasa de Cambio Oficial")


#================================
#Deteccion de outliers
#================================

detectar_outliers <- function(df, variable){
  
  x <- df[[variable]]
  x <- x[!is.na(x)]  #nos quedamos con las x que no son NA
  
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  iqr <- IQR(x)
  
  limite_inf <- q1 - 1.5 * iqr
  limite_sup <- q3 + 1.5 * iqr
  
  df %>%
    mutate(
      es_outlier = case_when(
        !!sym(variable) < limite_inf ~ "Outlier bajo",
        !!sym(variable) > limite_sup ~ "Outlier alto",
        TRUE ~ "Normal"))
  } #clasificamos las observaciones

#outliers inflacion
inflacion_outliers <- detectar_outliers(inflacion_deflactor,"tasa.de.inflacion")
inflacion_outliers %>% count(es_outlier) #vemos composicion de outliers
inflacion_outliers %>% filter(es_outlier != "Normal") %>% count(country_name, sort = TRUE)


#outliers PBI
PBI_outliers <- detectar_outliers(PBI_crecimiento,"tasa.de.crecimiento.PBI")
PBI_outliers %>% count(es_outlier)
PBI_outliers %>% filter(es_outlier != "Normal") %>% count(country_name, sort = TRUE)

#outliers desempleo
desempleo_outliers <- detectar_outliers(desempleo,"tasa.de.desempleo")
desempleo_outliers %>% count(es_outlier)
desempleo_outliers %>% filter(es_outlier != "Normal") %>% count(country_name, sort = TRUE)

#gasto publico
gasto_outliers <- detectar_outliers(gasto_publico,"tasa.de.gasto.publico")
gasto_outliers %>% count(es_outlier)
gasto_outliers %>% filter(es_outlier != "Normal") %>% count(country_name, sort = TRUE)

#tasa de cambio 
tasa_outliers <- detectar_outliers(tasa_de_cambio,"tasa_de_cambio_oficial")
tasa_outliers %>% count(es_outlier)
tasa_outliers %>% filter(es_outlier != "Normal") %>% count(country_name, sort = TRUE)


#TRATAMIENTO DE OUTLIERS
#-----------------------

#PBI: no se remueve outliers. Explayado en el informe. Mejoramos el histograma modificando el eje X.
ggplot(PBI_crecimiento, aes(x = tasa.de.crecimiento.PBI)) +
  geom_histogram(bins = 100, fill = "darkgreen", color = "white") +
  coord_cartesian(xlim = c(-20, 30)) +
  theme_minimal() +
  labs(title = "Histograma - Crecimiento del PBI modificado",
       x = "Tasa de crecimiento PBI (%)", y = "Frecuencia")
#----------------------------------------------------------
#Desempleo: no se remueve outliers. Explayado en el informe. 

#----------------------------------------------------------
#Tasa de cambio: no eliminamos outliers pero logaritmizamos. 
tasa_de_cambio_log <- tasa_de_cambio |>
  mutate(log_tasa = log(tasa_de_cambio_oficial))

#Histograma con log:
ggplot(tasa_de_cambio_log, aes(x = log_tasa)) +
  geom_histogram(bins = 40, fill = "red", color = "white") +
  theme_minimal() +
  labs(
    title = "Histograma – Tasa de cambio (log transformada)",
    x = "log(Tasa de cambio oficial)",
    y = "Frecuencia"
  )
#Boxplot con log:
ggplot(tasa_de_cambio_log, aes(y = log_tasa)) +
  geom_boxplot(fill = "red", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Boxplot – Tasa de cambio (log transformada)",
    y = "log(Tasa de cambio oficial)"
  )
#------------------------------------------------
#Inflacion: 

x <- inflacion_deflactor$tasa.de.inflacion
q_low <- quantile(x, probs = 0.01, na.rm = TRUE)
x_wins_low <- ifelse(x < q_low, q_low, x)
shift_value <- min(x_wins_low, na.rm = TRUE)
x_shifted <- x_wins_low - shift_value + 1
x_log <- log(x_shifted)

inflacion_log <- inflacion_deflactor %>%
  mutate(
    tasa_wins_low = x_wins_low,
    tasa_shifted = x_shifted,
    inflacion_log = x_log
  )

#Histograma de winzorizados
ggplot(inflacion_log, aes(x = tasa_wins_low)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Histograma – Inflación (winsorizada)",
    x = "Inflación winsorizada", y = "Frecuencia"
  )

#Histograma de inflacion logaritmizada con winzorizados 
ggplot(inflacion_log, aes(x = inflacion_log)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Histograma – Inflación (log transformada)",
    x = "log(inflación)", y = "Frecuencia"
  )

#Boxplot inflación logaritmizada con winzorizados 
ggplot(inflacion_log, aes(y = inflacion_log)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Boxplot - Inflación")

#-------------------------------------------------------
#Gasto público:  no se remueven outliers, se winsoriza el 1% de la cola superior e inferior (P1 y P99) ya que son valores extremos y el objetivo no es estudiar outliers sino variables macro estables.

#Winzorizamos gasto publico
gasto_publico_wins <- gasto_publico %>%
  mutate(
    p1  = quantile(tasa.de.gasto.publico, 0.01, na.rm = TRUE),
    p99 = quantile(tasa.de.gasto.publico, 0.99, na.rm = TRUE),
    gasto_wins = pmin(p99, pmax(tasa.de.gasto.publico, p1))
  ) %>%
  select(-p1, -p99)

#Histograma gasto público winsorizado
ggplot(gasto_publico_wins, aes(x = gasto_wins)) +
  geom_histogram(bins = 100, fill = "purple", color = "white") +
  theme_minimal() +
  labs(
    title = "Histograma - Gasto público winsorizado (1% inferior y superior)",
    x = "Tasa de crecimiento gasto público (winsorizado)",
    y = "Frecuencia"
  )

#Boxplot gasto público winsorizado
ggplot(gasto_publico_wins, aes(y = gasto_wins)) +
  geom_boxplot(fill = "purple") +
  theme_minimal() +
  labs(
    title = "Boxplot - Gasto público winsorizado (1% inferior y superior)",
    y = "Tasa de crecimiento gasto público (winsorizado)"
  )

#Guardamos las bases modificadas 
#-------------------------------

write.csv(PBI_crecimiento, "data/clean/PBI_crecimiento_clean.csv", row.names = FALSE)
write.csv(desempleo, "data/clean/desempleo_clean.csv", row.names = FALSE)
write.csv(tasa_de_cambio_log, "data/clean/tasa_de_cambio_log.csv", row.names = FALSE)
write.csv(inflacion_log, "data/clean/inflacion_log.csv", row.names = FALSE)
write.csv(gasto_publico_wins, "data/clean/gasto_publico_wins.csv", row.names = FALSE)
