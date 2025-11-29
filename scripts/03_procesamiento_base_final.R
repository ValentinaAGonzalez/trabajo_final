#======================================================
#PROCESAMIENTO DE BASE LIMPIA - ESTADÍSTICA DESCRIPTIVA
#======================================================

library(dplyr)
library(ggplot2)
library(psych)

#Input: cargamos los datos originales

inflacion_deflactor <- read.csv("data/clean/inflacion_deflactor.csv")
PBI_crecimiento <- read.csv("data/clean/PBI_crecimiento.csv")
desempleo <- read.csv("data/clean/desempleo.csv")
gasto_publico <- read.csv("data/clean/gasto_publico.csv")
tasa_de_cambio <- read.csv("data/clean/tasa_de_cambio_raw.csv")
bases_juntas <- list(
  inflacion = inflacion_deflactor,                 # original
  PBI_crecimiento = PBI_crecimiento,              # original
  desempleo = desempleo,                          # original
  gasto_publico = gasto_publico,                  # original
  tasa_de_cambio = tasa_de_cambio                 # original
)

#Input 2: las bases modificadas

inflacion_log <- read.csv("data/clean/inflacion_log.csv")
PBI_crecimiento_clean <- read.csv("data/clean/PBI_crecimiento_clean.csv")
desempleo_clean <- read.csv("data/clean/desempleo_clean.csv")
gasto_publico_wins <- read.csv("data/clean/gasto_publico_wins.csv")
tasa_de_cambio_log <- read.csv("data/clean/tasa_de_cambio_log.csv")
bases_juntas_limpias <- list(
  inflacion = inflacion_log %>% 
    select(tasa.de.inflacion, inflacion_log),     # log + winsorizada
  
  PBI_crecimiento = PBI_crecimiento_clean,        # limpia
  desempleo = desempleo_clean,                    # limpia
  gasto_publico = gasto_publico_wins,             # winsorizada
  tasa_de_cambio = tasa_de_cambio_log             # log
)

#Estadisticas antes de limpieza
estadistica_antes <- lapply(bases_juntas, function(df){
  df %>%
    select(where(is.numeric)) %>%
    select(-any_of("anio")) %>%      # elimino si existe
    psych::describe()
})

#Estadisticas despues de la limpieza
estadistica_despues <- lapply(bases_juntas_limpias, function(df){
  df %>%
    select(where(is.numeric)) %>%
    select(-any_of("anio")) %>%      
    psych::describe()
})

comparacion <- mapply(
  function(antes, despues){
    list(
      antes = antes,
      despues = despues
    )
  },
  estadistica_antes,
  estadistica_despues,
  SIMPLIFY = FALSE
)

#===============================================================
# FUNCIÓN: agrega fila con cambio porcentual
#===============================================================
agregar_cambio <- function(tab_antes, tab_despues) {
  a <- tab_antes 
  b <- tab_despues 
  
cols_num <- setdiff(intersect(colnames(a), colnames(b)), c("vars","n"))
  
# Fila base (sin log|wins)
i_base <- which(!grepl("(log|wins)", rownames(a), ignore.case = TRUE))
if (length(i_base) == 0) i_base <- which(!grepl("(log|wins)", rownames(b), ignore.case = TRUE))
i_base <- i_base[1]
  
 # Fila transformada (log|wins)
i_trans <- which(grepl("(log|wins)", rownames(b), ignore.case = TRUE))
if (length(i_trans) == 0) i_trans <- which(grepl("(log|wins)", rownames(a), ignore.case = TRUE))
if (length(i_trans) == 0) i_trans <- i_base
  
# valores comparables
base_vals <- if (i_base <= nrow(a)) a[i_base, cols_num, drop = FALSE] else b[i_base, cols_num, drop = FALSE]
trans_vals <- if (i_trans <= nrow(b)) b[i_trans, cols_num, drop = FALSE] else a[i_trans, cols_num, drop = FALSE]

# cambio porcentual
cambio <- 100 * (as.numeric(trans_vals) - as.numeric(base_vals)) /
ifelse(as.numeric(base_vals) == 0, NA, abs(as.numeric(base_vals)))
  
# nuev fila
fila_cambio <- b[1, , drop = FALSE]
fila_cambio[ , "vars"] <- NA
if ("n" %in% names(fila_cambio)) fila_cambio[ , "n"] <- NA
fila_cambio[ , cols_num] <- cambio
rownames(fila_cambio) <- "cambio_pct"
  
# output
rbind(b, fila_cambio)
}

#===============================================================
# COMPARACIÓN: antes vs después + cambio porcentual
#===============================================================
resultado_final <- Map(
  agregar_cambio,
  estadistica_antes,
  estadistica_despues
)

#===============================================================
# VISUALIZACIÓN NORMAL (tablas)
#===============================================================
for(nm in names(resultado_final)){
  cat("\n$", nm, "\n", sep = "")
  print(resultado_final[[nm]], digits = 4)
}

# VISUALIZACIÓN EN FORMATO LARGO (más clara para el informe)
#===============================================================
bloque_categoria <- function(tab) {  
  tb <- as.data.frame(tab)
  rn <- rownames(tb)
  
  i_base   <- which(!grepl("(log|wins|cambio_pct)", rn, ignore.case = TRUE))[1]
  i_trans  <- which(grepl("(log|wins)", rn, ignore.case = TRUE))[1]
  i_cambio <- which(rn == "cambio_pct")
  
  if (is.na(i_trans) || length(i_trans) == 0) i_trans <- i_base
  
  stats <- colnames(tb)
  keep  <- stats 
  
  out <- lapply(keep, function(st) {
    a <- as.numeric(tb[i_base,  st])
    d <- as.numeric(tb[i_trans, st])
    
    if (length(i_cambio) == 0) {
      cp <- 100 * (d - a) / ifelse(a == 0, NA, abs(a))
    } else {
      cp <- as.numeric(tb[i_cambio, st])
    }
    
    data.frame(
      estadistico = st,
      antes       = a,
      despues     = d,
      cambio_pct  = cp,
      check.names = FALSE
    )
  })
  
  do.call(rbind, out) |>
    dplyr::filter(!estadistico %in% c("vars", "n")) |>
    dplyr::mutate(
      estadistico = dplyr::recode(
        estadistico,
        mean     = "media",
        sd       = "desvío_estándar",
        median   = "mediana",
        trimmed  = "media_podada",
        mad      = "desviación_mediana",
        min      = "mínimo",
        max      = "máximo",
        range    = "rango_total (NO IQR)",
        skew     = "asimetría",
        kurtosis = "curtosis",
        se       = "error_estándar"
      )
    ) |>
    dplyr::mutate(across(c(antes, despues, cambio_pct), ~ round(.x, 4)))
}

bloques <- lapply(resultado_final, bloque_categoria)

invisible(lapply(names(bloques), function(nm){
  cat("\n$", nm, "\n", sep = "")
  print(bloques[[nm]], row.names = FALSE)
}))

#Los histogramas y los boxplots estan en el script 2


#EL RANGO INTERCUARTIL NO CAMBIA SI SOLO CAMBIAN LOS VALORES EXTREMOS. Se calcula P75-P25.
#Winsorizando solo cambiamos el P1 y P99. Por ende, no deberia cambiar el IQR.
rango_gasto_limpio <- IQR(gasto_publico_wins$gasto_wins, na.rm = TRUE)
rango_inflacion_limpio <- IQR(inflacion_log$tasa.de.inflacion, na.rm =TRUE)
rango_tasa_de_cambio_limpio <- IQR(tasa_de_cambio_log$tasa_de_cambio_oficial, na.rm = TRUE)

#Comparamos P75 y P25 antes y despues de la limpieza para corroborar
quantile(gasto_publico$tasa.de.gasto.publico, probs = c(0.25, 0.75), na.rm = TRUE)
quantile(gasto_publico_wins$gasto_wins, probs = c(0.25, 0.75), na.rm = TRUE)

quantile(inflacion_deflactor$tasa.de.inflacion, probs = c(0.25, 0.75), na.rm = TRUE)
quantile(inflacion_log$tasa.de.inflacion, probs = c(0.25, 0.75), na.rm = TRUE)

quantile(tasa_de_cambio$tasa_de_cambio_oficial, probs = c(0.25, 0.75), na.rm = TRUE)
quantile(tasa_de_cambio_log$tasa_de_cambio_oficial, probs = c(0.25, 0.75), na.rm = TRUE)