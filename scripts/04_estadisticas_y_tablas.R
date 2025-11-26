#===========================
#ANÁLISIS ESTADÍSTICO - TEST
#===========================

#¿Los países con gasto público alto tienen un crecimiento del PBI distinto a los de gasto público bajo?

#H0: μ_alto = μ_bajo
#H1: μ_alto ≠ μ_bajo

library(dplyr)
library(ggplot2)
library(car)       
library(psych)

inflacion_log         <- read.csv("data/clean/inflacion_log.csv")
PBI_crecimiento_clean <- read.csv("data/clean/PBI_crecimiento_clean.csv")
desempleo_clean       <- read.csv("data/clean/desempleo_clean.csv")
gasto_publico_wins    <- read.csv("data/clean/gasto_publico_wins.csv")
tasa_de_cambio_log    <- read.csv("data/clean/tasa_de_cambio_log.csv")


#SUPUESTOS:
df <- data.frame(
  inflacion = inflacion_log$inflacion_log,
  PBI = PBI_crecimiento_clean$tasa.de.crecimiento.PBI,
  desempleo = desempleo_clean$tasa.de.desempleo,
  gasto = gasto_publico_wins$gasto_wins
)
df$grupo_inflacion <- ifelse(df$inflacion > median(df$inflacion, na.rm = TRUE),
                             "alto", "bajo")
df$grupo_inflacion <- factor(df$grupo_inflacion)


#Normalidad (Shapiro-Wilk) 
#-------------------------

#PBI: 
shapiro_PBI_alto <- shapiro.test(df$PBI[df$grupo_inflacion == "alto"])
shapiro_PBI_bajo <- shapiro.test(df$PBI[df$grupo_inflacion == "bajo"])
#ambos no normales 

#Desempleo
shapiro_des_alto <- shapiro.test(df$desempleo[df$grupo_inflacion == "alto"])
shapiro_des_bajo <- shapiro.test(df$desempleo[df$grupo_inflacion == "bajo"])
#ambos no normales

#Gasto publico
shapiro_gasto_alto <- shapiro.test(df$gasto[df$grupo_inflacion == "alto"])
shapiro_gasto_bajo <- shapiro.test(df$gasto[df$grupo_inflacion == "bajo"])
#ambos no normales 


#Grafico Q-Q
#-----------

qq_plot <- function(variable, grupo, nombre_variable) {
  datos <- variable[df$grupo_inflacion == grupo]
  
  qqnorm(datos,
         main = paste("Q-Q Plot –", nombre_variable, "(Inflación", grupo, ")"),
         col = "black",
         pch = 19,
         cex = 0.6)
  
  qqline(datos, col = "red", lwd = 3)
}

#PBI
qq_plot(df$PBI, "alto", "PBI")
qq_plot(df$PBI, "bajo", "PBI")

#Desempleo
qq_plot(df$desempleo, "alto", "Desempleo")
qq_plot(df$desempleo, "bajo", "Desempleo")

#Gasto publico
qq_plot(df$gasto, "alto", "Gasto Público")
qq_plot(df$gasto, "bajo", "Gasto Público")

#Homogeneidad de Varianzas
#-------------------------

#PBI: 
leveneTest(PBI ~ grupo_inflacion, data = df) #varianzas homogeneas
#Desempleo:
leveneTest(desempleo ~ grupo_inflacion, data = df) #varianzas heterogeneas
#Gasto: 
leveneTest(gasto ~ grupo_inflacion, data = df) #varianzas homogeneas

#==============================================================================
#Aplicamos el test Wilcoxon Rank-Sum (Mann–Whitney) para PBI, desempleo y gasto

#PBI:
wilcox.test(PBI ~ grupo_inflacion, data=df)

#Desempleo
wilcox.test(desempleo ~ grupo_inflacion, data=df)

#Gasto público
wilcox.test(gasto ~ grupo_inflacion, data=df)
#==============================================================================
#ANOVA de Welch

#PBI: 
anova_PBI <- oneway.test(PBI ~ grupo_inflacion, data = df, var.equal = FALSE)
print(anova_PBI)

#Desempleo
anova_desempleo <- oneway.test(desempleo ~ grupo_inflacion, data = df, var.equal = FALSE)
print(anova_desempleo)

#Gasto
anova_gasto <- oneway.test(gasto ~ grupo_inflacion, data = df, var.equal = FALSE)
print(anova_gasto)

#Gráficos de ANOVA:
#------------------
ggplot(df, aes(x = grupo_inflacion, y = PBI, fill = grupo_inflacion)) +
  geom_boxplot() +
  labs(title = "Crecimiento del PBI según nivel de inflación",
       x = "Grupo de inflación", y = "Crecimiento del PBI") +
  theme_minimal() #PBI

ggplot(df, aes(x = grupo_inflacion, y = desempleo, fill = grupo_inflacion)) +
  geom_boxplot() +
  labs(title = "Desempleo según nivel de inflación",
       x = "Grupo de inflación", y = "Tasa de desempleo") +
  theme_minimal() #Desempleo

ggplot(df, aes(x = grupo_inflacion, y = gasto, fill = grupo_inflacion)) +
  geom_boxplot() +
  labs(title = "Gasto público según nivel de inflación",
       x = "Grupo de inflación", y = "Gasto público (winsorizado)") +
  theme_minimal() #Gasto 


