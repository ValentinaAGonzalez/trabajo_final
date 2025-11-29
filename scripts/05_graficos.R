#================================
#GRAFICOS EDITORIALIZADOS
#================================

library(ggplot2)
library(ggtext)
library(tidyverse)
library(showtext)
library(grid)

#seteamos tipografia: 
font_add_google("Roboto Condensed", "roboto")
showtext_auto()
df_clean <- df %>% drop_na()

df_med <- df_clean |>
  filter(!is.na(grupo_inflacion)) |>
  summarise(
    med_PBI_alto   = median(PBI[grupo_inflacion == "alto"],  na.rm = TRUE),
    med_PBI_bajo   = median(PBI[grupo_inflacion == "bajo"],  na.rm = TRUE),
    med_des_alto   = median(desempleo[grupo_inflacion == "alto"], na.rm = TRUE),
    med_des_bajo   = median(desempleo[grupo_inflacion == "bajo"], na.rm = TRUE),
    med_gasto_alto = median(gasto[grupo_inflacion == "alto"], na.rm = TRUE),
    med_gasto_bajo = median(gasto[grupo_inflacion == "bajo"], na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  separate(variable, into = c("var", "grupo"), sep = "_(?=[^_]+$)") |>
  mutate(
    var = case_when(
      str_detect(var, "med_PBI")   ~ "PBI",
      str_detect(var, "med_des")   ~ "Desempleo",
      str_detect(var, "med_gasto") ~ "Gasto público",
      TRUE ~ var
    ),
    grupo = if_else(grupo == "alto", "Inflación alta", "Inflación baja"),
    grupo = factor(grupo, levels = c("Inflación alta", "Inflación baja")),
    var   = factor(var,   levels = c("Desempleo", "Gasto público", "PBI"))
  )

#GRÁFICO 1: distribución del crecimiento del PBI según inflación
#---------------------------------------------------------------
df_pbi <- df_clean |>
  filter(!is.na(grupo_inflacion))
g1 <- ggplot(df_pbi, aes(x = grupo_inflacion, y = PBI, fill = grupo_inflacion)) +
  geom_violin(trim = FALSE, alpha = 0.35) +
  geom_boxplot(width = 0.08, outlier.shape = NA, alpha = 0.9) +
  geom_jitter(width = 0.08, alpha = 0.25, size = 1.2) +
  # mediana como punto negro
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  # etiqueta de la mediana encima
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.2f", after_stat(y))),
    vjust = -1.1,
    size = 3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_discrete(
    labels = c(alto = "Inflación alta", bajo = "Inflación baja")
  ) +
  scale_fill_manual(
    values = c(alto = "#f4a582", bajo = "#92c5de"),
    guide = "none"
  ) +
  labs(
    title = "¿Crece menos el PBI en países con alta inflación?",
    subtitle = "Distribución del crecimiento del PBI (2000–2023)\nsegún nivel de inflación",
    x = "Grupo de inflación",
    y = "Crecimiento del PBI (%)",
    caption = "Fuente: Penn World Table 10.0 — Elaboración propia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title.x  = element_text(face = "bold"),
    axis.title.y  = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
print(g1)
ggsave("grafico1_pbi_violin.pdf", g1, width = 8, height = 5)

#===============
#GRAFICO 2
#===============
g2 <- ggplot(df_med, aes(x = grupo, y = valor, group = var, colour = var)) +
  geom_line(size = 1.1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = sprintf("%.2f", valor)),
    vjust = -0.9,
    size  = 3
  ) +
  facet_wrap(~ var, ncol = 1, scales = "free_y") +
  scale_colour_manual(
    values = c(
      "Desempleo"     = "#d73027",
      "Gasto público" = "#1a9850",
      "PBI"           = "#4575b4"
    ),
    guide = "none"
  ) +
  labs(
    title = "Efectos de la inflación sobre variables macroeconómicas",
    subtitle = "Medianas entre 2000–2023 en países con inflación alta y baja",
    x = "",
    y = "",
    caption = "Fuente: Penn World Table 10.0 — Elaboración propia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    strip.text    = element_text(face = "bold", size = 12),
    axis.text.x   = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
print(g2)
ggsave("grafico2_slope_medianas.pdf", g2, width = 8, height = 6)
