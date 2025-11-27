
library(ggplot2)
library(ggtext)
library(tidyverse)
library(showtext)

df_viz <- df_clean |> 
  filter(!is.na(grupo_inflacion))

median_global <- median(df_viz$PBI, na.rm = TRUE)
ggplot(df_viz, aes(x = grupo_inflacion, y = PBI, fill = grupo_inflacion)) +
  geom_violin(trim = FALSE, alpha = .6, color = NA) +
  stat_summary(fun = median, geom = "point", size = 4, color = "black") +
  
  # Línea horizontal de referencia global
  geom_hline(yintercept = median_global, 
             linetype = "dashed", color = "gray40") +
  
  # Anotación editorial
  annotate("text",
           x = 1.05,
           y = median_global + 1,
           label = "Mediana global",
           size = 4,
           color = "gray30") +
  
  scale_fill_manual(values = c("alto" = "#EE9A9A", "bajo" = "#8DD3C7")) +
  
  labs(
    title = "¿Crece menos el PBI en países con alta inflación?",
    subtitle = "Distribución del crecimiento económico (2000–2023)\ncomparando países con inflación **alta** vs. **baja**",
    x = "Grupo de inflación",
    y = "Crecimiento del PBI (%)",
    caption = "Fuente: Penn World Table 10.0 — Elaboración propia"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(size = 22, face = "bold"),
    plot.subtitle = element_markdown(size = 13, color = "gray30"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.caption = element_text(size = 10, color = "gray40")
  )

#Grafico 2

df_med <- df_clean |>
  filter(!is.na(grupo_inflacion)) |>
  summarise(
    med_PBI_alto   = median(PBI[grupo_inflacion == "alto"], na.rm = TRUE),
    med_PBI_bajo   = median(PBI[grupo_inflacion == "bajo"], na.rm = TRUE),
    med_des_alto   = median(desempleo[grupo_inflacion == "alto"], na.rm = TRUE),
    med_des_bajo   = median(desempleo[grupo_inflacion == "bajo"], na.rm = TRUE),
    med_gasto_alto = median(gasto[grupo_inflacion == "alto"], na.rm = TRUE),
    med_gasto_bajo = median(gasto[grupo_inflacion == "bajo"], na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  separate(
    col  = variable,
    into = c("var", "grupo"),
    sep  = "_(?=[^_]+$)"   # esto ya es una regex, no hace falta regex = TRUE
  ) |>
  mutate(
    var = dplyr::case_when(
      var == "med_PBI"   ~ "PBI",
      var == "med_des"   ~ "Desempleo",
      var == "med_gasto" ~ "Gasto público",
      TRUE ~ var
    ),
    grupo = dplyr::recode(
      grupo,
      "alto" = "Inflación alta",
      "bajo" = "Inflación baja"
    )
  )

ggplot(df_med, aes(x = grupo, y = valor, group = var, color = var)) +
  # Línea y puntos
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3.8) +
  
  # Etiquetas numéricas en AMBOS puntos
  geom_text(
    aes(label = round(valor, 2)),
    vjust = -1.1,                  # un poquito por encima
    size = 3.8,
    fontface = "bold"
  ) +
  
  # Facetas: una por variable
  facet_wrap(~var, scales = "free_y", ncol = 1, strip.position = "top") +
  
  # Colores suaves y profesionales
  scale_color_manual(
    values = c(
      "Desempleo"      = "#D55E5E",  # rojo suave
      "Gasto público"  = "#70AD47",  # verde suave
      "PBI"            = "#4472C4"   # azul suave
    )
  ) +
  
  labs(
    title = "Efectos de la inflación sobre variables macroeconómicas",
    subtitle = "Comparación de medianas entre países con inflación alta y baja (2000–2023)",
    x = "",
    y = "",
    caption = "Fuente: Penn World Table 10.0 — Elaboración propia"
  ) +
  
  coord_cartesian(clip = "off") +   # deja que las etiquetas respiren fuera del panel
  
  theme_minimal(base_family = "lato") +
  theme(
    # Títulos
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 13, color = "gray30", hjust = 0),
    
    # Facet titles
    strip.text = element_text(size = 15, face = "bold"),
    strip.background = element_blank(),
    
    # Ejes
    axis.text.x = element_text(size = 12, face = "bold", color = "gray20"),
    axis.text.y = element_text(size = 11, color = "gray50"),
    
    # Grilla (más tenue)
    panel.grid.major.y = element_line(color = "gray90", size = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Sin leyenda (ya está explicado por facetas)
    legend.position = "none",
    
    # Márgenes para que entren bien las etiquetas
    plot.margin = margin(15, 40, 25, 20),
    
    # Caption
    plot.caption = element_text(size = 10, color = "gray40", hjust = 1)
  )
