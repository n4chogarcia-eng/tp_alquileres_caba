# =============================================================================
# INSTANCIA 3 - Visualización y comunicación de resultados
#
# Trabajo Práctico - Ciencia de Datos para Economía y Negocios (FCE-UBA)
# Grupo 4
#
# Dos gráficos, uno exploratorio y uno comunicativo, sobre los hallazgos de la
# segunda instancia:
#
#   exploratorio  : heatmap de las 45 series (comuna x tipo) por trimestre.
#                   Muestra a la vez el nivel de precio, los faltantes y el
#                   episodio de 2023. Es el gráfico de la hipótesis 1.
#   comunicativo  : la beta de cada serie cuando el mercado sube y cuando baja.
#                   Es el hallazgo de la hipótesis 2.
#
# Salida: output/graficos/grupo_04_exploratorio.jpg
#         output/graficos/grupo_04_comunicativo.jpg
#         (y una copia en la raíz, que es la que se envió por mail)
# =============================================================================

if (rstudioapi::isAvailable()) {
  setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", ".."))
}

# install.packages(c("tidyverse", "scales", "ggrepel"))
library(tidyverse)
library(scales)
library(ggrepel)

outstub <- "output/graficos"

# ---- Tema de la casa, como en la galería de la clase 22 ---------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),
      plot.subtitle    = element_text(color = "gray40", size = 10.5),
      plot.caption     = element_text(color = "gray50", hjust = 0, size = 8.5),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
)

FUENTE <- paste("Fuente: elaboración propia en base al Instituto de Estadística y Censos de la",
                "Ciudad de Buenos Aires (datos de Argenprop) e IPC del INDEC, región GBA.")

FUENTE_NOTA <- paste0(FUENTE, "\n",
  "Nota: se toman las comunas que publican su precio en los 34 trimestres. La Comuna 8 no lo hace ",
  "y queda afuera. Cuando falta\nalgún dato suelto, se estima a partir de los trimestres vecinos.")



# =============================================================================
# 1. GRÁFICO EXPLORATORIO
#    Heatmap: 45 series (comuna x tipo de departamento) x 34 trimestres.
#    El color es el precio de la comuna respecto del promedio de la Ciudad y
#    el gris oscuro marca los trimestres sin dato publicado.
# =============================================================================

alq <- read_csv("input/deflactados/alquileres_comunas.csv", show_col_types = FALSE) |>
  mutate(
    t    = (anio - 2018) * 4 + trimestre,
    zona = factor(zona, levels = c("Norte", "Centro", "Oeste", "Sur")),
    tipo = recode(ambientes, "1 ambiente" = "1 amb.",
                  "2 ambientes" = "2 amb.", "3 ambientes" = "3 amb.")
  )

# Ordenamos las filas por zona y después por comuna, para que las franjas de
# cada zona queden juntas y se pueda leer el patrón territorial.
orden <- alq |>
  distinct(zona, comuna, tipo) |>
  arrange(desc(zona), desc(comuna), desc(tipo)) |>
  mutate(serie = paste0("C", comuna, " · ", tipo))

alq <- alq |>
  mutate(serie = factor(paste0("C", comuna, " · ", tipo), levels = orden$serie))

# Líneas que separan las zonas y posición donde va la etiqueta de cada una
bloques <- orden |>
  mutate(fila = row_number()) |>
  group_by(zona) |>
  summarise(fin = max(fila), centro = mean(fila), .groups = "drop")

cortes_zona <- bloques |> filter(fin < nrow(orden)) |> pull(fin)

# Etiquetas del eje de tiempo: un año de por medio para que no se amontonen
etiquetas_t <- tibble(t = seq(1, 34, 4)) |>
  mutate(lab = paste0(2018 + (t - 1) %/% 4))

g_exploratorio <- ggplot(alq, aes(x = t, y = serie, fill = precio_relativo)) +
  geom_tile(color = "white", linewidth = 0.25) +
  # Cortes de los tres períodos regulatorios
  geom_vline(xintercept = c(10.5, 24.5), color = "gray20",
             linewidth = 0.6, linetype = "22") +
  geom_hline(yintercept = cortes_zona + 0.5, color = "gray20", linewidth = 0.5) +
  # Etiqueta de cada zona, al costado derecho de su bloque de filas
  geom_text(data = bloques, aes(x = 35.1, y = centro, label = zona),
            inherit.aes = FALSE, hjust = 0, fontface = "bold",
            size = 3.4, color = "gray25") +
  annotate("text", x = c(5.5, 17.5, 29.5), y = 46.6,
           label = c("Antes de la ley", "Ley de Alquileres 27.551",
                     "Tras la derogación"),
           size = 3.1, color = "gray25", fontface = "bold") +
  scale_fill_gradient2(
    low = "#2C7FB8", mid = "#F7F4EC", high = "#D95F0E", midpoint = 100,
    limits = c(64, 143), breaks = c(70, 85, 100, 115, 130),
    labels = label_number(suffix = "%"),
    na.value = "gray30",   # más oscuro que los dos extremos de la escala, para
                           # que el faltante se distinga también en blanco y negro
    name = "Precio de la comuna respecto del promedio de la Ciudad"
  ) +
  scale_x_continuous(breaks = etiquetas_t$t, labels = etiquetas_t$lab,
                     limits = c(0.5, 38.5), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 2.2))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 16, barheight = 0.6)) +
  labs(
    title = "Precio relativo y datos faltantes, comuna por comuna y trimestre a trimestre",
    subtitle = paste("Cada fila es una comuna y un tipo de departamento; cada columna, un trimestre.",
                     "En gris, los trimestres sin precio publicado:\nel Instituto solo publica una",
                     "comuna si alcanza una cantidad mínima de avisos, así que el faltante mide",
                     "indirectamente la oferta.\nCiudad de Buenos Aires, 1er trimestre de 2018 a",
                     "2do trimestre de 2026. 45 series, 1.530 observaciones."),
    caption = FUENTE,
    x = "Trimestre", y = "Comuna y tipo de departamento"
  ) +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 9),
    panel.grid  = element_blank(),
    legend.title = element_text(size = 9.5)
  )

ggsave(file.path(outstub, "grupo_04_exploratorio.jpg"), g_exploratorio,
       width = 25, height = 21, units = "cm", dpi = 300)


# =============================================================================
# 2. GRÁFICO COMUNICATIVO
#    Línea con índice base 100, al estilo del ejemplo de la clase 21:
#    título que afirma la conclusión, línea de referencia, un tramo resaltado
#    y anotaciones sobre los puntos que importan.
#    Mensaje único: cuando el alquiler cae, las comunas caras caen más.
# =============================================================================

library(zoo)

# Nivel de precio de cada comuna: el promedio de sus series (1, 2 y 3 ambientes).
nivel_comuna <- read_csv("output/tablas/t29_betas_por_serie.csv", show_col_types = FALSE) |>
  group_by(comuna) |>
  summarise(nivel = mean(nivel), .groups = "drop")

# Partimos en tercios las 14 comunas con serie completa y nos quedamos con los
# dos extremos: las 5 más caras y las 5 más baratas.
cortes <- quantile(nivel_comuna$nivel, c(1/3, 2/3))
grupos <- nivel_comuna |>
  mutate(grupo = case_when(nivel >= cortes[2] ~ "caras",
                           nivel <= cortes[1] ~ "baratas",
                           TRUE ~ "medias")) |>
  filter(grupo != "medias") |>
  select(comuna, grupo)

# Índice de alquiler real con base en el promedio de 2018, sobre el panel
# balanceado (las mismas series en los 34 trimestres).
indice <- read_csv("input/deflactados/alquileres_comunas.csv", show_col_types = FALSE) |>
  mutate(t = (anio - 2018) * 4 + trimestre,
         serie = paste0("C", comuna, " - ", ambientes)) |>
  group_by(serie) |>
  mutate(p = na.approx(precio_m2_real, x = t, maxgap = 4, na.rm = FALSE)) |>
  filter(all(!is.na(p))) |>
  ungroup() |>
  inner_join(grupos, by = "comuna") |>
  group_by(grupo, t) |>
  summarise(p = mean(p), .groups = "drop") |>
  group_by(grupo) |>
  mutate(indice = 100 * p / mean(p[t <= 4])) |>
  ungroup() |>
  mutate(grupo = factor(grupo, c("caras", "baratas"),
                        c("Las 5 comunas más caras", "Las 5 más baratas")))

T_PICO <- 24                                   # 2023-T4
pico  <- indice |> filter(t == T_PICO)
final <- indice |> filter(t == max(t))

# La caída desde el pico, medida contra el promedio del último año
caida <- indice |>
  group_by(grupo) |>
  summarise(caida = round(abs(100 * (mean(indice[t >= 31]) / indice[t == T_PICO] - 1))),
            .groups = "drop")

etq_pico <- pico |>
  mutate(x = T_PICO + ifelse(grepl("caras", grupo), 0, -1.7),
         y = indice + ifelse(grepl("caras", grupo), 4, 0),
         h = ifelse(grepl("caras", grupo), 0.5, 1))

texto_caida <- paste0("Desde el pico, las caras cayeron ",
                      caida$caida[caida$grupo == "Las 5 comunas más caras"], "%\n",
                      "y las baratas, ",
                      caida$caida[caida$grupo == "Las 5 más baratas"], "%.")

col_grupo <- c("Las 5 comunas más caras" = "#B2182B",
               "Las 5 más baratas"       = "#8FA3B0")

etq_anios <- tibble(t = seq(1, 34, 4)) |> mutate(lab = 2018 + (t - 1) %/% 4)

g_comunicativo <- ggplot(indice, aes(x = t, y = indice, color = grupo)) +
  # Nivel de 2018: la referencia contra la que se lee todo
  geom_hline(yintercept = 100, color = "gray45", linetype = "22", linewidth = 0.5) +
  annotate("text", x = 33.5, y = 102.5, label = "Mismo precio que en 2018", hjust = 1,
           size = 3.2, color = "gray45") +
  # El pico, que es donde empieza la caída
  geom_vline(xintercept = T_PICO, color = "gray75", linewidth = 0.4) +
  geom_line(linewidth = 1.3) +
  geom_point(data = pico, size = 2.6) +
  geom_point(data = final, size = 2.6) +
  # Valores en el pico, corridos a un costado para que no los tape la línea
  geom_text(data = etq_pico, aes(x = x, y = y, label = round(indice), hjust = h),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  # Etiquetas directas al final de cada línea, en lugar de leyenda
  geom_text(data = final,
            aes(label = paste0(ifelse(grepl("caras", grupo), "Más caras", "Más baratas"),
                               ": ", round(indice))),
            hjust = 0, nudge_x = 0.7, size = 3.7, fontface = "bold",
            show.legend = FALSE) +
  annotate("text", x = T_PICO - 1.4, y = 164, hjust = 1, size = 3.2,
           color = "gray40", lineheight = 1.15,
           label = paste("Se deroga la Ley de Alquileres",
                         "(diciembre de 2023)", sep = "\n")) +
  # La anotación que lleva el mensaje
  annotate("segment", x = 27.5, xend = 25.2, y = 123, yend = 117,
           linewidth = 0.4, color = "#B2182B",
           arrow = arrow(length = unit(0.016, "npc"), type = "closed")) +
  annotate("text", x = 27.8, y = 126, hjust = 0, vjust = 1, size = 3.5,
           color = "#B2182B", lineheight = 1.15, fontface = "bold",
           label = texto_caida) +
  scale_color_manual(values = col_grupo, guide = "none") +
  scale_x_continuous(breaks = etq_anios$t, labels = etq_anios$lab,
                     limits = c(1, 40), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(80, 160, 20),
                     labels = label_number(accuracy = 1)) +
  labs(
    title = "Cuando el alquiler cae, las comunas más caras caen más",
    subtitle = paste("Precio del alquiler por metro cuadrado, ya descontada la inflación.",
                     "Si la línea está en 100, el alquiler cuesta\nlo mismo que en 2018; si está",
                     "en 87, cuesta 13% menos. Se comparan las 5 comunas más caras de la\nCiudad",
                     "con las 5 más baratas. Ciudad de Buenos Aires, de 2018 a 2026."),
    caption = FUENTE_NOTA,
    x = "Trimestre", y = "Precio del alquiler (2018 = 100)"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10),
    plot.margin = margin(10, 12, 10, 10)
  )

ggsave(file.path(outstub, "grupo_04_comunicativo.jpg"), g_comunicativo,
       width = 24, height = 14, units = "cm", dpi = 300)

# Copia en la raíz del proyecto: son los dos archivos que se entregaron por
# mail y conviene que no queden desactualizados respecto de output/graficos.
for (f in c("grupo_04_exploratorio.jpg", "grupo_04_comunicativo.jpg")) {
  file.copy(file.path(outstub, f), f, overwrite = TRUE)
}

cat("\nListo. Los dos JPG están en", outstub, "y en la raíz del proyecto.\n")
