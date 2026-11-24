# =============================================================================
# 01 - ANÁLISIS EXPLORATORIO DE DATOS (EDA)
#
# Trabajo Práctico - Ciencia de Datos para Economía y Negocios (FCE-UBA)
# Instancia 1 y agregados de la devolución
# Base: Precio de departamentos en alquiler por comuna, Ciudad de Buenos Aires
#
# Trabaja sobre la base ya deflactada (input/deflactados/), con los precios en
# pesos constantes del 2do trimestre de 2026.
#
# Seguimos el recorrido de la práctica de la clase 8:
#   1. Primer vistazo: dimensiones, tipos, unidad de observación
#   2. Diccionario de variables
#   3. Valores faltantes
#   4. Estadísticas descriptivas: summary() y skim()
#   5. Tablas de frecuencia
#   6. Histogramas
#   7. Boxplots y valores atípicos
#   8. Percentiles y ratios entre percentiles
#
# Las tablas se guardan en output/tablas/ y los gráficos en output/graficos/.
# =============================================================================


# =============================================================================
# 0. PAQUETES Y CARGA DE LA BASE
# =============================================================================

# Carpeta de trabajo: la del proyecto (la que tiene raw/, input/, script/...).
# Si se corre desde RStudio (por ejemplo, seleccionando todo y ejecutando), R
# se ubica solo: toma la carpeta donde está este script y sube dos niveles
# (script/<etapa>/ -> carpeta del proyecto). Así funciona en cualquier
# computadora, sin tener que cambiar ninguna ruta a mano.
if (rstudioapi::isAvailable()) {
  setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", ".."))
}
getwd()   # tiene que terminar en .../tp_alquileres_caba

# install.packages(c("tidyverse", "skimr"))

library(tidyverse)
library(skimr)
library(zoo)

# Guardamos la configuración de gráficos original para poder volver a ella
par_original <- par(no.readonly = TRUE)

alquileres <- read_csv("input/deflactados/alquileres_comunas.csv",
                       show_col_types = FALSE) |>
  mutate(
    comuna    = factor(comuna, levels = 1:15),
    zona      = factor(zona, levels = c("Norte", "Centro", "Oeste", "Sur")),
    ambientes = factor(ambientes,
                       levels = c("1 ambiente", "2 ambientes", "3 ambientes"))
  )

total_caba <- read_csv("input/deflactados/total_caba.csv",
                       show_col_types = FALSE) |>
  mutate(ambientes = factor(ambientes,
                            levels = c("1 ambiente", "2 ambientes", "3 ambientes")))


# =============================================================================
# 1. PRIMER VISTAZO
# =============================================================================

dim(alquileres)      # filas y columnas
nrow(alquileres)
ncol(alquileres)
names(alquileres)

glimpse(alquileres)
head(alquileres)

# --- Unidad de observación ---------------------------------------------------

# Cada fila es una combinación de comuna, tipo de departamento y trimestre.
# Verificamos que no haya duplicados: si los hubiera, el archivo se leyó mal.
sum(duplicated(alquileres[, c("comuna", "ambientes", "periodo")]))

# Cobertura temporal
n_distinct(alquileres$periodo)
range(alquileres$anio)
min(alquileres$periodo)
max(alquileres$periodo)

# 15 comunas x 3 tipos x 34 trimestres
n_distinct(alquileres$comuna) * n_distinct(alquileres$ambientes) *
  n_distinct(alquileres$periodo)


# --- Tipo de cada variable ---------------------------------------------------

sapply(alquileres, class)

tipos <- tibble(
  variable = names(alquileres),
  tipo_r   = sapply(alquileres, function(x) class(x)[1])
)
tipos


# =============================================================================
# 2. DICCIONARIO DE VARIABLES
# =============================================================================

# La consigna pide describir cada variable: qué representa, de qué tipo es y
# cuántos nulos tiene. Armamos el diccionario como objeto de R para poder
# exportarlo y que se actualice solo.

nulos <- colSums(is.na(alquileres))

diccionario <- tribble(
  ~variable,          ~descripcion,                                                              ~tipo,
  "comuna",           "Comuna de la Ciudad de Buenos Aires (1 a 15)",                             "Categórica",
  "zona",             "Agrupamiento geográfico de comunas: Norte, Centro, Oeste, Sur",            "Categórica",
  "ambientes",        "Tipo de departamento: 1, 2 o 3 ambientes (usados)",                        "Categórica ordinal",
  "superficie_m2",    "Superficie de referencia del cuadro: 30, 43 o 70 m2",                      "Numérica",
  "anio",             "Año del dato (2018 a 2026)",                                               "Temporal",
  "trimestre",        "Trimestre del año (1 a 4)",                                                "Temporal",
  "periodo",          "Etiqueta año-trimestre, por ejemplo 2018-T1",                              "Temporal",
  "precio_corriente", "Alquiler mensual promedio publicado, en pesos corrientes (dato original)",  "Numérica",
  "precio_real",      "Alquiler mensual en pesos constantes de 2026-T2 (deflactado por IPC)",     "Numérica",
  "precio_m2_real",   "Precio real por metro cuadrado (precio_real / superficie_m2)",             "Numérica",
  "precio_relativo",  "Precio de la comuna como % del promedio de la Ciudad, mismo trimestre y tipo", "Numérica"
) |>
  left_join(
    tibble(variable = names(nulos), valores_nulos = as.integer(nulos)),
    by = "variable"
  ) |>
  mutate(pct_nulos = round(100 * valores_nulos / nrow(alquileres), 1))

print(diccionario, n = Inf)

write_csv(diccionario, "output/tablas/t01_diccionario_variables.csv")


# =============================================================================
# 3. VALORES FALTANTES
# =============================================================================

# La fuente aclara: "solo se muestran valores para las comunas que cumplen con
# la cantidad mínima de unidades ofertadas requerida en cada caso". Es decir,
# el dato falta cuando hubo poca oferta publicada. Por lo tanto los faltantes
# NO son aleatorios: son informativos sobre el propio mercado.
#
# Deflactar no cambia los faltantes: un NA dividido por el IPC sigue siendo NA.

# Cantidad total de faltantes en el precio
sum(is.na(alquileres$precio_real))
round(100 * mean(is.na(alquileres$precio_real)), 1)

# --- 3.1. Faltantes por comuna -----------------------------------------------

faltantes_comuna <- alquileres |>
  group_by(comuna, zona) |>
  summarise(
    n            = n(),
    faltantes    = sum(is.na(precio_real)),
    pct_faltante = round(100 * mean(is.na(precio_real)), 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_faltante))

faltantes_comuna

# --- 3.2. Faltantes por zona y por tipo de departamento ----------------------

faltantes_zona <- alquileres |>
  group_by(zona) |>
  summarise(
    n            = n(),
    faltantes    = sum(is.na(precio_real)),
    pct_faltante = round(100 * mean(is.na(precio_real)), 1),
    .groups = "drop"
  )

faltantes_zona

alquileres |>
  group_by(ambientes) |>
  summarise(pct_faltante = round(100 * mean(is.na(precio_real)), 1), .groups = "drop")

write_csv(faltantes_comuna, "output/tablas/t02_faltantes_por_comuna.csv")


# --- 3.3. Gráfico de faltantes -----------------------------------------------

png("output/graficos/g01_faltantes_por_comuna.png", width = 900, height = 600)
par(mar = c(5, 5, 4, 2))
orden_falt <- faltantes_comuna |> arrange(desc(pct_faltante))
barplot(
  orden_falt$pct_faltante,
  names.arg = orden_falt$comuna,
  main = "Datos faltantes por comuna\nPrecio de alquiler, 2018-2026",
  xlab = "Comuna", ylab = "% de trimestres sin dato publicado",
  # Misma paleta que el resto de la presentación
  col = c("Norte" = "#15505E", "Centro" = "#2E7D8F",
          "Oeste" = "#C8742A", "Sur" = "#9B5A3C")[as.character(orden_falt$zona)],
  border = NA, las = 1, ylim = c(0, 100)
)
legend("topright",
       legend = c("Norte", "Centro", "Oeste", "Sur"),
       fill = c("#15505E", "#2E7D8F", "#C8742A", "#9B5A3C"),
       border = NA, bty = "n")
dev.off()
par(par_original)

# Lectura: los faltantes se concentran casi por completo en las comunas del
# sur. La Comuna 8 (Villa Soldati, Villa Lugano, Villa Riachuelo) casi no tiene
# datos en toda la serie. Esto significa que la base describe mucho mejor al
# norte de la Ciudad que al sur, y hay que decirlo al presentar cualquier
# promedio "de la Ciudad".


# =============================================================================
# 4. ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

# --- 4.1. summary(): precios corrientes contra precios reales ------------------

summary(alquileres)
summary(alquileres$precio_corriente)
summary(alquileres$precio_real)
summary(alquileres$precio_relativo)

# Sobre la serie completa, comparamos cuánto se separa la media de la mediana
# en cada versión del precio. En pesos corrientes la media casi cuadruplica a
# la mediana; en pesos constantes quedan muy cerca. Es decir: casi toda la
# asimetría de la serie en pesos corrientes era inflación, no mercado.
media_mediana <- tibble(
  precio  = c("corriente", "real (pesos de 2026-T2)"),
  media   = c(mean(alquileres$precio_corriente, na.rm = TRUE),
              mean(alquileres$precio_real, na.rm = TRUE)),
  mediana = c(median(alquileres$precio_corriente, na.rm = TRUE),
              median(alquileres$precio_real, na.rm = TRUE))
) |>
  mutate(media_sobre_mediana = round(media / mediana, 2),
         across(c(media, mediana), ~ round(.x, 0)))

media_mediana
write_csv(media_mediana, "output/tablas/t03_media_mediana_corriente_real.csv")


# --- 4.2. skim() -------------------------------------------------------------

skim(alquileres)

# Tabla de descriptivas lista para la presentación
tabla_skim <- alquileres |>
  select(precio_corriente, precio_real, precio_m2_real, precio_relativo) |>
  skim() |>
  as_tibble() |>
  select(variable = skim_variable,
         n_faltantes = n_missing,
         completitud = complete_rate,
         media = numeric.mean, sd = numeric.sd,
         p0 = numeric.p0, p25 = numeric.p25, p50 = numeric.p50,
         p75 = numeric.p75, p100 = numeric.p100) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tabla_skim
write_csv(tabla_skim, "output/tablas/t04_descriptivas.csv")


# --- 4.3. Descriptivas del último trimestre disponible -----------------------

# El último trimestre es también el trimestre base de la deflactación, así que
# en él precio corriente y precio real coinciden: estos son los valores de hoy.

ultimo <- max(alquileres$periodo)
ultimo

descriptivas_ultimo <- alquileres |>
  filter(periodo == ultimo) |>
  group_by(ambientes) |>
  summarise(
    n       = sum(!is.na(precio_real)),
    media   = mean(precio_real, na.rm = TRUE),
    mediana = median(precio_real, na.rm = TRUE),
    sd      = sd(precio_real, na.rm = TRUE),
    cv      = sd(precio_real, na.rm = TRUE) / mean(precio_real, na.rm = TRUE),
    min     = min(precio_real, na.rm = TRUE),
    max     = max(precio_real, na.rm = TRUE),
    iqr     = IQR(precio_real, na.rm = TRUE),
    media_m2 = mean(precio_m2_real, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

descriptivas_ultimo
write_csv(descriptivas_ultimo, "output/tablas/t05_descriptivas_ultimo_trimestre.csv")


# --- 4.4. Cuánto cambiaron los precios: nominal contra real ------------------

# Con los precios deflactados ya podemos comparar el primer trimestre de la
# serie con el último. La comparación nominal se deja al lado para mostrar la
# diferencia.
crecimiento <- total_caba |>
  filter(periodo %in% c("2018-T1", ultimo)) |>
  select(ambientes, periodo, precio_caba_corriente, precio_caba_real) |>
  pivot_wider(names_from = periodo,
              values_from = c(precio_caba_corriente, precio_caba_real)) |>
  mutate(
    veces_nominal = round(.data[[paste0("precio_caba_corriente_", ultimo)]] /
                            .data[["precio_caba_corriente_2018-T1"]], 1),
    var_real_pct  = round(100 * (.data[[paste0("precio_caba_real_", ultimo)]] /
                                   .data[["precio_caba_real_2018-T1"]] - 1), 1)
  ) |>
  select(ambientes, veces_nominal, var_real_pct,
         real_2018_T1 = `precio_caba_real_2018-T1`,
         real_ultimo  = paste0("precio_caba_real_", ultimo)) |>
  mutate(across(c(real_2018_T1, real_ultimo), ~ round(.x, 0)))

crecimiento
write_csv(crecimiento, "output/tablas/t06_crecimiento_nominal_y_real.csv")

# Lectura: en pesos corrientes el alquiler se multiplicó por más de 70, pero el
# IPC se multiplicó por casi 90 en el mismo período. En términos reales el
# alquiler de 2026-T2 está entre 16% y 20% por debajo del de 2018-T1.


# --- 4.5. Descriptivas por comuna --------------------------------------------

# En 4.3 miramos las descriptivas según el tipo de departamento. La otra
# desagregación natural de la base es la territorial: cómo se comporta cada
# comuna.
#
# Para comparar comunas entre sí usamos el PRECIO RELATIVO: el precio de la
# comuna como porcentaje del promedio de la Ciudad en el mismo trimestre y tipo.
# Tiene dos ventajas: pone a los tres tipos de departamento en la misma escala,
# y no depende de la inflación (da igual con precios corrientes o reales).
#
# Agregamos además el alquiler de un 2 ambientes en el último trimestre, para
# anclar la tabla en un valor concreto en pesos.

precio_2amb_ultimo <- alquileres |>
  filter(ambientes == "2 ambientes", periodo == ultimo) |>
  select(comuna, precio_2amb_ultimo = precio_real)

descriptivas_comuna <- alquileres |>
  group_by(comuna, zona) |>
  summarise(
    n_validas = sum(!is.na(precio_relativo)),
    nulos     = sum(is.na(precio_relativo)),
    media     = mean(precio_relativo, na.rm = TRUE),
    mediana   = median(precio_relativo, na.rm = TRUE),
    sd        = sd(precio_relativo, na.rm = TRUE),
    min       = min(precio_relativo, na.rm = TRUE),
    max       = max(precio_relativo, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  left_join(precio_2amb_ultimo, by = "comuna") |>
  mutate(across(c(media, mediana, sd, min, max), ~ round(.x, 1))) |>
  arrange(desc(media))

descriptivas_comuna
write_csv(descriptivas_comuna, "output/tablas/t07_descriptivas_por_comuna.csv")

# Lectura: la columna sd es la más informativa. Es baja (4 a 6 puntos) en casi
# todas las comunas, lo que significa que la posición relativa de cada comuna
# se mantiene estable a lo largo de los ocho años. La Comuna 8 es la excepción
# (sd = 8,9), pero con solo 3 observaciones válidas ese número no es confiable.


# =============================================================================
# 5. TABLAS DE FRECUENCIA
# =============================================================================

# --- 5.1. Frecuencias simples ------------------------------------------------

table(alquileres$zona)
table(alquileres$ambientes)
round(prop.table(table(alquileres$zona)) * 100, 1)

# Observaciones efectivamente disponibles (sin NA) por zona
alquileres |>
  filter(!is.na(precio_real)) |>
  count(zona, name = "observaciones") |>
  mutate(porcentaje = round(100 * observaciones / sum(observaciones), 1))


# --- 5.2. Tabla cruzada ------------------------------------------------------

tabla_cruzada <- table(alquileres$zona, alquileres$ambientes)
addmargins(tabla_cruzada)

# Datos disponibles por zona y tipo de departamento
tabla_disponibles <- alquileres |>
  filter(!is.na(precio_real)) |>
  count(zona, ambientes) |>
  pivot_wider(names_from = ambientes, values_from = n, values_fill = 0)

tabla_disponibles


# =============================================================================
# 6. HISTOGRAMAS
# =============================================================================

# --- 6.1. El mismo histograma, en pesos corrientes y en pesos constantes -----

# Ponemos lado a lado la distribución del precio sobre toda la serie, antes y
# después de deflactar. Marcamos la media y la mediana, como en la clase 8.

png("output/graficos/g02_histogramas_precio.png", width = 1300, height = 440)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

hist(
  alquileres$precio_corriente / 1000,
  breaks = 40,
  main = "Todos los trimestres, en pesos corrientes",
  xlab = "Miles de pesos por mes", ylab = "Cantidad de observaciones",
  col = "#D9D2C4", border = "white"
)
abline(v = mean(alquileres$precio_corriente, na.rm = TRUE) / 1000,
       col = "#15505E", lwd = 2, lty = 2)
abline(v = median(alquileres$precio_corriente, na.rm = TRUE) / 1000,
       col = "#C8742A", lwd = 2, lty = 3)
legend("topright", legend = c("Media", "Mediana"),
       col = c("#15505E", "#C8742A"), lwd = 2, lty = c(2, 3), bty = "n")

hist(
  alquileres$precio_real / 1000,
  breaks = 40,
  main = paste0("Todos los trimestres, en pesos constantes de ", ultimo),
  xlab = "Miles de pesos por mes", ylab = "Cantidad de observaciones",
  col = "steelblue", border = "white"
)
abline(v = mean(alquileres$precio_real, na.rm = TRUE) / 1000,
       col = "#15505E", lwd = 2, lty = 2)
abline(v = median(alquileres$precio_real, na.rm = TRUE) / 1000,
       col = "#C8742A", lwd = 2, lty = 3)
legend("topright", legend = c("Media", "Mediana"),
       col = c("#15505E", "#C8742A"), lwd = 2, lty = c(2, 3), bty = "n")

dev.off()
par(par_original)

# Moraleja: en pesos corrientes casi todas las observaciones se apilan contra
# el cero (son pesos de 2018-2021) y la media queda lejísimos de la mediana.
# En pesos constantes la distribución muestra algo del mercado: los tres tipos
# de departamento y las diferencias entre comunas.


# =============================================================================
# 7. BOXPLOTS Y VALORES ATÍPICOS
# =============================================================================

# --- 7.1. Precio relativo por comuna -----------------------------------------

# Ordenamos las comunas por su mediana: un boxplot ordenado se lee mucho mejor.
orden <- alquileres |>
  filter(!is.na(precio_relativo)) |>
  group_by(comuna) |>
  summarise(m = median(precio_relativo)) |>
  arrange(desc(m)) |>
  pull(comuna) |>
  as.character()

alquileres <- alquileres |>
  mutate(comuna_ord = factor(comuna, levels = orden))

png("output/graficos/g03_boxplot_precio_relativo_comuna.png", width = 1000, height = 600)
par(mar = c(6, 5, 4, 2))
boxplot(
  precio_relativo ~ comuna_ord,
  data = alquileres,
  main = "Precio del alquiler por comuna, respecto del promedio de la Ciudad (2018-2026)",
  xlab = "Comuna (ordenada por mediana)",
  ylab = "% del promedio de la Ciudad",
  col = "#E8E0D2", border = "#15505E",
  varwidth = TRUE,
  las = 1
)
abline(h = 100, col = "#C8742A", lwd = 2, lty = 2)
dev.off()
par(par_original)

# --- 7.2. Qué hay adentro del boxplot y quiénes son los atípicos -------------

bp <- boxplot.stats(alquileres$precio_relativo)
bp$stats    # bigote inferior, Q1, mediana, Q3, bigote superior
bp$n
length(bp$out)

q1  <- quantile(alquileres$precio_relativo, 0.25, na.rm = TRUE)
q3  <- quantile(alquileres$precio_relativo, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_inf <- q1 - 1.5 * iqr
limite_sup <- q3 + 1.5 * iqr
c(q1 = q1, q3 = q3, iqr = iqr, inf = limite_inf, sup = limite_sup)

# Los atípicos no se borran: se miran. Acá son casi todos de 2023, el año en
# que la inflación trimestral superó el 40%.
atipicos <- alquileres |>
  filter(precio_relativo > limite_sup | precio_relativo < limite_inf) |>
  select(comuna, zona, ambientes, periodo, precio_real, precio_relativo) |>
  arrange(desc(precio_relativo))

atipicos
count(atipicos, anio = substr(periodo, 1, 4))

write_csv(atipicos, "output/tablas/t08_valores_atipicos.csv")


# =============================================================================
# 8. PERCENTILES Y RATIOS ENTRE PERCENTILES
# =============================================================================

# --- 8.1. Percentiles del precio relativo ------------------------------------

quantile(alquileres$precio_relativo, na.rm = TRUE)
quantile(alquileres$precio_relativo,
         probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)


# --- 8.2. Ratio p90/p10: qué tan desigual es la Ciudad -----------------------

# El ratio entre percentiles mide la brecha de precios dentro de la Ciudad.
# Lo calculamos año por año sobre el precio real por m2, que es lo que hace
# comparables los tres tipos de departamento. Usamos el precio real porque
# dentro de un mismo año también hay inflación: en 2023, con precios corrientes,
# el ratio medía la diferencia entre enero y diciembre más que entre comunas.

brecha_anual <- alquileres |>
  filter(!is.na(precio_m2_real)) |>
  group_by(anio) |>
  summarise(
    n           = n(),
    p10         = quantile(precio_m2_real, 0.10),
    p50         = quantile(precio_m2_real, 0.50),
    p90         = quantile(precio_m2_real, 0.90),
    ratio_90_10 = round(p90 / p10, 2),
    .groups = "drop"
  ) |>
  mutate(across(c(p10, p50, p90), ~ round(.x, 0)))

brecha_anual

# El mismo ratio sobre el precio relativo
brecha_relativa <- alquileres |>
  filter(!is.na(precio_relativo)) |>
  group_by(anio) |>
  summarise(
    p10 = round(quantile(precio_relativo, 0.10), 1),
    p90 = round(quantile(precio_relativo, 0.90), 1),
    ratio_90_10 = round(p90 / p10, 2),
    .groups = "drop"
  )

brecha_relativa

write_csv(brecha_anual,    "output/tablas/t09_brecha_precio_m2_anual.csv")
write_csv(brecha_relativa, "output/tablas/t10_brecha_relativa_anual.csv")


# --- 8.3. Precio real por m2 según tipo de departamento ------------------------

# ¿El metro cuadrado del monoambiente es más caro que el del 3 ambientes?
precio_m2_tipo <- total_caba |>
  group_by(ambientes) |>
  summarise(
    superficie_m2    = first(superficie_m2),
    precio_m2_2018   = round(precio_m2_caba_real[periodo == "2018-T1"], 0),
    precio_m2_ultimo = round(precio_m2_caba_real[periodo == ultimo], 0),
    .groups = "drop"
  )

precio_m2_tipo

# Prima del monoambiente sobre el 3 ambientes, trimestre a trimestre. Es un
# cociente dentro del mismo trimestre, así que da igual con precios reales o
# corrientes.
prima <- total_caba |>
  select(ambientes, periodo, precio_m2_caba_real) |>
  pivot_wider(names_from = ambientes, values_from = precio_m2_caba_real) |>
  mutate(prima_1amb = round(100 * (`1 ambiente` / `3 ambientes` - 1), 1))

summary(prima$prima_1amb)

write_csv(precio_m2_tipo, "output/tablas/t11_precio_m2_por_tipo.csv")


# Evolución del precio real por m2. Con los precios deflactados ya no hace
# falta la escala logarítmica: la serie se puede graficar en niveles.
png("output/graficos/g04_precio_m2_por_tipo.png", width = 900, height = 600)
par(mar = c(5, 5, 4, 2))
serie <- total_caba |>
  select(ambientes, periodo, precio_m2_caba_real) |>
  pivot_wider(names_from = ambientes, values_from = precio_m2_caba_real) |>
  arrange(periodo)

plot(seq_len(nrow(serie)), serie$`1 ambiente`, type = "l", lwd = 2,
     col = "#d73027",
     ylim = c(0, max(serie$`1 ambiente`) * 1.05),
     main = paste0("Precio real del alquiler por m2 según tipo de departamento\n",
                   "Total Ciudad de Buenos Aires, en pesos constantes de ", ultimo),
     xlab = "", ylab = paste0("Pesos de ", ultimo, " por m2"), xaxt = "n")
lines(seq_len(nrow(serie)), serie$`2 ambientes`, lwd = 2, col = "#fc8d59")
lines(seq_len(nrow(serie)), serie$`3 ambientes`, lwd = 2, col = "#4575b4")
axis(1, at = seq(1, nrow(serie), by = 4), labels = serie$periodo[seq(1, nrow(serie), by = 4)],
     las = 2, cex.axis = 0.8)
legend("bottomleft", legend = c("1 ambiente", "2 ambientes", "3 ambientes"),
       col = c("#d73027", "#fc8d59", "#4575b4"), lwd = 2, bty = "n")
dev.off()
par(par_original)


# =============================================================================
# 9. CIERRE
# =============================================================================

# Checklist de la instancia 1:
#
#  [x] Dimensiones de la base            -> dim(), nrow(), ncol()          (1)
#  [x] Unidad de observación             -> duplicated(), n_distinct()     (1)
#  [x] Descripción y tipo de variables   -> diccionario                    (2)
#  [x] Valores faltantes                 -> colSums(is.na()), por grupo    (3)
#  [x] Estadísticas descriptivas         -> summary(), skim()              (4)
#  [x] Tablas de frecuencia              -> table(), prop.table(), cut()   (5)
#  [x] Forma de las distribuciones       -> hist()                         (6)
#  [x] Dispersión y atípicos             -> boxplot(), boxplot.stats()     (7)
#  [x] Medidas de posición y brechas     -> quantile(), ratio p90/p10      (8)
#  [x] Relación entre variables          -> cor(), plot()                  (9)
#  [x] Periodicidad de la fuente         -> trimestral (difusión)
#
# Advertencias que dejamos anotadas para las próximas instancias:
#
# 1. Los precios están deflactados con el IPC del INDEC (región GBA). El IPC
#    mide la canasta de consumo general; no dice nada sobre los ingresos. Para
#    saber si el alquiler pesa más o menos en el bolsillo habría que compararlo
#    con salarios.
# 2. Los faltantes no son aleatorios: se concentran en el sur de la Ciudad.
#    Todo promedio "de la Ciudad" está sesgado hacia el norte.
# 3. La base mide precios DE PUBLICACIÓN, no precios efectivamente pactados.
#    Es la oferta, no las operaciones cerradas.

cat("\nListo. Tablas en output/tablas/ y gráficos en output/graficos/\n")


# =============================================================================
# 10. AGREGADOS A PARTIR DE LA DEVOLUCIÓN DE LA INSTANCIA 1
# =============================================================================

# El docente pidió tres cosas: reemplazar el análisis de correlación por una
# tabla, ver si la evolución del precio fue igual en todas las zonas (y si hubo
# rezagos) y mirar si las viviendas grandes se abarataron en unas comunas y en
# otras no. Estas dos tablas son las que respondieron esas preguntas.

# --- 10.1. Precio por m2 y prima del monoambiente, por comuna ----------------

# Promedio de los últimos 4 trimestres contra el promedio de 2018.
prima_comuna <- alquileres |>
  filter(!is.na(precio_m2_real)) |>
  mutate(tramo = case_when(anio == 2018 ~ "p2018",
                           periodo >= "2025-T3" ~ "hoy",
                           TRUE ~ NA_character_)) |>
  filter(!is.na(tramo)) |>
  group_by(comuna, zona, ambientes, tramo) |>
  summarise(p_m2 = mean(precio_m2_real), .groups = "drop") |>
  pivot_wider(names_from = c(tramo, ambientes), values_from = p_m2) |>
  mutate(
    prima_hoy  = 100 * (`hoy_1 ambiente` / `hoy_3 ambientes` - 1),
    prima_2018 = 100 * (`p2018_1 ambiente` / `p2018_3 ambientes` - 1),
    cambio_pp  = prima_hoy - prima_2018
  ) |>
  select(comuna, zona,
         m2_1amb = `hoy_1 ambiente`, m2_2amb = `hoy_2 ambientes`,
         m2_3amb = `hoy_3 ambientes`, prima_hoy, prima_2018, cambio_pp) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  arrange(desc(prima_hoy))

print(prima_comuna, n = Inf)
write_csv(prima_comuna, "output/tablas/t12_prima_por_comuna.csv")

# Lectura: la prima del monoambiente no se movió igual en todas las comunas.
# En la Comuna 1 creció 14,3 puntos (los departamentos grandes se abarataron
# más que los chicos) y en Palermo se achicó 10,5.


# --- 10.2. ¿Todas las zonas evolucionaron igual? -----------------------------

# Índice base promedio 2018 = 100 por zona y tipo, sobre series completas.
# Criterio: completamos los huecos de hasta 4 trimestres con interpolación
# lineal y nos quedamos solo con las series que así quedan completas. Si
# dejáramos entrar a una comuna a mitad de camino, el promedio de la zona se
# movería sin que hubiera cambiado ningún precio.
# Con este criterio, en el Sur quedan las comunas 4, 7 y 9 en 2 ambientes y
# solo la 7 en 1 y 3 ambientes.
evol_zona <- alquileres |>
  filter(comuna != "8") |>
  group_by(comuna, ambientes) |>
  arrange(anio, trimestre, .by_group = TRUE) |>
  mutate(p_int = zoo::na.approx(precio_m2_real, maxgap = 4, na.rm = FALSE)) |>
  filter(all(!is.na(p_int))) |>
  ungroup() |>
  group_by(zona, ambientes, anio, trimestre, periodo) |>
  summarise(p = mean(p_int), .groups = "drop") |>
  group_by(zona, ambientes) |>
  mutate(indice = 100 * p / mean(p[anio == 2018])) |>
  summarise(
    variacion_pct = round(mean(indice[periodo >= "2025-T3"]) - 100),
    periodo_pico  = periodo[which.max(indice)],
    .groups = "drop"
  ) |>
  pivot_wider(names_from = ambientes, values_from = c(variacion_pct, periodo_pico))

print(evol_zona)
write_csv(evol_zona, "output/tablas/t13_evolucion_por_zona.csv")

# Lectura: las cuatro zonas tocaron el pico casi al mismo tiempo (2023-T3 o
# 2023-T4), así que no hubo rezagos. Lo que cambia es cuánto perdieron: Norte y
# Centro más que Oeste y Sur.
