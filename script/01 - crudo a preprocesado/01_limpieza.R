# =============================================================================
# 01 - LIMPIEZA: de los archivos crudos a la base lista para el análisis
#
# Trabajo Práctico - Ciencia de Datos para Economía y Negocios (FCE-UBA)
# Instancia 1: Análisis Exploratorio de Datos
# Base: Precio de departamentos en alquiler, Ciudad de Buenos Aires
#
# Objetivo del script:
#   leer los tres archivos .xlsx descargados del Instituto de Estadística y
#   Censos de la Ciudad (carpeta raw/), pasarlos de formato ancho a formato
#   largo (tidy) y guardar una única base en input/, todavía en pesos
#   corrientes. La corrección por inflación se hace en la etapa siguiente
#   (script/02 - preprocesado a deflactado).
#
# Los archivos originales NO se modifican nunca: raw/ queda como está.
# =============================================================================


# =============================================================================
# 0. PAQUETES Y RUTAS
# =============================================================================

# install.packages(c("tidyverse", "readxl"))

library(tidyverse)
library(readxl)

# El script se corre parado en la carpeta del proyecto (tp_alquileres_caba/).
# En RStudio: Session > Set Working Directory > To Project Directory.


# =============================================================================
# 1. QUÉ TIENEN LOS ARCHIVOS CRUDOS
# =============================================================================

# Los tres archivos de precios por comuna tienen exactamente la misma forma:
#
#   fila 1        : título del cuadro
#   fila 2        : año (aparece una sola vez cada 4 columnas, celdas combinadas)
#   fila 3        : trimestre
#   fila 4        : "Total" (el promedio de toda la Ciudad)
#   filas 5 a 19  : las 15 comunas
#   filas 20 en adelante : notas al pie y fuente
#
#   "///" = dato que no corresponde presentar. Es el código de dato faltante:
#   la fuente aclara que "solo se muestran valores para las comunas que cumplen
#   con la cantidad mínima de unidades ofertadas requerida en cada caso".
#
# La superficie de referencia cambia según el tipo de departamento, y está
# aclarada en la nota al pie de cada archivo. La necesitamos para poder
# comparar los tres tipos entre sí (precio por m2).

archivos <- tribble(
  ~archivo,               ~ambientes,    ~superficie_m2,
  "MI_DAP_AX12.xlsx",     "1 ambiente",   30,
  "MI_DAP_AX13.xlsx",     "2 ambientes",  43,
  "MI_DAP_AX14.xlsx",     "3 ambientes",  70
)

archivos


# =============================================================================
# 2. FUNCIÓN DE LECTURA
# =============================================================================

# Como hay que hacer lo mismo tres veces, armamos una función. Está guardada
# en utils/, tal como pide la consigna del trabajo, y acá solo la cargamos.

source("utils/leer_precios_comuna.R")

# Probamos la función con un archivo antes de aplicarla a los tres.
prueba <- leer_precios_comuna("raw/MI_DAP_AX12.xlsx")
head(prueba)
dim(prueba)


# =============================================================================
# 3. LECTURA DE LOS TRES ARCHIVOS Y ARMADO DE LA BASE LARGA
# =============================================================================

alquileres_crudo <- archivos |>
  mutate(datos = map(file.path("raw", archivo), leer_precios_comuna)) |>
  unnest(datos)

glimpse(alquileres_crudo)


# --- 3.1. Separamos el total de la Ciudad de las comunas ---------------------

# El "Total" NO es una comuna: es el promedio de toda la Ciudad. Si lo
# dejáramos mezclado estaríamos sumando unidades de observación distintas,
# que es uno de los errores más comunes al armar una base.

total_caba <- alquileres_crudo |>
  filter(comuna_txt == "Total") |>
  select(ambientes, superficie_m2, anio, trimestre, periodo, precio) |>
  rename(precio_caba = precio) |>
  mutate(precio_m2_caba = precio_caba / superficie_m2)

alquileres <- alquileres_crudo |>
  filter(comuna_txt != "Total")


# --- 3.2. Variables derivadas y tipos ----------------------------------------

alquileres <- alquileres |>
  mutate(
    # La comuna es un identificador, no una cantidad: va como factor ordenado
    # de 1 a 15. Si la dejáramos numérica, R calcularía "comuna promedio",
    # que no significa nada.
    comuna = factor(as.numeric(comuna_txt), levels = 1:15),

    # Zona de la Ciudad. La usamos para agrupar comunas y comparar.
    # Agrupamiento geográfico usual: Norte (corredor del río y Palermo),
    # Centro, Oeste y Sur.
    zona = case_when(
      comuna_txt %in% c("2", "13", "14")            ~ "Norte",
      comuna_txt %in% c("1", "3", "5", "6", "15")   ~ "Centro",
      comuna_txt %in% c("10", "11", "12")           ~ "Oeste",
      comuna_txt %in% c("4", "7", "8", "9")         ~ "Sur"
    ),
    zona = factor(zona, levels = c("Norte", "Centro", "Oeste", "Sur")),

    ambientes = factor(ambientes,
                       levels = c("1 ambiente", "2 ambientes", "3 ambientes")),

    # Precio por metro cuadrado: es lo que hace comparables a los tres tipos
    # de departamento entre sí. Un 3 ambientes siempre va a ser más caro que
    # un monoambiente; la pregunta interesante es si es más caro POR METRO.
    precio_m2 = precio / superficie_m2
  ) |>
  select(comuna, zona, ambientes, superficie_m2, anio, trimestre, periodo,
         precio, precio_m2)


# --- 3.3. Precio relativo a la Ciudad ----------------------------------------

# Todos los precios están en pesos corrientes de cada trimestre. Entre 2018 y
# 2026 la inflación acumulada es enorme, así que comparar el precio de 2018
# con el de 2026 no dice nada sobre el mercado inmobiliario.
#
# Para comparar niveles entre años, en la etapa siguiente deflactamos con el
# IPC. Pero para comparar COMUNAS entre sí hay una forma más directa: mirar el
# precio de cada comuna COMO PORCENTAJE del precio promedio de la Ciudad en ese
# mismo trimestre y para ese mismo tipo de departamento. Si una comuna vale
# 120, está 20% arriba del promedio de la Ciudad.
#
# Como es un cociente entre dos precios del mismo trimestre, la inflación se
# cancela sola: el precio relativo da exactamente igual con precios corrientes
# que con precios deflactados.

alquileres <- alquileres |>
  left_join(total_caba |> select(ambientes, anio, trimestre, precio_caba),
            by = c("ambientes", "anio", "trimestre")) |>
  mutate(precio_relativo = 100 * precio / precio_caba) |>
  select(-precio_caba)


# =============================================================================
# 4. CONTROLES ANTES DE GUARDAR
# =============================================================================

# --- 4.1. ¿La unidad de observación es única? --------------------------------

# Cada fila debería ser una combinación única de comuna, tipo de departamento
# y trimestre. Si hay duplicados, algo se leyó mal.
alquileres |>
  count(comuna, ambientes, periodo) |>
  filter(n > 1)

nrow(alquileres)
n_distinct(alquileres$comuna, alquileres$ambientes, alquileres$periodo)


# --- 4.2. ¿Están todas las combinaciones? ------------------------------------

# 15 comunas x 3 tipos x 34 trimestres = 1530 filas esperadas
15 * 3 * n_distinct(alquileres$periodo)

table(alquileres$ambientes)
table(alquileres$comuna)


# --- 4.3. ¿Los valores tienen sentido? ---------------------------------------

summary(alquileres$precio)
summary(alquileres$precio_relativo)

# Ningún precio puede ser negativo ni cero
sum(alquileres$precio <= 0, na.rm = TRUE)


# =============================================================================
# 5. GUARDADO EN input/
# =============================================================================

write_csv(alquileres, "input/alquileres_comunas.csv")
write_csv(total_caba, "input/total_caba.csv")

cat("Base guardada en input/alquileres_comunas.csv\n")
cat("Filas:", nrow(alquileres), "- Columnas:", ncol(alquileres), "\n")
