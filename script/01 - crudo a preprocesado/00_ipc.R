# =============================================================================
# 00 - IPC: tabla auxiliar de inflación mensual y trimestral
#
# Trabajo Práctico - Ciencia de Datos para Economía y Negocios (FCE-UBA)
#
# Objetivo del script:
#   armar el índice de precios que usamos para deflactar los alquileres.
#   Sigue la misma lógica que el script 00_ipc.R del repositorio de ejemplo de
#   la cátedra (ejemplo_datos_empleo): cambiar la base del índice y pasarlo de
#   mensual a trimestral con el promedio de los tres meses.
#
# Entrada: auxiliar/serie_ipc_divisiones.csv (INDEC)
# Salida:  auxiliar/ipc_mensual.csv y auxiliar/ipc_trimestral.csv
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

# Librerías
library(tidyverse)

# Rutas
instub <- 'auxiliar'

# Funciones
source(file.path('utils', 'cambiar_indice_ipc.R'))


# --- 1. Qué IPC usamos y por qué ---------------------------------------------

# Usamos el IPC del INDEC, nivel general, región GBA (Ciudad de Buenos Aires
# y los 24 partidos del conurbano). Fuente:
# https://www.indec.gob.ar/ftp/cuadros/economia/serie_ipc_divisiones.csv
#
# Por qué este y no otro:
# - El IPC de la Ciudad (IPCBA, base 2021 = 100) arranca recién en febrero de
#   2022, y nuestra serie de alquileres empieza en 2018. Usarlo obligaría a
#   empalmar dos bases distintas del índice.
# - El IPC del INDEC tiene una serie única desde diciembre de 2016, así que
#   cubre todo nuestro período sin empalmes.
# - Entre las regiones del INDEC, GBA es la que corresponde a la Ciudad. El
#   nivel nacional (que es el que usa el ejemplo de la cátedra) también está en
#   el mismo archivo: alcanza con cambiar "GBA" por "Nacional" en el filtro.

# Cargar tabla. El archivo viene separado por punto y coma y con coma decimal.
data <- read_delim(file.path(instub, 'serie_ipc_divisiones.csv'),
                   delim = ';',
                   locale = locale(decimal_mark = ',', grouping_mark = '.'),
                   show_col_types = FALSE)

# Nos quedamos con el nivel general (código 0) de la región GBA
data <- data %>%
  filter(Codigo == '0', Region == 'GBA') %>%
  mutate(mes = ymd(paste0(Periodo, '01'))) %>%
  select(mes, ipc_indice = Indice_IPC)

# Control: el índice tiene que crecer todos los meses (no hubo deflación en el
# período) y no tiene que haber meses faltantes.
all(diff(data$ipc_indice) > 0)
nrow(data) == length(seq(min(data$mes), max(data$mes), by = 'month'))


# --- 2. Cambio de base ---------------------------------------------------------

# El INDEC publica el índice con base diciembre 2016 = 100. Lo llevamos a base
# junio 2026 = 100, el último mes del último trimestre de nuestra base de
# alquileres. El cambio de base no altera ninguna variación: solo hace que el
# índice se lea contra el presente.
data <- cambiar_base_ipc(data, '2026-06-01')
data <- data %>%
  select(mes, indice_ipc = indice_rebaseado)


# --- 3. Pasar a trimestral -----------------------------------------------------

# Los alquileres son trimestrales, así que el índice también tiene que serlo.
# Igual que en el ejemplo de la cátedra, usamos el promedio de los tres meses.
ipc_trimestral_promedio <- data %>%
  mutate(
    año = year(mes),
    trimestre = quarter(mes),
    periodo_trim = paste0(año, "-Q", trimestre)
  ) %>%
  group_by(año, trimestre, periodo_trim) %>%
  summarise(
    fecha_trimestre = min(mes),  # Primer mes del trimestre como referencia
    ipc_promedio = mean(indice_ipc, na.rm = TRUE),
    n_meses = n(),               # Para verificar que hay 3 meses por trimestre
    meses_incluidos = paste(format(mes, "%m"), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(fecha_trimestre)

# El último trimestre puede estar incompleto (por ejemplo, si el INDEC ya
# publicó julio y agosto pero no septiembre). Un promedio de dos meses no es
# comparable con uno de tres, así que nos quedamos solo con trimestres completos.
ipc_trimestral_promedio %>% filter(n_meses < 3)

ipc_trimestral_promedio <- ipc_trimestral_promedio %>%
  filter(n_meses == 3)

# Seleccionar columnas
ipc_trimestral_promedio <- ipc_trimestral_promedio %>%
  select(mes = fecha_trimestre, periodo_trim, ipc_promedio)


# --- 4. Guardar resultados -----------------------------------------------------

write_csv(data, file.path(instub, 'ipc_mensual.csv'))
write_csv(ipc_trimestral_promedio, file.path(instub, 'ipc_trimestral.csv'))
