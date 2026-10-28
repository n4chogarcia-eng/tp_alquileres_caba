# =============================================================================
# 01 - DEFLACTAR: de pesos corrientes a pesos constantes
#
# Trabajo Práctico - Ciencia de Datos para Economía y Negocios (FCE-UBA)
#
# Objetivo del script:
#   llevar los precios de alquiler de pesos corrientes a pesos constantes del
#   2do trimestre de 2026 (el último de la base), usando el IPC trimestral que
#   arma script/01 - crudo a preprocesado/00_ipc.R. Sigue la lógica de los
#   scripts de la etapa 02 del repositorio de ejemplo de la cátedra.
#
# Entradas: input/alquileres_comunas.csv, input/total_caba.csv,
#           auxiliar/ipc_trimestral.csv
# Salidas:  input/deflactados/alquileres_comunas.csv,
#           input/deflactados/total_caba.csv
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
instub    <- 'input'
outstub   <- 'input/deflactados'
auxiliar  <- 'auxiliar'
funciones <- 'utils'

# Fecha base: primer día del trimestre al que llevamos todos los precios.
# 2026-04-01 = 2do trimestre de 2026. En ese trimestre precio corriente y
# precio constante coinciden, así que los valores de hoy no cambian.
fecha_base <- '2026-04-01'

# Cargar IPC
ipc_trimestral <- read_csv(file.path(auxiliar, 'ipc_trimestral.csv'),
                           show_col_types = FALSE)
ipc_trimestral <- ipc_trimestral %>%
  select(fecha_trimestre = mes, ipc_promedio)

# Cargar funciones
files_fun <- list.files(file.path(funciones), pattern = '*.R', full.names = T)
for (i in 1:length(files_fun)) { source(files_fun[i]) }

# Cargar datos
alquileres <- read_csv(file.path(instub, 'alquileres_comunas.csv'),
                       show_col_types = FALSE)
total_caba <- read_csv(file.path(instub, 'total_caba.csv'),
                       show_col_types = FALSE)


# --- 1. Fecha del trimestre -----------------------------------------------------

# La función deflactar_serie() une los precios con el IPC por fecha, así que
# necesitamos una columna de tipo fecha: el primer día de cada trimestre.
# Trimestre 1 -> enero, 2 -> abril, 3 -> julio, 4 -> octubre.
alquileres <- alquileres %>%
  mutate(fecha_trimestre = make_date(anio, (trimestre - 1) * 3 + 1, 1))
total_caba <- total_caba %>%
  mutate(fecha_trimestre = make_date(anio, (trimestre - 1) * 3 + 1, 1))

# Control: todos los trimestres de la base tienen que tener IPC
setdiff(unique(alquileres$fecha_trimestre), ipc_trimestral$fecha_trimestre)


# --- 2. Deflactar los precios por comuna ------------------------------------------

# Fórmula (dentro de la función):
#   precio constante = precio corriente x (IPC del trimestre base / IPC del trimestre)
alquileres <- deflactar_serie(datos_corrientes = alquileres,
                              datos_ipc = ipc_trimestral,
                              fecha_base = fecha_base,
                              col_fecha_corrientes = 'fecha_trimestre',
                              col_monto_corrientes = 'precio',
                              col_fecha_ipc = 'fecha_trimestre',
                              col_ipc = 'ipc_promedio')

# El precio por m2 se recalcula a partir del precio deflactado. El precio
# relativo no se toca: es un cociente entre precios del mismo trimestre, así
# que la inflación ya estaba cancelada.
alquileres <- alquileres %>%
  mutate(precio_m2_real = monto_constante / superficie_m2) %>%
  select(comuna, zona, ambientes, superficie_m2, anio, trimestre, periodo,
         precio_corriente = monto_corriente,
         precio_real = monto_constante,
         precio_m2_real,
         precio_relativo)


# --- 3. Deflactar el total de la Ciudad -------------------------------------------

total_caba <- deflactar_serie(datos_corrientes = total_caba,
                              datos_ipc = ipc_trimestral,
                              fecha_base = fecha_base,
                              col_fecha_corrientes = 'fecha_trimestre',
                              col_monto_corrientes = 'precio_caba',
                              col_fecha_ipc = 'fecha_trimestre',
                              col_ipc = 'ipc_promedio')

total_caba <- total_caba %>%
  mutate(precio_m2_caba_real = monto_constante / superficie_m2) %>%
  select(ambientes, superficie_m2, anio, trimestre, periodo,
         precio_caba_corriente = monto_corriente,
         precio_caba_real = monto_constante,
         precio_m2_caba_real)


# --- 4. Controles ------------------------------------------------------------------

# En el trimestre base, precio corriente y precio real tienen que coincidir
alquileres %>%
  filter(periodo == '2026-T2') %>%
  summarise(max_diferencia = max(abs(precio_real - precio_corriente), na.rm = TRUE))

# Los faltantes no cambian: deflactar un NA da NA
sum(is.na(alquileres$precio_corriente)) == sum(is.na(alquileres$precio_real))

# Cuánto cambió el precio real de un 2 ambientes entre el primer y el último
# trimestre de la serie
total_caba %>%
  filter(periodo %in% c('2018-T1', '2026-T2')) %>%
  select(ambientes, periodo, precio_caba_corriente, precio_caba_real)


# --- 5. Guardar datos ----------------------------------------------------------------

write_csv(alquileres, file.path(outstub, 'alquileres_comunas.csv'))
write_csv(total_caba, file.path(outstub, 'total_caba.csv'))
