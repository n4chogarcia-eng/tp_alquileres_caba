# Función tomada sin cambios del repositorio de ejemplo de la cátedra:
# https://github.com/nsidicarocep/ejemplo_datos_empleo (utils/cambiar_indice_ipc.R)

# Función para cambiar la base de un índice de inflación
cambiar_base_ipc <- function(datos, fecha_base, valor_base = 100, 
                             col_fecha = "mes", col_indice = "ipc_indice") {
  
  # Cargar librerías necesarias
  library(dplyr)
  library(lubridate)
  
  # Convertir fecha_base a formato Date si es string
  if(is.character(fecha_base)) {
    fecha_base <- as.Date(fecha_base)
  }
  
  # Convertir la columna de fecha a Date si no lo está
  datos[[col_fecha]] <- as.Date(datos[[col_fecha]])
  
  # Encontrar el valor del índice en la fecha base
  valor_indice_base <- datos %>%
    filter(!!sym(col_fecha) == fecha_base) %>%
    pull(!!sym(col_indice))
  
  # Verificar que existe la fecha base
  if(length(valor_indice_base) == 0) {
    stop("La fecha base especificada no existe en los datos")
  }
  
  # Calcular el nuevo índice
  datos_nuevos <- datos %>%
    mutate(
      indice_original = !!sym(col_indice),
      indice_rebaseado = (!!sym(col_indice) / valor_indice_base) * valor_base,
      fecha_base_usada = fecha_base,
      valor_base_original = valor_indice_base
    )
  
  # Mensaje informativo
  cat("Índice rebaseado exitosamente.\n")
  cat("Fecha base:", as.character(fecha_base), "\n")
  cat("Valor original en fecha base:", round(valor_indice_base, 4), "\n")
  cat("Nuevo valor base:", valor_base, "\n\n")
  
  return(datos_nuevos)
}
