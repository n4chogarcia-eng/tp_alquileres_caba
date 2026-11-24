# Función tomada sin cambios del repositorio de ejemplo de la cátedra:
# https://github.com/nsidicarocep/ejemplo_datos_empleo (utils/deflactar.R)

# Función para convertir series a precios constantes
deflactar_serie <- function(datos_corrientes, datos_ipc, fecha_base, 
                            col_fecha_corrientes = "fecha_trimestre",
                            col_monto_corrientes = "monto_auh",
                            col_fecha_ipc = "fecha_trimestre", 
                            col_ipc = "ipc_promedio") {
  
  # Cargar librerías necesarias
  library(dplyr)
  library(lubridate)
  
  # Convertir fecha_base a Date si es string
  if(is.character(fecha_base)) {
    fecha_base <- as.Date(fecha_base)
  }
  
  # Asegurar que las fechas están en formato Date
  datos_corrientes[[col_fecha_corrientes]] <- as.Date(datos_corrientes[[col_fecha_corrientes]])
  datos_ipc[[col_fecha_ipc]] <- as.Date(datos_ipc[[col_fecha_ipc]])
  
  # Encontrar el IPC del período base
  ipc_base <- datos_ipc %>%
    filter(!!sym(col_fecha_ipc) == fecha_base) %>%
    pull(!!sym(col_ipc))
  
  # Verificar que existe el período base
  if(length(ipc_base) == 0) {
    stop("No se encontró el IPC para la fecha base especificada: ", fecha_base)
  }
  
  # Combinar las dos bases de datos
  datos_combinados <- datos_corrientes %>%
    left_join(datos_ipc, by = setNames(col_fecha_ipc, col_fecha_corrientes))
  
  # Verificar que todas las fechas tienen IPC
  fechas_sin_ipc <- datos_combinados %>%
    filter(is.na(!!sym(col_ipc))) %>%
    pull(!!sym(col_fecha_corrientes))
  
  if(length(fechas_sin_ipc) > 0) {
    warning("Las siguientes fechas no tienen IPC disponible: ", 
            paste(fechas_sin_ipc, collapse = ", "))
  }
  
  # Calcular valores a precios constantes
  resultado <- datos_combinados %>%
    mutate(
      # Mantener valores originales
      monto_corriente = !!sym(col_monto_corrientes),
      ipc_periodo = !!sym(col_ipc),
      
      # Calcular valores constantes
      # Fórmula: Valor_constante = Valor_corriente * (IPC_base / IPC_periodo)
      monto_constante = ifelse(
        !is.na(ipc_periodo) & ipc_periodo > 0,
        monto_corriente * (ipc_base / ipc_periodo),
        NA
      ),
      
      # Información adicional
      fecha_base_usada = fecha_base,
      ipc_base = ipc_base,
      factor_deflactor = ifelse(!is.na(ipc_periodo), ipc_base / ipc_periodo, NA)
    )
  
  # Mensaje informativo
  cat("Serie deflactada exitosamente.\n")
  cat("Fecha base para precios constantes:", as.character(fecha_base), "\n")
  cat("IPC del período base:", round(ipc_base, 4), "\n")
  cat("Períodos procesados:", nrow(resultado), "\n")
  if(length(fechas_sin_ipc) > 0) {
    cat("Períodos sin IPC:", length(fechas_sin_ipc), "\n")
  }
  cat("\n")
  
  return(resultado)
}
