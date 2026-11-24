# =============================================================================
# FUNCIÓN: leer_precios_comuna()
#
# Lee uno de los cuadros de precios de alquiler por comuna publicados por el
# Instituto de Estadística y Censos de la Ciudad (archivos MI_DAP_AX12/13/14)
# y lo devuelve en formato largo (una fila por comuna y trimestre).
#
# Argumento:
#   ruta : ruta al archivo .xlsx dentro de raw/
#
# Devuelve un data frame con las columnas:
#   comuna_txt : "Total" o el número de comuna, como texto
#   anio       : año (numérico)
#   trimestre  : número de trimestre, 1 a 4
#   periodo    : etiqueta del tipo "2018-T1"
#   precio     : precio promedio de publicación, en pesos corrientes
# =============================================================================

leer_precios_comuna <- function(ruta) {

  # Leemos sin nombres de columna: las dos primeras filas del Excel son
  # encabezados con celdas combinadas, así que los armamos a mano.
  crudo <- readxl::read_excel(ruta, sheet = 1, col_names = FALSE,
                              .name_repair = "minimal")

  # Fila 2: los años. Solo aparecen en la primera columna de cada bloque de
  # cuatro trimestres (celdas combinadas en el Excel), así que rellenamos
  # hacia la derecha con tidyr::fill() sobre el vector dado vuelta.
  anios <- as.character(unlist(crudo[2, ]))
  anios[1] <- NA                      # la primera celda dice "Comuna"
  anios <- tibble::tibble(a = anios) |>
    tidyr::fill(a, .direction = "down") |>
    dplyr::pull(a)

  # Fila 3: los trimestres. El asterisco marca dato provisorio; lo sacamos
  # para quedarnos solo con el número.
  trimestres <- as.character(unlist(crudo[3, ]))
  trimestres <- readr::parse_number(trimestres)

  # Filas 4 a 19: el Total de la Ciudad y las 15 comunas.
  cuerpo <- crudo[4:19, ]

  nombres <- c("comuna_txt", paste0("col_", seq_len(ncol(cuerpo) - 1)))
  names(cuerpo) <- nombres

  # Pasamos de ancho a largo y le pegamos año y trimestre a cada columna.
  equivalencias <- tibble::tibble(
    name      = nombres[-1],
    anio      = as.numeric(anios[-1]),
    trimestre = trimestres[-1]
  )

  cuerpo |>
    tidyr::pivot_longer(cols = -comuna_txt) |>
    dplyr::left_join(equivalencias, by = "name") |>
    dplyr::mutate(
      comuna_txt = trimws(comuna_txt),
      # "///" es el código de dato faltante de la fuente. as.numeric() lo
      # convierte en NA, que es exactamente lo que queremos. Usamos
      # suppressWarnings() porque R avisa de la conversión.
      precio  = suppressWarnings(as.numeric(value)),
      periodo = paste0(anio, "-T", trimestre)
    ) |>
    dplyr::select(comuna_txt, anio, trimestre, periodo, precio)
}
