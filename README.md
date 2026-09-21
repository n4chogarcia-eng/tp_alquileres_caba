# Precio de las viviendas en alquiler en la Ciudad de Buenos Aires

**Trabajo Práctico — Ciencia de Datos para Economía y Negocios (FCE-UBA), 2do cuatrimestre 2026**

## Integrantes

- Ignacio Garcia Carzoglio N°R: 916884
- Joaquín Martínez N°R: 917415
- Tomás Bendersky N°R: 910884

---

## Objetivo

Explorar cómo se distribuye el precio de los departamentos en alquiler entre las
15 comunas de la Ciudad de Buenos Aires y cómo evolucionó entre 2018 y 2026.

La hipótesis de trabajo que guía el análisis es que **existe una jerarquía
espacial estable de precios dentro de la Ciudad**: las comunas del norte se
mantienen sistemáticamente por encima del promedio y las del sur por debajo, y
esa posición relativa cambia poco a lo largo del tiempo, aun en un contexto de
inflación muy alta.

Esta entrega corresponde a la **Instancia 1**: presentación de la base,
descripción de las variables y primer análisis exploratorio (EDA).

---

## Datos

- **Fuente:** Instituto de Estadística y Censos de la Ciudad Autónoma de Buenos
  Aires (Jefatura de Gabinete de Ministros — GCBA), sobre la base de datos de
  Argenprop.
- **Link:** [Banco de datos — Mercado inmobiliario](https://www.estadisticaciudad.gob.ar/eyc/categoria-banco-datos/mercado-inmobiliario/)
- **Archivos utilizados:**
  - `MI_DAP_AX12.xlsx` — departamentos de 1 ambiente usados, por comuna
  - `MI_DAP_AX13.xlsx` — departamentos de 2 ambientes usados, por comuna
  - `MI_DAP_AX14.xlsx` — departamentos de 3 ambientes usados, por comuna
  - `MI_DAP_AX15.xlsx` — serie mensual del total de la Ciudad (se descargó como
    referencia, no se usa en esta instancia)
- **Período:** 1er trimestre de 2018 — 2do trimestre de 2026 (34 trimestres)
- **Unidad de observación:** comuna × tipo de departamento × trimestre
- **Periodicidad de difusión:** trimestral
- **Variable medida:** precio promedio de publicación del alquiler mensual, en
  pesos corrientes, expresado sobre una superficie de referencia fija
  (30 m² para 1 ambiente, 43 m² para 2 ambientes, 70 m² para 3 ambientes).
- **Base complementaria:** IPC del INDEC, nivel general, región GBA
  ([serie_ipc_divisiones.csv](https://www.indec.gob.ar/ftp/cuadros/economia/serie_ipc_divisiones.csv)),
  para expresar los precios en pesos constantes del 2do trimestre de 2026.

> **Importante:** la base mide precios **de publicación** (oferta), no precios
> efectivamente pactados en los contratos firmados.

### Por qué trabajamos por comuna y no por barrio

La fuente publica la misma serie de precios con dos desagregaciones
territoriales: por comuna (archivos `MI_DAP_AX12/13/14`) y por barrio
(`MI_DAP_AX09/10/11`), ambas para el mismo período. Elegimos la desagregación
por comuna por la cantidad de datos faltantes:

| | Por comuna | Por barrio |
|---|---:|---:|
| Unidades territoriales | 15 | 48 |
| Observaciones | 1.530 | 4.896 |
| Valores faltantes | 13% | 62% |
| Unidades sin ningún dato en toda la serie | 0 | 8 |
| Unidades con más de la mitad de la serie vacía | 1 | 32 |

La fuente solo publica el precio de las unidades territoriales que alcanzan una
cantidad mínima de avisos. Al bajar a nivel de barrio, ocho barrios (Agronomía,
La Paternal, Mataderos, Nueva Pompeya, Versalles, Villa Real, Villa Riachuelo y
Villa Soldati) no tienen un solo dato publicado entre 2018 y 2026, y solo unos
16 barrios quedan realmente analizables, casi todos del norte y el centro.

Como esos faltantes no son aleatorios sino que se concentran en el sur, trabajar
por barrio dejaría cualquier comparación entre grupos sesgada de entrada, algo
que condiciona los métodos previstos para la instancia 2 (ANOVA, chi-cuadrado,
clusters). La comuna agrega la oferta de varios barrios y por eso supera el
umbral de publicación con mucha más frecuencia.

---

## Análisis realizado

El procesamiento sigue la organización por etapas del
[repositorio de ejemplo de la cátedra](https://github.com/nsidicarocep/ejemplo_datos_empleo):
cada etapa toma como insumo la salida de la anterior.

**Etapa 1 — Crudo a preprocesado** (`script/01 - crudo a preprocesado/`)

| Script | Entrada | Salida | Qué hace |
|---|---|---|---|
| `00_ipc.R` | `auxiliar/serie_ipc_divisiones.csv` | `auxiliar/ipc_mensual.csv`, `auxiliar/ipc_trimestral.csv` | Filtra el IPC nivel general región GBA, lo lleva a base junio 2026 = 100 y calcula promedios trimestrales. |
| `01_limpieza.R` | `raw/MI_DAP_AX12/13/14.xlsx` | `input/alquileres_comunas.csv`, `input/total_caba.csv` | Pasa los tres cuadros de Excel (formato ancho, encabezados de dos filas) a formato largo, separa el total de la Ciudad y construye `zona` y `precio_relativo`. |

**Etapa 2 — Preprocesado a deflactado** (`script/02 - preprocesado a deflactado/`)

| Script | Salida | Qué hace |
|---|---|---|
| `01_deflactar_alquileres.R` | `input/deflactados/alquileres_comunas.csv`, `input/deflactados/total_caba.csv` | Lleva los precios a pesos constantes del 2do trimestre de 2026 con `deflactar_serie()`: precio real = precio corriente × IPC base ÷ IPC del trimestre. |

**Etapa 3 — Análisis exploratorio** (`script/03 - analisis exploratorio/`)

| Script | Salida | Qué hace |
|---|---|---|
| `01_exploratorio.R` | `output/tablas/`, `output/graficos/` | Dimensiones y unidad de observación, diccionario de variables, valores faltantes, descriptivas (`summary()` y `skim()`) por tipo de departamento y por comuna, tablas de frecuencia, histogramas, boxplots, percentiles y ratios entre percentiles, y correlaciones con sus diagramas de dispersión. |

### Nota metodológica: corrección por inflación

Entre 2018-T1 y 2026-T2 el IPC del GBA se multiplicó por casi 89. Cualquier
estadística calculada sobre toda la serie en pesos corrientes mide inflación, no
mercado inmobiliario: en pesos corrientes la media de la serie es 3,8 veces la
mediana; en pesos constantes, 1,1 veces.

Por eso deflactamos todos los precios a pesos del **2do trimestre de 2026**, el
último de la base. En ese trimestre precio corriente y precio real coinciden,
así que los valores de hoy no cambian.

**Por qué el IPC del INDEC y no el de la Ciudad.** El IPC de la Ciudad (IPCBA,
base 2021 = 100) empieza recién en febrero de 2022, y nuestra serie arranca en
2018: usarlo obligaría a empalmar dos bases distintas del índice. El IPC del
INDEC tiene una serie única desde diciembre de 2016, y su región GBA incluye a
la Ciudad. El nivel nacional (el que usa el ejemplo de la cátedra) está en el
mismo archivo y se puede usar cambiando un filtro en `00_ipc.R`.

**Precio relativo.** Para comparar comunas entre sí usamos además el precio de
cada comuna como porcentaje del promedio de la Ciudad **en el mismo trimestre y
tipo de departamento** (115 = 15% por encima del promedio). Como es un cociente
entre precios del mismo trimestre, da exactamente igual con precios corrientes
o deflactados.

Las funciones `cambiar_base_ipc()` y `deflactar_serie()` están tomadas sin
cambios del repositorio de ejemplo de la cátedra.

---

## Estructura del repositorio

```
tp_alquileres_caba/
├── raw/                          # Archivos .xlsx tal como se descargaron (no se modifican)
├── auxiliar/                     # IPC del INDEC (crudo y procesado)
├── input/                        # Bases preprocesadas, en pesos corrientes (etapa 1)
│   └── deflactados/              #   └─ En pesos constantes de 2026-T2 (etapa 2)
├── output/
│   ├── tablas/                   # Tablas exportadas por los scripts (t01 a t17)
│   └── graficos/                 # Gráficos exportados por los scripts (g01 a g07)
├── script/
│   ├── 01 - crudo a preprocesado/
│   │   ├── 00_ipc.R
│   │   └── 01_limpieza.R
│   ├── 02 - preprocesado a deflactado/
│   │   └── 01_deflactar_alquileres.R
│   └── 03 - analisis exploratorio/
│       └── 01_exploratorio.R
├── utils/
│   ├── leer_precios_comuna.R     # Lectura de los cuadros del Instituto
│   ├── cambiar_indice_ipc.R      # De la cátedra
│   └── deflactar.R               # De la cátedra
└── README.md
```

---

## Reproducción

### Paquetes necesarios

```r
install.packages(c("tidyverse", "readxl", "skimr"))
```

### Orden de ejecución

Parados en la carpeta del proyecto (`tp_alquileres_caba/`):

1. `script/01 - crudo a preprocesado/00_ipc.R`
2. `script/01 - crudo a preprocesado/01_limpieza.R`
3. `script/02 - preprocesado a deflactado/01_deflactar_alquileres.R`
4. `script/03 - analisis exploratorio/01_exploratorio.R`

No hace falta ningún paso manual: los archivos crudos ya están versionados en
`raw/` y `auxiliar/`.

**Para actualizar los datos** cuando se publique un trimestre nuevo: reemplazar
los tres `.xlsx` de `raw/` y el `serie_ipc_divisiones.csv` de `auxiliar/` por
las versiones nuevas, cambiar la fecha base en `00_ipc.R` y en
`01_deflactar_alquileres.R`, y volver a correr los cuatro scripts en orden.

---

## Principales hallazgos de esta instancia

1. **Los datos faltantes no son aleatorios: se concentran en el sur.** El 13% de
   las observaciones no tiene dato publicado, pero el 45% de las observaciones
   de la zona Sur están vacías contra el 0,3% de la zona Norte. La Comuna 8
   (Villa Soldati, Lugano, Riachuelo) no tiene dato en el 97% de los trimestres.
   La fuente aclara que solo publica comunas que alcanzan una cantidad mínima de
   unidades ofertadas: el faltante es, en sí mismo, información sobre el mercado.
2. **Los faltantes se reducen a partir de 2024**, de alrededor del 17% anual
   entre 2020 y 2023 al 4-8% en 2024-2026, lo que sugiere un aumento de la
   oferta publicada.
3. **Hay una jerarquía espacial estable.** Medido en precios relativos, el orden
   de las comunas casi no cambia en ocho años: Palermo (14) promedia 114% del
   valor de la Ciudad y Belgrano (13) 110%, contra 84-85% de las comunas 4 y 9.
   El desvío estándar del precio relativo dentro de cada comuna es de apenas
   unos 5 puntos.
4. **En términos reales, el alquiler cayó entre 16% y 20% desde 2018.** En
   pesos corrientes se multiplicó por 74, pero el IPC lo hizo por 89. Un
   2 ambientes costaba $913.746 en 2018-T1 medido en pesos de 2026-T2, contra
   $764.485 hoy. La serie real tiene un mínimo a fines de 2019, un pico a fines
   de 2023 —cuando se aceleró la inflación— y una caída fuerte en 2024; desde
   entonces está estable.
5. **El metro cuadrado del monoambiente es el más caro.** En 2026-T2 el precio
   por m² del 1 ambiente ($18.855) supera en 12% al del 3 ambientes ($16.776), y
   esa prima se mantuvo entre 8% y 21% durante todo el período.

La correlación entre el precio por m² de los tipos de departamento ilustra por
qué hacía falta deflactar: da 1,00 con precios corrientes, 0,92 con precios
constantes y 0,82 dentro de un solo trimestre.
