# =============================================================================
# INSTANCIA 2 - Métodos estadísticos y cuantitativos
#
# Trabajo Práctico - Ciencia de Datos para Economía y Negocios (FCE-UBA)
# Grupo 4
#
# Hipótesis 1: los faltantes como termómetro de la oferta y la Ley de Alquileres
#   - inferencia: chi-cuadrado de independencia (clase 10) + Wilcoxon apareado
#   - cuantitativo: índice base 100 e índice de Theil (clases 11 y 12)
#
# Hipótesis 2: las comunas caras amplifican el ciclo
#   - inferencia: ANOVA de la beta por zona (clase 10) + correlación de Spearman
#   - cuantitativo: beta tipo CAPM, volatilidad y amplitud del ciclo
#
# Entrada: input/deflactados/alquileres_comunas.csv
# Salida:  output/instancia2/*.csv  (todos los números de la presentación)
# =============================================================================

# Carpeta de trabajo: la del proyecto. Si se corre desde RStudio, R se ubica
# solo tomando la carpeta de este script y subiendo dos niveles.
if (rstudioapi::isAvailable()) {
  setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", ".."))
}
getwd()

# install.packages(c("tidyverse", "zoo", "car"))
library(tidyverse)
library(zoo)

outstub <- "output/instancia2"

alq <- read_csv("input/deflactados/alquileres_comunas.csv", show_col_types = FALSE) |>
  mutate(
    comuna    = factor(comuna, levels = 1:15),
    zona      = factor(zona, levels = c("Norte", "Centro", "Oeste", "Sur")),
    ambientes = factor(ambientes,
                       levels = c("1 ambiente", "2 ambientes", "3 ambientes")),
    t         = (anio - 2018) * 4 + trimestre,     # 1 a 34
    serie     = paste0("C", comuna, " - ", ambientes)
  ) |>
  arrange(serie, t)


# =============================================================================
# 1. PERÍODOS REGULATORIOS
# =============================================================================

# La Ley 27.551 rigió desde mediados de 2020 y fue derogada por el DNU 70/2023
# a fines de diciembre de 2023. Asignamos el régimen al trimestre completo.
alq <- alq |>
  mutate(periodo_reg = case_when(
    t <= 10 ~ "Antes de la ley",          # 2018-T1 a 2020-T2
    t <= 24 ~ "Ley de Alquileres",        # 2020-T3 a 2023-T4
    TRUE    ~ "Después de la derogación"  # 2024-T1 a 2026-T2
  ),
  periodo_reg = factor(periodo_reg,
                       levels = c("Antes de la ley", "Ley de Alquileres",
                                  "Después de la derogación")))

alq |> count(periodo_reg, trimestres = n_distinct(periodo))
alq |> distinct(periodo_reg, periodo) |> count(periodo_reg, name = "trimestres")


# =============================================================================
# 2. VALORES FALTANTES
# =============================================================================

cat("\n--- Faltantes ---\n")
sum(is.na(alq$precio_real)); round(100 * mean(is.na(alq$precio_real)), 1)

faltantes_corte <- bind_rows(
  alq |> summarise(corte = "Total", grupo = "Total",
                   pct = 100 * mean(is.na(precio_real)), n = n()),
  alq |> group_by(grupo = as.character(zona)) |>
    summarise(corte = "Zona", pct = 100 * mean(is.na(precio_real)), n = n(), .groups = "drop"),
  alq |> group_by(grupo = as.character(ambientes)) |>
    summarise(corte = "Tipo", pct = 100 * mean(is.na(precio_real)), n = n(), .groups = "drop"),
  alq |> group_by(grupo = as.character(periodo_reg)) |>
    summarise(corte = "Período", pct = 100 * mean(is.na(precio_real)), n = n(), .groups = "drop")
) |>
  select(corte, grupo, n, pct) |> mutate(pct = round(pct, 1))

print(faltantes_corte, n = Inf)
write_csv(faltantes_corte, file.path(outstub, "t01_faltantes_por_corte.csv"))

# Racha más larga sin dato, por serie
rachas <- alq |>
  group_by(serie) |>
  summarise(
    comuna = first(comuna), zona = first(zona), ambientes = first(ambientes),
    n_na = sum(is.na(precio_real)),
    racha_max = {
      r <- rle(is.na(precio_real))
      if (any(r$values)) max(r$lengths[r$values]) else 0L
    },
    .groups = "drop"
  ) |>
  arrange(desc(n_na))

print(head(rachas, 10))
write_csv(rachas, file.path(outstub, "t02_faltantes_por_serie.csv"))


# --- 2.1. Panel balanceado por interpolación ---------------------------------

# Para la hipótesis 2 hacen falta series completas (los retornos se calculan
# entre trimestres consecutivos). Interpolamos linealmente los huecos internos
# de hasta 4 trimestres. No extrapolamos: las series que empiezan o terminan
# con faltantes quedan afuera.
MAX_HUECO <- 4

alq <- alq |>
  group_by(serie) |>
  mutate(precio_m2_interp = na.approx(precio_m2_real, x = t, maxgap = MAX_HUECO,
                                      na.rm = FALSE)) |>
  ungroup()

panel <- alq |>
  group_by(serie) |>
  filter(all(!is.na(precio_m2_interp))) |>
  ungroup()

n_series_total <- n_distinct(alq$serie)
n_series_panel <- n_distinct(panel$serie)
cat("\nSeries totales:", n_series_total, "| en el panel balanceado:", n_series_panel, "\n")

cobertura_panel <- panel |>
  distinct(serie, zona, ambientes) |>
  count(zona, ambientes) |>
  pivot_wider(names_from = ambientes, values_from = n, values_fill = 0)
print(cobertura_panel)
write_csv(cobertura_panel, file.path(outstub, "t03_cobertura_panel.csv"))

# Cuántos valores se interpolaron y dónde
interpolados <- panel |>
  filter(is.na(precio_m2_real)) |>
  count(anio, name = "valores_interpolados")
print(interpolados)
cat("Total interpolado:", sum(interpolados$valores_interpolados), "de", nrow(panel), "\n")
write_csv(interpolados, file.path(outstub, "t04_interpolados_por_anio.csv"))


# =============================================================================
# 3. OUTLIERS (regla del rango intercuartílico)
# =============================================================================

regla_iqr <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  c(inf = unname(q[1] - 1.5 * iqr), sup = unname(q[2] + 1.5 * iqr))
}

# (a) sobre el precio relativo (no depende de la inflación)
lim_rel <- regla_iqr(alq$precio_relativo)
out_rel <- alq |>
  filter(precio_relativo < lim_rel["inf"] | precio_relativo > lim_rel["sup"]) |>
  select(comuna, zona, ambientes, periodo, precio_relativo)

# (b) sobre el log del precio real por m2, dentro de cada tipo
out_log <- alq |>
  filter(!is.na(precio_m2_real)) |>
  group_by(ambientes) |>
  mutate(lg = log(precio_m2_real),
         inf = regla_iqr(lg)["inf"], sup = regla_iqr(lg)["sup"]) |>
  ungroup() |>
  filter(lg < inf | lg > sup) |>
  select(comuna, zona, ambientes, periodo, precio_m2_real)

outliers_resumen <- tibble(
  criterio = c("Precio relativo (Ciudad = 100)",
               "log del precio real por m², por tipo"),
  casos    = c(nrow(out_rel), nrow(out_log)),
  en_2023  = c(sum(substr(out_rel$periodo, 1, 4) == "2023"),
               sum(substr(out_log$periodo, 1, 4) == "2023"))
)
print(outliers_resumen)
write_csv(outliers_resumen, file.path(outstub, "t05_outliers_resumen.csv"))
write_csv(out_rel, file.path(outstub, "t06_outliers_precio_relativo.csv"))


# =============================================================================
# 4. HIPÓTESIS 1 - INFERENCIA: CHI-CUADRADO DE INDEPENDENCIA
# =============================================================================

# H0: que una comuna-tipo tenga dato publicado es independiente del período
#     regulatorio.
# H1: la proporción de faltantes difiere entre períodos.

alq <- alq |> mutate(publicado = ifelse(is.na(precio_real), "Sin dato", "Con dato"))

tabla_chi <- table(alq$periodo_reg, alq$publicado)
print(addmargins(tabla_chi))
round(100 * prop.table(tabla_chi, 1), 1)

# Supuesto: frecuencias esperadas grandes
esperados <- chisq.test(tabla_chi)$expected
cat("\nMínimo esperado:", round(min(esperados), 1), "\n")

prueba_chi <- chisq.test(tabla_chi)
print(prueba_chi)

# V de Cramér (función de la clase 10)
v_cramer <- function(prueba) {
  tabla <- prueba$observed
  sqrt(unname(prueba$statistic) / (sum(tabla) * (min(dim(tabla)) - 1)))
}
cat("V de Cramér:", round(v_cramer(prueba_chi), 3), "\n")

# Residuos estandarizados: dónde está la asociación
res_chi <- as.data.frame.matrix(round(prueba_chi$stdres, 2)) |>
  rownames_to_column("periodo_reg")
print(res_chi)

tabla_chi_exp <- as.data.frame.matrix(tabla_chi) |>
  rownames_to_column("periodo_reg") |>
  mutate(pct_sin_dato = round(100 * `Sin dato` / (`Con dato` + `Sin dato`), 1),
         residuo_sin_dato = round(prueba_chi$stdres[, "Sin dato"], 2))
print(tabla_chi_exp)
write_csv(tabla_chi_exp, file.path(outstub, "t07_chi_periodo.csv"))


# --- 4.1. El mismo test dentro de cada zona ----------------------------------

chi_por_zona <- alq |>
  group_by(zona) |>
  group_modify(~ {
    tb <- table(.x$periodo_reg, .x$publicado)
    if (ncol(tb) < 2) return(tibble(pct_antes = NA))
    simular <- min(chisq.test(tb)$expected) < 5
    pr <- suppressWarnings(chisq.test(tb, simulate.p.value = simular, B = 5000))
    pct <- 100 * prop.table(tb, 1)[, "Sin dato"]
    tibble(
      antes = pct[1], ley = pct[2], despues = pct[3],
      p_valor = pr$p.value, v = v_cramer(pr), monte_carlo = simular
    )
  }) |>
  ungroup() |>
  mutate(across(c(antes, ley, despues), ~ round(.x, 1)),
         p_valor = round(p_valor, 4), v = round(v, 3))

print(chi_por_zona)
write_csv(chi_por_zona, file.path(outstub, "t08_chi_por_zona.csv"))


# --- 4.2. Robustez: test apareado por serie (Wilcoxon) -----------------------

# El supuesto débil del chi-cuadrado es la independencia: los 34 trimestres de
# una misma comuna-tipo no son independientes entre sí. Comparamos cada serie
# consigo misma, que es un diseño apareado.

prop_serie <- alq |>
  group_by(serie, periodo_reg) |>
  summarise(pct_na = mean(is.na(precio_real)), .groups = "drop") |>
  pivot_wider(names_from = periodo_reg, values_from = pct_na) |>
  rename(antes = `Antes de la ley`, ley = `Ley de Alquileres`,
         despues = `Después de la derogación`)

con_faltantes <- prop_serie |> filter(antes + ley + despues > 0)
cat("\nSeries con algún faltante:", nrow(con_faltantes), "de", nrow(prop_serie), "\n")

w_ley_desp <- wilcox.test(con_faltantes$ley, con_faltantes$despues, paired = TRUE)
w_ley_antes <- wilcox.test(con_faltantes$ley, con_faltantes$antes, paired = TRUE)
print(w_ley_desp); print(w_ley_antes)

wilcoxon_resumen <- tibble(
  comparacion = c("Ley vs. después de la derogación", "Ley vs. antes de la ley"),
  series_mas_faltantes_en_ley = c(sum(con_faltantes$ley > con_faltantes$despues),
                                  sum(con_faltantes$ley > con_faltantes$antes)),
  series_menos = c(sum(con_faltantes$ley < con_faltantes$despues),
                   sum(con_faltantes$ley < con_faltantes$antes)),
  V = c(unname(w_ley_desp$statistic), unname(w_ley_antes$statistic)),
  p_valor = round(c(w_ley_desp$p.value, w_ley_antes$p.value), 4)
)
print(wilcoxon_resumen)
write_csv(wilcoxon_resumen, file.path(outstub, "t09_wilcoxon_apareado.csv"))


# =============================================================================
# 5. HIPÓTESIS 1 - MÉTODO CUANTITATIVO: ÍNDICE BASE 100 Y THEIL
# =============================================================================

# --- 5.1. Índice de precio real, base promedio 2018 = 100 --------------------

# OJO con la composición: si promediáramos todas las comunas con dato, el
# índice bajaría en 2024 solo porque vuelven a publicar las comunas baratas del
# sur. Usamos el panel balanceado, que tiene las mismas 38 series en los 34
# trimestres.
indice_tipo <- panel |>
  group_by(ambientes, periodo, t, periodo_reg) |>
  summarise(p = mean(precio_m2_interp), .groups = "drop") |>
  group_by(ambientes) |>
  mutate(indice = 100 * p / mean(p[t <= 4])) |>
  ungroup()

indice_periodo <- indice_tipo |>
  group_by(ambientes, periodo_reg) |>
  summarise(indice = round(mean(indice), 1), .groups = "drop") |>
  pivot_wider(names_from = ambientes, values_from = indice)

print(indice_periodo)
write_csv(indice_periodo, file.path(outstub, "t10_indice_real_por_periodo.csv"))
write_csv(indice_tipo |> select(ambientes, periodo, t, periodo_reg, indice),
          file.path(outstub, "t11_indice_real_serie.csv"))

# Lo mismo sacando 2023, para separar la ley del salto inflacionario
indice_sin2023 <- indice_tipo |>
  filter(!(periodo_reg == "Ley de Alquileres" & substr(periodo, 1, 4) == "2023")) |>
  group_by(ambientes, periodo_reg) |>
  summarise(indice = round(mean(indice), 1), .groups = "drop") |>
  pivot_wider(names_from = ambientes, values_from = indice)
print(indice_sin2023)
write_csv(indice_sin2023, file.path(outstub, "t12_indice_real_sin2023.csv"))


# --- 5.2. Índice de Theil entre comunas --------------------------------------

# Función de la clase 12. Mide cuánta desigualdad de precios hay entre comunas
# en cada trimestre. Cada comuna pesa igual: la base no informa cuántas
# unidades ofrece cada una.
calcular_theil <- function(x, w = NULL) {
  x <- as.numeric(x)
  if (is.null(w)) w <- rep(1, length(x))
  ok <- !is.na(x) & !is.na(w) & w > 0 & x > 0
  x <- x[ok]; w <- w[ok]
  w_rel <- w / sum(w)
  media <- sum(x * w_rel)
  ratio <- x / media
  sum(w_rel * ratio * log(ratio))
}

# Mismo cuidado que con el índice: comunas fijas en todos los trimestres.
theil_trim <- panel |>
  group_by(ambientes, periodo, t, periodo_reg) |>
  summarise(theil = calcular_theil(precio_m2_interp), comunas = n(), .groups = "drop")

theil_periodo <- theil_trim |>
  group_by(ambientes, periodo_reg) |>
  summarise(theil_x1000 = round(1000 * mean(theil), 2), .groups = "drop") |>
  pivot_wider(names_from = ambientes, values_from = theil_x1000)

print(theil_periodo)
write_csv(theil_periodo, file.path(outstub, "t13_theil_por_periodo.csv"))
write_csv(theil_trim, file.path(outstub, "t14_theil_serie.csv"))

# Descomposición entre zonas / dentro de zonas (clase 12), en el tipo más
# completo (2 ambientes)
descomponer_theil <- function(datos) {
  mu <- mean(datos$precio_m2_real)
  det <- datos |>
    group_by(zona) |>
    summarise(s_g = n() / nrow(datos), mu_g = mean(precio_m2_real),
              theil_g = calcular_theil(precio_m2_real), .groups = "drop") |>
    mutate(entre = s_g * (mu_g / mu) * log(mu_g / mu),
           dentro = s_g * (mu_g / mu) * theil_g)
  tibble(T_observado = calcular_theil(datos$precio_m2_real),
         T_entre = sum(det$entre), T_dentro = sum(det$dentro))
}

theil_desc <- panel |>
  mutate(precio_m2_real = precio_m2_interp) |>
  filter(ambientes == "2 ambientes") |>
  group_by(periodo_reg) |>
  group_modify(~ descomponer_theil(.x)) |>
  ungroup() |>
  mutate(across(where(is.numeric), ~ round(1000 * .x, 2)),
         verifica = round(T_entre + T_dentro, 2),
         pct_entre = round(100 * T_entre / (T_entre + T_dentro)))

print(theil_desc)
write_csv(theil_desc, file.path(outstub, "t15_theil_descomposicion.csv"))


# =============================================================================
# 6. HIPÓTESIS 2 - LA BETA DE CADA COMUNA
# =============================================================================

# Retorno real trimestral = variación del logaritmo del precio real por m2.
panel <- panel |>
  group_by(serie) |>
  arrange(t, .by_group = TRUE) |>
  mutate(r = log(precio_m2_interp) - log(lag(precio_m2_interp))) |>
  ungroup()

# El "mercado" de cada serie es el promedio de las demás comunas del mismo
# tipo, EXCLUYÉNDOLA a ella: si se incluyera, parte de la correlación sería
# mecánica.
panel <- panel |>
  group_by(ambientes, t) |>
  mutate(r_mercado = (sum(r, na.rm = TRUE) - r) / (sum(!is.na(r)) - 1)) |>
  ungroup()

betas <- panel |>
  filter(!is.na(r), !is.na(r_mercado)) |>
  group_by(serie, comuna, zona, ambientes) |>
  summarise(
    n_ret      = n(),
    beta       = coef(lm(r ~ r_mercado))[2],
    r2         = summary(lm(r ~ r_mercado))$r.squared,
    volatilidad = sd(r) * 100,
    beta_sube  = coef(lm(r ~ r_mercado, data = pick(everything()) |>
                           filter(r_mercado > 0)))[2],
    beta_baja  = coef(lm(r ~ r_mercado, data = pick(everything()) |>
                           filter(r_mercado <= 0)))[2],
    .groups = "drop"
  )

# Nivel de precio de cada serie: precio relativo promedio (Ciudad = 100)
nivel <- alq |>
  filter(!is.na(precio_relativo)) |>
  group_by(serie) |>
  summarise(nivel = mean(precio_relativo), .groups = "drop")

betas <- betas |>
  left_join(nivel, by = "serie") |>
  mutate(across(c(beta, r2, volatilidad, beta_sube, beta_baja, nivel),
                ~ round(.x, 3))) |>
  arrange(desc(beta))

print(betas, n = Inf)
write_csv(betas, file.path(outstub, "t16_betas_por_serie.csv"))

beta_zona <- betas |>
  group_by(zona) |>
  summarise(
    n = n(),
    beta = round(mean(beta), 2),
    beta_sube = round(mean(beta_sube), 2),
    beta_baja = round(mean(beta_baja), 2),
    volatilidad = round(mean(volatilidad), 1),
    .groups = "drop"
  )
print(beta_zona)
write_csv(beta_zona, file.path(outstub, "t17_beta_por_zona.csv"))


# =============================================================================
# 7. HIPÓTESIS 2 - INFERENCIA
# =============================================================================

# --- 7.1. ANOVA de la beta por zona ------------------------------------------

# H0: la beta media es igual en las cuatro zonas.
# H1: al menos una zona tiene una beta media distinta.

cat("\n--- ANOVA ---\n")
anova_zona <- aov(beta ~ zona, data = betas)
print(summary(anova_zona))

tabla_a <- summary(anova_zona)[[1]]
eta2 <- tabla_a$`Sum Sq`[1] / sum(tabla_a$`Sum Sq`)
cat("eta cuadrado:", round(eta2, 3), "\n")

# Supuestos
cat("\nShapiro-Wilk sobre los residuos:\n"); print(shapiro.test(residuals(anova_zona)))
cat("Levene (Brown-Forsythe):\n"); print(car::leveneTest(beta ~ zona, data = betas))
desvios <- betas |> group_by(zona) |> summarise(sd = sd(beta))
cat("Cociente desvío mayor / menor:", round(max(desvios$sd) / min(desvios$sd), 2), "\n")

# Alternativas robustas
welch <- oneway.test(beta ~ zona, data = betas); print(welch)
kw <- kruskal.test(beta ~ zona, data = betas); print(kw)
cat("epsilon cuadrado:", round(unname(kw$statistic) / (nrow(betas) - 1), 3), "\n")

tukey <- TukeyHSD(anova_zona); print(tukey)

# ANOVA de dos factores: zona y tipo de departamento
anova_2f <- aov(beta ~ zona * ambientes, data = betas)
print(summary(anova_2f))

anova_resumen <- tibble(
  test = c("ANOVA (zona)", "ANOVA de Welch (zona)", "Kruskal-Wallis (zona)",
           "ANOVA 2 factores: zona", "ANOVA 2 factores: tipo",
           "ANOVA 2 factores: interacción"),
  estadistico = c(round(tabla_a$`F value`[1], 2), round(unname(welch$statistic), 2),
                  round(unname(kw$statistic), 2),
                  round(summary(anova_2f)[[1]]$`F value`[1], 2),
                  round(summary(anova_2f)[[1]]$`F value`[2], 2),
                  round(summary(anova_2f)[[1]]$`F value`[3], 2)),
  p_valor = round(c(tabla_a$`Pr(>F)`[1], welch$p.value, kw$p.value,
                    summary(anova_2f)[[1]]$`Pr(>F)`[1],
                    summary(anova_2f)[[1]]$`Pr(>F)`[2],
                    summary(anova_2f)[[1]]$`Pr(>F)`[3]), 4),
  tamano_efecto = c(paste("eta2 =", round(eta2, 2)), "",
                    paste("eps2 =", round(unname(kw$statistic) / (nrow(betas) - 1), 2)),
                    "", "", "")
)
print(anova_resumen)
write_csv(anova_resumen, file.path(outstub, "t18_anova_beta.csv"))

png(file.path("output/graficos", "i2_g05_qqplot_anova.png"), width = 700, height = 600)
par(mar = c(5, 5, 4, 2))
qqnorm(residuals(anova_zona), main = "Residuos del ANOVA de la beta",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles observados",
       pch = 19, col = "#1E5F6B")
qqline(residuals(anova_zona), lwd = 2, col = "#C8742A")
dev.off()


# --- 7.2. Correlación entre nivel de precio y beta ----------------------------

# La hipótesis habla de comunas caras, no de zonas. La prueba directa es
# relacionar la beta con el nivel de precio, que es una variable continua.
# Spearman (clase 10) trabaja sobre rangos: no supone normalidad ni linealidad.

cor_tests <- bind_rows(
  tibble(variable = "Beta",                     y = list(betas$beta)),
  tibble(variable = "Beta en caídas",           y = list(betas$beta_baja)),
  tibble(variable = "Beta en subas",            y = list(betas$beta_sube)),
  tibble(variable = "Volatilidad",              y = list(betas$volatilidad))
) |>
  rowwise() |>
  mutate(
    rho = round(unname(suppressWarnings(
      cor.test(betas$nivel, y, method = "spearman"))$estimate), 2),
    p_valor = round(suppressWarnings(
      cor.test(betas$nivel, y, method = "spearman"))$p.value, 4)
  ) |>
  ungroup() |> select(-y)

print(cor_tests)
write_csv(cor_tests, file.path(outstub, "t19_correlaciones_nivel.csv"))

pearson <- cor.test(betas$nivel, betas$beta)
cat("\nPearson nivel-beta: r =", round(unname(pearson$estimate), 2),
    " IC95% [", round(pearson$conf.int[1], 2), ";", round(pearson$conf.int[2], 2),
    "] p =", round(pearson$p.value, 4), "\n")


# --- 7.3. Asimetría: ¿amplifica más en las subas o en las bajas? --------------

asimetria <- betas |>
  group_by(zona) |>
  summarise(
    n = n(),
    # El test va PRIMERO: si se calculan antes las medias con los mismos
    # nombres, summarise() las reemplaza y el test termina comparando dos
    # números en vez de dos vectores.
    p_wilcoxon = round(suppressWarnings(
      wilcox.test(beta_baja, beta_sube, paired = TRUE))$p.value, 4),
    media_sube = round(mean(beta_sube), 2),
    media_baja = round(mean(beta_baja), 2),
    .groups = "drop"
  ) |>
  select(zona, n, beta_sube = media_sube, beta_baja = media_baja, p_wilcoxon)
print(asimetria)
write_csv(asimetria, file.path(outstub, "t20_asimetria.csv"))


# --- 7.4. Amplitud del ciclo por zona ----------------------------------------

indice_zona <- panel |>
  group_by(zona, ambientes, periodo, t) |>
  summarise(p = mean(precio_m2_interp), .groups = "drop") |>
  group_by(zona, ambientes) |>
  mutate(indice = 100 * p / mean(p[t <= 4])) |>
  ungroup()

amplitud <- indice_zona |>
  group_by(zona, ambientes) |>
  summarise(
    piso = min(indice[t <= 12]),
    pico = max(indice[t >= 13 & t <= 24]),
    final = mean(indice[t >= 31]),
    .groups = "drop"
  ) |>
  group_by(zona) |>
  summarise(
    suba_al_pico = round(100 * (mean(pico) / mean(piso) - 1)),
    caida_desde_pico = round(100 * (mean(final) / mean(pico) - 1)),
    .groups = "drop"
  )
print(amplitud)
write_csv(amplitud, file.path(outstub, "t21_amplitud_ciclo.csv"))
write_csv(indice_zona, file.path(outstub, "t22_indice_zona.csv"))

cat("\nListo. Resultados en", outstub, "\n")
