# =============================================================================
# INSTANCIA 2 - Gráficos
#
# Usa las tablas que deja 01_metodos.R en output/tablas/.
# Gráficos base de R, como en la clase 8.
# Salida: output/graficos/i2_g*.png
# =============================================================================

if (rstudioapi::isAvailable()) {
  setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", ".."))
}

library(tidyverse)

instub  <- "output/tablas"
outstub <- "output/graficos"

# Colores de la presentación
AZUL   <- "#15505E"   # principal
AZUL2  <- "#2E7D8F"   # secundario
OCRE   <- "#C8742A"   # acento
GRIS   <- "#8A9699"
BANDA  <- "#E8E0D2"   # fondo del período de la ley

etiqueta_t <- function(t) paste0(2018 + (t - 1) %/% 4, "-T", (t - 1) %% 4 + 1)


# =============================================================================
# G1. Faltantes por trimestre, con los períodos regulatorios
# =============================================================================

alq <- read_csv("input/deflactados/alquileres_comunas.csv", show_col_types = FALSE) |>
  mutate(t = (anio - 2018) * 4 + trimestre)

falt_t <- alq |>
  group_by(t, periodo) |>
  summarise(pct = 100 * mean(is.na(precio_real)), .groups = "drop") |>
  arrange(t)

png(file.path(outstub, "i2_g01_faltantes_trimestre.png"), width = 1000, height = 520)
par(mar = c(5, 5, 4, 2))
plot(falt_t$t, falt_t$pct, type = "n", ylim = c(0, 30), xaxt = "n",
     main = "Comunas-tipo sin dato publicado, por trimestre",
     xlab = "", ylab = "% de las 45 series sin dato")
# Banda de la Ley de Alquileres (2020-T3 a 2023-T4)
rect(10.5, -5, 24.5, 35, col = BANDA, border = NA)
lines(falt_t$t, falt_t$pct, lwd = 3, col = AZUL)
points(falt_t$t, falt_t$pct, pch = 19, col = AZUL, cex = 0.8)
axis(1, at = seq(1, 34, 4), labels = etiqueta_t(seq(1, 34, 4)), las = 2, cex.axis = 0.85)
abline(h = 13, lty = 3, col = GRIS)
text(17.5, 28, "Ley de Alquileres", col = OCRE, font = 2, cex = 1.1)
text(5, 28, "Antes", col = GRIS, font = 2, cex = 1.1)
text(30, 28, "Después", col = GRIS, font = 2, cex = 1.1)
text(33, 14.3, "promedio 13%", col = GRIS, cex = 0.8, pos = 2)
box()
dev.off()


# =============================================================================
# G2. Índice de precio real base 2018 = 100
# =============================================================================

idx <- read_csv(file.path(instub, "t24_indice_real_serie.csv"), show_col_types = FALSE)
idx_w <- idx |> select(ambientes, t, indice) |>
  pivot_wider(names_from = ambientes, values_from = indice) |> arrange(t)

png(file.path(outstub, "i2_g02_indice_real.png"), width = 1000, height = 520)
par(mar = c(5, 5, 4, 2))
plot(idx_w$t, idx_w$`2 ambientes`, type = "n", ylim = c(80, 165), xaxt = "n",
     main = "Alquiler real por m², índice promedio 2018 = 100",
     xlab = "", ylab = "Índice (2018 = 100)")
rect(10.5, 50, 24.5, 200, col = BANDA, border = NA)
abline(h = 100, lty = 3, col = GRIS)
lines(idx_w$t, idx_w$`1 ambiente`,  lwd = 2.5, col = OCRE)
lines(idx_w$t, idx_w$`2 ambientes`, lwd = 2.5, col = AZUL)
lines(idx_w$t, idx_w$`3 ambientes`, lwd = 2.5, col = AZUL2)
axis(1, at = seq(1, 34, 4), labels = etiqueta_t(seq(1, 34, 4)), las = 2, cex.axis = 0.85)
text(17.5, 160, "Ley de Alquileres", col = OCRE, font = 2, cex = 1.1)
legend("bottomleft", legend = c("1 ambiente", "2 ambientes", "3 ambientes"),
       col = c(OCRE, AZUL, AZUL2), lwd = 2.5, bty = "n", cex = 0.95)
box()
dev.off()


# =============================================================================
# G3. Theil entre comunas, por trimestre
# =============================================================================

th <- read_csv(file.path(instub, "t27_theil_serie.csv"), show_col_types = FALSE) |>
  filter(ambientes == "2 ambientes") |> arrange(t)

png(file.path(outstub, "i2_g03_theil.png"), width = 1000, height = 520)
par(mar = c(5, 5, 4, 2))
plot(th$t, 1000 * th$theil, type = "n", xaxt = "n", ylim = c(0, max(1000 * th$theil) * 1.15),
     main = "Desigualdad de precios entre comunas (índice de Theil × 1.000)",
     xlab = "", ylab = "Theil × 1.000 — 2 ambientes")
rect(10.5, -5, 24.5, 100, col = BANDA, border = NA)
lines(th$t, 1000 * th$theil, lwd = 3, col = AZUL)
points(th$t, 1000 * th$theil, pch = 19, col = AZUL, cex = 0.7)
axis(1, at = seq(1, 34, 4), labels = etiqueta_t(seq(1, 34, 4)), las = 2, cex.axis = 0.85)
text(17.5, max(1000 * th$theil) * 1.08, "Ley de Alquileres", col = OCRE, font = 2, cex = 1.1)
box()
dev.off()


# =============================================================================
# G4. Beta contra nivel de precio (la prueba central de la hipótesis 2)
# =============================================================================

b <- read_csv(file.path(instub, "t29_betas_por_serie.csv"), show_col_types = FALSE)
col_zona <- c(Norte = AZUL, Centro = AZUL2, Oeste = OCRE, Sur = "#9B5A3C")

png(file.path(outstub, "i2_g04_beta_vs_nivel.png"), width = 1000, height = 600)
par(mar = c(5, 5, 4, 2))
plot(b$nivel, b$beta, pch = 19, cex = 1.6, col = col_zona[b$zona],
     main = "Las comunas caras se mueven más que el promedio de la Ciudad",
     xlab = "Nivel de precio de la comuna (Ciudad = 100)",
     ylab = "Beta (sensibilidad al ciclo de la Ciudad)")
abline(lm(beta ~ nivel, data = b), lwd = 2.5, col = AZUL)
abline(h = 1, lty = 3, col = GRIS)
legend("topleft", legend = names(col_zona), pch = 19, col = col_zona,
       bty = "n", cex = 1, pt.cex = 1.4)
legend("bottomright", legend = expression(paste("Spearman ", rho, " = 0,47   p = 0,003")),
       bty = "n", cex = 1.05)
box()
dev.off()


# =============================================================================
# G5. Beta por zona (el ANOVA, visto)
# =============================================================================

png(file.path(outstub, "i2_g05_beta_por_zona.png"), width = 900, height = 560)
par(mar = c(5, 5, 4, 2), cex.main = 1.25, cex.lab = 1.2, cex.axis = 1.15)
b$zona <- factor(b$zona, levels = c("Norte", "Centro", "Oeste", "Sur"))
boxplot(beta ~ zona, data = b, col = BANDA, border = AZUL, outline = FALSE,
        main = "Beta por zona: las diferencias existen, pero las zonas\nson muy heterogéneas por dentro",
        xlab = "", ylab = "Beta", ylim = range(b$beta) + c(-0.05, 0.05))
set.seed(4)
points(jitter(as.numeric(b$zona), 0.6), b$beta, pch = 19,
       col = col_zona[as.character(b$zona)], cex = 1.3)
abline(h = 1, lty = 3, col = GRIS)
box()
dev.off()


# =============================================================================
# G6. Asimetría: la beta en subas y en caídas del mercado
# =============================================================================

asim <- read_csv(file.path(instub, "t36_asimetria.csv"), show_col_types = FALSE)
m <- t(as.matrix(asim[, c("beta_sube", "beta_baja")]))
colnames(m) <- asim$zona

png(file.path(outstub, "i2_g06_asimetria.png"), width = 900, height = 560)
par(mar = c(5, 5, 4, 2))
bp <- barplot(m, beside = TRUE, col = c(AZUL2, OCRE), border = NA, ylim = c(0, 1.45),
              main = "El Norte amplifica las caídas; el Centro, las subas",
              xlab = "", ylab = "Beta promedio")
abline(h = 1, lty = 3, col = GRIS)
text(bp, as.vector(m) + 0.05, format(as.vector(m), nsmall = 2), cex = 0.9, col = AZUL)
legend("topright", legend = c("Cuando el mercado sube", "Cuando el mercado baja"),
       fill = c(AZUL2, OCRE), border = NA, bty = "n")
box()
dev.off()

# =============================================================================
# G7. La regresión de la beta, vista
#     El profesor pidió mostrar la regresión. Graficamos tres series: la de
#     beta más alta, una del medio y la más baja, con la recta estimada.
# =============================================================================

library(zoo)

panel <- read_csv("input/deflactados/alquileres_comunas.csv", show_col_types = FALSE) |>
  mutate(t = (anio - 2018) * 4 + trimestre,
         serie = paste0("C", comuna, " - ", ambientes)) |>
  group_by(serie) |>
  mutate(p = na.approx(precio_m2_real, x = t, maxgap = 4, na.rm = FALSE)) |>
  filter(all(!is.na(p))) |>
  arrange(t, .by_group = TRUE) |>
  mutate(r = log(p) - log(lag(p))) |>
  ungroup() |>
  group_by(ambientes, t) |>
  mutate(r_mercado = (sum(r, na.rm = TRUE) - r) / (sum(!is.na(r)) - 1)) |>
  ungroup() |>
  filter(!is.na(r), !is.na(r_mercado))

d <- read_csv(file.path(instub, "t30_regresion_diagnostico.csv"), show_col_types = FALSE) |>
  arrange(desc(beta))
elegidas <- d$serie[c(1, ceiling(nrow(d) / 2), nrow(d))]

png(file.path(outstub, "i2_g07_regresion.png"), width = 1200, height = 440)
par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), cex.main = 1.3, cex.lab = 1.15, cex.axis = 1.05)
for (sr in elegidas) {
  x <- filter(panel, serie == sr)
  f <- filter(d, serie == sr)
  plot(100 * x$r_mercado, 100 * x$r, pch = 19, col = AZUL2, cex = 1.3,
       xlim = 100 * range(panel$r_mercado), ylim = 100 * range(panel$r),
       main = sr, xlab = "Retorno del mercado (%)", ylab = "Retorno de la serie (%)")
  abline(h = 0, v = 0, col = "gray85")
  abline(lm(r * 100 ~ I(r_mercado * 100), data = x), lwd = 3, col = OCRE)
  legend("topleft", bty = "n", cex = 1.15, text.col = AZUL,
         legend = c(paste0("beta = ", formatC(f$beta, 2, format = "f", decimal.mark = ",")),
                    paste0("R2 = ",   formatC(f$r2,   2, format = "f", decimal.mark = ",")),
                    paste0("p ", ifelse(f$p_beta < 0.001, "< 0,001",
                           paste("=", formatC(f$p_beta, 3, format = "f", decimal.mark = ","))))))
  box()
}
dev.off()

cat("\nGráficos guardados en", outstub, "\n")
