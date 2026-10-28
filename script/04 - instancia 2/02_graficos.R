# =============================================================================
# INSTANCIA 2 - Gráficos
#
# Usa las tablas que deja 01_metodos.R en output/instancia2/.
# Gráficos base de R, como en la clase 8.
# Salida: output/graficos/i2_g*.png
# =============================================================================

if (rstudioapi::isAvailable()) {
  setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", ".."))
}

library(tidyverse)

instub  <- "output/instancia2"
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

idx <- read_csv(file.path(instub, "t11_indice_real_serie.csv"), show_col_types = FALSE)
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

th <- read_csv(file.path(instub, "t14_theil_serie.csv"), show_col_types = FALSE) |>
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

b <- read_csv(file.path(instub, "t16_betas_por_serie.csv"), show_col_types = FALSE)
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

asim <- read_csv(file.path(instub, "t20_asimetria.csv"), show_col_types = FALSE)
m <- t(as.matrix(asim[, c("beta_sube", "beta_baja")]))
colnames(m) <- asim$zona

png(file.path(outstub, "i2_g06_asimetria.png"), width = 900, height = 560)
par(mar = c(5, 5, 4, 2))
bp <- barplot(m, beside = TRUE, col = c(AZUL2, OCRE), border = NA, ylim = c(0, 1.45),
              main = "El Norte amplifica las caídas, no las subas",
              xlab = "", ylab = "Beta promedio")
abline(h = 1, lty = 3, col = GRIS)
text(bp, as.vector(m) + 0.05, format(as.vector(m), nsmall = 2), cex = 0.9, col = AZUL)
legend("topright", legend = c("Cuando el mercado sube", "Cuando el mercado baja"),
       fill = c(AZUL2, OCRE), border = NA, bty = "n")
box()
dev.off()

cat("\nGráficos guardados en", outstub, "\n")


# =============================================================================
# G7 y G8. Los dos gráficos que agregamos a partir de la devolución
# =============================================================================

# --- G7. La prima del monoambiente, hoy contra 2018 --------------------------

prima <- read_csv("output/tablas/t18_prima_por_comuna.csv", show_col_types = FALSE) |>
  filter(!is.na(prima_2018)) |>
  arrange(cambio_pp)

png(file.path(outstub, "i2_g07_prima_comuna.png"), width = 1000, height = 560)
par(mar = c(5, 5, 4, 2))
m <- rbind(prima$prima_2018, prima$prima_hoy)
bp <- barplot(m, beside = TRUE, col = c(GRIS, AZUL), border = NA,
              names.arg = paste("C", prima$comuna, sep = ""),
              ylim = c(0, 26), las = 1,
              main = "Cuánto más caro es el m² del monoambiente que el del 3 ambientes",
              xlab = "Comuna", ylab = "Prima del monoambiente (%)")
legend("topleft", legend = c("Promedio 2018", "Último año"),
       fill = c(GRIS, AZUL), border = NA, bty = "n", cex = 1.05)
box()
dev.off()


# --- G8. La evolución del precio real por zona -------------------------------

ev <- read_csv("input/deflactados/alquileres_comunas.csv", show_col_types = FALSE) |>
  filter(!is.na(precio_m2_real), comuna != 8, ambientes == "2 ambientes") |>
  mutate(t = (anio - 2018) * 4 + trimestre) |>
  group_by(zona, t) |>
  summarise(p = mean(precio_m2_real), .groups = "drop") |>
  group_by(zona) |>
  mutate(indice = 100 * p / mean(p[t <= 4])) |>
  ungroup()

col_z <- c(Norte = AZUL, Centro = AZUL2, Oeste = OCRE, Sur = "#9B5A3C")

png(file.path(outstub, "i2_g08_evolucion_zona.png"), width = 1000, height = 560)
par(mar = c(5, 5, 4, 2))
plot(NA, xlim = c(1, 34), ylim = c(80, 175), xaxt = "n",
     main = "Alquiler real de un 2 ambientes por zona (promedio 2018 = 100)",
     xlab = "", ylab = "Índice (2018 = 100)")
abline(h = 100, lty = 3, col = GRIS)
for (z in names(col_z)) {
  d <- ev |> filter(zona == z) |> arrange(t)
  lines(d$t, d$indice, lwd = 2.5, col = col_z[z])
}
axis(1, at = seq(1, 34, 4), labels = etiqueta_t(seq(1, 34, 4)), las = 2, cex.axis = 0.85)
abline(v = 24, lty = 2, col = OCRE)
text(24, 172, "pico 2023-T4", col = OCRE, pos = 2, cex = 0.95)
legend("topleft", legend = names(col_z), col = col_z, lwd = 2.5, bty = "n", cex = 1.05)
box()
dev.off()
