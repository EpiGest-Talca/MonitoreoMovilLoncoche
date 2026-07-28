# -07_Corr
# 
# Este codigo calcula el factor de correccion de las mediciones del movil y
# del sitio central, usando los tramos duplicados (D1, D2) y la referencia
# diaria de SINCA generada en 05; son 20 dias de campaña en total,
# estructura de la base de datos:
# 
#   la base de entrada (med) contiene la siguiente informacion:
#   - pmmov / pmsc: corresponden a la concentracion de PM2.5 medida por el
#                   monitoreo movil y por el sitio central, ya en ug/m3 y
#                   con negativos filtrados a NA.
#   
#   - date: corresponde al dia de campaña, llave de union con SINCA y con el
#           factor de correccion diario.
#   
#   - mean_sinca: corresponde al promedio diario de SINCA en la ventana de
#                 ruta (calculado en 05), usado como valor de referencia.
#   
#   - corr: corresponde al factor de correccion del movil, calculado como el
#           promedio de los ratios pmsc/pmmov en los duplicados D1 y D2 de
#           ese dia (protegido contra NA, division por cero y valores no
#           fisicos <= 0).
# 
# Que resultado esperar de este codigo:
#   - un objeto med consolidado y guardado en Out/med.RData y Out/med.csv,
#     con sus respectivas variables:
#     * sc_corr: correccion del sitio central hacia SINCA, calculada como
#                pmsc * (mean_sinca / meansc).
#     * mov_corr: correccion del movil, calculada como pmmov * (mean_sinca /
#                 meansc) * corr, combinando la correccion hacia SINCA con
#                 el ajuste propio del movil obtenido de los duplicados.
#     * hour: hora del dia extraida de Datetime, para analisis posterior.
# 
# Nota: el dia 27/08 tiene el factor corr sobrescrito manualmente
#       (0.3899949), calculado como el promedio de los dias vecinos en la
#       version original del codigo (sin el filtro de negativos actual). Si
#       cambia el filtrado aguas arriba, conviene re-derivar este valor.

rm(list = ls())
gc()

load("Data/Processed/raw.RData")          # raw, med, b1, b2, d1, d2
load("Data/Processed/sinca/sinca.RData")  # objeto: sinca (date, mean_sinca)

# Ratio pmsc/pmmov en cada duplicado. Protege contra NA, division por cero y
# valores NO FISICOS (<= 0): un pm negativo o cero contaminaria la media del
# ratio. Este filtro es el que limpia los negativos en los duplicados, que en
# el merge solo se trataron sobre 'med'.
d1$ratio <- with(d1, ifelse(
  is.na(pmsc) | is.na(pmmov) | pmsc <= 0 | pmmov <= 0,
  NA_real_,
  pmsc / pmmov))

d2$ratio <- with(d2, ifelse(
  is.na(pmsc) | is.na(pmmov) | pmsc <= 0 | pmmov <= 0,
  NA_real_,
  pmsc / pmmov))

# Media diaria del ratio en cada duplicado
corr0 <- aggregate(d1$ratio, list(d1$date), mean, na.rm = TRUE)
names(corr0) <- c("date", "meand1")

corr1 <- aggregate(d2$ratio, list(d2$date), mean, na.rm = TRUE)
names(corr1) <- c("date", "meand2")

# mean() devuelve NaN si todos los ratios del dia son NA; pasarlo a NA normal
corr0$meand1[is.nan(corr0$meand1)] <- NA_real_
corr1$meand2[is.nan(corr1$meand2)] <- NA_real_

# Promedio entre ambos duplicados = factor de correccion movil
corr <- merge(corr0, corr1, by = "date", all.x = TRUE)
corr$corr <- (corr$meand1 + corr$meand2) / 2

# Parche manual del 27/08: factor imputado a mano = (0.50086864 + 0.27912113)/2,
# promedio de los dias vecinos en la version original (calculados sin filtro de
# negativos). Sobrescribe lo que se calcule para ese dia. Si en algun momento
# cambias el filtrado aguas arriba, conviene re-derivar este valor.
corr$corr[corr$date == "2024-08-27"] <- 0.3899949

corr$meand1 <- NULL
corr$meand2 <- NULL

# Incorporar SINCA y el factor de correccion a med
med <- merge(med, sinca, by = "date", all.x = TRUE)
med <- merge(med, corr,  by = "date", all.x = TRUE)

# Promedio diario del sitio central (su propia media en la ventana de ruta)
sc <- aggregate(med$pmsc, list(med$date), mean, na.rm = TRUE)
names(sc) <- c("date", "meansc")
med <- merge(med, sc, by = "date", all.x = TRUE)

# 1) Correccion del sitio central hacia SINCA
med$sc_corr <- with(med, ifelse(
  is.na(meansc) | meansc == 0 | is.na(pmsc) | is.na(mean_sinca),
  NA_real_,
  pmsc * (mean_sinca / meansc)))
summary(med$sc_corr)

# 2) Correccion del movil: factor SINCA/central + ajuste de los duplicados
med$mov_corr <- with(med, ifelse(
  is.na(meansc) | meansc == 0 | is.na(pmmov) | is.na(mean_sinca) | is.na(corr),
  NA_real_,
  pmmov * (mean_sinca / meansc) * corr))
summary(med$mov_corr)

# Eliminar Inf residuales en columnas numericas
med[] <- lapply(med, function(x) {
  if (is.numeric(x)) x[is.infinite(x)] <- NA_real_
  x
})

med$hour <- as.numeric(format(med$Datetime, "%H"))

save(med, file = "Data/Processed/med.RData")

# CSV con NA como vacio para compartir externamente
write.csv(med, file = "Data/Processed/med.csv", row.names = FALSE, na = "")
