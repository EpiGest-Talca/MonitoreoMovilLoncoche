# -06_MergeBases
# 
# Este codigo une los tres stacks generados por 04 (gps, sc, m) en una unica
# base de datos por Datetime, segmenta las etapas de muestreo (blanco,
# duplicado, muestra) y realiza la interpolacion espacial del GPS dentro de
# la ruta; son 20 dias de campaña en total, estructura de la base de datos:
# 
#   la base unida (raw) contiene la siguiente informacion:
#   - Datetime: corresponde a la hora del dato, llave de union entre movil,
#               central y GPS.
#   
#   - pmmov / pmsc: corresponden a la concentracion de PM2.5 medida por el
#                   monitoreo movil y por el sitio central, respectivamente.
#   
#   - tipo: corresponde a la etapa de muestreo segun el monitoreo movil
#           (blanco inicial, duplicado 1, muestra, duplicado 2, blanco
#           final), usada para clasificar tambien al sitio central.
#   
#   - lon / lat / ele / ruta: corresponden a la posicion GPS asociada a ese
#                              Datetime, interpolada dentro de tramos
#                              continuos cuando falta el dato puntual.
#   
#   - date: corresponde al dia de campaña, desplazado -4 horas para que una
#           ruta que cruce medianoche quede asociada al dia en que empezo.
# 
# Que resultado esperar de este codigo:
#   - una base de datos consolidada (raw) y sus 5 subconjuntos por etapa,
#     guardados en Out/raw.RData, con sus respectivas variables:
#     * b1 / b2: mediciones de blanco inicial y final (tipo 0 y 4).
#     * d1 / d2: mediciones de duplicado 1 y 2 (tipo 1 y 3), usadas mas
#                adelante para calcular el factor de correccion del movil.
#     * med: mediciones en ruta (tipo 2), con pmmov y pmsc ya convertidos
#            de mg/m3 a ug/m3 y con negativos filtrados a NA; es la base
#            que se usara para las correcciones finales.
# 
# Nota: la conversion de unidades y el filtro de negativos se aplican
#       unicamente sobre med; b1/b2/d1/d2 quedan en su unidad original.
#       La interpolacion espacial del GPS corta la continuidad si hay un
#       vacio mayor a 3 minutos, y considera viaje nuevo si el vacio supera
#       los 60 minutos.

rm(list = ls())
gc()

library(dplyr)
library(zoo)
library(tidyr)

load("Data/Processed/gps/stack_gps.RData")        # objeto: gps  (lon, lat, ele, time, ruta)
load("Data/Processed/sc/stack_sc.RData")          # objeto: sc
load("Data/Processed/movil/stack_movil.RData")    # objeto: m

# Homologar nombres de columnas antes del merge
names(gps)[names(gps) == "time"] <- "Datetime"
names(sc)[names(sc)   == "pm2_5"]   <- "pmsc"
names(m)[names(m)     == "pm2_5"]   <- "pmmov"

# Join movil + central por Datetime (manda el movil)
raw0 <- merge(m, sc, by = "Datetime", all.x = TRUE)
raw0$tipo.y <- NULL
names(raw0)[names(raw0) == "tipo.x"] <- "tipo"

# Agregar GPS (trae lat / lon / ele / ruta)
raw <- merge(raw0, gps, by = "Datetime", all.x = TRUE)

# "dia" desplazado -4h: el dia parte a las 4am, asi una ruta que cruce
# medianoche queda asociada al dia en que empezo. Opera sobre hora local,
# por lo que no se ve afectado por el cambio de hora (rutas diurnas).
raw$date <- as.Date(trunc(raw$Datetime - 4 * 60 * 60, "day"), format = "%Y-%m-%d")

# Segmentos: blancos, duplicados y medicion en ruta
b1  <- subset(raw, tipo == "0")
d1  <- subset(raw, tipo == "1")
d2  <- subset(raw, tipo == "3")
b2  <- subset(raw, tipo == "4")
med <- subset(raw, tipo == "2")

# Conversion de unidades (mg/m3 -> ug/m3).
# NOTA: igual que el original, solo se aplica a 'med'. Si vas a usar las
# concentraciones de blancos/duplicados (b1/b2/d1/d2), quedan en su unidad
# original y con negativos sin tratar.
med$pmmov <- med$pmmov * 1000
med$pmsc  <- med$pmsc  * 1000

# Analisis de negativos (solo en med, igual que el original)
summary(med)
med$pmsc[med$pmsc < 0]   <- NA
med$pmmov[med$pmmov < 0] <- NA

# Interpolacion espacial dentro de segmentos continuos.
# Regla: hueco >3 min = corte de continuidad (no se interpola a traves);
#        hueco >60 min = viaje nuevo (tramo_id).
med <- med %>%
  arrange(Datetime) %>%
  mutate(
    diff_minutos        = as.numeric(difftime(Datetime, dplyr::lag(Datetime), units = "mins")),
    diff_minutos        = replace_na(diff_minutos, 0),
    es_nuevo_viaje      = diff_minutos > 60,
    tramo_id            = cumsum(es_nuevo_viaje),
    corte_continuidad   = diff_minutos > 3,
    grupo_interpolacion = cumsum(corte_continuidad)
  ) %>%
  group_by(grupo_interpolacion) %>%
  mutate(
    # na.approx necesita >=2 valores no-NA por grupo; si no, deja el vector original
    lat = if (sum(!is.na(lat)) >= 2) na.approx(lat, maxgap = 300, rule = 2, na.rm = FALSE) else lat,
    lon = if (sum(!is.na(lon)) >= 2) na.approx(lon, maxgap = 300, rule = 2, na.rm = FALSE) else lon,
    ele = if (sum(!is.na(ele)) >= 2) na.approx(ele, maxgap = 300, rule = 2, na.rm = FALSE) else ele
  ) %>%
  ungroup() %>%
  # Opcional: propagar la sub-ruta (la paz/huiscapi/loncoche) a los puntos
  # interpolados, dentro de cada grupo continuo. Descomentar el bloque:
  # Propagación bidireccional (hacia abajo y hacia arriba)
  group_by(grupo_interpolacion) %>%
  tidyr::fill(ruta, .direction = "downup") %>%
  ungroup() %>%
  select(-diff_minutos, -es_nuevo_viaje, -corte_continuidad, -grupo_interpolacion)

save(raw, med, b1, b2, d1, d2, file = "Data/Processed/raw.RData")

