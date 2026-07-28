# Los tildes se omiten en el proyecto por temas de codificacion
# 
# -04_ApilarDias
# 
# Este codigo apila los .RData diarios generados por 01, 02 y 03 en tres
# bases de datos unicas: gps (stack_gps), sc (stack_sc) y m (stack_movil);
# son 20 dias de campaña en total, estructura de la base de datos:
# 
#   cada objeto diario apilado contiene la siguiente data:
#   - GPS: lon / lat / ele / time / ruta, uno por punto registrado por el
#          Garmin durante el monitoreo movil.
#   
#   - Central (sc): Datetime / pm2_5 / tipo, uno por segundo medido por el
#                   sensor fijo del sitio central.
#   
#   - Movil (m): Datetime / pm2_5 / tipo, uno por segundo medido por el
#                DustTrak del monitoreo movil.
# 
# Que resultado esperar de este codigo:
#   - tres objetos consolidados en el environment de R, cada uno con sus
#     respectivas variables:
#     * gps: stack de los 20 dias de tracks GPS, con su columna ruta
#            (la paz / huiscapi / loncoche) para diferenciar cada sub-ruta.
#     * sc: stack de los 20 dias del sitio central, con su columna tipo
#           para diferenciar blanco, duplicado y muestra.
#     * m: stack de los 20 dias del monitoreo movil, con su columna tipo
#          para diferenciar blanco, duplicado y muestra.

rm(list = ls())
graphics.off()
gc()

# ---- GPS ----

load("Data/Processed/gps/gps1.RData")
load("Data/Processed/gps/gps2.RData")
load("Data/Processed/gps/gps3.RData")
load("Data/Processed/gps/gps4.RData")
load("Data/Processed/gps/gps5.RData")
load("Data/Processed/gps/gps6.RData")
load("Data/Processed/gps/gps7.RData")
load("Data/Processed/gps/gps8.RData")
load("Data/Processed/gps/gps9.RData")
load("Data/Processed/gps/gps10.RData")
load("Data/Processed/gps/gps11.RData")
load("Data/Processed/gps/gps12.RData")
load("Data/Processed/gps/gps13.RData")
load("Data/Processed/gps/gps14.RData")
load("Data/Processed/gps/gps15.RData")
load("Data/Processed/gps/gps16.RData")
load("Data/Processed/gps/gps17.RData")
load("Data/Processed/gps/gps18.RData")
load("Data/Processed/gps/gps19.RData")
load("Data/Processed/gps/gps20.RData")

gps <- rbind(
  gps1, gps2, gps3, gps4, gps5,
  gps6, gps7, gps8, gps9, gps10,
  gps11, gps12, gps13, gps14, gps15,
  gps16, gps17, gps18, gps19, gps20
)

save(gps, file = "Data/Processed/gps/stack_gps.RData")
rm(list = ls())


# ---- Central ----
load("Data/Processed/sc/sc_240727.RData")
load("Data/Processed/sc/sc_240728.RData")
load("Data/Processed/sc/sc_240729.RData")
load("Data/Processed/sc/sc_240730.RData")
load("Data/Processed/sc/sc_240812.RData")
load("Data/Processed/sc/sc_240813.RData")
load("Data/Processed/sc/sc_240814.RData")
load("Data/Processed/sc/sc_240816.RData")
load("Data/Processed/sc/sc_240819.RData")
load("Data/Processed/sc/sc_240823.RData")
load("Data/Processed/sc/sc_240824.RData")
load("Data/Processed/sc/sc_240827.RData")
load("Data/Processed/sc/sc_240828.RData")
load("Data/Processed/sc/sc_240911.RData")
load("Data/Processed/sc/sc_240912.RData")
load("Data/Processed/sc/sc_240913.RData")
load("Data/Processed/sc/sc_240914.RData")
load("Data/Processed/sc/sc_240915.RData")
load("Data/Processed/sc/sc_240916.RData")
load("Data/Processed/sc/sc_240926.RData")

sc <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
            a14, a15, a16, a17, a18, a19, a20)

save(sc, file = "Data/Processed/sc/stack_sc.RData")
rm(list = ls())


# ---- Movil ----
load("Data/Processed/movil/movil_240727.RData")
load("Data/Processed/movil/movil_240728.RData")
load("Data/Processed/movil/movil_240729.RData")
load("Data/Processed/movil/movil_240730.RData")
load("Data/Processed/movil/movil_240812.RData")
load("Data/Processed/movil/movil_240813.RData")
load("Data/Processed/movil/movil_240814.RData")
load("Data/Processed/movil/movil_240816.RData")
load("Data/Processed/movil/movil_240819.RData")
load("Data/Processed/movil/movil_240823.RData")
load("Data/Processed/movil/movil_240824.RData")
load("Data/Processed/movil/movil_240827.RData")
load("Data/Processed/movil/movil_240828.RData")
load("Data/Processed/movil/movil_240911.RData")
load("Data/Processed/movil/movil_240912.RData")
load("Data/Processed/movil/movil_240913.RData")
load("Data/Processed/movil/movil_240914.RData")
load("Data/Processed/movil/movil_240915.RData")
load("Data/Processed/movil/movil_240916.RData")
load("Data/Processed/movil/movil_240926.RData")

m <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
           d14, d15, d16, d17, d18, d19, d20)

save(m, file = "Data/Processed/movil/stack_movil.RData")

