# Los tildes se omiten en el proyecto por temas de codificacion
# 
# -04_ApilarDias
# 
# Este codigo apila los .RData diarios generados por 01, 02 y 03 en tres
# bases de datos unicas: gps (stack_gps), sc (stack_sc) y m (stack_movil);

rm(list = ls())
graphics.off()
gc()

# ---- GPS ----

load("Processed/gps/gps1.RData")
load("Processed/gps/gps2.RData")
load("Processed/gps/gps3.RData")
load("Processed/gps/gps4.RData")
load("Processed/gps/gps5.RData")
load("Processed/gps/gps6.RData")
load("Processed/gps/gps7.RData")
load("Processed/gps/gps8.RData")
load("Processed/gps/gps9.RData")
load("Processed/gps/gps10.RData")
load("Processed/gps/gps11.RData")
load("Processed/gps/gps12.RData")
load("Processed/gps/gps13.RData")
load("Processed/gps/gps14.RData")
load("Processed/gps/gps15.RData")
load("Processed/gps/gps16.RData")
load("Processed/gps/gps17.RData")
load("Processed/gps/gps18.RData")
load("Processed/gps/gps19.RData")
load("Processed/gps/gps20.RData")

gps <- rbind(
  gps1, gps2, gps3, gps4, gps5,
  gps6, gps7, gps8, gps9, gps10,
  gps11, gps12, gps13, gps14, gps15,
  gps16, gps17, gps18, gps19, gps20
)

save(gps, file = "Processed/gps/stack_gps.RData")
rm(list = ls())


# ---- Central ----
load("Processed/sc/sc_240727.RData")
load("Processed/sc/sc_240728.RData")
load("Processed/sc/sc_240729.RData")
load("Processed/sc/sc_240730.RData")
load("Processed/sc/sc_240812.RData")
load("Processed/sc/sc_240813.RData")
load("Processed/sc/sc_240814.RData")
load("Processed/sc/sc_240816.RData")
load("Processed/sc/sc_240819.RData")
load("Processed/sc/sc_240823.RData")
load("Processed/sc/sc_240824.RData")
load("Processed/sc/sc_240827.RData")
load("Processed/sc/sc_240828.RData")
load("Processed/sc/sc_240911.RData")
load("Processed/sc/sc_240912.RData")
load("Processed/sc/sc_240913.RData")
load("Processed/sc/sc_240914.RData")
load("Processed/sc/sc_240915.RData")
load("Processed/sc/sc_240916.RData")
load("Processed/sc/sc_240926.RData")

sc <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
            a14, a15, a16, a17, a18, a19, a20)

save(sc, file = "Processed/sc/stack_sc.RData")
rm(list = ls())


# ---- Movil ----
load("Processed/movil/movil_240727.RData")
load("Processed/movil/movil_240728.RData")
load("Processed/movil/movil_240729.RData")
load("Processed/movil/movil_240730.RData")
load("Processed/movil/movil_240812.RData")
load("Processed/movil/movil_240813.RData")
load("Processed/movil/movil_240814.RData")
load("Processed/movil/movil_240816.RData")
load("Processed/movil/movil_240819.RData")
load("Processed/movil/movil_240823.RData")
load("Processed/movil/movil_240824.RData")
load("Processed/movil/movil_240827.RData")
load("Processed/movil/movil_240828.RData")
load("Processed/movil/movil_240911.RData")
load("Processed/movil/movil_240912.RData")
load("Processed/movil/movil_240913.RData")
load("Processed/movil/movil_240914.RData")
load("Processed/movil/movil_240915.RData")
load("Processed/movil/movil_240916.RData")
load("Processed/movil/movil_240926.RData")

m <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
           d14, d15, d16, d17, d18, d19, d20)

save(m, file = "Processed/movil/stack_movil.RData")