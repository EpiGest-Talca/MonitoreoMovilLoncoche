# -03_ReadGPS
# 
# Este codigo lee los archivos GPX crudos del GPS Garmin usado durante el
# monitoreo movil y los carga diferenciando la sub-ruta de origen
#  
# Nota: segun los graficos de tracks por dia, el dia 5 (240812) no registra
#       monitoreo en la ruta La Paz, por lo que ese dia se carga solo con
#       huiscapi y loncoche.

rm(list = ls())
graphics.off()
gc()

library(sf)

leer_gpx <- function(archivo, ruta, capa = "track_points") {
  capas <- tryCatch(sf::st_layers(archivo)$name, error = function(e) character(0))
  if (length(capas) && !(capa %in% capas)) {
    cand <- intersect(c("track_points", "tracks", "route_points", "routes", "waypoints"), capas)
    if (length(cand)) capa <- cand[1]
  }
  temp <- sf::st_read(archivo, layer = capa, quiet = TRUE)
  coords <- sf::st_coordinates(temp)
  ele  <- if ("ele"  %in% names(temp)) temp$ele  else NA_real_
  time <- if ("time" %in% names(temp)) temp$time else NA
  
  df <- data.frame(lon = coords[, 1], lat = coords[, 2],
                   ele = ele, time = time, ruta = ruta)
  df <- df[, colSums(is.na(df)) < nrow(df), drop = FALSE]
  df <- stats::na.omit(df)
  rownames(df) <- NULL
  df
}


armar_dia <- function(archivos, rutas) {
  partes <- Map(leer_gpx, archivo = archivos, ruta = rutas)
  partes <- unname(partes)              
  out <- do.call(rbind, partes)
  rownames(out) <- NULL                 
  out
}

if (!dir.exists("Processed/gps")) {
  dir.create("Processed/gps")
}

# ---- d1 (240727) ----
gps1 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240727.gpx",
               "Data/gps/GPS Huiscapi 240727.gpx",
               "Data/gps/GPS Loncoche 240727.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps1, file = "Processed/gps/gps1.RData")

# ---- d2 (240728) ----
gps2 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240728.gpx",
               "Data/gps/GPS Huiscapi 240728.gpx",
               "Data/gps/GPS Loncoche 240728.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps2, file = "Processed/gps/gps2.RData")

# ---- d3 (240729) ----
gps3 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240729.gpx",
               "Data/gps/GPS Huiscapi 240729.gpx",
               "Data/gps/GPS Loncoche 240729.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps3, file = "Processed/gps/gps3.RData")

# ---- d4 (240730) ----
gps4 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240730.gpx",
               "Data/gps/GPS Huiscapi 240730.gpx",
               "Data/gps/GPS Loncoche 240730.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps4, file = "Processed/gps/gps4.RData")

# ---- d5 (240812) — sin La Paz ----
gps5 <- armar_dia(
  archivos = c("Data/gps/GPS Huiscapi 240812.gpx",
               "Data/gps/GPS Loncoche 240812.gpx"),
  rutas    = c("huiscapi", "loncoche")
)
save(gps5, file = "Processed/gps/gps5.RData")

# ---- d6 (240813) — Loncoche dividido en 2 archivos ----
gps6 <- armar_dia(
  archivos = c("Data/gps/GPS LaPaz 240813.gpx",
               "Data/gps/GPS Huiscapi 240813.gpx",
               "Data/gps/GPS Loncoche1 240813.gpx",
               "Data/gps/GPS Loncoche2 240813.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche", "loncoche")
)
save(gps6, file = "Processed/gps/gps6.RData")

# ---- d7 (240814) ----
gps7 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240814.gpx",
               "Data/gps/GPS Huiscapi 240814.gpx",
               "Data/gps/GPS Loncoche 240814.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps7, file = "Processed/gps/gps7.RData")

# ---- d8 (240816) ----
gps8 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240816.gpx",
               "Data/gps/GPS Huiscapi 240816.gpx",
               "Data/gps/GPS Loncoche 240816.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps8, file = "Processed/gps/gps8.RData")

# ---- d9 (240819) ----
gps9 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240819.gpx",
               "Data/gps/GPS Huiscapi 240819.gpx",
               "Data/gps/GPS Loncoche 240819.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps9, file = "Processed/gps/gps9.RData")

# ---- d10 (240823) ----
gps10 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240823.gpx",
               "Data/gps/GPS Huiscapi 240823.gpx",
               "Data/gps/GPS Loncoche 240823.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps10, file = "Processed/gps/gps10.RData")

# ---- d11 (240824) ----
gps11 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240824.gpx",
               "Data/gps/GPS Huiscapi 240824.gpx",
               "Data/gps/GPS Loncoche 240824.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps11, file = "Processed/gps/gps11.RData")

# ---- d12 (240827) ----
gps12 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240827.gpx",
               "Data/gps/GPS Huiscapi 240827.gpx",
               "Data/gps/GPS Loncoche 240827.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps12, file = "Processed/gps/gps12.RData")

# ---- d13 (240828) ----
# Nota original: el archivo "GPS Huiscapi 240828.gpx" trae internamente el
# track con nombre LONCO, pero la imagen indica que corresponde a Huiscapi.
gps13 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240828.gpx",
               "Data/gps/GPS Huiscapi 240828.gpx",
               "Data/gps/GPS Loncoche 240828.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps13, file = "Processed/gps/gps13.RData")

# ---- d14 (240911) ----
gps14 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240911.gpx",
               "Data/gps/GPS Huiscapi 240911.gpx",
               "Data/gps/GPS Loncoche 240911.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps14, file = "Processed/gps/gps14.RData")

# ---- d15 (240912) ----
gps15 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240912.gpx",
               "Data/gps/GPS Huiscapi 240912.gpx",
               "Data/gps/GPS Loncoche 240912.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps15, file = "Processed/gps/gps15.RData")

# ---- d16 (240913) ----
gps16 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240913.gpx",
               "Data/gps/GPS Huiscapi 240913.gpx",
               "Data/gps/GPS Loncoche 240913.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps16, file = "Processed/gps/gps16.RData")

# ---- d17 (240914) ----
gps17 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240914.gpx",
               "Data/gps/GPS Huiscapi 240914.gpx",
               "Data/gps/GPS Loncoche 240914.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps17, file = "Processed/gps/gps17.RData")

# ---- d18 (240915) ----
gps18 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240915.gpx",
               "Data/gps/GPS Huiscapi 240915.gpx",
               "Data/gps/GPS Loncoche 240915.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps18, file = "Processed/gps/gps18.RData")

# ---- d19 (240916) ----
gps19 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240916.gpx",
               "Data/gps/GPS Huiscapi 240916.gpx",
               "Data/gps/GPS Loncoche 240916.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps19, file = "Processed/gps/gps19.RData")

# ---- d20 (240926) ----
gps20 <- armar_dia(
  archivos = c("Data/gps/GPS La Paz 240926.gpx",
               "Data/gps/GPS Huiscapi 240926.gpx",
               "Data/gps/GPS Loncoche 240926.gpx"),
  rutas    = c("la paz", "huiscapi", "loncoche")
)
save(gps20, file = "Processed/gps/gps20.RData")

# library(ggplot2)
# library(dplyr)
# library(htmltools)
# 
# etiquetar_dia <- function(df, n) {
#   df$dia <- n
#   df
# }
# 
# gps_list <- list(gps1, gps2, gps3, gps4, gps5,
#                   gps6, gps7, gps8, gps9, gps10,
#                   gps11, gps12, gps13, gps14, gps15,
#                   gps16, gps17, gps18, gps19, gps20)
# 
# colores <- c("la paz" = "#0072B2", "huiscapi" = "#E69F00", "loncoche" = "#009E73")
# 
# if (!dir.exists("Processed/gps/png_dias")) dir.create("Processed/gps/png_dias", recursive = TRUE)
# 
# tags_dias <- list()
# 
# for (i in seq_along(gps_list)) {
#   df <- etiquetar_dia(gps_list[[i]], i)   # toma gps1, gps2, ... gps20 del entorno
# 
#   p <- ggplot(df, aes(x = lon, y = lat, color = ruta)) +
#     geom_point(size = 0.6, alpha = 0.7) +
#     scale_color_manual(values = colores, drop = FALSE) +
#     coord_fixed() +
#     labs(x = "Longitud", y = "Latitud",
#          color = "Ruta", title = paste0("Dia ", i)) +
#     theme_bw()
# 
#   archivo_png <- file.path("Processed/gps/png_dias", paste0("dia_", sprintf("%02d", i), ".png"))
#   ggsave(archivo_png, plot = p, width = 9, height = 6, dpi = 150)
# 
#   tags_dias[[i]] <- tagList(
#     h2(paste0("Dia ", i)),
#     tags$img(src = archivo_png, style = "max-width:900px; width:100%;"),
#     hr()
#   )
# }
# 
# html_final <- tagList(
#   tags$head(tags$title("Tracks GPS por dia - Loncoche")),
#   tags$h1("Tracks GPS por dia de campaña (Loncoche)"),
#   tags_dias
# )
# 
# save_html(html_final, file = "Processed/gps/Tracks_gps1_gps20.html")