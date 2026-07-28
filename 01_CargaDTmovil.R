# Los tildes se omiten en el proyecto por temas de codificacion
# 
# -01_CargaDTmovil
# 
# Este primer codigo lee los archivos TXT crudos del DustTrak y
# las carga diferenciando los diferentes tramos de la toma de datos;
# son 20 dias de campaña en total, estructura de la base de datos:
# 
#   cada uno de los txt contiene la siguiente data:
#   - Datetime: corresponde a la hora del dato medido, resolucion del datetime
#               es segundo a segundo. 
#   
#   - pm2_5: corresponde a la concentracion del aire en el segundo medido.
# 
# Que resultado esperar de este codigo:
#   - cada txt cargado en el environment de R, con sus respecticas variables:
#     * tipo: que diferencia el blanco del dupli y de la muestra para poder 
#             realizar futuras correcciones y el analisis de datos como tal.
#     * Datetime: Tiempo de cada dato con resolucion de segundo.
#     * pm2_5: es el pm2.5 captado por el DustTrak.

rm(list = ls()) #limpian el entorno
gc() #limpia la memoria ram

# Lector para archivos con separador coma
abrir <- function(a, b, c, d, e, f, g, h, i) {
  data <- tryCatch(
    read.delim(a, header = TRUE, row.names = NULL,
               fill = TRUE, check.names = FALSE, comment.char = ""),
    error = function(e1) {
      tmp <- read.delim(a, header = FALSE, row.names = NULL,
                        fill = TRUE, check.names = FALSE, comment.char = "")
      names(tmp) <- paste0("V", seq_len(ncol(tmp)))
      tmp
    }
  )
  
  data$var1 <- data[[1]]
  data[[1]] <- NULL
  
  # Asignar etiqueta de tramo segun los rangos de fila
  data$var <- substr(data$var1, 1, 3)
  data$tipo <- 2
  data[b:c, "tipo"] <- 0  # B1
  data[d:e, "tipo"] <- 1  # D1
  data[f:g, "tipo"] <- 3  # D2
  data[h:i, "tipo"] <- 4  # B2
  
  # Eliminar filas de metadatos que el equipo escribe en el TXT
  borrar <- function(pat, y) y[!grepl(pat, y$var), , drop = FALSE]
  patrones <- c(",Av", ",Ca", ",Da", ",Ma", ",Mi", ",Ti", ",Un",
                "Cal", "Dat", "dd-", "Log", "Mod", "Not", "Num", "Ser", "Sta", "Tes", "Dur")
  
  df <- data
  for (p in patrones) df <- borrar(p, df)
  
  # Conservar solo filas con formato dd-mm-yyyy,HH:MM:SS,
  idx <- grepl("^\\d{2}-\\d{2}-\\d{4},\\d{2}:\\d{2}:\\d{2},", df$var1)
  df <- df[idx, , drop = FALSE]
  
  # Normalizar decimal con coma al final (ej: "12,34" -> "12.34")
  df$var1 <- gsub("(\\d),(\\d+)$", "\\1.\\2", df$var1)
  
  # Separar en Fecha / Hora / Valor
  parts <- strsplit(df$var1, ",", fixed = TRUE)
  mat <- do.call(rbind, parts)
  
  df$Datetime <- as.POSIXct(strptime(paste(mat[, 1], mat[, 2]), "%d-%m-%Y %H:%M:%S"))
  df$pm2_5 <- suppressWarnings(as.numeric(gsub(",", ".", mat[, 3])))
  
  df$var <- NULL
  df$var1 <- NULL
  rownames(df) <- NULL
  
  return(df)
}

if (!dir.exists("Data/Processed")) { #crea la carpeta para guardar los datos procesados
  dir.create("Data/Processed")
}

if (!dir.exists("Data/Processed/movil")) { #crea la carpeta para guardar los datos procesados
  dir.create("Data/Processed/movil")
}

# ---- Dia 1 ---- 
d1 <- abrir(
  a = "Data/Raw/movil/DT5203 240727.txt",
  b = 25, c = 88,    # B1
  d = 113, e = 334,  # D1
  f = 9271, g = 9458,   # D2
  h = 9509, i = 9585    # B2
)[-c(9243:9244), ] #de aqui en adelante si aparece este comando de "-c" es 
# porque habian intervalos de datos que tenia que borrar

save(d1, file = "Data/Processed/movil/movil_240727.RData") # para guardar el procesado 

# ---- Dia 2 ---- 
d2 <- abrir(
  a = "Data/Raw/movil/DT5203 240728.txt",
  b = 391, c = 460,  # B1
  d = 485, e = 686,  # D1
  f = 10646, g = 10841,  # D2
  h = 10855, i = 10929   # B2
)[-c(1:270), ]

save(d2, file = "Data/Processed/movil/movil_240728.RData")

# ---- Dia 3 ---- 
d3 <- abrir(
  a = "Data/Raw/movil/DT5203 240729.txt",
  b = 25, c = 86,    # B1
  d = 111, e = 300,  # D1
  f = 9548, g = 9741,   # D2
  h = 9766, i = 9829    # B2
)

save(d3, file = "Data/Processed/movil/movil_240729.RData")

# ---- Dia 4 ---- 
d4 <- abrir(
  a = "Data/Raw/movil/DT5203 240730.txt",
  b = 25, c = 89,    # B1
  d = 114, e = 303,  # D1
  f = 10495, g = 10683,  # D2
  h = 10708, i = 10771   # B2
)

save(d4, file = "Data/Processed/movil/movil_240730.RData")


#-------------------------------------------------------------------------------
# Este dia tenia dos registros en un solo TXT, se identifico cual correspondia
# a movil y el otro se elimino
#------------------------------------------------------
# ---- Dia 5 ---- valores duplicados revisar
# Descarga manual del equipo (comentarios codigo original)
#d5 <- abrir(
#  a = "Data/Raw/movil/DT5203 240812 comma.txt",
#  b = 25, c = 87,    # B1
#  d = 112, e = 299,  # D1
#  f = 14347, g = 14538,  # D2
#  h = 14539, i = 14791   # B2
#)[-c(14510:23883), ]

d5 <- abrir(
  a = "Data/Raw/movil/DT5203 240812 comma.txt",
  b = 14479, c = 14718,    # B1
  d = 14719, e = 14955,  # D1
  f = 23921, g = 24197,  # D2
  h = 24198, i = 24360   # B2
)[-c(0:14510), ]



save(d5, file = "Data/Processed/movil/movil_240812.RData")

# ---- Dia 6 ----
d6 <- abrir(
  a = "Data/Raw/movil/DT5203 240813 comma.txt",
  b = 25, c = 86,    # B1
  d = 111, e = 300,  # D1
  f = 11041, g = 11246,  # D2
  h = 11271, i = 11336   # B2
)

save(d6, file = "Data/Processed/movil/movil_240813.RData")

# ---- Dia 7 ---- 
d7 <- abrir(
  a = "Data/Raw/movil/DT5203 240814 comma.txt",
  b = 25, c = 116,   # B1
  d = 141, e = 331,  # D1
  f = 12366, g = 12555,  # D2
  h = 12580, i = 12643   # B2
)

save(d7, file = "Data/Processed/movil/movil_240814.RData")

# ---- Dia 8 ---- 
d8 <- abrir(
  a = "Data/Raw/movil/DT5203 240816 comma.txt",
  b = 25, c = 90,    # B1
  d = 115, e = 301,  # D1
  f = 11466, g = 11648,  # D2
  h = 11673, i = 11735   # B2
)

save(d8, file = "Data/Processed/movil/movil_240816.RData")

# ---- Dia 9 ---- 
d9 <- abrir(
  a = "Data/Raw/movil/DT5203 240819 comma.txt",
  b = 25, c = 93,    # B1
  d = 118, e = 308,  # D1
  f = 11021, g = 11211,  # D2
  h = 11236, i = 11301   # B2
)

save(d9, file = "Data/Processed/movil/movil_240819.RData")

# ---- Dia 10 ---- 
d10 <- abrir(
  a = "Data/Raw/movil/DT5203 240823 comma.txt",
  b = 25, c = 86,    # B1
  d = 111, e = 296,  # D1
  f = 11001, g = 11191,  # D2
  h = 11216, i = 11278   # B2
)[-c(10973:10976,10757:10760), ]

save(d10, file = "Data/Processed/movil/movil_240823.RData")

# ---- Dia 11 ----
d11 <- abrir(
  a = "Data/Raw/movil/DT5203 240824 comma.txt",
  b = 25, c = 90,    # B1
  d = 115, e = 297,  # D1
  f = 11024, g = 11208,  # D2
  h = 11233, i = 11298   # B2
)

save(d11, file = "Data/Processed/movil/movil_240824.RData")

# ---- Dia 12 ----
d12 <- abrir(
  a = "Data/Raw/movil/DT5203 240827 comma.txt",
  b = 25, c = 86,    # B1
  d = 111, e = 298,  # D1
  f = 9808, g = 9995,   # D2
  h = 10022, i = 10082  # B2
)

save(d12, file = "Data/Processed/movil/movil_240827.RData")

# ---- Dia 13 ----
d13 <- abrir(
  a = "Data/Raw/movil/DT5203 240828 comma.txt",
  b = 25, c = 87,    # B1
  d = 112, e = 295,  # D1
  f = 10766, g = 10952,  # D2
  h = 10997, i = 11039   # B2
)

save(d13, file = "Data/Processed/movil/movil_240828.RData")

# ---- Dia 14 ----
d14 <- abrir(
  a = "Data/Raw/movil/DT5203 240911 comma.txt",
  b = 25, c = 88,    # B1
  d = 113, e = 298,  # D1
  f = 10696, g = 10882,  # D2
  h = 10907, i = 10969   # B2
)

save(d14, file = "Data/Processed/movil/movil_240911.RData")

# ---- Dia 15 ----
d15 <- abrir(
  a = "Data/Raw/movil/DT5203 240912 comma.txt",
  b = 25, c = 86,    # B1
  d = 111, e = 293,  # D1
  f = 10130, g = 10318,  # D2
  h = 10343, i = 10407   # B2
)

save(d15, file = "Data/Processed/movil/movil_240912.RData")

# ---- Dia 16 ----
d16 <- abrir(
  a = "Data/Raw/movil/DT5203 240913 comma.txt",
  b = 25, c = 87,    # B1
  d = 112, e = 295,  # D1
  f = 10703, g = 10885,  # D2
  h = 10910, i = 10972   # B2
)

save(d16, file = "Data/Processed/movil/movil_240913.RData")

# ---- Dia 17 ----
d17 <- abrir(
  a = "Data/Raw/movil/DT5203 240914 comma.txt",
  b = 25, c = 90,    # B1
  d = 115, e = 297,  # D1
  f = 11240, g = 11427,  # D2
  h = 11452, i = 11514   # B2
)

save(d17, file = "Data/Processed/movil/movil_240914.RData")

# ---- Dia 18 ----
d18 <- abrir(
  a = "Data/Raw/movil/DT5203 240915 comma.txt",
  b = 25, c = 87,    # B1
  d = 112, e = 297,  # D1
  f = 9749, g = 9933,   # D2
  h = 9958, i = 10019   # B2
)

save(d18, file = "Data/Processed/movil/movil_240915.RData")

# ---- Dia 19 ----
d19 <- abrir(
  a = "Data/Raw/movil/DT5203 240916 comma.txt",
  b = 25, c = 89,    # B1
  d = 114, e = 299,  # D1
  f = 10261, g = 10452,  # D2
  h = 10477, i = 10538   # B2
)

save(d19, file = "Data/Processed/movil/movil_240916.RData")

# ---- Dia 20 ----
d20 <- abrir(
  a = "Data/Raw/movil/DT5203 240926 comma.txt",
  b = 25, c = 88,    # B1
  d = 113, e = 297,  # D1
  f = 11243, g = 11430,  # D2
  h = 11455, i = 11520   # B2
)

save(d20, file = "Data/Processed/movil/movil_240926.RData")


# esta parte del codigo era para verificar que todos los "tipos" de datos 
# queden bien clasificados, se ven con la serie de tiempo

# library(ggplot2)
# library(dplyr)
# 
# etiquetar <- function(df) {
#   df %>%
#     mutate(etapa = factor(
#       case_when(
#         tipo %in% c(0, 4) ~ "Blanco",
#         tipo %in% c(1, 3) ~ "Duplicado",
#         tipo == 2         ~ "Muestra"
#       ),
#       levels = c("Blanco", "Duplicado", "Muestra")
#     ))
# }
# colores <- c("Blanco" = "#9E9E9E", "Duplicado" = "#E69F00", "Muestra" = "#0072B2")
# pdf("Series2_d1_d20.pdf", width = 11, height = 6)
# for (i in 1:20) {
#   df <- etiquetar(get(paste0("d", i)))   # toma d1, d2, ... d20 del entorno
#   p <- ggplot(df, aes(x = Datetime, y = pm2_5 * 1000, color = etapa)) +
#     geom_point(size = 0.6) +
#     scale_color_manual(values = colores, drop = FALSE) +
#     labs(x = "Hora", y = expression(PM[2.5] ~ "(" * mu * g/m^3 * ")"),
#          color = "Etapa", title = paste0("d", i)) +
#     theme_bw()
# 
#   print(p)
# }
# dev.off()
