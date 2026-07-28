# -02_CargaSC
# 
# Este codigo lee los archivos TXT crudos del sensor del Sitio Central
# y los carga diferenciando los mismos tramos de la toma
# de datos que en el monitoreo movil

rm(list = ls())
gc()

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
  
  data$var <- substr(data$var1, 1, 3)
  data$tipo <- 2
  data[b:c, "tipo"] <- 0  # B1
  data[d:e, "tipo"] <- 1  # D1
  data[f:g, "tipo"] <- 3  # D2
  data[h:i, "tipo"] <- 4  # B2
  
  borrar <- function(pat, y) y[!grepl(pat, y$var), , drop = FALSE]
  patrones <- c(",Av", ",Ca", ",Da", ",Ma", ",Mi", ",Ti", ",Un",
                "Cal", "Dat", "dd-", "Log", "Mod", "Not", "Num", "Ser", "Sta", "Tes", "Dur")
  
  df <- data
  for (p in patrones) df <- borrar(p, df)
  
  idx <- grepl("^\\d{2}-\\d{2}-\\d{4},\\d{2}:\\d{2}:\\d{2},", df$var1)
  df <- df[idx, , drop = FALSE]
  
  df$var1 <- gsub("(\\d),(\\d+)$", "\\1.\\2", df$var1)
  
  parts <- strsplit(df$var1, ",", fixed = TRUE)
  mat <- do.call(rbind, parts)
  
  df$Datetime <- as.POSIXct(strptime(paste(mat[, 1], mat[, 2]), "%d-%m-%Y %H:%M:%S"))
  df$pm2_5 <- suppressWarnings(as.numeric(gsub(",", ".", mat[, 3])))
  
  df$var <- NULL
  df$var1 <- NULL
  rownames(df) <- NULL
  
  return(df)
}

if (!dir.exists("Data/Processed/sc")) {  # crea el archivo dentro de processed
  dir.create("Data/Processed/sc", recursive = TRUE)
}

# ---- Dia 1 ---- 
a1 <- abrir(
  a = "Data/Raw/sc/DT261395 240727.txt",
  b = 25, c = 88, #B1
  d = 113, e = 333, #D1
  f = 14887, g = 15074, #D2
  h = 15075, i = 15163 #B2
)

save(a1, file = "Data/Processed/sc/sc_240727.RData")

# ---- Dia 2 ---- 
a2 <- abrir(
  a = "Data/Raw/sc/DT261395 240728.txt",
  b = 25, c = 92, #B1
  d = 117, e = 314, #D1
  f = 16174, g = 16372, #D2
  h = 16397, i = 16461 #B2
)

save(a2, file = "Data/Processed/sc/sc_240728.RData")

# ---- Dia 3 ---- 
a3 <- abrir(
  a = "Data/Raw/sc/DT261395 240729.txt",
  b = 25, c = 86, #B1
  d = 111, e = 300, #D1
  f = 16719, g = 16912, #D2
  h = 16937, i = 16999 #B2
)

save(a3, file = "Data/Processed/sc/sc_240729.RData")

# ---- Dia 4 ----
a4 <- abrir(
  a = "Data/Raw/sc/DT261395 240730.txt",
  b = 25, c = 88, #B1
  d = 113, e = 302, #D1
  f = 16129, g = 16316, #D2
  h = 16341, i = 16403 #B2
)

save(a4, file = "Data/Processed/sc/sc_240730.RData")

# ---- Dia 5 ----
a5 <- abrir(
  a = "Data/Raw/sc/DT261395 240812 comma.txt",
  b = 25, c = 87, #B1
  d = 112, e = 299, #D1
  f = 14350, g = 14538, #D2
  h = 14563, i = 14630 #B2
)

save(a5, file = "Data/Processed/sc/sc_240812.RData")

# ---- Dia 6 ----
a6 <- abrir(
  a = "Data/Raw/sc/DT261395 240813 comma.txt",
  b = 25, c = 88, #B1
  d = 113, e = 301, #D1
  f = 16480, g = 16678, #D2
  h = 16703, i = 16764 #B2
)

save(a6, file = "Data/Processed/sc/sc_240813.RData")

# ---- Dia 7 ----
a7 <- abrir(
  a = "Data/Raw/sc/DT261395 240814 comma.txt",
  b = 25, c = 89, #B1
  d = 114, e = 307, #D1
  f = 19841, g = 20030, #D2
  h = 20055, i = 20117 #B2
)

save(a7, file = "Data/Processed/sc/sc_240814.RData")

# ---- Dia 8 ----
a8 <- abrir(
  a = "Data/Raw/sc/DT261395 240816 comma.txt",
  b = 25, c = 88, #B1
  d = 113, e = 302, #D1
  f = 18100, g = 18284, #D2
  h = 18309, i = 18373 #B2
)

save(a8, file = "Data/Processed/sc/sc_240816.RData")

# ---- Dia 9 ----
a9 <- abrir(
  a = "Data/Raw/sc/DT261395 240819 comma.txt",
  b = 25, c = 86, #B1
  d = 111, e = 299, #D1
  f = 16956, g = 17142, #D2
  h = 17167, i = 17232 #B2
)

save(a9, file = "Data/Processed/sc/sc_240819.RData")

# ---- Dia 10 ----
a10 <- abrir(
  a = "Data/Raw/sc/DT261395 240823 comma.txt",
  b = 25, c = 86, #B1
  d = 111, e = 296, #D1
  f = 17633, g = 17823, #D2
  h = 17848, i = 17909 #B2
)

save(a10, file = "Data/Processed/sc/sc_240823.RData")

# ---- Dia 11 ----
a11 <- abrir(
  a = "Data/Raw/sc/DT261395 240824 comma.txt",
  b = 25, c = 87, #B1
  d = 112, e = 294, #D1
  f = 17365, g = 17547, #D2
  h = 17572, i = 17634 #B2
)

save(a11, file = "Data/Processed/sc/sc_240824.RData")

# ---- Dia 12 ----
a12 <- abrir(
  a = "Data/Raw/sc/DT261395 240827 comma.txt",
  b = 25, c = 89, #B1
  d = 114, e = 300, #D1
  f = 16182, g = 16368, #D2
  h = 16393, i = 16459 #B2
)

save(a12, file = "Data/Processed/sc/sc_240827.RData")

# ---- Dia 13 ----
a13 <- abrir(
  a = "Data/Raw/sc/DT261395 240828 comma.txt",
  b = 25, c = 101, #B1
  d = 126, e = 309, #D1
  f = 17557, g = 17740, #D2
  h = 17765, i = 17828 #B2
)

save(a13, file = "Data/Processed/sc/sc_240828.RData")

# ---- Dia 14 ----
a14 <- abrir(
  a = "Data/Raw/sc/DT261395 240911 comma.txt",
  b = 25, c = 90, #B1
  d = 115, e = 299, #D1
  f = 17593, g = 17776, #D2
  h = 17801, i = 17863 #B2
)

save(a14, file = "Data/Processed/sc/sc_240911.RData")

# ---- Dia 15 ----
a15 <- abrir(
  a = "Data/Raw/sc/DT261395 240912 comma.txt",
  b = 25, c = 87, #B1
  d = 112, e = 295, #D1
  f = 16671, g = 16858, #D2
  h = 16883, i = 16944 #B2
)

save(a15, file = "Data/Processed/sc/sc_240912.RData")

# ---- Dia 16 ----
a16 <- abrir(
  a = "Data/Raw/sc/DT261395 240913 comma.txt",
  b = 25, c = 87, #B1
  d = 112, e = 296, #D1
  f = 17255, g = 17437, #D2
  h = 17462, i = 17523 #B2
)

save(a16, file = "Data/Processed/sc/sc_240913.RData")

# ---- Dia 17 ----
a17 <- abrir(
  a = "Data/Raw/sc/DT261395 240914 comma.txt",
  b = 25, c = 88, #B1
  d = 113, e = 295, #D1
  f = 18730, g = 18922, #D2
  h = 18947, i = 19010 #B2
)

save(a17, file = "Data/Processed/sc/sc_240914.RData")

# ---- Dia 18 ----
a18 <- abrir(
  a = "Data/Raw/sc/DT261395 240915 comma.txt",
  b = 25, c = 86, #B1
  d = 111, e = 295, #D1
  f = 16237, g = 16421, #D2
  h = 16446, i = 16508 #B2
)

save(a18, file = "Data/Processed/sc/sc_240915.RData")

# ---- Dia 19 ----
a19 <- abrir(
  a = "Data/Raw/sc/DT261395 240916 comma.txt",
  b = 25, c = 87, #B1
  d = 112, e = 293, #D1
  f = 17407, g = 17600, #D2
  h = 17625, i = 17686 #B2
)

save(a19, file = "Data/Processed/sc/sc_240916.RData")

# ---- Dia 20 ----
a20 <- abrir(
  a = "Data/Raw/sc/DT261395 240926 comma.txt",
  b = 25, c = 87, #B1
  d = 112, e = 295, #D1
  f = 17442, g = 17625, #D2
  h = 17650, i = 17713 #B2
)

save(a20, file = "Data/Processed/sc/sc_240926.RData")

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
# 
# colores <- c("Blanco" = "#9E9E9E", "Duplicado" = "#E69F00", "Muestra" = "#0072B2")
# 
# pdf("Series_a1_a20.pdf", width = 11, height = 6)
# for (i in 1:20) {
#   df <- etiquetar(get(paste0("a", i)))   # toma d1, d2, ... d20 del entorno
# 
#   p <- ggplot(df, aes(x = Datetime, y = pm * 1000, color = etapa)) +
#     geom_point(size = 0.6) +
#     scale_color_manual(values = colores, drop = FALSE) +
#     labs(x = "Hora", y = expression(PM[2.5] ~ "(" * mu * g/m^3 * ")"),
#          color = "Etapa", title = paste0("d", i)) +
#     theme_bw()
# 
#   print(p)
# }
# dev.off()