# Rasterizacion final de PM2.5 corregido (mov_corr) por localidad (Loncoche,
# Huiscapi, La Paz) y generacion de mapas categoricos sobre imagen satelital.
#
# Para cada localidad y cada resolucion de grilla definida en config_zonas:
#   1. Asigna cada punto GPS/PM2.5 a una celda de la grilla (cellFromXY)
#   2. Agrega el promedio de mov_corr por celda a nivel hora, dia y total
#      de campana (Total_Mean = promedio de los promedios diarios)
#   3. Exporta el raster continuo (Total_Mean) como GeoTIFF para QGIS
#   4. Clasifica el raster en categorias discretas (cortes especificos por
#      zona) y genera un mapa PNG/TIFF sobre imagen satelital Esri
#      (paleta secuencial khaki-amarillo-naranjo-rojo), con flecha de norte
#      y escala grafica
#
# Los cortes de clasificacion son propios de cada zona (Loncoche llega hasta
# 300+ ug/m3, Huiscapi hasta 90+ ug/m3, La Paz hasta 60+ ug/m3), reflejando
# que la estandarizacion es intra-zona: el objetivo es identificar puntos
# calientes dentro de cada localidad, no comparar magnitudes absolutas entre
# ellas. La escala de La Paz es deliberadamente mas chica que la de Huiscapi
# porque sus concentraciones registradas son mas bajas.

rm(list = ls()); gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(maptiles)
library(tidyterra)
library(ggplot2)
library(ggspatial)   # annotation_north_arrow() y annotation_scale()

CRS_UTM <- "EPSG:32718"

load("Processed/med.RData")
load("Processed/EmptyRasters.RData")

if (!dir.exists("Figs/Mapas_Finales")) dir.create("Figs/Mapas_Finales", recursive = TRUE)
if (!dir.exists("Processed/Rasters_Finales")) dir.create("Processed/Rasters_Finales", recursive = TRUE)

med <- med %>%
  mutate(area_slug = tolower(gsub(" ", "_", ruta)))

med_ok <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(area_slug))

# A partir de los cortes, genera las etiquetas "a - b" + el bin abierto "> ultimo"
generar_etiquetas <- function(cortes_base) {
  n <- length(cortes_base)
  c(paste(cortes_base[-n], cortes_base[-1], sep = " - "), paste0("> ", cortes_base[n]))
}

# sin blanco en el extremo bajo: blanco/transparente queda reservado a "sin dato" (NA)
generar_colores <- function(etiquetas) {
  cols <- colorRampPalette(c("khaki", "yellow", "orange", "red", "darkred"))(length(etiquetas))
  names(cols) <- etiquetas
  cols
}

inyectar_a_raster <- function(plantilla, matriz_datos, id_columna) {
  nombres_capas <- setdiff(names(matriz_datos), id_columna)
  r_stack <- rast(rep(plantilla, length(nombres_capas)))
  names(r_stack) <- nombres_capas
  if (nrow(matriz_datos) > 0) {
    r_stack[matriz_datos[[id_columna]]] <- as.data.frame(matriz_datos[, nombres_capas])
  }
  r_stack
}

# zoom aproximado de Esri World Imagery segun el "diametro" real del area (m)
zoom_por_extent <- function(diag_m) {
  if (diag_m < 600)  return(19)
  if (diag_m < 1200) return(18)
  if (diag_m < 3000) return(17)
  if (diag_m < 8000) return(16)
  return(15)
}

procesar_resolucion <- function(df, coords, r_plantilla, res_m, area_label, zlab,
                                cortes_zona, etiquetas_zona, colores_zona) {
  col_celda <- paste0("cell_", res_m)
  df[[col_celda]] <- cellFromXY(r_plantilla, coords)
  
  n_fuera <- sum(is.na(df[[col_celda]]))
  if (n_fuera > 0) {
    message("  ", area_label, " ", res_m, "m: ", n_fuera, "/", nrow(df),
            " puntos fuera de la grilla (descartados)")
  }
  df <- df %>% filter(!is.na(.data[[col_celda]]))
  
  res_hora <- df %>%
    group_by(.data[[col_celda]], date, hour) %>%
    summarise(pm_mean = mean(mov_corr, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("H_", format(date, "%Y%m%d"), "_", sprintf("%02d", hour)))
  matriz_hora <- res_hora %>% select(all_of(col_celda), layer_name, pm_mean) %>%
    pivot_wider(names_from = layer_name, values_from = pm_mean)
  
  res_dia <- df %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(pm_mean = mean(mov_corr, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("D_", format(date, "%Y%m%d")))
  matriz_dia <- res_dia %>% select(all_of(col_celda), layer_name, pm_mean) %>%
    pivot_wider(names_from = layer_name, values_from = pm_mean)
  
  res_total <- res_dia %>%
    group_by(.data[[col_celda]]) %>%
    summarise(Total_Mean = mean(pm_mean, na.rm = TRUE), Dias_Medidos = n(), .groups = "drop") %>%
    select(all_of(col_celda), Total_Mean)
  
  stack_horas <- inyectar_a_raster(r_plantilla, matriz_hora, col_celda)
  stack_dias  <- inyectar_a_raster(r_plantilla, matriz_dia,  col_celda)
  stack_total <- inyectar_a_raster(r_plantilla, res_total,   col_celda)
  raster_final <- stack_total[["Total_Mean"]]
  
  writeRaster(raster_final,
              filename = sprintf("Processed/Rasters_Finales/QGIS_PM25_%s_%dm.tif", zlab, res_m),
              overwrite = TRUE)
  
  val_max <- suppressWarnings(max(values(raster_final), na.rm = TRUE))
  message("  ", area_label, " ", res_m, "m: PM2.5 max = ", round(val_max, 1),
          " ug/m3, ", sum(!is.na(values(raster_final))), " celdas con dato")
  
  if (!is.finite(val_max)) {
    message("  ", area_label, " ", res_m, "m: sin datos validos, no se genera figura.")
    return(list(horas = wrap(stack_horas), dias = wrap(stack_dias), total = wrap(stack_total)))
  }
  
  cortes <- c(cortes_zona, max(val_max, cortes_zona[length(cortes_zona)] + 0.1) + 0.1)
  
  # clasificar a categorias discretas con la escala propia de esta zona
  rcl <- cbind(cortes[-length(cortes)], cortes[-1], seq_along(etiquetas_zona))
  raster_clas <- classify(raster_final, rcl, include.lowest = TRUE)
  levels(raster_clas) <- data.frame(id = seq_along(etiquetas_zona), categoria = etiquetas_zona)
  names(raster_clas) <- "categoria"
  
  # --- extent real del raster + margen chico; basemap recortado a ESE extent exacto ---
  ext_r <- ext(raster_final)
  pad   <- max(ext_r[2] - ext_r[1], ext_r[4] - ext_r[3]) * 0.08
  ext_buf <- ext(ext_r[1] - pad, ext_r[2] + pad, ext_r[3] - pad, ext_r[4] + pad)
  
  diag_m <- sqrt((ext_r[2] - ext_r[1])^2 + (ext_r[4] - ext_r[3])^2)
  sat <- get_tiles(vect(ext_buf, crs = crs(raster_final)),
                   provider = "Esri.WorldImagery", zoom = zoom_por_extent(diag_m), crop = TRUE)
  
  aspecto <- as.numeric((ext_r[2] - ext_r[1]) / (ext_r[4] - ext_r[3]))
  
  df_clas <- as.data.frame(raster_clas, xy = TRUE, na.rm = TRUE)
  
  # Titulo como texto flotante DENTRO del panel (no como labs(title=...), que
  # reserva una franja de canvas aparte). Se posiciona cerca del borde
  # superior del extent real, centrado horizontalmente.
  x_centro <- (ext_r[1] + ext_r[2]) / 2
  y_titulo <- ext_r[4] + pad * 0.55
  
  g <- ggplot() +
    geom_spatraster_rgb(data = sat, maxcell = 5e6) +
    geom_tile(data = df_clas, aes(x = x, y = y, fill = categoria),
              color = "black", linewidth = 0.1, alpha = 0.7) +
    scale_fill_manual(values = colores_zona, na.value = NA, na.translate = FALSE,
                      name = "PM2.5\n(ug/m3)", drop = FALSE) +
    annotate("label", x = x_centro, y = y_titulo,
             label = sprintf("PM2.5 - %s - %d m", area_label, res_m),
             color = "white", fill = "black", alpha = 0.6, label.size = 0,
             fontface = "bold", size = 5) +
    # Escala grafica en km, abajo a la izquierda (raster esta en CRS_UTM,
    # unidades metricas, por eso el km queda correcto sin transformar)
    annotation_scale(location = "bl", width_hint = 0.25, unit_category = "metric",
                     style = "bar", line_col = "white", text_col = "white",
                     bar_cols = c("black", "white"), text_cex = 0.8,
                     pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
    # Flecha de norte, abajo a la derecha
    annotation_north_arrow(location = "tr", which_north = "true",
                          height = unit(1.6, "cm"), width = unit(1.6, "cm"),
                          pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
                          style = north_arrow_fancy_orienteering(
                            fill = c("white", "black"), text_col = "white"))+
    coord_sf(xlim = c(ext_r[1] - pad, ext_r[2] + pad),
             ylim = c(ext_r[3] - pad, ext_r[4] + pad), expand = FALSE) +
    theme_void() +
    theme(legend.position = c(0.985, 0.5),
          legend.justification = c(1, 0.5),
          legend.background = element_rect(fill = alpha("white", 0.85), color = "black"),
          legend.margin = margin(6, 8, 6, 8),
          legend.key.size = unit(0.9, "lines"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          plot.margin = margin(0, 0, 0, 0))
  
  # Sin ancho extra reservado: el panel ocupa TODO el lienzo, sin bordes
  # negros. Leyenda, titulo, norte y escala quedan flotando sobre el mapa.
  ancho_mapa <- 10
  alto_mapa  <- ancho_mapa / aspecto
  
  ggsave(sprintf("Figs/Mapas_Finales/Fig3_PM25_%s_%dm.tif", zlab, res_m),
         g, width = ancho_mapa, height = alto_mapa,
         dpi = 300, units = "in")
  
  list(horas = wrap(stack_horas), dias = wrap(stack_dias), total = wrap(stack_total))
}

# Cortes de PM2.5 propios de cada zona. 
config_zonas <- list(
  "loncoche" = list(label = "Loncoche", res = c(50, 75, 100, 150), cortes = c(0, 50, 100, 150, 200, 250, 300)),
  "huiscapi" = list(label = "Huiscapi", res = c(50, 75, 100),      cortes = c(0, 20, 40, 60, 80, 100, 120)),
  "la_paz"   = list(label = "La Paz",   res = c(25, 50),           cortes = c(0, 10, 20, 30, 40, 50, 60))
)

for (slug in names(config_zonas)) {
  area_label   <- config_zonas[[slug]]$label
  resoluciones <- config_zonas[[slug]]$res
  zlab <- gsub(" ", "", area_label)
  
  df_a <- med_ok %>% filter(area_slug == slug)
  if (nrow(df_a) == 0) { message("Sin datos PM2.5 en ", area_label, " - se salta."); next }
  
  sf_a   <- st_as_sf(df_a, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(st_transform(sf_a, CRS_UTM))
  df_df  <- st_drop_geometry(sf_a)
  
  cortes_zona    <- config_zonas[[slug]]$cortes
  etiquetas_zona <- generar_etiquetas(cortes_zona)
  colores_zona   <- generar_colores(etiquetas_zona)
  
  resultados <- list()
  for (res in resoluciones) {
    nombre_obj <- paste0("r", res, "_wrap_", slug)
    r_pl <- unwrap(get(nombre_obj))
    
    message("Rasterizando PM2.5 en ", area_label, " a ", res, " m ...")
    resultados[[as.character(res)]] <- procesar_resolucion(df_df, coords, r_pl, res, area_label, zlab,
                                                           cortes_zona, etiquetas_zona, colores_zona)
  }
  save(resultados, file = sprintf("Processed/Rasters_Finales/Stacks_PM25_%s.RData", slug))
}

