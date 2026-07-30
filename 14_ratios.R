# Ratio movil/central (mov_corr / sc_corr) rasterizado por zona y resolucion.
# Escala divergente fija (blanco ~1.0), igual para las 3 zonas.
# Mismo motor de ploteo que 11_rasters.R: maptiles + tidyterra + ggplot2,
# basemap recortado al extent real del raster, zoom dinamico por tamano.

rm(list = ls()); gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(maptiles)
library(tidyterra)
library(ggplot2)

CRS_UTM <- "EPSG:32718"

load("Processed/med.RData")
load("Processed/EmptyRasters.RData")

if (!dir.exists("Figs/Mapas_Finales")) dir.create("Figs/Mapas_Finales", recursive = TRUE)
if (!dir.exists("Processed/Rasters_Finales")) dir.create("Processed/Rasters_Finales", recursive = TRUE)

# Escala divergente fija del ratio (blanco centrado en 0.9-1.1)
RAT_CORTES_BASE <- c(0, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 2.0)
RAT_ETIQUETAS   <- c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "0.9 - 1.1",
                     "1.1 - 1.3", "1.3 - 1.5", "1.5 - 1.7", "1.7 - 2.0", "> 2.0")
RAT_COLORES <- c("#004385", "#0066b3", "#0083bf", "#cbe2f0", "#ffffff",
                 "#ffccaa", "#ffab7f", "#f0755a", "#d9604b", "#ac0023")
names(RAT_COLORES) <- RAT_ETIQUETAS

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

procesar_ratio <- function(df, coords, r_plantilla, res_m, area_label, zlab) {
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
    summarise(rat_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("H_", format(date, "%Y%m%d"), "_", sprintf("%02d", hour)))
  matriz_hora <- res_hora %>% select(all_of(col_celda), layer_name, rat_mean) %>%
    pivot_wider(names_from = layer_name, values_from = rat_mean)
  
  res_dia <- df %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(rat_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    mutate(layer_name = paste0("D_", format(date, "%Y%m%d")))
  matriz_dia <- res_dia %>% select(all_of(col_celda), layer_name, rat_mean) %>%
    pivot_wider(names_from = layer_name, values_from = rat_mean)
  
  res_total <- res_dia %>%
    group_by(.data[[col_celda]]) %>%
    summarise(Total_Rat = mean(rat_mean, na.rm = TRUE), Dias_Medidos = n(), .groups = "drop") %>%
    select(all_of(col_celda), Total_Rat)
  
  stack_horas <- inyectar_a_raster(r_plantilla, matriz_hora, col_celda)
  stack_dias  <- inyectar_a_raster(r_plantilla, matriz_dia,  col_celda)
  stack_total <- inyectar_a_raster(r_plantilla, res_total,   col_celda)
  raster_final <- stack_total[["Total_Rat"]]
  
  writeRaster(raster_final,
              filename = sprintf("Processed/Rasters_Finales/QGIS_Ratio_%s_%dm.tif", zlab, res_m),
              overwrite = TRUE)
  
  val_max <- suppressWarnings(max(values(raster_final), na.rm = TRUE))
  message("  ", area_label, " ", res_m, "m: ratio max = ", round(val_max, 2),
          ", ", sum(!is.na(values(raster_final))), " celdas con dato")
  
  if (!is.finite(val_max)) {
    message("  ", area_label, " ", res_m, "m: sin datos de ratio, no se genera figura.")
    return(list(horas = wrap(stack_horas), dias = wrap(stack_dias), total = wrap(stack_total)))
  }
  
  cortes <- c(RAT_CORTES_BASE, max(val_max, 2.0 + 0.01) + 0.01)
  
  # clasificar a categorias discretas con la escala divergente fija
  rcl <- cbind(cortes[-length(cortes)], cortes[-1], seq_along(RAT_ETIQUETAS))
  raster_clas <- classify(raster_final, rcl, include.lowest = TRUE)
  levels(raster_clas) <- data.frame(id = seq_along(RAT_ETIQUETAS), categoria = RAT_ETIQUETAS)
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
  
  g <- ggplot() +
    geom_spatraster_rgb(data = sat, maxcell = 5e6) +
    geom_tile(data = df_clas, aes(x = x, y = y, fill = categoria),
              color = "black", linewidth = 0.1, alpha = 0.75) +
    scale_fill_manual(values = RAT_COLORES, na.value = NA, na.translate = FALSE,
                      name = "Ratio\n(Mobile/Central)", drop = FALSE) +
    coord_sf(xlim = c(ext_r[1] - pad, ext_r[2] + pad),
             ylim = c(ext_r[3] - pad, ext_r[4] + pad), expand = FALSE) +
    labs(title = sprintf("PM2.5 Ratio (Mobile/Central) - %s - %d m", area_label, res_m)) +
    theme_void() +
    theme(legend.position = "right",
          legend.background = element_rect(fill = "white", color = "black"),
          plot.title = element_text(color = "white", size = 14, face = "bold",
                                    hjust = 0.5, margin = margin(t = 10, b = -28)),
          plot.background = element_rect(fill = "black", color = NA))
  
  ggsave(sprintf("Figs/Mapas_Finales/Fig4_RatioMap_%s_%dm.tif", zlab, res_m),
         g, width = 10, height = 10 / aspecto, dpi = 300, units = "in")
  
  list(horas = wrap(stack_horas), dias = wrap(stack_dias), total = wrap(stack_total))
}

# ------------------------------------------------------------------
# Preparacion de datos y Ratio a nivel punto
# ------------------------------------------------------------------
med <- med %>%
  mutate(area_slug = tolower(gsub(" ", "_", ruta)))

med_ok <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr),
         sc_corr > 0, !is.na(area_slug)) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

config_zonas <- list(
  "loncoche" = list(label = "Loncoche", res = c(50, 75, 100, 150)),
  "huiscapi" = list(label = "Huiscapi", res = c(50, 75, 100)),
  "la_paz"   = list(label = "La Paz",   res = c(25, 50))
)

for (slug in names(config_zonas)) {
  area_label   <- config_zonas[[slug]]$label
  resoluciones <- config_zonas[[slug]]$res
  zlab <- gsub(" ", "", area_label)
  
  df_a <- med_ok %>% filter(area_slug == slug)
  if (nrow(df_a) == 0) { message("Sin datos de ratio en ", area_label, " - se salta."); next }
  
  sf_a   <- st_as_sf(df_a, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(st_transform(sf_a, CRS_UTM))
  df_df  <- st_drop_geometry(sf_a)
  
  resultados_rat <- list()
  for (res in resoluciones) {
    nombre_obj <- paste0("r", res, "_wrap_", slug)
    r_pl <- unwrap(get(nombre_obj))
    
    message("Rasterizando Ratio en ", area_label, " a ", res, " m ...")
    resultados_rat[[as.character(res)]] <- procesar_ratio(df_df, coords, r_pl, res, area_label, zlab)
  }
  save(resultados_rat, file = sprintf("Processed/Rasters_Finales/Stacks_Ratio_%s.RData", slug))
}

cat("\n¡Fig4 + GeoTIFF + stacks de ratio por zona listos!\n")