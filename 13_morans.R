# Mapas LISA (Local Moran) del PM2.5 corregido (mov_corr) y del ratio
# movil/central (mov_corr / sc_corr), por zona y resolucion.
# Clasifica cada celda en los 4 cuadrantes de Moran + "Not Significant".
# Mismo motor de ploteo que 11_rasters.R: maptiles + tidyterra + ggplot2,
# basemap recortado al extent real de los poligonos, zoom dinamico por
# tamano, con titulo/leyenda flotantes, flecha de norte, escala grafica y
# sin bordes negros.
#
# Se corre el LISA sobre DOS variables distintas porque responden preguntas
# distintas:
#   - PM2.5 (mov_corr): clusters espaciales de concentracion -> hotspots
#     de exposicion real (Fig5b)
#   - Ratio (mov_corr/sc_corr): clusters espaciales de discrepancia entre
#     el movil y el central -> diagnostico de representatividad del sensor
#     movil, no de contaminacion en si (Fig5)
# Exigen minimo 3 dias medidos por celda (Dias >= 3), igual que en Talca,
# para que una celda con una sola pasada puntual no entre al analisis como
# si fuera representativa del promedio.

rm(list = ls()); gc()

library(terra)
library(sf)
library(dplyr)
library(spdep)
library(maptiles)
library(tidyterra)
library(ggplot2)
library(ggspatial)   # annotation_north_arrow() y annotation_scale()

CRS_UTM <- "EPSG:32718"
MIN_CELDAS <- 10   # minimo de celdas para intentar el LISA
MIN_DIAS   <- 3    # minimo de dias medidos por celda

load("Processed/med.RData")
load("Processed/EmptyRasters.RData")

if (!dir.exists("Figs/Mapas_Finales")) dir.create("Figs/Mapas_Finales", recursive = TRUE)
if (!dir.exists("Processed/Shapefiles_QGIS")) dir.create("Processed/Shapefiles_QGIS", recursive = TRUE)

med <- med %>%
  mutate(area_slug = tolower(gsub(" ", "_", ruta)))

colores_leyenda <- c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "pink",
                     "Low-High" = "lightblue", "Not Significant" = "gray80")
# Not Significant queda mas transparente para que los clusters reales resalten
alpha_lisa <- c("High-High" = 0.75, "Low-Low" = 0.75, "High-Low" = 0.75,
                "Low-High" = 0.75, "Not Significant" = 0.35)

# zoom aproximado de Esri World Imagery segun el "diametro" real del area (m)
zoom_por_extent <- function(diag_m) {
  if (diag_m < 600)  return(19)
  if (diag_m < 1200) return(18)
  if (diag_m < 3000) return(17)
  if (diag_m < 8000) return(16)
  return(15)
}

# ------------------------------------------------------------------
# Motor generico de LISA + ploteo, parametrizado por la variable a analizar.
# var_col      : nombre de la columna en df (ya filtrada de NA) a usar
# titulo_base  : texto base del titulo flotante, ej. "Local Moran's I PM2.5"
# nombre_shp   : prefijo del shapefile de salida
# nombre_fig   : prefijo del archivo de figura (Fig5 = ratio, Fig5b = pm2.5)
# ------------------------------------------------------------------
generar_lisa_map <- function(df, coords, r_plantilla, res_m, area_label, zlab,
                             var_col, titulo_base, nombre_shp, nombre_fig) {
  
  col_celda <- paste0("cell_", res_m)
  df[[col_celda]] <- cellFromXY(r_plantilla, coords)
  
  # valor por celda-dia -> celda-campana; exige minimo MIN_DIAS por celda
  res_total <- df %>%
    filter(!is.na(.data[[col_celda]])) %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(val_mean = mean(.data[[var_col]], na.rm = TRUE), .groups = "drop") %>%
    group_by(.data[[col_celda]]) %>%
    summarise(Val_Total = mean(val_mean, na.rm = TRUE), Dias = n(), .groups = "drop") %>%
    filter(Dias >= MIN_DIAS)
  
  # GUARD 1: muy pocas celdas
  if (nrow(res_total) < MIN_CELDAS) {
    message("  ", area_label, " ", res_m, "m [", nombre_fig, "]: ", nrow(res_total),
            " celdas (< ", MIN_CELDAS, "), se omite LISA.")
    return(invisible(NULL))
  }
  
  # GUARD 2: sin varianza (no se puede estandarizar)
  if (sd(res_total$Val_Total, na.rm = TRUE) == 0 || is.na(sd(res_total$Val_Total, na.rm = TRUE))) {
    message("  ", area_label, " ", res_m, "m [", nombre_fig, "]: variable sin varianza, se omite LISA.")
    return(invisible(NULL))
  }
  
  r_val <- r_plantilla
  values(r_val) <- NA
  r_val[res_total[[col_celda]]] <- res_total$Val_Total
  
  poligonos <- as.polygons(r_val, na.rm = TRUE, dissolve = FALSE) %>% st_as_sf()
  vec_val <- as.numeric(st_drop_geometry(poligonos)[[1]])
  
  # LISA dentro de tryCatch por si la matriz de pesos queda degenerada
  ok <- tryCatch({
    suppressWarnings({ vecinos <- poly2nb(poligonos, queen = TRUE) })
    pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
    lisa  <- localmoran(vec_val, pesos, zero.policy = TRUE)
    
    z_val    <- as.numeric(scale(vec_val))
    z_lag    <- lag.listw(pesos, z_val, zero.policy = TRUE)
    p_values <- lisa[, 5]
    
    poligonos$Cluster <- "Not Significant"
    sig <- 0.05
    poligonos$Cluster[z_val > 0 & z_lag > 0 & p_values <= sig] <- "High-High"
    poligonos$Cluster[z_val < 0 & z_lag < 0 & p_values <= sig] <- "Low-Low"
    poligonos$Cluster[z_val > 0 & z_lag < 0 & p_values <= sig] <- "High-Low"
    poligonos$Cluster[z_val < 0 & z_lag > 0 & p_values <= sig] <- "Low-High"
    poligonos$Cluster <- factor(poligonos$Cluster,
                                levels = c("High-High", "Low-Low", "High-Low",
                                           "Low-High", "Not Significant"))
    TRUE
  }, error = function(e) { message("  ", area_label, " ", res_m, "m [", nombre_fig, "]: LISA fallo (",
                                   conditionMessage(e), "), se omite."); FALSE })
  if (!isTRUE(ok)) return(invisible(NULL))
  
  # Shapefile para QGIS
  suppressWarnings(st_write(poligonos,
                            sprintf("Processed/Shapefiles_QGIS/%s_%s_%dm.shp", nombre_shp, zlab, res_m),
                            delete_dsn = TRUE, quiet = TRUE))
  
  # --- basemap satelital recortado al extent real de los poligonos LISA ---
  bb  <- st_bbox(poligonos)
  pad <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) * 0.08
  ext_buf <- ext(bb["xmin"] - pad, bb["xmax"] + pad, bb["ymin"] - pad, bb["ymax"] + pad)
  
  diag_m <- sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2)
  sat <- get_tiles(vect(ext_buf, crs = CRS_UTM),
                   provider = "Esri.WorldImagery", zoom = zoom_por_extent(diag_m), crop = TRUE)
  
  aspecto <- as.numeric((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))
  
  # Titulo como texto flotante DENTRO del panel (no labs(title=...), que
  # reserva una franja de canvas aparte y genera borde negro).
  x_centro <- (bb["xmin"] + bb["xmax"]) / 2
  y_titulo <- bb["ymax"] + pad * 0.55
  
  g <- ggplot() +
    geom_spatraster_rgb(data = sat, maxcell = 5e6) +
    geom_sf(data = poligonos, aes(fill = Cluster, alpha = Cluster),
            color = "black", linewidth = 0.1) +
    scale_fill_manual(values = colores_leyenda, name = "LISA Clusters", drop = FALSE) +
    scale_alpha_manual(values = alpha_lisa, guide = "none") +
    annotate("label", x = x_centro, y = y_titulo,
             label = sprintf("%s - %s - %d m", titulo_base, area_label, res_m),
             color = "white", fill = "black", alpha = 0.6, label.size = 0,
             fontface = "bold", size = 5) +
    # Escala grafica en km, abajo a la izquierda
    annotation_scale(location = "bl", width_hint = 0.25, unit_category = "metric",
                     style = "bar", line_col = "white", text_col = "white",
                     bar_cols = c("black", "white"), text_cex = 0.8,
                     pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
    # Flecha de norte, arriba a la derecha
    annotation_north_arrow(location = "tr", which_north = "true",
                           height = unit(1.6, "cm"), width = unit(1.6, "cm"),
                           pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
                           style = north_arrow_fancy_orienteering(
                             fill = c("white", "black"), text_col = "white")) +
    coord_sf(xlim = c(bb["xmin"] - pad, bb["xmax"] + pad),
             ylim = c(bb["ymin"] - pad, bb["ymax"] + pad), expand = FALSE) +
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
  # negros. Titulo, leyenda, norte y escala quedan flotando sobre el mapa.
  ancho_mapa <- 10
  alto_mapa  <- ancho_mapa / aspecto
  
  ggsave(sprintf("Figs/Mapas_Finales/%s_%s_%dm.tif", nombre_fig, zlab, res_m),
         g, width = ancho_mapa, height = alto_mapa, dpi = 300, units = "in")
  
  message("  ", area_label, " ", res_m, "m [", nombre_fig, "]: LISA OK (", nrow(res_total), " celdas).")
  invisible(NULL)
}

config_zonas <- list(
  "loncoche" = list(label = "Loncoche", res = c(50, 75, 100, 150)),
  "huiscapi" = list(label = "Huiscapi", res = c(50, 75, 100)),
  "la_paz"   = list(label = "La Paz",   res = c(25, 50))
)

# ------------------------------------------------------------------
# Preparacion de datos por variable
# ------------------------------------------------------------------

# --- PM2.5 corregido (Fig5b): solo requiere mov_corr valido ---
med_pm <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(area_slug))

# --- Ratio movil/central (Fig5): requiere sc_corr valido y ratio acotado ---
med_ratio <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr),
         sc_corr > 0, !is.na(area_slug)) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

for (slug in names(config_zonas)) {
  area_label   <- config_zonas[[slug]]$label
  resoluciones <- config_zonas[[slug]]$res
  zlab <- gsub(" ", "", area_label)
  
  # --- PM2.5 ---
  df_pm <- med_pm %>% filter(area_slug == slug)
  if (nrow(df_pm) == 0) {
    message("Sin datos de PM2.5 en ", area_label, " - se salta Fig5b.")
  } else {
    sf_pm     <- st_as_sf(df_pm, coords = c("lon", "lat"), crs = 4326)
    coords_pm <- st_coordinates(st_transform(sf_pm, CRS_UTM))
    df_pm_df  <- st_drop_geometry(sf_pm)
    
    for (res in resoluciones) {
      nombre_obj <- paste0("r", res, "_wrap_", slug)
      r_pl <- unwrap(get(nombre_obj))
      
      message("LISA PM2.5 en ", area_label, " a ", res, " m ...")
      generar_lisa_map(df_pm_df, coords_pm, r_pl, res, area_label, zlab,
                       var_col = "mov_corr",
                       titulo_base = "Local Moran's I PM2.5",
                       nombre_shp = "LISA_Hotspots_PM25",
                       nombre_fig = "Fig5b_LISA_Hotspots_PM25")
    }
  }
  
  # --- Ratio movil/central ---
  df_rat <- med_ratio %>% filter(area_slug == slug)
  if (nrow(df_rat) == 0) {
    message("Sin datos de ratio en ", area_label, " - se salta Fig5.")
  } else {
    sf_rat     <- st_as_sf(df_rat, coords = c("lon", "lat"), crs = 4326)
    coords_rat <- st_coordinates(st_transform(sf_rat, CRS_UTM))
    df_rat_df  <- st_drop_geometry(sf_rat)
    
    for (res in resoluciones) {
      nombre_obj <- paste0("r", res, "_wrap_", slug)
      r_pl <- unwrap(get(nombre_obj))
      
      message("LISA Ratio en ", area_label, " a ", res, " m ...")
      generar_lisa_map(df_rat_df, coords_rat, r_pl, res, area_label, zlab,
                       var_col = "ratio_pm25",
                       titulo_base = "Local Moran's I PM2.5 Ratio",
                       nombre_shp = "LISA_Hotspots_Ratio",
                       nombre_fig = "Fig5_LISA_Hotspots_Ratio")
    }
  }
}

cat("\n¡Mapas LISA (Fig5 ratio + Fig5b PM2.5) procesados y exportados a shapefiles!\n")