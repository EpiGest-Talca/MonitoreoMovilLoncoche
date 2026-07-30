# Mapas LISA (Local Moran) del ratio movil/central por zona y resolucion.
# Clasifica cada celda en los 4 cuadrantes de Moran + "Not Significant".
# Mismo motor de ploteo que 11_rasters.R: maptiles + tidyterra + ggplot2,
# basemap recortado al extent real de los poligonos, zoom dinamico por tamano.

rm(list = ls()); gc()

library(terra)
library(sf)
library(dplyr)
library(spdep)
library(maptiles)
library(tidyterra)
library(ggplot2)

CRS_UTM <- "EPSG:32718"
MIN_CELDAS <- 10   # minimo de celdas para intentar el LISA

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

generar_lisa_map <- function(df, coords, r_plantilla, res_m, area_label, zlab) {
  
  col_celda <- paste0("cell_", res_m)
  df[[col_celda]] <- cellFromXY(r_plantilla, coords)
  
  # ratio por celda-dia -> celda-campana; filtro relajado a N>=1 para garantizar vecindad topologica
  res_total <- df %>%
    filter(!is.na(.data[[col_celda]])) %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(ratio_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data[[col_celda]]) %>%
    summarise(Ratio_Total = mean(ratio_mean, na.rm = TRUE), Dias = n(), .groups = "drop") %>%
    filter(Dias >= 1)
  
  # GUARD 1: muy pocas celdas
  if (nrow(res_total) < MIN_CELDAS) {
    message("  ", area_label, " ", res_m, "m: ", nrow(res_total),
            " celdas (< ", MIN_CELDAS, "), se omite LISA.")
    return(invisible(NULL))
  }
  
  # GUARD 2: sin varianza (no se puede estandarizar)
  if (sd(res_total$Ratio_Total, na.rm = TRUE) == 0 || is.na(sd(res_total$Ratio_Total, na.rm = TRUE))) {
    message("  ", area_label, " ", res_m, "m: ratio sin varianza, se omite LISA.")
    return(invisible(NULL))
  }
  
  r_ratio <- r_plantilla
  values(r_ratio) <- NA
  r_ratio[res_total[[col_celda]]] <- res_total$Ratio_Total
  
  poligonos <- as.polygons(r_ratio, na.rm = TRUE, dissolve = FALSE) %>% st_as_sf()
  vec_ratio <- as.numeric(st_drop_geometry(poligonos)[[1]])
  
  # LISA dentro de tryCatch por si la matriz de pesos queda degenerada
  ok <- tryCatch({
    suppressWarnings({ vecinos <- poly2nb(poligonos, queen = TRUE) })
    pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
    lisa  <- localmoran(vec_ratio, pesos, zero.policy = TRUE)
    
    z_ratio  <- as.numeric(scale(vec_ratio))
    z_lag    <- lag.listw(pesos, z_ratio, zero.policy = TRUE)
    p_values <- lisa[, 5]
    
    poligonos$Cluster <- "Not Significant"
    sig <- 0.05
    poligonos$Cluster[z_ratio > 0 & z_lag > 0 & p_values <= sig] <- "High-High"
    poligonos$Cluster[z_ratio < 0 & z_lag < 0 & p_values <= sig] <- "Low-Low"
    poligonos$Cluster[z_ratio > 0 & z_lag < 0 & p_values <= sig] <- "High-Low"
    poligonos$Cluster[z_ratio < 0 & z_lag > 0 & p_values <= sig] <- "Low-High"
    poligonos$Cluster <- factor(poligonos$Cluster,
                                levels = c("High-High", "Low-Low", "High-Low",
                                           "Low-High", "Not Significant"))
    TRUE
  }, error = function(e) { message("  ", area_label, " ", res_m, "m: LISA fallo (",
                                   conditionMessage(e), "), se omite."); FALSE })
  if (!isTRUE(ok)) return(invisible(NULL))
  
  # Shapefile para QGIS
  suppressWarnings(st_write(poligonos,
                            sprintf("Processed/Shapefiles_QGIS/LISA_Hotspots_%s_%dm.shp", zlab, res_m),
                            delete_dsn = TRUE, quiet = TRUE))
  
  # --- basemap satelital recortado al extent real de los poligonos LISA ---
  bb  <- st_bbox(poligonos)
  pad <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) * 0.08
  ext_buf <- ext(bb["xmin"] - pad, bb["xmax"] + pad, bb["ymin"] - pad, bb["ymax"] + pad)
  
  diag_m <- sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2)
  sat <- get_tiles(vect(ext_buf, crs = CRS_UTM),
                   provider = "Esri.WorldImagery", zoom = zoom_por_extent(diag_m), crop = TRUE)
  
  aspecto <- as.numeric((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))
  
  g <- ggplot() +
    geom_spatraster_rgb(data = sat, maxcell = 5e6) +
    geom_sf(data = poligonos, aes(fill = Cluster, alpha = Cluster),
            color = "black", linewidth = 0.1) +
    scale_fill_manual(values = colores_leyenda, name = "LISA Clusters", drop = FALSE) +
    scale_alpha_manual(values = alpha_lisa, guide = "none") +
    coord_sf(xlim = c(bb["xmin"] - pad, bb["xmax"] + pad),
             ylim = c(bb["ymin"] - pad, bb["ymax"] + pad), expand = FALSE) +
    labs(title = sprintf("Local Moran's I PM2.5 Ratio - %s - %d m", area_label, res_m)) +
    theme_void() +
    theme(legend.position = "right",
          legend.background = element_rect(fill = "white", color = "black"),
          plot.title = element_text(color = "white", size = 14, face = "bold",
                                    hjust = 0.5, margin = margin(t = 10, b = -28)),
          plot.background = element_rect(fill = "black", color = NA))
  
  ggsave(sprintf("Figs/Mapas_Finales/Fig5_LISA_Hotspots_%s_%dm.tif", zlab, res_m),
         g, width = 10, height = 10 / aspecto, dpi = 300, units = "in")
  
  message("  ", area_label, " ", res_m, "m: LISA OK (", nrow(res_total), " celdas).")
  invisible(NULL)
}

# ------------------------------------------------------------------
# Filtrado de Ratio a nivel punto
# ------------------------------------------------------------------
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
  
  for (res in resoluciones) {
    nombre_obj <- paste0("r", res, "_wrap_", slug)
    r_pl <- unwrap(get(nombre_obj))
    
    message("LISA en ", area_label, " a ", res, " m ...")
    generar_lisa_map(df_df, coords, r_pl, res, area_label, zlab)
  }
}

cat("\n¡Mapas LISA (Fig5) procesados y exportados a shapefiles!\n")