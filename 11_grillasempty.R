# -11_GrillasVacias
# 
# Este codigo genera grillas raster vacias a distintas resoluciones para
# cada localidad (Loncoche, Huiscapi, La Paz), recortadas al limite urbano
# digitalizado en 08, y las visualiza sobre un mapa satelital para revisar
# la cobertura de cada resolucion antes de usarlas en la interpolacion
# espacial, estructura de la base de datos:


rm(list = ls())
gc()

library(terra)
library(OpenStreetMap)
library(sf)

CRS_UTM <- "EPSG:32718"

if (!dir.exists("Figs")) dir.create("Figs")
if (!dir.exists("Processed")) dir.create("Processed")
if (!dir.exists("Figs/Grillas")) dir.create("Figs/Grillas", recursive = TRUE)

config_resoluciones <- list(
  "Loncoche" = c(50, 75, 100, 150),
  "Huiscapi" = c(50, 75, 100),
  "La_Paz"   = c(25, 50)
)

# ------------------------------------------------------------------
# Limites urbanos digitizados a mano (Processed/Shape/shapesurbanos)
# ------------------------------------------------------------------
dir_urbanos <- "Processed/Shape/shapesurbanos"
poly_loncoche <- st_read(file.path(dir_urbanos, "zona_urbana_loncoche.shp"), quiet = TRUE) |> st_transform(4326)
poly_huiscapi <- st_read(file.path(dir_urbanos, "zona_urbana_huiscapi.shp"), quiet = TRUE) |> st_transform(4326)
poly_lapaz    <- st_read(file.path(dir_urbanos, "zona_urbana_la_paz.shp"),   quiet = TRUE) |> st_transform(4326)

poligonos_4326 <- list(
  "Loncoche" = poly_loncoche,
  "Huiscapi" = poly_huiscapi,
  "La_Paz"   = poly_lapaz
)

poligonos_utm <- list(
  "Loncoche" = st_transform(poly_loncoche, CRS_UTM),
  "Huiscapi" = st_transform(poly_huiscapi, CRS_UTM),
  "La_Paz"   = st_transform(poly_lapaz, CRS_UTM)
)

procesar_localidad <- function(nombre_loc, poli_utm, poli_4326, resoluciones_loc) {
  res_max <- max(resoluciones_loc)
  bb <- st_bbox(poli_utm)
  xmin_real <- as.numeric(floor(bb["xmin"] / res_max) * res_max)
  ymin_real <- as.numeric(floor(bb["ymin"] / res_max) * res_max)
  xmax_real <- as.numeric(ceiling(bb["xmax"] / res_max) * res_max)
  ymax_real <- as.numeric(ceiling(bb["ymax"] / res_max) * res_max)
  
  ancho_lx <- xmax_real - xmin_real
  alto_ly  <- ymax_real - ymin_real
  
  caja_ll <- st_bbox(poli_4326)
  m_lon <- (caja_ll["xmax"] - caja_ll["xmin"]) * 0.05
  m_lat <- (caja_ll["ymax"] - caja_ll["ymin"]) * 0.05
  map_base <- openmap(c(caja_ll["ymax"] + m_lat, caja_ll["xmin"] - m_lon),
                      c(caja_ll["ymin"] - m_lat, caja_ll["xmax"] + m_lon),
                      type = "esri-imagery", zoom = 15)
  
  ancho_mapa <- map_base$bbox$p2[1] - map_base$bbox$p1[1]
  alto_mapa  <- map_base$bbox$p1[2] - map_base$bbox$p2[2]
  proporcion <- alto_mapa / ancho_mapa
  
  rasters_wrap_loc <- list()
  
  for (res in resoluciones_loc) {
    r <- rast(xmin = xmin_real, xmax = xmin_real + ancho_lx,
              ymin = ymin_real, ymax = ymin_real + alto_ly,
              resolution = res, crs = CRS_UTM)
    values(r) <- 0
    r_recortado <- mask(r, vect(poli_utm))
    
    nombre_archivo <- paste0("Figs/Grillas/Fig2_EmptyRaster_", nombre_loc, "_", res, "m.tif")
    tiff(nombre_archivo, units = "in", res = 300, width = 10, height = 10 * proporcion)
    par(mar = c(0, 0, 2.5, 0), xaxs = "i", yaxs = "i")
    
    puntos_merc <- project(as.points(r_recortado), "EPSG:3857")
    coords <- crds(puntos_merc)
    
    plot(map_base)
    pt_size <- if (res >= 150) 0.6 else if (res == 100) 0.5 else if (res == 75) 0.35 else if (res == 50) 0.2 else 0.05
    points(x = coords[, 1], y = coords[, 2], col = "cyan", pch = 19, cex = pt_size)
    plot(st_geometry(st_transform(poli_4326, 3857)), border = "yellow", lwd = 2, add = TRUE)
    
    title(main = paste(gsub("_", " ", nombre_loc), "-", res, "m"), col.main = "white", cex.main = 1.8)
    box(col = "black", lwd = 4, which = "figure")
    dev.off()
    
    nombre_obj <- paste0("r", res, "_wrap_", tolower(nombre_loc))
    rasters_wrap_loc[[nombre_obj]] <- wrap(r_recortado)
  }
  return(rasters_wrap_loc)
}

todos_los_rasters <- list()
for (loc in names(poligonos_utm)) {
  cat("Procesando:", loc, "...\n")
  rasters_loc <- procesar_localidad(loc, poligonos_utm[[loc]], poligonos_4326[[loc]], config_resoluciones[[loc]])
  todos_los_rasters <- c(todos_los_rasters, rasters_loc)
}

list2env(todos_los_rasters, envir = .GlobalEnv)
save(list = names(todos_los_rasters), file = "Processed/EmptyRasters.RData")
