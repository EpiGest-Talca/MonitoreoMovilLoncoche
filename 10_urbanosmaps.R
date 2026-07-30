# -10_MapaTresZonas
# 
# Este codigo construye los mapas base satelitales de las tres zonas de
# Loncoche (Loncoche, Huiscapi, La Paz), superpone la ruta movil GPS y
# genera la Figura 1 con un panel por zona, ademas de exportar las capas
# a shapefile para uso en QGIS.

# Nota: la caja de cada mapa se calcula a partir de la extension real de
#       los puntos GPS de esa ruta, con un margen del 15% y forzada al
#       aspecto de panel de la figura (ancho/alto = 2.0), para que el mapa
#       llene el panel sin bordes blancos y la ruta no quede lejana ni
#       cortada.

library(OpenStreetMap)
library(sf)
library(prettymapr)

load("Data/Processed/gps/stack_gps.RData")   # gps: lon, lat, ele, time, ruta

# ------------------------------------------------------------------
# Límites urbanos digitizados a mano (Out/shapesurbanos)
# ------------------------------------------------------------------
dir_urbanos <- "Data/Shape/shapesurbanos"
limite_loncoche <- st_read(file.path(dir_urbanos, "zona_urbana_loncoche.shp"), quiet = TRUE) |> st_transform(3857)
limite_huiscapi <- st_read(file.path(dir_urbanos, "zona_urbana_huiscapi.shp"), quiet = TRUE) |> st_transform(3857)
limite_la_paz   <- st_read(file.path(dir_urbanos, "zona_urbana_la_paz.shp"),   quiet = TRUE) |> st_transform(3857)

# ------------------------------------------------------------------
# Dimensiones de la figura -> aspecto de cada panel (todo deriva de aqui)
# ------------------------------------------------------------------
fig_w <- 9      # ancho figura (in)
fig_h <- 16     # alto figura (in)
n_pan <- 3      # paneles (uno por zona)
ASPECTO <- fig_w / (fig_h / n_pan)   # ancho/alto de cada panel = 2.0

PAD  <- 0.15    # margen alrededor de la ruta (15%)
ZOOM <- NULL    # NULL = openmap elige el zoom segun la extension; sube el numero para mas detalle

gps_sf <- st_as_sf(gps, coords = c("lon", "lat"), crs = 4326)

# ------------------------------------------------------------------
# 1. Caja ajustada a la ruta + basemap por zona
# ------------------------------------------------------------------
# Devuelve esquinas c(lat,lon) para openmap: caja en torno a la ruta, con
# margen PAD, expandida en el eje corto hasta alcanzar 'aspecto' (ancho/alto).
caja_panel <- function(sf_pts, sf_limite, aspecto, pad = PAD) {
  m_pts <- st_transform(sf_pts, 3857)
  m_lim <- st_transform(sf_limite, 3857)
  
  bb_pts <- st_bbox(m_pts)
  bb_lim <- st_bbox(m_lim)
  
  # bbox combinado: la union de ambos rangos, no solo la ruta
  bb <- c(
    xmin = min(bb_pts["xmin"], bb_lim["xmin"]),
    ymin = min(bb_pts["ymin"], bb_lim["ymin"]),
    xmax = max(bb_pts["xmax"], bb_lim["xmax"]),
    ymax = max(bb_pts["ymax"], bb_lim["ymax"])
  )
  
  cx <- (bb[["xmin"]] + bb[["xmax"]]) / 2
  cy <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  hw <- (bb[["xmax"]] - bb[["xmin"]]) / 2 * (1 + pad)
  hh <- (bb[["ymax"]] - bb[["ymin"]]) / 2 * (1 + pad)
  if (hw / hh < aspecto) hw <- hh * aspecto else hh <- hw / aspecto
  esquinas <- st_sfc(
    st_point(c(cx - hw, cy + hh)),   # noroeste
    st_point(c(cx + hw, cy - hh)),   # sureste
    crs = 3857
  )
  ll <- st_coordinates(st_transform(esquinas, 4326))
  list(ul = c(ll[1, 2], ll[1, 1]), br = c(ll[2, 2], ll[2, 1]))
}

make_map <- function(nombre_ruta, sf_limite, aspecto = ASPECTO, zoom = ZOOM) {
  z  <- subset(gps_sf, ruta == nombre_ruta)
  bx <- caja_panel(z, sf_limite, aspecto)
  openmap(bx$ul, bx$br, type = "esri-imagery", zoom = zoom)
}

map1 <- make_map("loncoche", limite_loncoche)   # Loncoche
map2 <- make_map("huiscapi", limite_huiscapi)   # Huiscapi
map3 <- make_map("la paz",   limite_la_paz)     # La Paz
save(map1, map2, map3, file = "Data/Shape/Loncochemap.RData")

# ------------------------------------------------------------------
# 2. Ruta de cada zona, transformada al CRS del basemap (Mercator 3857)
# ------------------------------------------------------------------
ruta_zona <- function(nombre_ruta) {
  st_transform(subset(gps_sf, ruta == nombre_ruta), 3857)
}


# ------------------------------------------------------------------
# 3. Exportar capas para QGIS
# ------------------------------------------------------------------
dir_qgis <- "Out/Shapefiles_Mapas_Base"
if (!dir.exists(dir_qgis)) dir.create(dir_qgis, recursive = TRUE)
suppressWarnings(st_write(gps_sf, paste0(dir_qgis, "/Ruta_Movil_GPS.shp"),
                          delete_dsn = TRUE, quiet = TRUE))
suppressWarnings(st_write(limite_loncoche, paste0(dir_qgis, "/Limite_Urbano_Loncoche.shp"), delete_dsn = TRUE, quiet = TRUE))
suppressWarnings(st_write(limite_huiscapi, paste0(dir_qgis, "/Limite_Urbano_Huiscapi.shp"), delete_dsn = TRUE, quiet = TRUE))
suppressWarnings(st_write(limite_la_paz,   paste0(dir_qgis, "/Limite_Urbano_LaPaz.shp"),    delete_dsn = TRUE, quiet = TRUE))
# suppressWarnings(st_write(sites_sf,        paste0(dir_qgis, "/Estaciones_Monitoreo.shp"),  delete_dsn = TRUE, quiet = TRUE))

# ------------------------------------------------------------------
# 4. Figura 1: un panel por zona
# ------------------------------------------------------------------
if (!dir.exists("Figs")) dir.create("Figs")

tiff("Figs/Fig1_Loncoche_ThreeMaps.tif", units = "in", res = 600, width = fig_w, height = fig_h)
par(mfrow = c(n_pan, 1), mai = c(0, 0, 0, 0))

paneles <- list(
  list(mapa = map1, ruta = "loncoche", titulo = "Loncoche", limite = limite_loncoche),
  list(mapa = map2, ruta = "huiscapi", titulo = "Huiscapi", limite = limite_huiscapi),
  list(mapa = map3, ruta = "la paz",  titulo = "La Paz",   limite = limite_la_paz)
)

for (p in paneles) {
  plot(p$mapa)
  
  plot(st_geometry(ruta_zona(p$ruta)),
       col = "yellow", pch = 18, cex = 0.3, add = TRUE)
  
  # --- límite urbano digitizado ---
  plot(st_geometry(p$limite), border = "cyan", lwd = 3, add = TRUE)
  
  # --- estaciones (cuando las tengas), transformadas a 3857 ---
  # plot(st_geometry(st_transform(sites_sf, 3857)), pch = 21, cex = 3.5, bg = "red", col = "white", lwd = 2, add = TRUE)
  
  legend("topleft", inset = 0.02, bg = "white", title = p$titulo,
         legend = c("Mobile Route", "Urban limit"), pch = c(15, NA), lty = c(NA, 1),
         col = c("yellow", "white"), pt.cex = 2, lwd = c(NA, 3), cex = 1.2)
  
  prettymapr::addnortharrow(scale = 1.2, lwd = 2)
  prettymapr::addscalebar(lwd = 2, label.cex = 1.2)
  box(col = "black", lwd = 3, which = "plot")
}

box(col = "black", lwd = 10, which = "outer")
dev.off()

