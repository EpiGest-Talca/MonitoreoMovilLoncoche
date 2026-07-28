# -09_MapasChile
# 
# Este codigo genera una cascada de 7 mapas satelitales a distinta escala,
# desde el detalle urbano de cada localidad hasta el contexto nacional,
# usando las capas administrativas de Chile y los poligonos urbanos
# digitalizados en 08

# Nota: cada mapa usa una imagen satelital de fondo (esri-imagery) via
#       OpenStreetMap::openmap, con margen del 5% alrededor del poligono
#       de borde; el grosor del borde y la escala/norte se ajustan segun
#       el nivel de zoom (mapas mas alejados usan borde mas fino).
#
#       Fixes aplicados:
#       - Las dimensiones del TIFF se calculan preservando el aspecto real
#         del mapa descargado (map_base$bbox); si el ancho resultante es
#         menor al minimo, se escalan ANCHO y ALTO juntos (no solo el
#         ancho), para no generar margenes blancos por el asp=TRUE interno
#         de plot.OpenStreetMap.
#       - El zoom de los mapas urbanos pequeños (La Paz, Huiscapi, Loncoche)
#         se subio para evitar que el raster se vea borroso al imprimirse
#         grande (antes zoom=16 para las 3 localidades).

rm(list = ls())
gc()

library(OpenStreetMap)
library(sf)
library(prettymapr)
library(chilemapas)
library(grDevices)

if (!dir.exists("Figs")) dir.create("Figs")
if (!dir.exists("Figs/mapaschile")) dir.create("Figs/mapaschile")

# ------------------------------------------------------------------
# 1. Capas base macro
# ------------------------------------------------------------------
chile_sf <- st_as_sf(generar_regiones()) %>% st_transform(4326)
chile_continental <- st_crop(st_union(chile_sf),
                             st_bbox(c(xmin=-75.5, ymin=-57, xmax=-66, ymax=-17), crs=4326))

region_araucania <- subset(chile_sf, codigo_region == "09")
prov_sf <- st_as_sf(generar_provincias()) %>% st_transform(4326)
prov_cautin <- subset(prov_sf, codigo_provincia == "091")
comunas_sf <- st_as_sf(mapa_comunas) %>% st_transform(4326)
comuna_loncoche_completa <- subset(comunas_sf, codigo_comuna == "09109")

# ------------------------------------------------------------------
# 2. Polígonos Urbanos (Loncoche, Huiscapi y La Paz) - digitizados a mano
# ------------------------------------------------------------------
dir_urbanos <- "Data/Shape/shapesurbanos"
poly_loncoche <- st_read(file.path(dir_urbanos, "zona_urbana_loncoche.shp"), quiet = TRUE) %>% st_transform(4326)
poly_huiscapi <- st_read(file.path(dir_urbanos, "zona_urbana_huiscapi.shp"), quiet = TRUE) %>% st_transform(4326)
poly_lapaz    <- st_read(file.path(dir_urbanos, "zona_urbana_la_paz.shp"),   quiet = TRUE) %>% st_transform(4326)

# st_union es la forma más robusta de unir múltiples geometrías sin que colapsen las columnas de atributos
tres_urbanos <- st_union(c(st_geometry(poly_loncoche), st_geometry(poly_huiscapi), st_geometry(poly_lapaz)))

# ------------------------------------------------------------------
# 3. Helper: dimensiones del TIFF preservando aspecto real
# ------------------------------------------------------------------
calcular_dimensiones <- function(aspecto, alto_base = 10, minimo = 3) {
  ancho <- alto_base * aspecto
  if (ancho < minimo) {
    factor <- minimo / ancho
    ancho <- minimo
    alto_base <- alto_base * factor
  }
  list(ancho = ancho, alto = alto_base)
}

# ------------------------------------------------------------------
# 4. Función de exportación (Sin textos ni puntos, solo polígonos)
# ------------------------------------------------------------------
exportar_mapa_satelital <- function(poligono_borde, poligono_achurado, nombre_archivo,
                                    nivel_zoom, titulo_mapa, color_exterior) {
  
  caja_4326 <- st_bbox(poligono_borde)
  margen_lon <- (caja_4326["xmax"] - caja_4326["xmin"]) * 0.05
  margen_lat <- (caja_4326["ymax"] - caja_4326["ymin"]) * 0.05
  
  map_base <- openmap(c(caja_4326["ymax"] + margen_lat, caja_4326["xmin"] - margen_lon),
                      c(caja_4326["ymin"] - margen_lat, caja_4326["xmax"] + margen_lon),
                      type = "esri-imagery", zoom = nivel_zoom)
  
  # Extraer solo la geometría para evitar errores de ploteo
  borde_3857 <- st_transform(st_geometry(poligono_borde), 3857)
  achurado_3857 <- st_transform(st_geometry(poligono_achurado), 3857)
  
  # Aspecto real desde el bbox del mapa descargado (no del poligono de borde)
  bb_map <- map_base$bbox
  aspecto <- as.numeric((bb_map$p2[1] - bb_map$p1[1]) / (bb_map$p1[2] - bb_map$p2[2]))
  dim_fig <- calcular_dimensiones(aspecto)
  
  tiff(paste0("Figs/mapaschile/", nombre_archivo, ".tif"), units="in", res=300,
       width = dim_fig$ancho, height = dim_fig$alto)
  par(mai=c(0,0,0,0), xaxs="i", yaxs="i")
  
  plot(map_base)
  
  plot(achurado_3857, density=8, angle=45, col=adjustcolor("yellow", 0.4), lwd=2, border="yellow", add=TRUE)
  
  grosor <- ifelse(nivel_zoom <= 5, 0.6, 3)
  plot(borde_3857, border=color_exterior, lwd=grosor, col=NA, add=TRUE)
  
  # Título
  legend("top", legend=titulo_mapa, bty="n", text.col="white", cex=2.5, text.font=2, inset=0.02)
  
  prettymapr::addnortharrow(scale=0.8)
  try(prettymapr::addscalebar(label.col="white"), silent=TRUE)
  box(lwd=3)
  dev.off()
}

# ------------------------------------------------------------------
# 5. Generación de la Cascada (7 mapas)
# ------------------------------------------------------------------
# zoom subido de 16 a 18 para los 3 mapas urbanos (evita borrosidad al
# imprimir grande, ya que el area cubierta es muy pequeña)
exportar_mapa_satelital(poly_loncoche, poly_loncoche, "FigA1_Urban_Loncoche", 18, "Urban Loncoche", "yellow")
exportar_mapa_satelital(poly_huiscapi, poly_huiscapi, "FigA2_Urban_Huiscapi", 18, "Urban Huiscapi", "yellow")
exportar_mapa_satelital(poly_lapaz, poly_lapaz, "FigA3_Urban_La_Paz", 18, "Urban La Paz", "yellow")

exportar_mapa_satelital(comuna_loncoche_completa, tres_urbanos, "FigB_Loncoche_Commune", 13, "Loncoche Commune", "cyan")
exportar_mapa_satelital(prov_cautin, comuna_loncoche_completa, "FigC_Cautin_Province", 9, "Cautin Province", "cyan")
exportar_mapa_satelital(region_araucania, prov_cautin, "FigD_Araucania_Region", 8, "Araucania Region", "cyan")
exportar_mapa_satelital(chile_continental, region_araucania, "FigE_Chile", 5, " ", "cyan")
