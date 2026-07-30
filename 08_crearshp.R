# -08_ShapesUrbanos
# 
# Este codigo permite digitalizar manualmente los poligonos que delimitan
# la zona urbana de cada localidad de campaña (La Paz, Loncoche, Huiscapi),
# dibujando directamente sobre un mapa interactivo centrado en las
# coordenadas aproximadas de cada una

# Nota: la digitalizacion es manual e interactiva (mapedit::drawFeatures);
#       el codigo abre un mapa por localidad donde hay que dibujar el
#       poligono y presionar "Done" antes de pasar a la siguiente. El zoom
#       de cada mapa esta ajustado por localidad segun su tamaño relativo
#       (18 para La Paz y Huiscapi, 15 para Loncoche).

rm(list = ls())
gc()

library(sf)
library(mapedit)
library(leaflet)
library(maptiles)
library(tidyterra)
library(ggplot2)


if (!dir.exists("Processed/Shape/shapesurbanos")) dir.create("Processed/Shape/shapesurbanos", recursive = TRUE)

# coordenadas aproximadas de cada localidad (ajusta lng/lat según corresponda)
coords <- list(
  la_paz   = list(lng = -72.71157908967322, lat = -39.41052420074523, zoom = 18),
  loncoche = list(lng = -72.63404824771222, lat = -39.36815877752536, zoom = 15),
  huiscapi = list(lng = -72.40349790257375, lat = -39.29801703317173, zoom = 18)
)

mapa_zoom <- function(c) {
  leaflet() |> addProviderTiles("OpenStreetMap") |> setView(lng = c$lng, lat = c$lat, zoom = c$zoom)
}

# --- digitización: uno a la vez, dibujas el polígono y click "Done" ---
zona_la_paz   <- mapedit::drawFeatures(mapa_zoom(coords$la_paz))
zona_loncoche <- mapedit::drawFeatures(mapa_zoom(coords$loncoche))
zona_huiscapi <- mapedit::drawFeatures(mapa_zoom(coords$huiscapi))

# --- proyección a CRS consistente con el resto del pipeline ---
zona_la_paz   <- st_transform(zona_la_paz,   32718)
zona_loncoche <- st_transform(zona_loncoche, 32718)
zona_huiscapi <- st_transform(zona_huiscapi, 32718)

zona_la_paz$nombre   <- "La Paz - zona urbana"
zona_loncoche$nombre <- "Loncoche - zona urbana"
zona_huiscapi$nombre <- "Huiscapi - zona urbana"

# --- guardar shapefiles en Processed/Shape/shapesurbanos ---
st_write(zona_la_paz,   file.path("Processed/Shape/shapesurbanos", "zona_urbana_la_paz.shp"),   delete_layer = TRUE)
st_write(zona_loncoche, file.path("Processed/Shape/shapesurbanos", "zona_urbana_loncoche.shp"), delete_layer = TRUE)
st_write(zona_huiscapi, file.path("Processed/Shape/shapesurbanos", "zona_urbana_huiscapi.shp"), delete_layer = TRUE)