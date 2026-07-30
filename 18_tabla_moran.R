# Tabla 4 (una por zona): indice global de Moran para PM2.5 absoluto y para
# ratio mobile/central, calculado sobre cada resolucion de esa zona.
# Usa matriz de pesos por contiguedad reina y solo celdas con >= 3 dias.
#
# Esta tabla es nueva en el pipeline de Loncoche (no existia equivalente
# antes de adaptar desde Talca) - distinta de los mapas LISA locales que ya
# genera 12_AQUIVOY_morans.R: aqui es UN solo numero (Moran's I global) por
# resolucion, no un mapa de clusters.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(spdep)
library(flextable)
library(officer)

CRS_UTM <- "EPSG:32718"
MIN_CELDAS <- 10   # minimo de celdas para intentar el test (misma guarda que en LISA)

if (!file.exists("Processed/med.RData"))          stop("No existe Processed/med.RData")
if (!file.exists("Processed/EmptyRasters.RData")) stop("No existe Processed/EmptyRasters.RData")

load("Processed/med.RData")
load("Processed/EmptyRasters.RData")

if (!dir.exists("Tables")) dir.create("Tables")

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

# Calcula Moran's I global para PM y Ratio en una resolucion. Devuelve NULL
# (en vez de cortar la zona completa) si no hay suficientes celdas o el test
# falla por matriz de pesos degenerada.
calcular_moran_global <- function(df, coords, r_plantilla, res_m) {
  col_celda <- paste0("cell_", res_m)
  df[[col_celda]] <- cellFromXY(r_plantilla, coords)
  
  res_total <- df %>%
    filter(!is.na(.data[[col_celda]])) %>%
    group_by(.data[[col_celda]], date) %>%
    summarise(pm_mean = mean(mov_corr, na.rm = TRUE),
              ratio_mean = mean(ratio_pm25, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data[[col_celda]]) %>%
    summarise(PM_Total = mean(pm_mean, na.rm = TRUE),
              Ratio_Total = mean(ratio_mean, na.rm = TRUE),
              Dias = n(), .groups = "drop") %>%
    filter(Dias >= 3)
  
  if (nrow(res_total) < MIN_CELDAS) {
    message("    ", res_m, "m: ", nrow(res_total), " celdas (< ", MIN_CELDAS, "), se omite.")
    return(NULL)
  }
  
  r_pm <- r_plantilla; values(r_pm) <- NA
  r_pm[res_total[[col_celda]]] <- res_total$PM_Total
  
  r_rat <- r_plantilla; values(r_rat) <- NA
  r_rat[res_total[[col_celda]]] <- res_total$Ratio_Total
  
  r_temp <- c(r_pm, r_rat)
  poligonos <- as.polygons(r_temp, na.rm = TRUE, dissolve = FALSE) %>% st_as_sf()
  
  ok <- tryCatch({
    suppressWarnings({ vecinos <- poly2nb(poligonos, queen = TRUE) })
    pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
    
    vec_pm  <- as.numeric(st_drop_geometry(poligonos)[[1]])
    vec_rat <- as.numeric(st_drop_geometry(poligonos)[[2]])
    
    test_pm  <- moran.test(vec_pm, pesos, zero.policy = TRUE)
    test_rat <- moran.test(vec_rat, pesos, zero.policy = TRUE)
    list(test_pm = test_pm, test_rat = test_rat)
  }, error = function(e) {
    message("    ", res_m, "m: Moran fallo (", conditionMessage(e), "), se omite.")
    NULL
  })
  if (is.null(ok)) return(NULL)
  
  p_val_pm  <- ifelse(ok$test_pm$p.value  < 0.001, "<0.001", sprintf("%.3f", ok$test_pm$p.value))
  p_val_rat <- ifelse(ok$test_rat$p.value < 0.001, "<0.001", sprintf("%.3f", ok$test_rat$p.value))
  
  list(
    PM  = data.frame(Pollutant = "", Resolution = paste(res_m, "m"),
                     `Moran's I` = round(ok$test_pm$estimate[1], 3), `p-value` = p_val_pm, check.names = FALSE),
    Ratio = data.frame(Pollutant = "", Resolution = paste(res_m, "m"),
                       `Moran's I` = round(ok$test_rat$estimate[1], 3), `p-value` = p_val_rat, check.names = FALSE)
  )
}

tabla_moran_zona <- function(slug, area_label, resoluciones) {
  df_a <- med_ok %>% filter(area_slug == slug)
  if (nrow(df_a) == 0) { message("Sin datos en ", area_label, " - no se genera tabla."); return(invisible()) }
  
  sf_a   <- st_as_sf(df_a, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(st_transform(sf_a, CRS_UTM))
  df_df  <- st_drop_geometry(sf_a)
  
  bloques_pm <- list(); bloques_rat <- list()
  for (res in resoluciones) {
    nombre_obj <- paste0("r", res, "_wrap_", slug)
    if (!exists(nombre_obj)) { message("    AVISO: no existe '", nombre_obj, "'"); next }
    r_pl <- unwrap(get(nombre_obj))
    
    message("  ", area_label, " ", res, "m ...")
    resultado <- calcular_moran_global(df_df, coords, r_pl, res)
    if (!is.null(resultado)) {
      bloques_pm[[as.character(res)]]  <- resultado$PM
      bloques_rat[[as.character(res)]] <- resultado$Ratio
    }
  }
  
  if (length(bloques_pm) == 0) { message("Sin resultados validos en ", area_label, " - no se genera tabla."); return(invisible()) }
  
  bloque_pm  <- bind_rows(bloques_pm)
  bloque_rat <- bind_rows(bloques_rat)
  bloque_pm$Pollutant[1]  <- "PM2.5 (ug m-3)"
  bloque_rat$Pollutant[1] <- "PM2.5 ratio"
  
  tabla_moran <- bind_rows(bloque_pm, bloque_rat)
  fila_rat_titulo <- nrow(bloque_pm) + 1
  
  ft <- flextable(tabla_moran)
  ft <- theme_booktabs(ft)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1:2, align = "left", part = "all")
  
  ft <- compose(ft, i = 1, j = 4, part = "header", value = as_paragraph("p-value", as_sup("a")))
  ft <- compose(ft, i = 1, j = 1, part = "body", value = as_paragraph("PM", as_sub("2.5"), " (µg/m", as_sup("3"), ")"))
  ft <- compose(ft, i = fila_rat_titulo, j = 1, part = "body", value = as_paragraph("PM", as_sub("2.5"), " ratio"))
  
  ft <- add_footer_lines(ft, values = paste0("a Global Moran Test - ", area_label, "."))
  ft <- fontsize(ft, part = "footer", size = 9)
  ft <- autofit(ft)
  
  archivo <- sprintf("Tables/Tabla4_Moran_Global_%s.docx", gsub(" ", "", area_label))
  save_as_docx(
    values = setNames(list(ft), paste0("Table 4: Global Moran's I results - ", area_label)),
    path = archivo
  )
  
  if (file.exists(archivo)) {
    chequeo <- officer::docx_summary(officer::read_docx(archivo))
    if (any(grepl("table", chequeo$content_type, ignore.case = TRUE))) {
      message("Tabla guardada y verificada OK: ", normalizePath(archivo))
    } else {
      message("AVISO: el archivo se guardo pero NO contiene ninguna tabla: ", normalizePath(archivo))
    }
  }
}

for (slug in names(config_zonas)) {
  area_label   <- config_zonas[[slug]]$label
  resoluciones <- config_zonas[[slug]]$res
  message("\n--- Procesando ", area_label, " ---")
  tryCatch({
    tabla_moran_zona(slug, area_label, resoluciones)
  }, error = function(e) {
    message("ERROR en ", area_label, ": ", conditionMessage(e))
  })
}

cat("\nTablas 4 (Moran global, una por zona) listas en Tables/.\n")