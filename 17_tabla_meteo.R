# Tabla 2 (una por zona): estadisticas diarias de PM2.5 y ratio agregadas por
# categorias de variables meteorologicas (humedad, temperatura, velocidad y
# direccion de viento) del sitio SINCA Villarrica (referencia compartida por
# las 3 zonas). La agregacion espacial se hace primero a nivel de celda
# (una resolucion "media" por zona, ver res_meteo abajo) y luego se promedia
# al dia completo para cada dia de campana.
#
# SUPUESTO A VERIFICAR: asume Processed/sinca/sinca.RData (diario) con columnas
# date, mean_temp, mean_hr, mean_ws, mean_wd. Los cortes de categorizacion
# (7.5 °C, 95% HR, 1 m/s, cuadrantes de viento) son los del protocolo del
# estudio, heredados sin cambio del pipeline de Talca.

rm(list = ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

CRS_UTM <- "EPSG:32718"

if (!file.exists("Processed/med.RData"))          stop("No existe Processed/med.RData")
if (!file.exists("Processed/sinca/sinca.RData"))  stop("No existe Processed/sinca/sinca.RData")
if (!file.exists("Processed/EmptyRasters.RData")) stop("No existe Processed/EmptyRasters.RData")

load("Processed/med.RData")
load("Processed/sinca/sinca.RData")
load("Processed/EmptyRasters.RData")

if (!dir.exists("Tables")) dir.create("Tables")

med <- med %>%
  mutate(area_slug = tolower(gsub(" ", "_", ruta)))

med_ok <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr),
         sc_corr > 0, !is.na(area_slug)) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

# Resolucion "media" usada para la agregacion espacial previa, una por zona.
# Ajustable: la idea es evitar que dias con muchos puntos GPS muy juntos
# (vehiculo detenido) sesguen el promedio diario.
config_zonas <- list(
  "loncoche" = list(label = "Loncoche", res_meteo = 100),
  "huiscapi" = list(label = "Huiscapi", res_meteo = 75),
  "la_paz"   = list(label = "La Paz",   res_meteo = 50)
)

# Devuelve una fila con N, p05, p25, mediana, media, p75, p95 para PM y Ratio.
# Si se pasan columna_categoria y valor_categoria filtra antes de agregar
calcular_fila <- function(df, columna_categoria = NULL, valor_categoria = NULL) {
  if (!is.null(columna_categoria)) {
    df <- df %>% filter(!!sym(columna_categoria) == valor_categoria)
  }
  if (nrow(df) == 0) {
    return(data.frame(
      N_PM = NA, P05_PM = NA, P25_PM = NA, Med_PM = NA, Mean_PM = NA, P75_PM = NA, P95_PM = NA,
      N_Rat = NA, P05_Rat = NA, P25_Rat = NA, Med_Rat = NA, Mean_Rat = NA, P75_Rat = NA, P95_Rat = NA
    ))
  }
  data.frame(
    N_PM    = nrow(df),
    P05_PM  = round(quantile(df$PM_Dia, 0.05, na.rm = TRUE), 2),
    P25_PM  = round(quantile(df$PM_Dia, 0.25, na.rm = TRUE), 2),
    Med_PM  = round(median(df$PM_Dia, na.rm = TRUE), 2),
    Mean_PM = round(mean(df$PM_Dia, na.rm = TRUE), 2),
    P75_PM  = round(quantile(df$PM_Dia, 0.75, na.rm = TRUE), 2),
    P95_PM  = round(quantile(df$PM_Dia, 0.95, na.rm = TRUE), 2),
    
    N_Rat    = nrow(df),
    P05_Rat  = round(quantile(df$Ratio_Dia, 0.05, na.rm = TRUE), 2),
    P25_Rat  = round(quantile(df$Ratio_Dia, 0.25, na.rm = TRUE), 2),
    Med_Rat  = round(median(df$Ratio_Dia, na.rm = TRUE), 2),
    Mean_Rat = round(mean(df$Ratio_Dia, na.rm = TRUE), 2),
    P75_Rat  = round(quantile(df$Ratio_Dia, 0.75, na.rm = TRUE), 2),
    P95_Rat  = round(quantile(df$Ratio_Dia, 0.95, na.rm = TRUE), 2)
  )
}

tabla_meteo_zona <- function(slug, area_label, res_meteo) {
  df_a <- med_ok %>% filter(area_slug == slug)
  if (nrow(df_a) == 0) { message("Sin datos en ", area_label, " - no se genera tabla."); return(invisible()) }
  
  nombre_obj <- paste0("r", res_meteo, "_wrap_", slug)
  if (!exists(nombre_obj)) { message("AVISO: no existe '", nombre_obj, "' - se omite ", area_label); return(invisible()) }
  r_pl <- unwrap(get(nombre_obj))
  
  sf_a   <- st_as_sf(df_a, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(st_transform(sf_a, CRS_UTM))
  df_df  <- st_drop_geometry(sf_a)
  
  df_df$cell <- cellFromXY(r_pl, coords)
  df_df <- df_df %>% filter(!is.na(cell))
  message("  ", area_label, " (", res_meteo, "m): ", nrow(df_df), " puntos asignados a celda")
  
  # Promedio por celda-dia (para no sesgar dias con muchos puntos), luego
  # promedio espacial por dia
  celdas_diarias <- df_df %>%
    group_by(cell, date) %>%
    summarise(pm_cell = mean(mov_corr, na.rm = TRUE),
              ratio_cell = mean(ratio_pm25, na.rm = TRUE), .groups = "drop")
  
  datos_diarios <- celdas_diarias %>%
    group_by(date) %>%
    summarise(PM_Dia = mean(pm_cell, na.rm = TRUE),
              Ratio_Dia = mean(ratio_cell, na.rm = TRUE), .groups = "drop")
  
  datos_completos <- inner_join(datos_diarios, sinca, by = "date")
  message("  ", area_label, ": ", nrow(datos_completos), " dias con meteorologia cruzada")
  
  if (nrow(datos_completos) == 0) { message("  Sin dias cruzados con SINCA - se omite ", area_label); return(invisible()) }
  
  # Categorizacion meteorologica; cortes del protocolo del estudio
  datos_completos <- datos_completos %>%
    mutate(
      Temp_Cat = ifelse(mean_temp < 7.5, "<7.5 °C", ">=7.5 °C"),
      HR_Cat   = ifelse(is.na(mean_hr), "NA", ifelse(mean_hr < 95, "<95%", ">=95%")),
      WS_Cat   = ifelse(mean_ws < 1, "<1 m/s", ">=1 m/s"),
      WD_Cat   = case_when(
        mean_wd >= 45 & mean_wd < 135 ~ "E",
        mean_wd >= 135 & mean_wd < 225 ~ "S",
        mean_wd >= 225 & mean_wd < 315 ~ "W",
        TRUE ~ "N"
      )
    )
  
  filas <- list(
    cbind(Category = "Overall", calcular_fila(datos_completos)),
    cbind(Category = "Relative Humidity", calcular_fila(datos_completos[0, ])),
    cbind(Category = "<95%", calcular_fila(datos_completos, "HR_Cat", "<95%")),
    cbind(Category = ">=95%", calcular_fila(datos_completos, "HR_Cat", ">=95%")),
    cbind(Category = "NA", calcular_fila(datos_completos, "HR_Cat", "NA")),
    cbind(Category = "Temperature", calcular_fila(datos_completos[0, ])),
    cbind(Category = "<7.5 °C", calcular_fila(datos_completos, "Temp_Cat", "<7.5 °C")),
    cbind(Category = ">=7.5 °C", calcular_fila(datos_completos, "Temp_Cat", ">=7.5 °C")),
    cbind(Category = "Wind Speed", calcular_fila(datos_completos[0, ])),
    cbind(Category = "<1 m/s", calcular_fila(datos_completos, "WS_Cat", "<1 m/s")),
    cbind(Category = ">=1 m/s", calcular_fila(datos_completos, "WS_Cat", ">=1 m/s")),
    cbind(Category = "Wind Direction", calcular_fila(datos_completos[0, ])),
    cbind(Category = "E", calcular_fila(datos_completos, "WD_Cat", "E")),
    cbind(Category = "N", calcular_fila(datos_completos, "WD_Cat", "N")),
    cbind(Category = "S", calcular_fila(datos_completos, "WD_Cat", "S")),
    cbind(Category = "W", calcular_fila(datos_completos, "WD_Cat", "W"))
  )
  
  tabla_export <- bind_rows(filas)
  
  ft <- flextable(tabla_export)
  ft <- set_header_labels(ft,
                          Category = "Category",
                          N_PM = "N (Days)", P05_PM = "p05", P25_PM = "p25", Med_PM = "Median", Mean_PM = "Mean", P75_PM = "p75", P95_PM = "p95",
                          N_Rat = "N (Days)", P05_Rat = "p05", P25_Rat = "p25", Med_Rat = "Median", Mean_Rat = "Mean", P75_Rat = "p75", P95_Rat = "p95")
  
  ft <- add_header_row(ft, values = c("", paste0("PM2.5 (ug m-3) - ", area_label), "PM2.5 ratio"), colwidths = c(1, 7, 7))
  ft <- add_header_row(ft, values = c("", "Daily Aggregated Mobile Measurements"), colwidths = c(1, 14))
  
  ft <- theme_booktabs(ft)
  ft <- fontsize(ft, part = "all", size = 9)
  ft <- padding(ft, padding = 3, part = "all")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  
  ft <- flextable::compose(ft, i = 2, j = 9, part = "header", value = as_paragraph("PM", as_sub("2.5"), " ratio", as_sup("a")))
  ft <- flextable::compose(ft, i = 2, j = 2, part = "header", value = as_paragraph("PM", as_sub("2.5"), " (µg/m", as_sup("3"), ") - ", area_label))
  
  ft <- flextable::compose(ft, i = 2, j = 1, part = "body", value = as_paragraph(as_chunk("Relative Humidity", props = fp_text_default(underlined = TRUE))))
  ft <- flextable::compose(ft, i = 6, j = 1, part = "body", value = as_paragraph(as_chunk("Temperature", props = fp_text_default(underlined = TRUE))))
  ft <- flextable::compose(ft, i = 9, j = 1, part = "body", value = as_paragraph(as_chunk("Wind Speed", props = fp_text_default(underlined = TRUE))))
  ft <- flextable::compose(ft, i = 12, j = 1, part = "body", value = as_paragraph(as_chunk("Wind Direction", props = fp_text_default(underlined = TRUE))))
  
  ft <- padding(ft, i = c(3, 4, 5, 7, 8, 10, 11, 13, 14, 15, 16), j = 1, padding.left = 15)
  
  ft <- add_footer_lines(ft, values = paste0(
    "a Ratio compared to SINCA Villarrica reference site. Spatial aggregation at ", res_meteo,
    " m cell resolution before daily averaging. p05: 5th percentile. p25: 25th percentile. p75: 75th percentile. p95: 95th percentile."))
  ft <- autofit(ft)
  
  sect_properties <- prop_section(
    page_size = page_size(orient = "landscape", width = 11, height = 8.5),
    type = "continuous",
    page_margins = page_mar()
  )
  
  archivo <- sprintf("Tables/Tabla2_Met_Stats_%s.docx", gsub(" ", "", area_label))
  save_as_docx(
    values = setNames(list(ft), paste0("Table 2: Daily mobile PM2.5/ratio by meteorological category - ", area_label)),
    path = archivo,
    pr_section = sect_properties
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
  area_label <- config_zonas[[slug]]$label
  res_meteo  <- config_zonas[[slug]]$res_meteo
  message("\n--- Procesando ", area_label, " ---")
  tryCatch({
    tabla_meteo_zona(slug, area_label, res_meteo)
  }, error = function(e) {
    message("ERROR en ", area_label, ": ", conditionMessage(e))
  })
}

cat("\nTablas 2 (meteorologia, una por zona) listas en Tables/.\n")