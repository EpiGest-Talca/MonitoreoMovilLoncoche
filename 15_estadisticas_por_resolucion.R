# Tabla de estadisticas resumen de PM2.5 y ratio DENTRO de las celdas, por
# resolucion. UN ARCHIVO POR ZONA (Loncoche / Huiscapi / La Paz), cada uno con
# las resoluciones de esa zona. Solo celdas con N>=3 dias.
#
# Metodologia (igual que Talca): media celda-dia -> media celda-campana ->
# estadisticas (p05/p25/mediana/media/p75/p95) sobre las celdas.

rm(list = ls()); gc()

library(terra)
library(sf)
library(dplyr)
library(flextable)
library(officer)

CRS_UTM <- "EPSG:32718"

message("Working directory: ", getwd())

if (!file.exists("Processed/med.RData"))          stop("No existe Processed/med.RData")
if (!file.exists("Processed/EmptyRasters.RData")) stop("No existe Processed/EmptyRasters.RData")

load("Processed/med.RData")
load("Processed/EmptyRasters.RData")

if (!dir.exists("Tables")) dir.create("Tables")
message("Tablas se guardaran en: ", normalizePath("Tables"))

med <- med %>%
  mutate(area_slug = tolower(gsub(" ", "_", ruta)))

# Ratio a nivel punto (mismo filtro que en 12_Rasterizacion_Ratio.R)
med_ok <- med %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mov_corr), !is.na(sc_corr),
         sc_corr > 0, !is.na(area_slug)) %>%
  mutate(ratio_pm25 = mov_corr / sc_corr) %>%
  filter(is.finite(ratio_pm25), ratio_pm25 < 10)

message("med_ok: ", nrow(med_ok), " filas totales, zonas: ",
        paste(names(table(med_ok$area_slug)), collapse = ", "))

config_zonas <- list(
  "loncoche" = list(label = "Loncoche", res = c(50, 75, 100, 150)),
  "huiscapi" = list(label = "Huiscapi", res = c(50, 75, 100)),
  "la_paz"   = list(label = "La Paz",   res = c(25, 50))
)

# Verificacion temprana: confirmar que los rasters esperados existen en el
# entorno (si EmptyRasters.RData no trae alguno, esto lo dice ANTES de fallar
# a medio armar la tabla)
for (slug in names(config_zonas)) {
  for (res in config_zonas[[slug]]$res) {
    nombre_obj <- paste0("r", res, "_wrap_", slug)
    if (!exists(nombre_obj)) {
      message("AVISO: no existe el objeto '", nombre_obj, "' en Processed/EmptyRasters.RData")
    }
  }
}

# Estadisticas de un vector (devuelve fila con N y percentiles; NA si vacio)
fila_stats <- function(etiqueta, x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(data.frame(Resolution = etiqueta, N_cells = 0L,
                      p05 = NA, p25 = NA, Median = NA, Mean = NA, p75 = NA, p95 = NA))
  }
  data.frame(
    Resolution = etiqueta, N_cells = length(x),
    p05 = round(quantile(x, 0.05), 2), p25 = round(quantile(x, 0.25), 2),
    Median = round(median(x), 2), Mean = round(mean(x), 2),
    p75 = round(quantile(x, 0.75), 2), p95 = round(quantile(x, 0.95), 2)
  )
}

# Para una zona: recorre sus resoluciones (leyendo r{res}_wrap_{slug} desde
# EmptyRasters.RData, igual que 11_rasters.R / 12_Rasterizacion_Ratio.R) y
# arma bloques PM y ratio
stats_por_zona <- function(df_df, coords, slug, resoluciones) {
  pm_filas <- list(); rat_filas <- list()
  for (res in resoluciones) {
    nombre_obj <- paste0("r", res, "_wrap_", slug)
    r_pl <- unwrap(get(nombre_obj))
    
    d <- df_df
    d$cell <- cellFromXY(r_pl, coords)
    d <- d %>% filter(!is.na(cell))
    message("    ", slug, " ", res, "m: ", nrow(d), " puntos asignados a celda, ",
            length(unique(d$cell)), " celdas distintas")
    
    res_dia <- d %>%
      group_by(cell, date) %>%
      summarise(pm = mean(mov_corr, na.rm = TRUE),
                rat = mean(ratio_pm25, na.rm = TRUE), .groups = "drop")
    res_total <- res_dia %>%
      group_by(cell) %>%
      summarise(PM = mean(pm, na.rm = TRUE), RAT = mean(rat, na.rm = TRUE),
                Dias = n(), .groups = "drop") %>%
      filter(Dias >= 3)
    message("    ", slug, " ", res, "m: ", nrow(res_total), " celdas con >=3 dias")
    
    etiqueta <- paste(res, "m")
    pm_filas[[as.character(res)]]  <- fila_stats(etiqueta, res_total$PM)
    rat_filas[[as.character(res)]] <- fila_stats(etiqueta, res_total$RAT)
  }
  list(PM = bind_rows(pm_filas), Ratio = bind_rows(rat_filas))
}

# Construye y guarda el docx de una zona
tabla_docx_zona <- function(slug, area_label, resoluciones) {
  df_a <- med_ok %>% filter(area_slug == slug)
  if (nrow(df_a) == 0) { message("Sin datos en ", area_label, " - no se genera tabla."); return(invisible()) }
  message("  ", area_label, ": ", nrow(df_a), " puntos de ratio disponibles")
  
  sf_a   <- st_as_sf(df_a, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(st_transform(sf_a, CRS_UTM))
  df_df  <- st_drop_geometry(sf_a)
  
  bloques <- stats_por_zona(df_df, coords, slug, resoluciones)
  
  titulo_pm  <- data.frame(Resolution = "PM2.5 (ug/m3)", N_cells = NA,
                           p05 = NA, p25 = NA, Median = NA, Mean = NA, p75 = NA, p95 = NA)
  titulo_rat <- data.frame(Resolution = "PM2.5 ratio", N_cells = NA,
                           p05 = NA, p25 = NA, Median = NA, Mean = NA, p75 = NA, p95 = NA)
  
  tabla <- bind_rows(titulo_pm, bloques$PM, titulo_rat, bloques$Ratio)
  names(tabla) <- c("Resolution", "N cells", "p05", "p25", "Median", "Mean", "p75", "p95")
  
  fila_rat_titulo <- nrow(bloques$PM) + 2   # fila donde empieza el bloque ratio
  
  ft <- flextable(tabla)
  ft <- add_header_row(ft, values = c("", "Statistics within cells"), colwidths = c(1, 7))
  ft <- theme_booktabs(ft)
  ft <- colformat_double(ft, digits = 2, na_str = "")
  ft <- colformat_int(ft, na_str = "")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  
  ft <- flextable::compose(ft, i = 1, j = 1, part = "body",
                           value = as_paragraph(
                             as_chunk("PM", props = fp_text_default(underlined = TRUE)),
                             as_chunk("2.5", props = fp_text_default(vertical.align = "subscript", underlined = TRUE)),
                             as_chunk(" (ug/m", props = fp_text_default(underlined = TRUE)),
                             as_chunk("3", props = fp_text_default(vertical.align = "superscript", underlined = TRUE)),
                             as_chunk(")", props = fp_text_default(underlined = TRUE))))
  ft <- flextable::compose(ft, i = fila_rat_titulo, j = 1, part = "body",
                           value = as_paragraph(
                             as_chunk("PM", props = fp_text_default(underlined = TRUE)),
                             as_chunk("2.5", props = fp_text_default(vertical.align = "subscript", underlined = TRUE)),
                             as_chunk(" ratio", props = fp_text_default(underlined = TRUE)),
                             as_chunk("a", props = fp_text_default(vertical.align = "superscript"))))
  
  ft <- add_footer_lines(ft, values = paste0(
    "a Ratio of mobile measurement compared to the central site (located in Loncoche). ",
    "Only cells with at least 3 measurement days are included."))
  ft <- fontsize(ft, part = "footer", size = 9)
  ft <- autofit(ft)
  
  archivo <- sprintf("Tables/Tabla_Resolution_Stats_%s.docx", gsub(" ", "", area_label))
  save_as_docx(
    values = setNames(list(ft), paste0("Summary statistics by spatial resolution - ", area_label)),
    path = archivo)
  
  if (file.exists(archivo)) {
    chequeo <- officer::docx_summary(officer::read_docx(archivo))
    if (any(grepl("table", chequeo$content_type, ignore.case = TRUE))) {
      message("Tabla guardada y verificada OK: ", normalizePath(archivo))
    } else {
      message("AVISO: el archivo se guardo pero NO contiene ninguna tabla: ", normalizePath(archivo))
    }
  } else {
    message("AVISO: save_as_docx no tiro error pero el archivo no aparece en disco: ", archivo)
  }
}

# ------------------------------------------------------------------
# Un archivo por zona (cada una aislada en tryCatch)
# ------------------------------------------------------------------
for (slug in names(config_zonas)) {
  area_label   <- config_zonas[[slug]]$label
  resoluciones <- config_zonas[[slug]]$res
  message("\n--- Procesando ", area_label, " ---")
  
  tryCatch({
    tabla_docx_zona(slug, area_label, resoluciones)
  }, error = function(e) {
    message("ERROR en ", area_label, ": ", conditionMessage(e))
  })
}

cat("\nTablas por resolucion (una por zona) listas en Tables/.\n")