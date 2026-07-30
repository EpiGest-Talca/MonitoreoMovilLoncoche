# -05_MediaSinca
# 
# Este codigo carga los datos de referencia de la estacion SINCA (PM2.5) y
# de la estacion meteorologica INIA, y calcula el promedio de esas variables
# solo durante la ventana horaria en que el monitoreo movil y el sitio
# central estuvieron efectivamente en ruta (tipo == 2) cada dia

rm(list = ls())
gc()

library(dplyr)
library(lubridate)
library(purrr)
library(conflicted)

conflict_prefer("filter", "dplyr");conflict_prefer("select", "dplyr");conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr");conflict_prefer("summarise", "dplyr");conflict_prefer("group_by", "dplyr")

if (!dir.exists("Processed/sinca")) {
  dir.create("Processed/sinca", recursive = TRUE)
}

# ------------------------------------------------------------------
# 1) PM2.5
# ------------------------------------------------------------------
sinca_raw <- read.csv("Data/sinca/datosPM25_240720_240930.csv", sep = ";")

sinca_pm25_clean <- sinca_raw %>%
  mutate(
    hora_str   = sprintf("%04d", HORA..HHMM.),
    hora_hh_mm = paste0(substr(hora_str, 1, 2), ":", substr(hora_str, 3, 4)),
    Datetime_sinca = ymd_hm(paste(FECHA..YYMMDD., hora_hh_mm), tz = "America/Santiago"),
    conc = coalesce(Registros.validados, Registros.preliminares)
  ) %>%
  filter(!is.na(conc), !is.na(Datetime_sinca)) %>%
  select(Datetime_sinca, conc)

# ------------------------------------------------------------------
# 2) Meteorologia  (estacion INIA agrometeorologia - un solo CSV horario)   # <-- METEO
# ------------------------------------------------------------------
ARCHIVO_METEO <- "Data/sinca/agrometeorologia.csv" 
WS_A_MS       <- FALSE   # <-- METEO: TRUE si necesitas el viento en m/s en vez de km/h

leer_agromet <- function(ruta, ws_a_ms = WS_A_MS) {
  if (!file.exists(ruta)) return(NULL)
  raw <- read.csv(ruta, sep = ",", skip = 5, header = TRUE,
                  check.names = FALSE, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8")
  names(raw)[1:5] <- c("tiempo",  "Temp","HR", "WS", "WD")
  # Conservar solo filas de datos (botar pie con la cita de INIA y blancos)
  raw <- raw[grepl("^\\d{2}-\\d{2}-\\d{4}", raw$tiempo), , drop = FALSE]
  
  out <- raw %>%
    mutate(
      dt_utc4        = dmy_hm(tiempo, tz = "Etc/GMT+4"),     # dmy (no ymd): DD-MM-YYYY
      Datetime_sinca = with_tz(dt_utc4, "America/Santiago"), # a hora local, mismo instante
      HR   = as.numeric(gsub(",", ".", HR)),
      Temp = as.numeric(gsub(",", ".", Temp)),
      WS   = as.numeric(gsub(",", ".", WS)),
      WD   = as.numeric(gsub(",", ".", WD))
    )
  
  if (ws_a_ms) out$WS <- out$WS / 3.6           # km/h -> m/s
  out$WD[out$WS == 0] <- NA_real_               # calma: direccion indefinida (no sesga media circular)
  
  out %>%
    filter(!is.na(Datetime_sinca)) %>%
    select(Datetime_sinca, Temp, HR, WS, WD)
}


# ---- (B) PM2.5 + meteorologia ----------------------------   # <-- METEO
df_meteo <- leer_agromet(ARCHIVO_METEO)

# full_join conserva las horas de meteo aunque falte el PM2.5 SINCA esa hora
# (el agromet esta completo). Los promedios diarios usan na.rm, asi que las
# horas sin conc no rompen mean_sinca.
sinca_clean <- full_join(sinca_pm25_clean, df_meteo, by = "Datetime_sinca") %>%
  arrange(Datetime_sinca)

# ------------------------------------------------------------------
# 3) Ventana horaria por dia de ruta
#    Para cada dia se define [inicio, fin] combinando los timestamps del movil
#    y del central (solo tramo en ruta, tipo == 2) y se extraen las horas SINCA
#    que caen dentro de esa ventana.
# ------------------------------------------------------------------
files_movil <- list.files("Processed/movil", pattern = "movil_.*\\.RData", full.names = TRUE)

sinca_temp_full <- map_df(files_movil, function(path_m) {
  
  fecha_id <- gsub(".*movil_(\\d{6})\\.RData", "\\1", path_m)
  path_a   <- file.path("Processed/sc", paste0("sc_", fecha_id, ".RData"))   # Loncoche: central -> Processed/sc/sc_*
  
  if (!file.exists(path_a)) return(NULL)
  
  env_m <- new.env(); load(path_m, envir = env_m)
  env_a <- new.env(); load(path_a, envir = env_a)
  
  df_m <- env_m[[ls(env_m)[1]]]   # objeto dN
  df_a <- env_a[[ls(env_a)[1]]]   # objeto aN
  
  # Solo mediciones en ruta (tipo == 2), no los tramos B1/D1/D2/B2
  if ("tipo" %in% colnames(df_m)) df_m <- df_m %>% filter(tipo == 2)
  if ("tipo" %in% colnames(df_a)) df_a <- df_a %>% filter(tipo == 2)
  
  if (nrow(df_m) == 0 | nrow(df_a) == 0) return(NULL)
  
  dt_m <- force_tz(df_m$Datetime, "America/Santiago")
  dt_a <- force_tz(df_a$Datetime, "America/Santiago")
  
  inicio_real <- min(min(dt_m, na.rm = TRUE), min(dt_a, na.rm = TRUE))
  fin_real    <- max(max(dt_m, na.rm = TRUE), max(dt_a, na.rm = TRUE))
  
  # Redondeo a hora completa para que la ventana cuadre con el timestamp SINCA
  win_inicio <- floor_date(inicio_real, unit = "hour")
  win_fin    <- floor_date(fin_real,    unit = "hour")
  
  datos_sinca_ventana <- sinca_clean %>%
    filter(Datetime_sinca >= win_inicio, Datetime_sinca <= win_fin)
  
  if (nrow(datos_sinca_ventana) == 0) return(NULL)
  
  datos_sinca_ventana %>%
    mutate(
      Fecha_ID        = fecha_id,
      Trip_Start_Real = inicio_real
    )
})

# ------------------------------------------------------------------
# 4) Resumen por dia
# ------------------------------------------------------------------
sinca <- sinca_temp_full %>%
  group_by(Fecha_ID) %>%
  summarise(
    date       = as.Date(min(Trip_Start_Real), tz = "America/Santiago"),
    mean_sinca = mean(conc, na.rm = TRUE),
    # --- meteo ---                                                                 
    mean_temp = mean(Temp, na.rm = TRUE),                                           
    mean_hr   = mean(HR,   na.rm = TRUE),                                           
    mean_ws   = mean(WS,   na.rm = TRUE),                                           
    mean_wd   = atan2(mean(sin(WD * pi / 180), na.rm = TRUE), mean(cos(WD * pi / 180), na.rm = TRUE)) * 180 / pi,  # <-- METEO
    .groups = "drop"
  ) %>%
  mutate(mean_wd = ifelse(mean_wd < 0, mean_wd + 360, mean_wd)) %>%                 
  select(date, mean_sinca, mean_temp, mean_hr, mean_ws, mean_wd)                    

sinca_hour <- sinca_temp_full %>%
  mutate(
    Datetime = Datetime_sinca,
    Hour     = hour(Datetime_sinca),
    SC_PM25  = conc
  ) %>%
  select(Datetime, Hour, SC_PM25, Temp, HR, WS, WD, Fecha_ID)                             

save(sinca_hour, file = "Processed/sinca/sinca_hour.RData")
save(sinca,      file = "Processed/sinca/sinca.RData")