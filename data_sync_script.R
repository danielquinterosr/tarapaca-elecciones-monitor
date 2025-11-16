# data_sync_script.R - Procesador para GitHub Actions
# Lee Sheets (Wide), transforma a Long, y sube a GitHub Gist.

library(googlesheets4)
library(dplyr)
library(tidyr)
library(readr)
library(httr2) 

# =========================================================================
# 1. CONFIGURACIÓN (Lee desde los Secrets de GitHub)
# =========================================================================

SHEET_ID       <- Sys.getenv("GOOGLE_SHEET_ID")
SHEET_NAME     <- "votos" # Asegúrate que la pestaña en Google Sheets se llame 'votos'
GIST_ID        <- Sys.getenv("GIST_ID") 
GH_TOKEN       <- Sys.getenv("GH_TOKEN") 
GIST_FILE_NAME <- "votos_tarapaca_long.csv" 

# =========================================================================
# 2. FUNCIÓN DE SUBIDA A GIST (Usando httr2 y el token de GitHub)
# =========================================================================

upload_to_gist <- function(content, filename, gist_id, token) {
  
  files_payload <- list()
  files_payload[[filename]] <- list(content = content)
  
  payload <- list(
    description = paste("Actualización de resultados:", Sys.time()),
    files = files_payload
  )
  
  url <- paste0("https://api.github.com/gists/", gist_id)
  
  req <- request(url) |>
    req_method("PATCH") |>
    req_headers(
      "Authorization" = paste("token", token),
      "Accept" = "application/vnd.github.v3+json"
    ) |>
    req_body_json(payload) |>
    req_error(is_error = function(resp) {
      if (resp_status(resp) >= 400) TRUE else FALSE
    })
  
  resp <- req_perform(req)
  
  if (resp_status(resp) == 200) {
    cat("Subida a GitHub Gist exitosa (Status 200).\n")
  } else {
    stop(paste("Error al subir a Gist. Status:", resp_status(resp), "Cuerpo:", resp_body_string(resp)))
  }
}

# =========================================================================
# 3. LECTURA, TRANSFORMACIÓN (WIDE -> LONG) Y TRAZABILIDAD
# =========================================================================

cat("Iniciando autenticación de Google Sheets (gs4_deauth())...\n")
# Usa gs4_deauth() para forzar el modo de lectura pública/no interactiva.
googlesheets4::gs4_deauth()

cat("Leyendo datos de Google Sheets (Ancho)...")
votos_wide_df <- tryCatch({
  # Lee la hoja 'votos' de tu URL pública
  read_sheet(ss = SHEET_ID, sheet = SHEET_NAME)
}, error = function(e) {
  stop(paste("ERROR al leer Google Sheets. Asegúrate de que el link de Compartir esté en 'Cualquier usuario con el enlace' y que la pestaña se llame 'votos'. Detalle:", e$message))
})

# Captura el Timestamp de la modificación de la Hoja (Trazabilidad)
sheet_meta <- gs4_get(SHEET_ID)
last_modified_gs <- as.character(sheet_meta$drive_resource$modifiedTime)

# Renombrar las primeras dos columnas (mesa_id y ultima_modificacion del Excel)
votos_wide_df <- votos_wide_df |>
    rename(
        mesa_id = 1,     
        timestamp_digitador = 2    
    ) |>
    filter(!is.na(mesa_id) & mesa_id != "") # Limpia filas vacías al final

cat("Transformando de Ancho a Largo (Pivot)...")
votos_long_df <- votos_wide_df |>
  # Usa la lógica de pivot_longer: rota todas las columnas que NO sean mesa_id y timestamp_digitador
  pivot_longer(
    cols = -c(mesa_id, timestamp_digitador), 
    names_to = "cand_id", # cand_id ahora contiene el nombre completo del candidato (lo que necesita tu app Shiny)
    values_to = "votos",
    values_transform = list(votos = as.integer)
  ) |>
  mutate(
    mesa_id   = as.character(mesa_id),
    votos     = replace_na(votos, 0L), 
    # Usamos el timestamp de la columna como la hora de digitación de la mesa
    timestamp = coalesce(as.character(timestamp_digitador), last_modified_gs), 
    sync_time = as.character(Sys.time()) # Tiempo de procesamiento en la nube
  ) %>%
  # Limpiamos y dejamos el formato final que espera la app (excepto cand_id que ahora es nombre)
  select(mesa_id, cand_id, votos, timestamp, sync_time)


# =========================================================================
# 4. ESCRITURA EN GITHUB GIST
# =========================================================================

csv_content <- format_csv(votos_long_df)

upload_to_gist(
  content = csv_content,
  filename = GIST_FILE_NAME,
  gist_id = GIST_ID,
  token = GH_TOKEN
)
