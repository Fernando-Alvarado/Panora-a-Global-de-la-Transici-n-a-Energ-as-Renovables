# Instala janitor si no lo tienes: install.packages("janitor")
library(dplyr)
library(readr)
library(purrr)
library(tools)
library(janitor) # <--- CLAVE PARA QUE NO FALLE

archivos <- c(
  "share-electricity-solar.csv",
  "modern-renewable-energy-consumption.csv",
  "cumulative-installed-wind-energy-capacity-gigawatts.csv",
  "share-electricity-renewables.csv",
  "installed-geothermal-capacity.csv",
  "renewable-share-energy.csv",
  "biofuel-production.csv",
  "solar-share-energy.csv",
  "hydro-share-energy.csv",
  "installed-solar-pv-capacity.csv",
  "wind-generation.csv",
  "fossil-fuel-primary-energy.csv",
  "fossil-fuel-consumption-by-type.csv",
  "share-electricity-wind.csv",
  "wind-share-energy.csv",
  "fossil-fuels-share-energy.csv",
  "modern-renewable-prod.csv",
  "hydropower-generation.csv",
  "share-electricity-fossil-fuels.csv",
  "share-electricity-hydro.csv",
  "solar-energy-consumption.csv"
)

# Función optimizada para limpiar y unir
leer_archivo_limpio <- function(nombre) {
  df <- read_csv(nombre, show_col_types = FALSE) %>% 
    clean_names() # 1. Convierte "Entity" a "entity", quita espacios y caracteres raros
  
  # Quitamos 'code' (ahora en minúscula gracias a clean_names)
  if ("code" %in% names(df)) {
    df <- df %>% select(-code)
  }
  
  # Limpiamos el nombre del archivo para que no tenga guiones medios
  base <- file_path_sans_ext(nombre) %>% make_clean_names()
  
  claves <- c("entity", "year") # Siempre minúsculas ahora
  otras <- setdiff(names(df), claves)
  
  # 2. Usamos doble guion bajo "__" como separador seguro. 
  # Ejemplo: "solar_twh" + "solar_consumption" = "solar_twh__solar_consumption"
  names(df)[names(df) %in% otras] <- paste0(otras, "__", base)
  
  df
}

# Procesar lista
lista_df <- archivos %>%
  set_names() %>% 
  map(leer_archivo_limpio)

# Unir todo (Full Join)
panel_energia <- reduce(
  lista_df,
  .f = ~ full_join(.x, .y, by = c("entity", "year"))
)

# Guardar archivo limpio
write_csv(panel_energia, "CRUCE_ER_CF.csv", na = "")

# ==============================================================================
# IMPRIMIR NOMBRES DE COLUMNAS PARA COPIAR
# ==============================================================================
cat("--- COPIA DESDE AQUÍ HACIA ABAJO ---\n")
dput(names(panel_energia))
cat("--- FIN DE LA COPIA ---\n")