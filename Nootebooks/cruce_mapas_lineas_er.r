library(dplyr)
library(readr)
library(purrr)
library(tools)
library(janitor)

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


leer_archivo_limpio <- function(nombre) {
  df <- read_csv(nombre, show_col_types = FALSE) %>% 
    clean_names()
  
  if ("code" %in% names(df)) {
    df <- df %>% select(-code)
  }
  
  base <- file_path_sans_ext(nombre) %>% make_clean_names()
  
  claves <- c("entity", "year")
  otras <- setdiff(names(df), claves)

  names(df)[names(df) %in% otras] <- paste0(otras, "__", base)
  
  df
}

lista_df <- archivos %>%
  set_names() %>% 
  map(leer_archivo_limpio)


panel_energia <- reduce(
  lista_df,
  .f = ~ full_join(.x, .y, by = c("entity", "year"))
)


write_csv(panel_energia, "CRUCE_ER_CF.csv", na = "")


cat("--- COPIA DESDE AQUÍ HACIA ABAJO ---\n")
dput(names(panel_energia))
cat("--- FIN DE LA COPIA ---\n")