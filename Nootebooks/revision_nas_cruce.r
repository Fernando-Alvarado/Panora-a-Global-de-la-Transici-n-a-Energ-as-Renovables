

library(dplyr)
library(readr)
library(purrr)


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
  "global-fossil-fuel-consumption.csv",
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


contar_na <- function(nombre_archivo) {
  df <- read_csv(nombre_archivo, show_col_types = FALSE)

  tiene_entity <- "Entity" %in% names(df)
  tiene_year   <- "Year"   %in% names(df)

  tibble(
    archivo   = nombre_archivo,
    filas     = nrow(df),
    na_Entity = if (tiene_entity) sum(is.na(df$Entity)) else NA_integer_,
    na_Year   = if (tiene_year)   sum(is.na(df$Year))   else NA_integer_
  )
}


resumen_na <- map_df(archivos, contar_na)


print(resumen_na, n = 50)
