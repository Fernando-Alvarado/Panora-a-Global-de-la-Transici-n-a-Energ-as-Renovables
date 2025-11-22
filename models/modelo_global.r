library(readr)
library(dplyr)
library(fixest)
library(broom)
library(tibble)


data_panel <- read_csv("data.csv", show_col_types = FALSE)


if ("...1" %in% names(data_panel)) {
  data_panel <- data_panel %>% select(-`...1`)
}

cat("Panel principal - dimensiones:\n")
cat("Filas:", nrow(data_panel), " Columnas:", ncol(data_panel), "\n")
print(names(data_panel))


entidades_a_excluir <- c(
  "ASEAN (Ember)", 
  "CIS (EI)", 
  "EU (Ember)", "European Union (27)",
  "G20 (Ember)", "G7 (Ember)", 
  "OECD (EI)", "OECD (Ember)", 
  "Non-OECD (EI)",
  "Africa", "Africa (EI)", "Africa (Ember)",
  "Asia", "Asia (Ember)", "Asia Pacific (EI)", "Other Asia Pacific (EI)",
  "Europe", "Europe (EI)", "Europe (Ember)", "Other Europe (EI)",
  "Latin America and Caribbean (Ember)", "South and Central America (EI)", "South America",
  "Middle East (EI)", "Middle East (Ember)", "Other Middle East (EI)",
  "North America", "North America (EI)", "North America (Ember)",
  "Oceania", "Oceania (Ember)",
  "Other CIS (EI)",
  "High-income countries", 
  "Low-income countries", 
  "Lower-middle-income countries", 
  "Upper-middle-income countries",
  "World"
)

cat("\nFilas antes de filtrar agregados:", nrow(data_panel), "\n")

data_panel <- data_panel %>%
  filter(!Entity %in% entidades_a_excluir)

cat("Filas después de filtrar agregados (SOLO PAÍSES):", nrow(data_panel), "\n")


res_renew_info <- data_panel %>%
  group_by(Entity, Code) %>%
  summarise(
    n_obs        = n(),
    n_na_renew   = sum(is.na(Renewables)),
    all_na_renew = all(is.na(Renewables)),
    .groups      = "drop"
  )

paises_sin_info <- res_renew_info %>%
  filter(all_na_renew)

cat("\nPaíses con todos los registros NA en Renewables:\n")
print(paises_sin_info)

if (nrow(paises_sin_info) > 0) {
  paises_sin_info %>%
    mutate(Sin_info = TRUE) %>%
    write_csv("paises_sin_info_renovables.csv", na = "")
}

data_panel_clean <- data_panel %>%
  anti_join(paises_sin_info %>% select(Entity, Code),
            by = c("Entity", "Code"))

cat("\nPanel tras excluir países sin info en Renewables:\n")
cat("Filas:", nrow(data_panel_clean), " Columnas:", ncol(data_panel_clean), "\n")


targets_raw <- read_csv("net-zero-targets.csv", show_col_types = FALSE)

cat("\nTargets net-zero - dimensiones:\n")
cat("Filas:", nrow(targets_raw), " Columnas:", ncol(targets_raw), "\n")
print(names(targets_raw))

targets <- targets_raw %>%
  rename(Target_Year = Year) %>%
  select(Entity, Code, Target_Year) %>%
  group_by(Code) %>%
  arrange(Target_Year) %>%
  slice_tail(n = 1) %>%
  ungroup()

cat("\nTargets net-zero (procesado) - primeras filas:\n")
print(head(targets, 10))


panel_joined <- data_panel_clean %>%
  left_join(targets, by = c("Code", "Entity"))

cat("\nTras el join con targets, dimensiones:\n")
cat("Filas:", nrow(panel_joined), " Columnas:", ncol(panel_joined), "\n")

panel_joined <- panel_joined %>%
  mutate(
    HasTarget         = !is.na(Target_Year),
    YearsToTarget     = ifelse(HasTarget, Target_Year - Year, NA_real_),
    YearsToTarget_dec = ifelse(HasTarget, YearsToTarget / 10, NA_real_) # en décadas
  )

cat("\nResumen de YearsToTarget (solo países con target):\n")
panel_joined %>%
  filter(HasTarget) %>%
  summarise(
    n      = n(),
    min_yt = min(YearsToTarget, na.rm = TRUE),
    p25_yt = quantile(YearsToTarget, 0.25, na.rm = TRUE),
    med_yt = median(YearsToTarget, na.rm = TRUE),
    p75_yt = quantile(YearsToTarget, 0.75, na.rm = TRUE),
    max_yt = max(YearsToTarget, na.rm = TRUE)
  ) %>% print()

panel <- panel_joined %>%
  arrange(Entity, Year) %>%
  group_by(Entity) %>%
  mutate(
    Renewables_lag = dplyr::lag(Renewables, n = 1)
  ) %>%
  ungroup()

panel_model <- panel %>%
  filter(!is.na(Renewables_lag))

cat("\nFilas con rezago disponible:", nrow(panel_model), "\n")


panel_model <- panel_model %>%
  filter(HasTarget, !is.na(YearsToTarget_dec)) %>%
  mutate(
    YearsToTarget_dec_sq = YearsToTarget_dec^2
  )

cat("Filas usadas en el modelo (solo países con target):",
    nrow(panel_model), "\n")


vars_necesarias <- c(
  "Entity", "Year",
  "Renewables", "Renewables_lag",
  "Fossil_Fuels",
  "Income_Group",
  "YearsToTarget_dec_sq"
)

faltan <- setdiff(vars_necesarias, names(panel_model))
if (length(faltan) > 0) {
  stop("Faltan columnas en panel_model: ", paste(faltan, collapse = ", "))
}


fml <- Renewables ~ Renewables_lag + Fossil_Fuels + YearsToTarget_dec_sq +
  i(Income_Group) | Entity + Year

cat("\nFórmula del modelo (con YearsToTarget_dec_sq):\n")
print(fml)

model_panel_sq <- feols(
  fml,
  data    = panel_model,
  cluster = ~ Entity
)

cat("\nResumen del modelo (término cuadrado):\n")
print(summary(model_panel_sq))


if (!is.null(model_panel_sq$collin.var)) {
  cat("\nVariables marcadas como colineales:\n")
  print(model_panel_sq$collin.var)
}


coef_table_sq <- broom::tidy(model_panel_sq) %>%
  arrange(term)

write_csv(coef_table_sq, "modelo_panel_coeficientes_con_target_cuadrado.csv", na = "")


fe_list_sq <- fixef(model_panel_sq)


if ("Entity" %in% names(fe_list_sq)) {
  fe_country_sq <- fe_list_sq[["Entity"]] %>%
    as.data.frame() %>%
    rownames_to_column(var = "Entity") %>%
    rename(effect_country = ".")

  write_csv(fe_country_sq, "modelo_panel_efectos_fijos_pais_con_target_cuadrado.csv", na = "")
}


if ("Year" %in% names(fe_list_sq)) {
  fe_year_sq <- fe_list_sq[["Year"]] %>%
    as.data.frame() %>%
    rownames_to_column(var = "Year") %>%
    mutate(Year = as.numeric(Year)) %>%
    rename(effect_year = ".")

  write_csv(fe_year_sq, "modelo_panel_efectos_fijos_anio_con_target_cuadrado.csv", na = "")
}


panel_preds_sq <- panel_model %>%
  mutate(
    Pred_Renewables = as.numeric(predict(model_panel_sq, newdata = panel_model))
  )

write_csv(panel_preds_sq, "modelo_panel_predicciones_pais_anio_con_target_cuadrado.csv", na = "")

cat("\nSe guardaron los CSV:\n",
    "- paises_sin_info_renovables.csv\n",
    "- modelo_panel_coeficientes_con_target_cuadrado.csv\n",
    "- modelo_panel_efectos_fijos_pais_con_target_cuadrado.csv\n",
    "- modelo_panel_efectos_fijos_anio_con_target_cuadrado.csv\n",
    "- modelo_panel_predicciones_pais_anio_con_target_cuadrado.csv\n")
