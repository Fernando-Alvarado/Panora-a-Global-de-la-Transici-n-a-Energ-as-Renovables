library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tensorflow)
library(keras3)

# ==============================================================================
# 1. CONFIGURACIÓN
# ==============================================================================
ENTITY        <- "World"
H_FUTURE      <- 20
WINDOW_RANGE  <- 4:20
UNITS_GRID    <- c(8, 16, 32, 64, 128)
EPOCHS_SEARCH <- 50
EPOCHS_FINAL  <- 200

# Cargar el CSV limpio
panel <- read_csv("CRUCE_ER_CF.csv", show_col_types = FALSE)

# ==============================================================================
# 2. DEFINICIÓN DE COLUMNAS (NOMBRES LIMPIOS)
# ==============================================================================
# Estos son los nombres generados por janitor::clean_names()
COL_FOSSIL_SHARE <- "fossil_fuels_percent_equivalent_primary_energy__fossil_fuels_share_energy"
COL_RENEW_SHARE  <- "renewables_percent_equivalent_primary_energy__renewable_share_energy"

# Preparación del dataframe
df_world <- panel %>%
  filter(entity == ENTITY) %>%  # 'entity' en minúscula
  arrange(year) %>%             # 'year' en minúscula
  select(year, !!COL_FOSSIL_SHARE, !!COL_RENEW_SHARE) %>%
  rename(
    fossil_share = !!COL_FOSSIL_SHARE,
    renew_share  = !!COL_RENEW_SHARE
  )

# ==============================================================================
# 3. FUNCIONES AUXILIARES (LSTM)
# ==============================================================================

make_supervised_split <- function(serie_scaled, window_size, train_ratio = 0.8) {
  n <- length(serie_scaled)
  if (n <= window_size + 1) {
    stop("serie demasiado corta para la ventana")
  }
  n_train_years <- floor(n * train_ratio)
  n_samples <- n - window_size
  X <- matrix(NA_real_, nrow = n_samples, ncol = window_size)
  y <- numeric(n_samples)
  target_index <- integer(n_samples)
  for (i in 1:n_samples) {
    X[i, ] <- serie_scaled[i:(i + window_size - 1)]
    y[i]   <- serie_scaled[i + window_size]
    target_index[i] <- i + window_size
  }
  train_mask <- target_index <= n_train_years
  test_mask  <- target_index > n_train_years
  if (sum(train_mask) < 2 || sum(test_mask) < 1) {
    stop("muy pocos datos de train o test para esa ventana")
  }
  X_train <- X[train_mask, , drop = FALSE]
  y_train <- y[train_mask]
  X_test  <- X[test_mask,  , drop = FALSE]
  y_test  <- y[test_mask]
  list(
    X_train = X_train,
    y_train = y_train,
    X_test  = X_test,
    y_test  = y_test
  )
}

rmse <- function(a, p) sqrt(mean((a - p)^2))

train_eval_lstm_split <- function(serie, window_size, units, epochs_search, train_ratio = 0.8) {
  ok <- !is.na(serie)
  serie <- serie[ok]
  min_val <- min(serie, na.rm = TRUE)
  max_val <- max(serie, na.rm = TRUE)
  serie_scaled <- (serie - min_val) / (max_val - min_val)
  split <- tryCatch(
    make_supervised_split(serie_scaled, window_size, train_ratio),
    error = function(e) return(NULL)
  )
  if (is.null(split)) {
    return(list(test_rmse = Inf))
  }
  X_train_mat <- split$X_train
  y_train <- split$y_train
  X_test_mat  <- split$X_test
  y_test <- split$y_test

  X_train <- array(X_train_mat, dim = c(nrow(X_train_mat), window_size, 1))
  X_test  <- array(X_test_mat,  dim = c(nrow(X_test_mat),  window_size, 1))

  tensorflow::set_random_seed(42L)

  model <- keras_model_sequential() |>
    layer_lstm(units = units, input_shape = c(window_size, 1)) |>
    layer_dense(units = 1)

  model |> compile(
    loss = "mse",
    optimizer = "adam"
  )

  model |>
    fit(
      x = X_train,
      y = y_train,
      epochs = epochs_search,
      batch_size = 8,
      verbose = 0
    )

  y_pred_scaled <- predict(model, X_test)[,1]
  y_test_real   <- y_test * (max_val - min_val) + min_val
  y_pred_real   <- y_pred_scaled * (max_val - min_val) + min_val

  list(test_rmse = rmse(y_test_real, y_pred_real))
}

grid_search_lstm_split <- function(serie, label,
                                   window_range,
                                   units_grid,
                                   epochs_search,
                                   train_ratio = 0.8) {
  cat(label, "\n")
  ok <- !is.na(serie)
  serie <- serie[ok]
  resultados <- list()
  idx <- 1
  for (w in window_range) {
    for (u in units_grid) {
      res <- train_eval_lstm_split(serie, w, u, epochs_search, train_ratio)
      resultados[[idx]] <- data.frame(
        window_size = w,
        units       = u,
        test_rmse   = res$test_rmse
      )
      cat("w", w, "u", u, "rmse_test", round(res$test_rmse, 4), "\n")
      idx <- idx + 1
    }
  }
  df_res <- bind_rows(resultados) %>% arrange(test_rmse)
  print(head(df_res, 10))
  best <- df_res[1, ]
  list(
    best_window = best$window_size,
    best_units  = best$units,
    min_test_rmse = best$test_rmse
  )
}

fit_lstm_final_and_forecast <- function(serie, years, label,
                                        best_window,
                                        best_units,
                                        H_FUTURE,
                                        epochs_final) {
  cat("final", label, "w", best_window, "u", best_units, "\n")
  ok <- !is.na(serie)
  serie <- serie[ok]
  years <- years[ok]
  last_year <- tail(years, 1)
  min_val <- min(serie, na.rm = TRUE)
  max_val <- max(serie, na.rm = TRUE)
  serie_scaled <- (serie - min_val) / (max_val - min_val)

  n <- length(serie_scaled)
  n_samples <- n - best_window
  X_mat <- matrix(NA_real_, nrow = n_samples, ncol = best_window)
  y_vec <- numeric(n_samples)
  for (i in 1:n_samples) {
    X_mat[i, ] <- serie_scaled[i:(i + best_window - 1)]
    y_vec[i]   <- serie_scaled[i + best_window]
  }
  X_array <- array(X_mat, dim = c(nrow(X_mat), best_window, 1))

  tensorflow::set_random_seed(42L)

  model <- keras_model_sequential() |>
    layer_lstm(units = best_units, input_shape = c(best_window, 1)) |>
    layer_dense(units = 1)

  model |> compile(
    loss = "mse",
    optimizer = "adam"
  )

  model |>
    fit(
      x = X_array,
      y = y_vec,
      epochs = epochs_final,
      batch_size = 8,
      verbose = 0
    )

  y_hat_scaled <- predict(model, X_array)[,1]
  train_rmse <- rmse(y_vec * (max_val - min_val) + min_val,
                     y_hat_scaled * (max_val - min_val) + min_val)
  cat("train_rmse", round(train_rmse, 4), "\n")

  window_current <- tail(serie_scaled, best_window)
  lstm_forecast_scaled <- numeric(H_FUTURE)
  for (h in 1:H_FUTURE) {
    x_in <- array(window_current, dim = c(1, best_window, 1))
    y_hat <- predict(model, x_in)[1,1]
    lstm_forecast_scaled[h] <- y_hat
    window_current <- c(window_current[-1], y_hat)
  }
  lstm_forecast <- lstm_forecast_scaled * (max_val - min_val) + min_val
  years_future <- (last_year + 1):(last_year + H_FUTURE)

  list(
    years_hist    = years,
    values_hist   = serie,
    years_future  = years_future,
    lstm_forecast = lstm_forecast
  )
}

# ==============================================================================
# 4. EJECUCIÓN DEL MODELO
# ==============================================================================

years_all    <- df_world$year
serie_fossil <- df_world$fossil_share
serie_renew  <- df_world$renew_share

# Grid Search Fósiles
search_fossil <- grid_search_lstm_split(
  serie         = serie_fossil,
  label         = "Fossil",
  window_range  = WINDOW_RANGE,
  units_grid    = UNITS_GRID,
  epochs_search = EPOCHS_SEARCH,
  train_ratio   = 0.8
)

# Grid Search Renovables
search_renew <- grid_search_lstm_split(
  serie         = serie_renew,
  label         = "Renew",
  window_range  = WINDOW_RANGE,
  units_grid    = UNITS_GRID,
  epochs_search = EPOCHS_SEARCH,
  train_ratio   = 0.8
)

# Ajuste Final y Pronóstico
res_fossil <- fit_lstm_final_and_forecast(
  serie       = serie_fossil,
  years       = years_all,
  label       = "Fossil",
  best_window = search_fossil$best_window,
  best_units  = search_fossil$best_units,
  H_FUTURE    = H_FUTURE,
  epochs_final= EPOCHS_FINAL
)

res_renew <- fit_lstm_final_and_forecast(
  serie       = serie_renew,
  years       = years_all,
  label       = "Renew",
  best_window = search_renew$best_window,
  best_units  = search_renew$best_units,
  H_FUTURE    = H_FUTURE,
  epochs_final= EPOCHS_FINAL
)

# ==============================================================================
# 5. GUARDAR RESULTADOS
# ==============================================================================

df_hist <- bind_rows(
  tibble(
    Year = res_fossil$years_hist,
    Type = "Fossil",
    Real = res_fossil$values_hist
  ),
  tibble(
    Year = res_renew$years_hist,
    Type = "Renewable",
    Real = res_renew$values_hist
  )
)

df_future <- bind_rows(
  tibble(
    Year = res_fossil$years_future,
    Type = "Fossil",
    Real = NA_real_,
    Pred_LSTM = res_fossil$lstm_forecast
  ),
  tibble(
    Year = res_renew$years_future,
    Type = "Renewable",
    Real = NA_real_,
    Pred_LSTM = res_renew$lstm_forecast
  )
)

df_hist_full <- df_hist %>%
  mutate(Pred_LSTM = NA_real_)

df_all <- bind_rows(df_hist_full, df_future) %>%
  arrange(Type, Year)

write_csv(df_all, "predicciones_world_fossil_renew_lstm_train_test.csv", na = "null")

# ==============================================================================
# 6. GRAFICAR
# ==============================================================================

split_year <- max(res_fossil$years_hist)

df_plot_long <- bind_rows(
  df_all %>%
    filter(!is.na(Real)) %>%
    mutate(Model = "Real", value = Real),
  df_all %>%
    filter(!is.na(Pred_LSTM)) %>%
    mutate(Model = "LSTM", value = Pred_LSTM)
) %>%
  select(Year, Type, Model, value) %>%
  mutate(
    Energy = if_else(Type == "Fossil", "Fossil", "Renewable"),
    Model  = factor(Model, levels = c("Real", "LSTM"))
  )


colores_energy <- c(
  "Fossil"    = "#3B4CC0",
  "Renewable" = "#89D548"
)

linetypes_model <- c(
  "Real" = "solid",
  "LSTM" = "dotdash"
)

ggplot(df_plot_long,
       aes(x = Year, y = value,
           color = Energy, linetype = Model)) +
  geom_rect(aes(xmin = split_year + 0.5,
                xmax = Inf,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey92",
            alpha = 0.4,
            inherit.aes = FALSE) +
  geom_vline(xintercept = split_year + 0.5,
             linetype = "dotted",
             linewidth = 0.6,
             color = "grey40") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_manual(values = colores_energy, name = "Tipo de energía") +
  scale_linetype_manual(values = linetypes_model, name = "Serie") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = paste("Participación de energías fósiles y renovables -", ENTITY),
    subtitle = "Línea sólida: datos históricos · Línea segmentada: predicción LSTM · Área sombreada: horizonte de pronóstico",
    x = "Año",
    y = "% de energía primaria equivalente"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "#333333"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )