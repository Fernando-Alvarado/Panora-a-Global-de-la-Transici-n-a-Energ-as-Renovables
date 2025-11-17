
funcion_main <- function(datos, año1 = 2024, medida = "sd"){

  # Agregando el continente que le conrresponde a cada país
  datos <- datos %>%
    dplyr::mutate(
      Continent = countrycode(
        Entity,
        origin = "country.name",
        destination = "continent")
    )
  
  #-----------------------------------------------------------------------------
  # Preguntas guía
  # 1. ¿Qué países tienen mayor proporción de energía renovable?
  # Filtrando los datos del último año registrado y obteniendo el top 15 de los países con mayor proporción de energías renovable.
    
  top_renew <- datos %>%
    filter(Year == año1) %>% ### Aqui pase el año
    arrange(desc(Renewables)) %>%
    slice_head(n = 15) %>%
    mutate(Entity = fct_reorder(Entity, Renewables))
  
  Graf1 <- ggplot(top_renew,
         aes(x = Renewables,
             y = Entity,
             fill = Renewables,
             text = paste0(
               "<b>", Entity, "</b><br>",
               "Renovables: ", round(Renewables, 1), "%<br>",
               "Fósiles: ", round(Fossil_Fuels, 1), "%<br>",
               "Continente: ", Continent, "<br>",
               "Ingreso: ", Income_Group
             ))) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(Renewables, 1), "%")),
              hjust = -0.1, size = 3) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.15)),
      labels = function(x) paste0(x, "%")
    ) +
    scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
    labs(
      title = paste0("Top 15 países por proporción de energías renovables (", año1, ")"),
      x = "% de energías renovables",
      y = NULL,
      fill = "% renovables"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  #-----------------------------------------------------------------------------
  # 2. ¿Qué tan rápido crece la participación de energías renovables en el tiempo?
  # Obteniendo las tasas de crecimiento para cada continente a partir regresión lineal
  tc_contientes <- datos %>%
    dplyr::filter(Year >= 2000) %>% 
    dplyr::group_by(Continent) %>%
    dplyr::reframe(
      broom::tidy(lm(Renewables ~ Year))
    ) %>%
    dplyr::filter(term == "Year") %>% 
    dplyr::arrange(desc(estimate))
  
  # Graficando las tasas de crecimiento
  Graf2 <- ggplot(tc_contientes, aes(x = reorder(Continent, estimate), y = estimate)) +
    geom_segment(aes(xend = Continent, yend = 0), 
                 color = "gray50", linewidth = 1) +
    geom_point(aes(color = estimate > 0), size = 5) +
    geom_text(aes(label = sprintf("%+.2f %%/año", estimate)),
              nudge_x = 0.3, size = 3.5) +
    coord_flip() +
    expand_limits(y = c(
      min(tc_contientes$estimate) * 1.2,
      max(tc_contientes$estimate) * 1.2
    )) +
    labs(
      title = "Crecimiento anual del consumo de energías renovables",
      x = NULL,
      y = NULL
    ) +
    scale_color_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#CD5C5C"), 
                       guide = "none") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  #-----------------------------------------------------------------------------
  # 3. Qué relación existe entre el desarrollo económico y el uso de energías limpias?
  #   *Low income*: 1,135 dólares o menos
  # *Lower-middle income*: Entre 1,136 y 4,495 dólares
  # *Upper-middle income*: Entre 4,496 y 13,935 dólares
  # *High income*: Más de 13,935 dólares
  
  # Obteniendo la evolución promedio por grupo de ingreso
  tendencia_grupo <- datos %>%
    dplyr::filter(Year >= 2000) %>% 
    dplyr::group_by(Year, Income_Group) %>%
    dplyr::summarise(
      Renewables_sd = sd(Renewables, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ gsub(" countries", "", .)
      )
    ) %>%
    dplyr::arrange(desc(Income_Group))
  
  # Graficando la tendencia
  Graf3 <- ggplot(tendencia_grupo, aes(x = Year, y = Renewables_sd, color = Income_Group)) +
    geom_line(size = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "Tendencia: Energías Renovables por Nivel de Ingreso",
      x = NULL,
      y = "Desviación estándar del % de ER",
      color = NULL,
    ) +
    scale_color_manual(
      values = c(
        "Low-income"           = "#66C2A5",
        "Lower-middle-income"  = "#FC8D62",  
        "Upper-middle-income"  = "#8DA0CB",  
        "High-income"          = "#E78AC3" 
      )
    ) +
    scale_x_continuous(breaks = seq(2000, 2024, 4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
      legend.position = "top",
      legend.box.spacing = unit(0, "cm"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 9),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  #-----------------------------------------------------------------------------
  # 4. ¿Hay diferencias por región?
  #   *Pregunta específica*: ¿La desigualdad entre países en el porcentaje de energías renovables ha bajado o subido con el tiempo (convergencia o divergencia)?
  
  energia_paises <- datos %>%
    dplyr::filter(nchar(Code) == 3)
  
  disp_cont <- energia_paises %>%
    dplyr::group_by(Continent, Year) %>%
    dplyr::summarise(

      #Aqui pon la decision de la medida   -----------------------------------------------------------------------------
      
      sd_renew = sd(Renewables, na.rm = TRUE),


      n_paises = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      Year >= 2000,
      n_paises >= 3 # quitamos años con muy poquitos países (sobre todo Oceania)
    )
  
  Graf4 <- ggplot(disp_cont,
         aes(x = Continent, y = sd_renew, fill = Continent)) +
    geom_boxplot(alpha = 0.8, show.legend = FALSE) +
    labs(
      title = "Dispersión del % de renovables entre países por continente",
      subtitle = "Distribución de la desviación estándar anual (2000-2024)",
      x = "Continente",
      y = "Desviación estándar del % de renovables"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
      plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 12),
      legend.position = "none"
    )
  
  #-----------------------------------------------------------------------------
  
  resultado <- list(Graf1, Graf2, Graf3, Graf4)
  #Print hola
  return(resultado)
}




# Cargando la base con los datos limpios
