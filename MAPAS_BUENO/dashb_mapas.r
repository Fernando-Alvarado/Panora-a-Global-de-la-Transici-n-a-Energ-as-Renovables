library(shiny)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(bslib)
library(countrycode)
library(shinyWidgets)
library(ggplot2)
library(htmltools)
library(tidyr)
library(plotly)
library(RColorBrewer)

# ==============================================================================
# 1. CONFIGURACIÓN TÉCNICA Y PALETAS
# ==============================================================================

cols_pal <- list(
  Solar = "#FFC107", 
  Wind  = "#00BCD4", 
  Hydro = "#1565C0", 
  Bio   = "#2E7D32", 
  Geo   = "#D84315"  
)

to_rgba <- function(hex, alpha = 0.6) {
  rgb_vals <- col2rgb(hex)
  paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
}

cols_def <- list(
  Solar = list(
    gen = "solar_generation_t_wh__modern_renewable_energy_consumption",
    share = "solar_percent_electricity__share_electricity_solar",
    color = cols_pal$Solar,
    fill  = to_rgba(cols_pal$Solar),
    map_palette = "YlOrRd",
    icon = "sun",
    metric_label = "Generación Solar",
    desc = "Electricidad generada por energía solar (PV y CSP)."
  ),
  Wind = list(
    gen = "wind_generation_t_wh__modern_renewable_energy_consumption",
    share = "wind_percent_electricity__share_electricity_wind",
    color = cols_pal$Wind,
    fill  = to_rgba(cols_pal$Wind),
    map_palette = "PuBu",
    icon = "wind",
    metric_label = "Generación Eólica",
    desc = "Electricidad generada por el viento (onshore y offshore)."
  ),
  Hydro = list(
    gen = "hydro_generation_t_wh__modern_renewable_energy_consumption",
    share = "hydro_percent_electricity__share_electricity_hydro",
    color = cols_pal$Hydro,
    fill  = to_rgba(cols_pal$Hydro),
    map_palette = "GnBu",
    icon = "water",
    metric_label = "Generación Hidroeléctrica",
    desc = "Electricidad generada por centrales hidroeléctricas."
  ),
  Bio = list(
    gen = "biofuels_production_t_wh__biofuel_production",
    share = NULL, 
    color = cols_pal$Bio,
    fill  = to_rgba(cols_pal$Bio),
    map_palette = "YlGn",
    icon = "leaf",
    metric_label = "Producción Biocombustibles",
    desc = "Producción de biocombustibles líquidos (etanol/biodiesel)."
  ),
  Geo = list(
    gen = "other_renewables_including_geothermal_and_biomass_electricity_generation_t_wh__modern_renewable_energy_consumption",
    share = NULL, 
    color = cols_pal$Geo,
    fill  = to_rgba(cols_pal$Geo),
    map_palette = "OrRd",
    icon = "fire",
    metric_label = "Geotérmica y Otros",
    desc = "Generación por biomasa, residuos, geotermia y fuentes marinas."
  )
)

# ==============================================================================
# 2. CARGA Y PROCESAMIENTO (CORRECCIÓN "null")
# ==============================================================================

# CORRECCIÓN CRÍTICA: Agregamos na.strings = "null" para que R entienda tus vacíos
er_data <- read.csv("CRUCE_ER_CF.csv", na.strings = c("null", "NA", "")) 
pred_data <- read.csv("predicciones_world_fossil_renew_lstm_train_test.csv", na.strings = c("null", "NA", ""))

# Aseguramos que sean numéricos (por si acaso queda basura)
pred_data$Real <- as.numeric(pred_data$Real)
pred_data$Pred_LSTM <- as.numeric(pred_data$Pred_LSTM)

er_clean <- er_data %>%
  mutate(
    iso3 = countrycode(entity, origin = "country.name", destination = "iso3c", warn = FALSE),
    Is_Country = !is.na(iso3)
  )

if ("World" %in% er_clean$entity) {
  er_world <- er_clean %>% filter(entity == "World")
} else {
  er_world <- er_clean %>%
    filter(Is_Country) %>%
    group_by(year) %>%
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>%
    mutate(entity = "World")
}

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% select(iso_a3, geometry)

# ==============================================================================
# 3. UI
# ==============================================================================

energy_block_ui <- function(id, title, color, icon_name, description, has_share, metric_label_text) {
  ns <- NS(id)
  
  div(class = "card shadow-sm mb-5 border-0",
      div(class = "card-header bg-white py-3", style=paste0("border-top: 5px solid ", color, ";"),
          h3(icon(icon_name), title, style = paste0("color:", color, "; font-weight: 800; margin: 0;")),
          p(class="text-muted mb-0 mt-2 small", style="text-align: justify;", description)
      ),
      div(class = "card-body bg-light",
          layout_columns(
            col_widths = c(3, 9),
            div(class="p-3 bg-white rounded shadow-sm h-100",
                h5("Filtros", class="border-bottom pb-2"),
                uiOutput(ns("slider_ui"))
            ),
            div(class="bg-white rounded shadow-sm p-3",
                tabsetPanel(id = ns("tabs_metric"),
                            tabPanel(paste(metric_label_text, "(TWh)"), value = "gen",
                                     br(), leafletOutput(ns("map_gen"), height = "450px")),
                            if(has_share) {
                              tabPanel("Participación (%)", value = "share",
                                       br(), leafletOutput(ns("map_share"), height = "450px"))
                            } else NULL
                )
            )
          ),
          br(),
          layout_columns(
            col_widths = c(4, 4, 4),
            card(card_header("Tendencia Mundial"), plotlyOutput(ns("plot_world"), height = "320px")),
            card(card_header("Top 5 (Trayectoria Histórica)"), plotlyOutput(ns("plot_top5"), height = "320px")),
            card(card_header(textOutput(ns("country_title"))), plotlyOutput(ns("plot_country"), height = "320px"))
          )
      )
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  tags$head(tags$style(HTML("
    .leaflet-container { background: #f0f2f5; } 
    .legend { font-size: 11px; padding: 8px; background: rgba(255,255,255,0.95); border-radius: 4px; border: 1px solid #eee; }
    .card-header { font-weight: bold; font-size: 1rem; color: #2c3e50; background-color: #fff; }
  "))),
  
  div(class = "container-fluid py-5 text-center bg-white shadow-sm mb-4",
      h1("Monitor Global de Transición Energética", style="font-weight: 900; color: #2c3e50;"),
      p("Análisis integral de la matriz energética mundial: Historia y Proyecciones (LSTM).", class="text-muted")
  ),
  
  div(class = "container-fluid px-4 mb-5",
      div(class = "card shadow-sm border-0",
          div(class = "card-header bg-white py-3", style="border-top: 5px solid #2c3e50;",
              h3(icon("chart-line"), "Panorama Global y Proyecciones", style = "color: #2c3e50; font-weight: 800; margin: 0;")
          ),
          div(class = "card-body",
              tabsetPanel(
                tabPanel("Historia: Comparativa por Fuente (TWh)", br(),
                         fluidRow(column(12, 
                            div(class="d-flex align-items-center bg-light p-3 rounded mb-3",
                                strong("Línea de Tiempo: ", class="me-3"),
                                div(style="flex-grow: 1;", uiOutput("slider_global_ui"))
                            )
                         )),
                         fluidRow(column(12, plotlyOutput("global_plot", height = "500px")))
                ),
                tabPanel("Proyección IA: Fósiles vs Renovables", br(),
                         div(class="alert alert-light border", icon("robot"), 
                             strong("Modelo LSTM:"), " Tendencia proyectada de la participación en energía primaria."),
                         plotlyOutput("forecast_plot", height = "500px")
                )
              )
          )
      )
  ),
  
  div(class = "container-fluid px-4",
      energy_block_ui("solar", "Energía Solar", cols_def$Solar$color, "sun", cols_def$Solar$desc, TRUE, cols_def$Solar$metric_label),
      energy_block_ui("wind", "Energía Eólica", cols_def$Wind$color, "wind", cols_def$Wind$desc, TRUE, cols_def$Wind$metric_label),
      energy_block_ui("hydro", "Energía Hidroeléctrica", cols_def$Hydro$color, "water", cols_def$Hydro$desc, TRUE, cols_def$Hydro$metric_label),
      energy_block_ui("bio", "Biocombustibles", cols_def$Bio$color, "leaf", cols_def$Bio$desc, FALSE, cols_def$Bio$metric_label),
      energy_block_ui("geo", "Geotérmica y Otros", cols_def$Geo$color, "fire", cols_def$Geo$desc, FALSE, cols_def$Geo$metric_label)
  ),
  
  div(class="container-fluid py-4 mt-5 bg-light text-center border-top",
      p(class="mb-1 text-muted small", style="font-weight: 600;", "Información obtenida de:"),
      p(class="text-muted small", style="font-style: italic;", "Ember (2025); Energy Institute - Statistical Review of World Energy (2025) – with major processing by Our World in Data")
  )
)

# ==============================================================================
# 4. SERVIDOR
# ==============================================================================

energy_server_logic <- function(id, tech_key, data_full, world_geo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- cols_def[[tech_key]]
    selected_iso <- reactiveVal("MEX") 
    
    observeEvent(input$map_gen_shape_click, { if(!is.null(input$map_gen_shape_click$id)) selected_iso(input$map_gen_shape_click$id) })
    observeEvent(input$map_share_shape_click, { if(!is.null(input$map_share_shape_click$id)) selected_iso(input$map_share_shape_click$id) })
    
    output$country_title <- renderText({
      req(selected_iso())
      nm <- data_full %>% filter(iso3 == selected_iso()) %>% pull(entity) %>% unique()
      if(length(nm) == 0) return("Seleccione un País")
      paste("Evolución Histórica:", nm[1])
    })
    
    output$slider_ui <- renderUI({
      sliderInput(ns("year"), "Año:", min = min(data_full$year), max = max(data_full$year), 
                  value = max(data_full$year), step = 1, sep = "", 
                  animate = animationOptions(interval = 800, loop = FALSE))
    })
    
    render_custom_map <- function(type) {
      renderLeaflet({
        req(input$year)
        if(type == "gen") { col_name <- conf$gen; unit <- "TWh"; bins <- c(0, 1, 10, 50, 100, 500, Inf) } 
        else { col_name <- conf$share; unit <- "%"; bins <- c(0, 1, 5, 10, 20, 50, 100) }
        map_dat <- data_full %>% filter(year == input$year, Is_Country) %>% select(iso3, entity, Val = all_of(col_name))
        geo <- world_geo %>% left_join(map_dat, by = c("iso_a3" = "iso3"))
        vals <- geo$Val
        pal <- colorBin(palette = conf$map_palette, domain = vals, bins = bins, na.color = "#ffffff") 
        leaflet(geo) %>% setMaxBounds(-180, -90, 180, 90) %>%
          addMapPane("fondo", zIndex = 400) %>% addMapPane("datos", zIndex = 450) %>% addMapPane("etiquetas", zIndex = 500) %>%
          addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(pane = "fondo")) %>%
          addPolygons(fillColor = ~pal(Val), weight = 1, color = "white", opacity = 1, fillOpacity = 0.85,
                      layerId = ~iso_a3, options = pathOptions(pane = "datos"),
                      highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE),
                      label = sprintf("<strong>%s</strong><br/>%s", geo$entity, ifelse(is.na(geo$Val), "Sin Info", paste0(format(round(geo$Val, 2), big.mark=","), " ", unit))) %>% lapply(HTML)) %>%
          addProviderTiles("CartoDB.PositronOnlyLabels", options = providerTileOptions(pane = "etiquetas")) %>%
          setView(0, 20, 1.5) %>% addLegend(pal = pal, values = vals, title = unit, position = "bottomright", na.label = "Sin Info")
      })
    }
    output$map_gen <- render_custom_map("gen")
    output$map_share <- render_custom_map("share")
    
    output$plot_world <- renderPlotly({
      active_tab <- if(is.null(input$tabs_metric)) "gen" else input$tabs_metric
      if(active_tab == "share") { col_name <- conf$share; unit <- "%" } else { col_name <- conf$gen; unit <- "TWh" }
      w_dat <- er_world %>% select(year, Val = all_of(col_name)) %>% arrange(year)
      plot_ly(w_dat, x = ~year, y = ~Val, type = 'scatter', mode = 'lines+markers', 
              fill = 'tozeroy', fillcolor = conf$fill, marker = list(size = 3, color = conf$color), line = list(color = conf$color, width = 2.5),
              hovertemplate = paste0("%{y:,.1f} ", unit)) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit), hovermode = "x unified")
    })
    
    output$plot_top5 <- renderPlotly({
      req(input$year)
      active_tab <- if(is.null(input$tabs_metric)) "gen" else input$tabs_metric
      if(active_tab == "share") { col_name <- conf$share; unit <- "%" } else { col_name <- conf$gen; unit <- "TWh" }
      
      top_names <- data_full %>% filter(year == input$year, Is_Country) %>% 
        arrange(desc(.data[[col_name]])) %>% pull(entity) %>% unique() %>% head(5)
      if(length(top_names) == 0) return(plotly_empty())
      
      top_hist <- data_full %>% filter(entity %in% top_names) %>% select(year, entity, Val = all_of(col_name))
      
      # Ordenamiento del factor para Hover
      factor_order <- top_hist %>% group_by(entity) %>% summarize(max_val = max(Val, na.rm=TRUE)) %>% arrange(desc(max_val)) %>% pull(entity)
      top_hist$entity <- factor(top_hist$entity, levels = factor_order)
      top_hist <- top_hist %>% arrange(entity, year)
      
      plot_ly(top_hist, x = ~year, y = ~Val, color = ~entity, colors = "Set1",
              type = 'scatter', mode = 'lines+markers', marker = list(size=3), line = list(width=2), fill = 'none', 
              hovertemplate = paste0("<b>%{y:,.1f}</b> ", unit)) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit), 
               legend = list(orientation="h", xanchor = "center", x=0.5, y=-0.3), margin = list(b=80, l=50, r=20, t=20), hovermode = "x unified")
    })
    
    output$plot_country <- renderPlotly({
      req(selected_iso())
      active_tab <- if(is.null(input$tabs_metric)) "gen" else input$tabs_metric
      if(active_tab == "share") { col_name <- conf$share; unit <- "%" } else { col_name <- conf$gen; unit <- "TWh" }
      c_dat <- data_full %>% filter(iso3 == selected_iso()) %>% arrange(year)
      if(nrow(c_dat) == 0) return(plotly_empty())
      plot_ly(c_dat, x = ~year, y = ~get(col_name), type = 'scatter', mode = 'lines+markers',
              fill = 'tozeroy', fillcolor = conf$fill, line = list(color = conf$color, width = 2.5), marker = list(size = 4, color = conf$color),
              hovertemplate = paste0("%{y:,.2f} ", unit)) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit), hovermode = "x unified")
    })
  })
}

server <- function(input, output, session) {
  global_long <- reactive({
    df <- er_world %>% select(year, Solar = cols_def$Solar$gen, Eólica = cols_def$Wind$gen, Hidro = cols_def$Hydro$gen, Bio = cols_def$Bio$gen, Geo = cols_def$Geo$gen) %>%
      pivot_longer(cols = -year, names_to = "Fuente", values_to = "TWh") %>% filter(TWh != 0)
    total_por_fuente <- df %>% group_by(Fuente) %>% summarise(Total = max(TWh)) %>% arrange(desc(Total))
    df$Fuente <- factor(df$Fuente, levels = total_por_fuente$Fuente)
    df %>% arrange(year)
  })
  
  output$slider_global_ui <- renderUI({
    dat <- global_long()
    sliderInput("anim_year", "Año:", min = min(dat$year), max = max(dat$year), value = max(dat$year), step = 1, sep = "", width = "100%", animate = animationOptions(interval = 600, loop = FALSE))
  })
  
  output$global_plot <- renderPlotly({
    req(input$anim_year)
    dat <- global_long() %>% filter(year <= input$anim_year)
    cols_g <- c("Solar"=cols_def$Solar$color, "Eólica"=cols_def$Wind$color, "Hidro"=cols_def$Hydro$color, "Bio"=cols_def$Bio$color, "Geo"=cols_def$Geo$color)
    plot_ly(dat, x = ~year, y = ~TWh, color = ~Fuente, colors = cols_g, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy', hovertemplate = "<b>%{y:,.0f} TWh</b>") %>%
      layout(yaxis = list(title = "Generación (TWh)"), legend = list(orientation = "h", y = 1.1), hovermode = "x unified")
  })
  
  output$forecast_plot <- renderPlotly({
    p_dat <- pred_data %>% mutate(Energy_Type = ifelse(Type == "Fossil", "Fósiles", "Renovables"))
    hist_dat <- p_dat %>% filter(!is.na(Real))
    pred_dat_raw <- p_dat %>% filter(!is.na(Pred_LSTM))
    
    last_real_year <- max(hist_dat$Year, na.rm = TRUE)
    stitch_points <- hist_dat %>% filter(Year == last_real_year) %>% mutate(Pred_LSTM = Real) %>% select(Year, Type, Energy_Type, Pred_LSTM)
    pred_dat_stitched <- bind_rows(stitch_points, pred_dat_raw) %>% arrange(Year)
    
    cols_pred <- c("Fósiles" = "#37474F", "Renovables" = "#2E7D32")
    
    plot_ly() %>%
      add_trace(data = hist_dat, x = ~Year, y = ~Real, color = ~Energy_Type, colors = cols_pred, type = 'scatter', mode = 'lines', line = list(width = 3), name = ~paste(Energy_Type, "(Hist)")) %>%
      add_trace(data = pred_dat_stitched, x = ~Year, y = ~Pred_LSTM, color = ~Energy_Type, colors = cols_pred, type = 'scatter', mode = 'lines', line = list(width = 3, dash = 'dot'), name = ~paste(Energy_Type, "(Pred)"), showlegend = FALSE) %>%
      layout(yaxis = list(title = "Participación (%)"), xaxis = list(title = "Año"), hovermode = "x unified",
             shapes = list(list(type = "line", x0 = last_real_year, x1 = last_real_year, y0 = 0, y1 = 1, yref = "paper", line = list(color = "gray", width = 1, dash = "dash"))),
             annotations = list(list(x = last_real_year, y = 0.05, text = "Inicio Proyección", showarrow = FALSE, xanchor = "right", yref="paper", font=list(size=10, color="gray"))))
  })
  
  energy_server_logic("solar", "Solar", er_clean, world_sf)
  energy_server_logic("wind", "Wind", er_clean, world_sf)
  energy_server_logic("hydro", "Hydro", er_clean, world_sf)
  energy_server_logic("bio", "Bio", er_clean, world_sf)
  energy_server_logic("geo", "Geo", er_clean, world_sf)
}

runApp(shinyApp(ui, server), launch.browser = TRUE)