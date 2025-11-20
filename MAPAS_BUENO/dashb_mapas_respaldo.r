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
# 1. CONFIGURACIÓN TÉCNICA Y TEXTOS (OWID / Cita Oficial)
# ==============================================================================

# Paleta de alto contraste profesional
cols_pal <- list(
  Solar = "#FFC107", # Amber 500
  Wind  = "#00BCD4", # Cyan 500
  Hydro = "#1565C0", # Blue 800
  Bio   = "#2E7D32", # Green 800
  Geo   = "#D84315"  # Deep Orange 800
)

# Función auxiliar para generar relleno con transparencia (mismo tono que la línea)
to_rgba <- function(hex, alpha = 0.2) {
  rgb_vals <- col2rgb(hex)
  paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
}

cols_def <- list(
  Solar = list(
    gen = "Solar.generation...TWh..modern.renewable.energy.consumption.",
    share = "Solar.....electricity..share.electricity.solar.",
    color = cols_pal$Solar,
    fill  = to_rgba(cols_pal$Solar, 0.2),
    icon = "sun",
    metric_label = "Generación Eléctrica Solar",
    desc = "Electricidad producida mediante tecnología fotovoltaica (PV). El dato corresponde a la generación eléctrica neta (output), no a la energía primaria equivalente."
  ),
  Wind = list(
    gen = "Wind.generation...TWh..modern.renewable.energy.consumption.",
    share = "Wind.....electricity..share.electricity.wind.",
    color = cols_pal$Wind,
    fill  = to_rgba(cols_pal$Wind, 0.2),
    icon = "wind",
    metric_label = "Generación Eléctrica Eólica",
    desc = "Electricidad generada por el viento. Incluye la producción de parques eólicos tanto en tierra (onshore) como en mar (offshore)."
  ),
  Hydro = list(
    gen = "Hydro.generation...TWh..modern.renewable.energy.consumption.",
    share = "Hydro.....electricity..share.electricity.hydro.",
    color = cols_pal$Hydro,
    fill  = to_rgba(cols_pal$Hydro, 0.2),
    icon = "water",
    metric_label = "Generación Hidroeléctrica",
    desc = "Electricidad generada por centrales hidroeléctricas (de embalse y de pasada). Excluye la energía hidroeléctrica de bombeo puro."
  ),
  Bio = list(
    gen = "Biofuels.production...TWh..biofuel.production.",
    share = NULL, 
    color = cols_pal$Bio,
    fill  = to_rgba(cols_pal$Bio, 0.2),
    icon = "leaf",
    metric_label = "Producción de Biocombustibles",
    desc = "Producción de combustibles líquidos (bioetanol y biodiesel). Nota: La unidad TWh aquí representa el contenido energético del combustible, no electricidad generada."
  ),
  Geo = list(
    gen = "Other.renewables..including.geothermal.and.biomass..electricity.generation...TWh..modern.renewable.energy.consumption.",
    share = NULL, 
    color = cols_pal$Geo,
    fill  = to_rgba(cols_pal$Geo, 0.2),
    icon = "fire",
    metric_label = "Generación Eléctrica (Otras)",
    desc = "Electricidad generada a partir de biomasa sólida (madera/residuos), energía geotérmica y tecnologías marinas (mareomotriz/undimotriz)."
  )
)

# ==============================================================================
# 2. CARGA DE DATOS
# ==============================================================================

er_data <- read.csv("cruce_er_cf.csv")
pred_data <- read.csv("predicciones_world_fossil_renew_lstm_train_test.csv")

er_clean <- er_data %>%
  mutate(
    iso3 = countrycode(Entity, origin = "country.name", destination = "iso3c", warn = FALSE),
    Is_Country = !is.na(iso3)
  )

if ("World" %in% er_clean$Entity) {
  er_world <- er_clean %>% filter(Entity == "World")
} else {
  er_world <- er_clean %>%
    filter(Is_Country) %>%
    group_by(Year) %>%
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>%
    mutate(Entity = "World")
}

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% select(iso_a3, geometry)

# ==============================================================================
# 3. INTERFAZ DE USUARIO (UI)
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
            card(card_header("Tendencia Mundial"), plotlyOutput(ns("plot_world"), height = "250px")),
            # Título Actualizado
            card(card_header("Top 5 (Evolución Comparada)"), plotlyOutput(ns("plot_top5"), height = "250px")),
            card(card_header(textOutput(ns("country_title"))), plotlyOutput(ns("plot_country"), height = "250px"))
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
    .leaflet-tooltip { font-family: sans-serif; font-size: 12px; font-weight: bold; color: #333; background: rgba(255,255,255,0.9); border: 1px solid #ccc; }
  "))),
  
  div(class = "container-fluid py-5 text-center bg-white shadow-sm mb-4",
      h1("Monitor Global de Transición Energética", style="font-weight: 900; color: #2c3e50;"),
      p("Análisis integral de la matriz energética mundial: Historia y Proyecciones (LSTM).", class="text-muted")
  ),
  
  # --- BLOQUE 1: PANORAMA GLOBAL ---
  div(class = "container-fluid px-4 mb-5",
      div(class = "card shadow-sm border-0",
          div(class = "card-header bg-white py-3", style="border-top: 5px solid #2c3e50;",
              h3(icon("chart-line"), "Panorama Global y Proyecciones", style = "color: #2c3e50; font-weight: 800; margin: 0;")
          ),
          div(class = "card-body",
              tabsetPanel(
                tabPanel("Historia: Comparativa por Fuente (TWh)", br(),
                         sidebarLayout(
                           sidebarPanel(width = 3, h5("Línea de Tiempo"), uiOutput("slider_global_ui")),
                           mainPanel(width = 9, plotlyOutput("global_plot", height = "450px"))
                         )
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
  
  # --- BLOQUES TEMÁTICOS ---
  div(class = "container-fluid px-4",
      energy_block_ui("solar", "Energía Solar", cols_def$Solar$color, "sun", cols_def$Solar$desc, TRUE, cols_def$Solar$metric_label),
      energy_block_ui("wind", "Energía Eólica", cols_def$Wind$color, "wind", cols_def$Wind$desc, TRUE, cols_def$Wind$metric_label),
      energy_block_ui("hydro", "Energía Hidroeléctrica", cols_def$Hydro$color, "water", cols_def$Hydro$desc, TRUE, cols_def$Hydro$metric_label),
      energy_block_ui("bio", "Biocombustibles", cols_def$Bio$color, "leaf", cols_def$Bio$desc, FALSE, cols_def$Bio$metric_label),
      energy_block_ui("geo", "Geotérmica y Otros", cols_def$Geo$color, "fire", cols_def$Geo$desc, FALSE, cols_def$Geo$metric_label)
  ),
  
  # --- PIE DE PÁGINA CON CITA OFICIAL ---
  div(class="container-fluid py-4 mt-5 bg-light text-center border-top",
      p(class="mb-1 text-muted small", style="font-weight: 600;", 
        "Información obtenida de:"),
      p(class="text-muted small", style="font-style: italic;", 
        "Ember (2025); Energy Institute - Statistical Review of World Energy (2025) – with major processing by Our World in Data")
  )
)

# ==============================================================================
# 4. SERVIDOR (LOGICA)
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
      nm <- data_full %>% filter(iso3 == selected_iso()) %>% pull(Entity) %>% unique()
      paste("Evolución Histórica:", nm[1])
    })
    
    active_metric <- reactive({ if(is.null(input$tabs_metric)) "gen" else input$tabs_metric })
    
    # Slider con Play
    output$slider_ui <- renderUI({
      sliderInput(ns("year"), "Año:", min = min(data_full$Year), max = max(data_full$Year), 
                  value = max(data_full$Year), step = 1, sep = "", 
                  animate = animationOptions(interval = 800, loop = FALSE))
    })
    
    # MAPA SÁNDWICH V2 (USO DE TOOLTIPS EN VEZ DE LABELS DIRECTOS PARA EVITAR CONFLICTOS)
    render_custom_map <- function(type) {
      renderLeaflet({
        req(input$year)
        
        # 1. Selección de columnas según pestaña (Generación vs Participación)
        col_name <- if(type == "gen") conf$gen else conf$share
        unit <- if(type == "gen") "TWh" else "%"
        
        # 2. Preparación de Datos Espaciales
        map_dat <- data_full %>% 
          filter(Year == input$year, Is_Country) %>% 
          select(iso3, Entity, Val = all_of(col_name))
        
        geo <- world_geo %>% left_join(map_dat, by = c("iso_a3" = "iso3"))
        vals <- geo$Val
        
        # 3. Definición de Paletas (Ceros vs NAs)
        if(type == "gen") {
          bins <- c(0, 1, 10, 50, 100, 500, Inf)
          pal <- colorBin("YlGnBu", domain = vals, bins = bins, na.color = "#ffffff") 
        } else {
          bins <- c(0, 1, 5, 10, 20, 50, 100)
          pal <- colorBin("PuBuGn", domain = vals, bins = bins, na.color = "#ffffff")
        }
        
        # 4. Creación del Tooltip (Lo que sale al pasar el mouse sobre el país)
        tooltip_content <- sprintf(
          "<strong>%s</strong><br/>%s", 
          geo$Entity,
          ifelse(is.na(geo$Val), "Sin Información", paste0(format(round(geo$Val, 2), big.mark=","), " ", unit))
        ) %>% lapply(HTML)
        
        # 5. Construcción del Mapa (Estrategia de Capas)
        leaflet(geo) %>%
          # Fija los límites para que el mapa no se repita ni se vaya al gris
          setMaxBounds(-180, -90, 180, 90) %>%
          
          # CAPA 1 (FONDO): Mapa base SIN etiquetas (Tierra y Mar)
          addProviderTiles("CartoDB.PositronNoLabels", 
                           options = providerTileOptions(noWrap = TRUE)) %>%
          
          # CAPA 2 (MEDIO): Polígonos con tus datos
          addPolygons(
            fillColor = ~pal(Val), 
            weight = 1, 
            color = "white",       # Borde blanco delgado entre países
            opacity = 1, 
            fillOpacity = 0.85,
            layerId = ~iso_a3,
            
            # Configuración del Tooltip (Etiqueta interactiva)
            label = tooltip_content,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            
            # Highlight: bringToFront = FALSE es CLAVE para no tapar las letras de la Capa 3
            highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = FALSE)
          ) %>%
          
          # CAPA 3 (ARRIBA): Solo etiquetas (Nombres de países)
          # zIndex = 650 fuerza a que esta capa esté "más cerca" de tus ojos que los polígonos
          addProviderTiles("CartoDB.PositronOnlyLabels", 
                           options = providerTileOptions(noWrap = TRUE, zIndex = 650)) %>%
          
          # Vista inicial y Leyenda
          setView(0, 20, 1.5) %>%
          addLegend(pal = pal, values = vals, title = unit, position = "bottomright", na.label = "Sin Info")
      })
    }
    output$map_gen <- render_custom_map("gen")
    output$map_share <- render_custom_map("share")
    
    # GRÁFICA MUNDIAL (FILLCOLOR CORREGIDO)
    output$plot_world <- renderPlotly({
      col_name <- if(active_metric() == "gen") conf$gen else conf$share
      unit <- if(active_metric() == "gen") "TWh" else "%"
      w_dat <- er_world %>% select(Year, Val = all_of(col_name)) %>% arrange(Year)
      
      plot_ly(w_dat, x = ~Year, y = ~Val, type = 'scatter', mode = 'lines+markers', 
              fill = 'tozeroy',
              fillcolor = conf$fill, # Uso explícito del color RGBA transparente
              marker = list(size = 3, color = conf$color), 
              line = list(color = conf$color, width = 2.5),
              hovertemplate = paste0("<b>%{x}</b>: %{y:,.1f} ", unit, "<extra></extra>")) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit))
    })
    
    # TOP 5 (EVOLUCIÓN COMPARADA)
    output$plot_top5 <- renderPlotly({
      req(input$year)
      col_name <- if(active_metric() == "gen") conf$gen else conf$share
      unit <- if(active_metric() == "gen") "TWh" else "%"
      
      top_iso <- data_full %>% filter(Year == input$year, Is_Country) %>% arrange(desc(.data[[col_name]])) %>% head(5) %>% pull(iso3)
      if(length(top_iso) == 0) return(plotly_empty())
      
      top_hist <- data_full %>% filter(iso3 %in% top_iso) %>% select(Year, Entity, Val = all_of(col_name)) %>% arrange(Year)
      
      plot_ly(top_hist, x = ~Year, y = ~Val, color = ~Entity, colors = "Set1",
              type = 'scatter', mode = 'lines+markers', marker = list(size=3), line = list(width=2),
              hovertemplate = paste0("<b>%{x}</b><br>%{y:,.1f} ", unit, "<extra></extra>")) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit), legend = list(orientation="h", y=-0.2))
    })
    
    # PAÍS (FILLCOLOR CORREGIDO)
    output$plot_country <- renderPlotly({
      req(selected_iso())
      col_name <- if(active_metric() == "gen") conf$gen else conf$share
      unit <- if(active_metric() == "gen") "TWh" else "%"
      c_dat <- data_full %>% filter(iso3 == selected_iso()) %>% arrange(Year)
      if(nrow(c_dat) == 0) return(plotly_empty())
      
      plot_ly(c_dat, x = ~Year, y = ~get(col_name), type = 'scatter', mode = 'lines+markers',
              fill = 'tozeroy',
              fillcolor = conf$fill,
              line = list(color = conf$color, width = 2.5), 
              marker = list(size = 4, color = conf$color),
              hovertemplate = paste0("<b>%{x}</b>: %{y:,.2f} ", unit, "<extra></extra>")) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit))
    })
  })
}

server <- function(input, output, session) {
  
  # PREDICCIÓN CON LÍNEA DE CORTE
  output$forecast_plot <- renderPlotly({
    p_dat <- pred_data %>% mutate(Energy_Type = ifelse(Type == "Fossil", "Fósiles", "Renovables"))
    last_real_f <- p_dat %>% filter(Energy_Type=="Fósiles", !is.na(Real)) %>% slice_max(Year, n=1)
    last_real_r <- p_dat %>% filter(Energy_Type=="Renovables", !is.na(Real)) %>% slice_max(Year, n=1)
    cut_year <- last_real_f$Year
    
    stitch_f <- data.frame(Year=last_real_f$Year, Type="Fossil", Real=NA, Pred_LSTM=last_real_f$Real, Energy_Type="Fósiles")
    stitch_r <- data.frame(Year=last_real_r$Year, Type="Renewable", Real=NA, Pred_LSTM=last_real_r$Real, Energy_Type="Renovables")
    
    final_pred <- bind_rows(p_dat, stitch_f, stitch_r) %>% arrange(Year)
    cols_pred <- c("Fósiles" = "#37474F", "Renovables" = "#2E7D32")
    
    plot_ly(final_pred) %>%
      add_trace(x = ~Year, y = ~Real, color = ~Energy_Type, colors = cols_pred,
                type = 'scatter', mode = 'lines+markers', marker = list(size=4), line = list(width=3),
                name = ~paste(Energy_Type, "(Hist)")) %>%
      add_trace(x = ~Year, y = ~Pred_LSTM, color = ~Energy_Type, colors = cols_pred,
                type = 'scatter', mode = 'lines+markers', marker = list(size=4), 
                line = list(width=3, dash='dot'),
                name = ~paste(Energy_Type, "(Pred)"), showlegend = FALSE) %>%
      layout(
        yaxis = list(title = "Participación en Energía Primaria (%)"), 
        xaxis = list(title = "Año"), 
        hovermode = "x unified",
        shapes = list(list(type = "line", x0 = cut_year, x1 = cut_year, y0 = 0, y1 = 1, yref = "paper",
                           line = list(color = "gray", width = 1, dash = "dash"))),
        annotations = list(list(x = cut_year, y = 0.05, text = "Inicio Proyección", showarrow = FALSE, xanchor = "right", yref="paper", font=list(size=10, color="gray")))
      )
  })
  
  # HISTORIA GLOBAL (Filtro estricto de Ceros)
  global_long <- reactive({
    er_world %>%
      select(Year, Solar = cols_def$Solar$gen, Eólica = cols_def$Wind$gen, Hidro = cols_def$Hydro$gen, 
             Bio = cols_def$Bio$gen, Geo = cols_def$Geo$gen) %>%
      pivot_longer(cols = -Year, names_to = "Fuente", values_to = "TWh") %>%
      filter(TWh != 0) %>% arrange(Year)
  })
  
  output$slider_global_ui <- renderUI({
    dat <- global_long()
    sliderInput("anim_year", "Año:", min = min(dat$Year), max = max(dat$Year), value = min(dat$Year),
                step = 1, sep = "", animate = animationOptions(interval = 600, loop = FALSE))
  })
  
  output$global_plot <- renderPlotly({
    req(input$anim_year)
    dat <- global_long() %>% filter(Year <= input$anim_year) %>% arrange(Year)
    
    cols_g <- c("Solar"=cols_def$Solar$color, "Eólica"=cols_def$Wind$color, 
                "Hidro"=cols_def$Hydro$color, "Bio"=cols_def$Bio$color, "Geo"=cols_def$Geo$color)
    
    fills_g <- c("Solar"=cols_def$Solar$fill, "Eólica"=cols_def$Wind$fill, 
                 "Hidro"=cols_def$Hydro$fill, "Bio"=cols_def$Bio$fill, "Geo"=cols_def$Geo$fill)
    
    p <- plot_ly()
    for(src in names(cols_g)) {
      d_src <- dat %>% filter(Fuente == src)
      if(nrow(d_src) > 0) {
        p <- p %>% add_trace(data = d_src, x = ~Year, y = ~TWh, 
                             type = 'scatter', mode = 'lines+markers',
                             name = src,
                             line = list(color = cols_g[[src]], width = 2.5),
                             fill = 'tozeroy',
                             fillcolor = fills_g[[src]], 
                             marker = list(size = 3, color = cols_g[[src]]),
                             hovertemplate = "%{y:,.0f} TWh<extra></extra>")
      }
    }
    p %>% layout(yaxis = list(title = "TWh"), legend = list(orientation = "h", y = 1.1))
  })
  
  energy_server_logic("solar", "Solar", er_clean, world_sf)
  energy_server_logic("wind", "Wind", er_clean, world_sf)
  energy_server_logic("hydro", "Hydro", er_clean, world_sf)
  energy_server_logic("bio", "Bio", er_clean, world_sf)
  energy_server_logic("geo", "Geo", er_clean, world_sf)
}

runApp(shinyApp(ui, server), launch.browser = TRUE)