# ============================================================
# mod_raster.R — Módulo de visualización y análisis raster
# ============================================================

library(terra)
library(tidyterra)
library(ggplot2)
library(leaflet)
library(scales)

# ── UI ──────────────────────────────────────────────────────
mod_raster_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Opciones Raster",
      
      h6(class = "text-muted fw-bold text-uppercase small", "Banda"),
      
      selectInput(ns("band"), "Seleccionar banda:", choices = NULL),
      
      h6(class = "text-muted fw-bold text-uppercase small", "Paleta"),
      
      selectInput(ns("raster_pal"), "Paleta:",
                  choices = c("viridis", "magma", "plasma", "inferno",
                              "terrain", "topo", "heat"),
                  selected = "viridis"),
      
      checkboxInput(ns("reverse_pal"), "Invertir paleta", value = FALSE),
      
      hr(),
      
      h6(class = "text-muted fw-bold text-uppercase small", "Operaciones"),
      
      selectInput(ns("operation"), "Operación:",
                  choices = c(
                    "Ninguna"                    = "none",
                    "Normalizar (0–1)"           = "normalize",
                    "Escala logarítmica"         = "log",
                    "Umbral (mayor que)"         = "threshold"
                  )),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'threshold'", ns("operation")),
        numericInput(ns("threshold_val"), "Valor umbral:", value = 0)
      ),
      
      hr(),
      
      h6(class = "text-muted fw-bold text-uppercase small", "Exportar"),
      
      downloadButton(ns("download_rst"), "Descargar GeoTIFF",
                     class = "btn-sm btn-outline-primary w-100")
    ),
    
    # ── Panel principal ──────────────────────────────────────
    layout_columns(
      col_widths = c(8, 4),
      
      # Mapa / gráfico raster
      navset_card_tab(
        nav_panel(
          "Mapa interactivo",
          card_body(
            class = "p-0",
            style = "height: 500px;",
            leafletOutput(ns("raster_map"), height = "100%")
          )
        ),
        nav_panel(
          "Gráfico ggplot",
          card_body(
            plotOutput(ns("raster_plot"), height = "450px")
          )
        )
      ),
      
      # Panel de estadísticas raster
      card(
        card_header(icon("calculator"), " Estadísticas de banda"),
        card_body(
          tableOutput(ns("raster_stats")),
          hr(),
          plotOutput(ns("histogram"), height = "200px")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_raster_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    # ── Actualizar selector de bandas ─────────────────────────
    observeEvent(shared$raster_data, {
      req(shared$raster_data)
      r <- shared$raster_data
      bandas <- seq_len(terra::nlyr(r))
      names(bandas) <- paste0("Banda ", bandas,
                              ifelse(!is.null(names(r)),
                                     paste0(" (", names(r), ")"), ""))
      updateSelectInput(session, "band", choices = bandas, selected = 1)
    })
    
    # ── Banda seleccionada y operación aplicada ───────────────
    raster_band <- reactive({
      req(shared$raster_data, input$band)
      r <- shared$raster_data[[as.integer(input$band)]]
      
      switch(input$operation,
        "normalize"  = (r - terra::global(r, "min", na.rm = TRUE)$min) /
                       (terra::global(r, "max", na.rm = TRUE)$max -
                        terra::global(r, "min", na.rm = TRUE)$min),
        "log"        = terra::app(r, fun = function(x) log1p(pmax(x, 0))),
        "threshold"  = {
          req(input$threshold_val)
          r > input$threshold_val
        },
        r  # "none" por defecto
      )
    })
    
    # ── Mapa leaflet con raster ───────────────────────────────
    output$raster_map <- renderLeaflet({
      req(raster_band())
      r <- raster_band()
      
      # Reproyectar a WGS84 si es necesario
      if (!terra::is.lonlat(r)) {
        r <- terra::project(r, "EPSG:4326")
      }
      
      vals     <- terra::values(r, na.rm = TRUE)
      pal_fn   <- if (input$reverse_pal) 
                    colorNumeric(rev(get_palette(input$raster_pal)), vals, na.color = NA)
                  else
                    colorNumeric(get_palette(input$raster_pal), vals, na.color = NA)
      
      # Convertir a raster de R para leaflet
      r_raster <- raster::raster(r)
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
        addProviderTiles(providers$CartoDB.Positron,   group = "Light") %>%
        addProviderTiles(providers$Esri.WorldImagery,  group = "Satélite") %>%
        addRasterImage(r_raster, colors = pal_fn, opacity = 0.85, group = "Raster") %>%
        addLegend("bottomright", pal = pal_fn, values = vals,
                  title = paste("Banda", input$band), opacity = 0.9) %>%
        addLayersControl(
          baseGroups  = c("Light", "Dark", "Satélite"),
          overlayGroups = c("Raster"),
          options     = layersControlOptions(collapsed = FALSE)
        ) %>%
        addScaleBar()
    })
    
    # ── Gráfico ggplot con tidyterra ──────────────────────────
    output$raster_plot <- renderPlot({
      req(raster_band())
      r <- raster_band()
      
      pal_dir <- if (input$reverse_pal) -1 else 1
      
      ggplot() +
        tidyterra::geom_spatraster(data = r) +
        scale_fill_viridis_c(
          option    = if (input$raster_pal %in% c("viridis","magma","plasma","inferno"))
                        input$raster_pal else "viridis",
          direction = pal_dir,
          na.value  = "transparent",
          name      = paste("Banda", input$band)
        ) +
        theme_minimal(base_family = "sans") +
        theme(
          plot.background = element_rect(fill = "#F8FAF9", color = NA),
          legend.position = "right"
        ) +
        labs(title = paste("Raster — Banda", input$band),
             x = "Longitud", y = "Latitud")
    })
    
    # ── Estadísticas ──────────────────────────────────────────
    output$raster_stats <- renderTable({
      req(raster_band())
      r  <- raster_band()
      
      vals <- terra::values(r, na.rm = TRUE)
      
      data.frame(
        Estadística = c("Mínimo", "Máximo", "Media", "Mediana",
                        "Desv. Estándar", "NAs", "Total celdas"),
        Valor = c(
          round(min(vals),   4),
          round(max(vals),   4),
          round(mean(vals),  4),
          round(median(vals),4),
          round(sd(vals),    4),
          sum(is.na(terra::values(r))),
          terra::ncell(r)
        )
      )
    }, striped = TRUE, bordered = TRUE, small = TRUE)
    
    # ── Histograma ────────────────────────────────────────────
    output$histogram <- renderPlot({
      req(raster_band())
      r    <- raster_band()
      vals <- terra::values(r, na.rm = TRUE)
      
      df <- data.frame(v = vals)
      
      ggplot(df, aes(x = v)) +
        geom_histogram(bins = 50, fill = "#52B788", color = NA, alpha = 0.8) +
        theme_minimal(base_size = 10) +
        theme(plot.background = element_rect(fill = "white", color = NA)) +
        labs(x = "Valor", y = "Frecuencia", title = "Distribución de valores")
    })
    
    # ── Descarga ──────────────────────────────────────────────
    output$download_rst <- downloadHandler(
      filename = function() paste0("raster_banda_", input$band, ".tif"),
      content  = function(file) {
        terra::writeRaster(raster_band(), file, overwrite = TRUE)
      }
    )
    
  })
}

# ── Helper: paletas ──────────────────────────────────────────
get_palette <- function(name) {
  switch(name,
    "viridis"  = viridisLite::viridis(256),
    "magma"    = viridisLite::magma(256),
    "plasma"   = viridisLite::plasma(256),
    "inferno"  = viridisLite::inferno(256),
    "terrain"  = terrain.colors(256),
    "topo"     = topo.colors(256),
    "heat"     = heat.colors(256),
    viridisLite::viridis(256)
  )
}
