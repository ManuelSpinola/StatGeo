# ============================================================
# mod_vector.R — Módulo de visualización vectorial (v2)
# Soporta: Point, MultiPoint, Line, MultiLine, Polygon, MultiPolygon
# Mapas: interactivo (leaflet) + estático (ggplot2 + sf)
# ============================================================


# ── UI ──────────────────────────────────────────────────────
mod_vector_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Opciones",

      # ── Tipo de mapa ──────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small",
         "Tipo de mapa"),

      radioButtons(ns("map_type"), NULL,
                   choices  = c("Interactivo (Leaflet)" = "leaflet",
                                "Estatico (ggplot2)"    = "ggplot"),
                   selected = "leaflet",
                   inline   = TRUE),

      hr(),

      # ── Variable para colorear ────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small",
         "Visualizacion"),

      selectInput(ns("color_col"), "Colorear por variable:",
                  choices = NULL),

      selectInput(ns("color_pal"), "Paleta:",
                  choices = c("viridis", "magma", "plasma", "inferno",
                              "RdYlGn", "Blues", "Spectral", "Set1",
                              "Dark2"),
                  selected = "viridis"),

      # Opciones especificas del mapa interactivo
      conditionalPanel(
        condition = sprintf("input['%s'] == 'leaflet'", ns("map_type")),
        selectInput(ns("basemap"), "Mapa base:",
                    choices = c("CartoDB Positron"  = "CartoDB.Positron",
                                "CartoDB Dark"      = "CartoDB.DarkMatter",
                                "OpenStreetMap"     = "OpenStreetMap",
                                "Satelite (ESRI)"   = "Esri.WorldImagery",
                                "Topo (ESRI)"       = "Esri.WorldTopoMap"),
                    selected = "CartoDB.Positron"),
        sliderInput(ns("point_radius"), "Tamanio de puntos:",
                    min = 2, max = 15, value = 5, step = 1),
        sliderInput(ns("fill_opacity"), "Opacidad de relleno:",
                    min = 0.1, max = 1, value = 0.7, step = 0.05)
      ),

      # Opciones especificas del mapa estatico
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ggplot'", ns("map_type")),
        selectInput(ns("gg_theme"), "Tema:",
                    choices = c("Minimal"   = "minimal",
                                "Void"      = "void",
                                "Classic"   = "classic",
                                "Dark"      = "dark"),
                    selected = "minimal"),
        checkboxInput(ns("gg_north"), "Mostrar norte / escala",
                      value = FALSE),
        downloadButton(ns("download_plot"), "Guardar PNG",
                       class = "btn-sm btn-outline-secondary w-100 mt-1")
      ),

      hr(),

      # ── Reproyeccion ──────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small",
         "Proyeccion"),

      selectInput(ns("target_crs"), "Reproyectar a:",
                  choices = c(
                    "Sin cambio"           = "original",
                    "WGS84 (EPSG:4326)"    = "4326",
                    "Mercator (EPSG:3857)" = "3857",
                    "CRTM05 (EPSG:5367)"   = "5367"
                  )),

      hr(),

      # ── Filtros ───────────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small",
         "Filtrar atributos"),

      selectInput(ns("filter_col"), "Columna para filtrar:",
                  choices = NULL),

      uiOutput(ns("filter_ui")),

      hr(),

      # ── Exportar datos ────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small",
         "Exportar datos"),

      selectInput(ns("export_fmt"), "Formato:",
                  choices = c("GeoJSON"            = "geojson",
                              "GeoPackage"         = "gpkg",
                              "CSV (sin geometria)" = "csv")),

      downloadButton(ns("download_vec"), "Descargar",
                     class = "btn-sm btn-outline-success w-100")
    ),

    # ── Panel principal ──────────────────────────────────────
    layout_columns(
      col_widths = 12,

      # Mapa (interactivo o estatico segun radio)
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(icon("map"), " Mapa"),
          uiOutput(ns("map_header_info"))
        ),
        card_body(
          class = "p-0",
          style = "min-height: 500px;",
          # Mapa leaflet
          conditionalPanel(
            condition = sprintf("input['%s'] == 'leaflet'", ns("map_type")),
            leafletOutput(ns("map_leaflet"), height = "500px")
          ),
          # Mapa ggplot
          conditionalPanel(
            condition = sprintf("input['%s'] == 'ggplot'", ns("map_type")),
            plotOutput(ns("map_ggplot"), height = "500px")
          )
        )
      ),

      # Tabla de atributos
      card(
        card_header(icon("table"), " Tabla de atributos"),
        card_body(
          DTOutput(ns("attr_table"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_vector_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Detectar tipo de geometria ────────────────────────────
    geom_type <- reactive({
      req(shared$sf_data)
      gt <- as.character(sf::st_geometry_type(shared$sf_data,
                                              by_geometry = FALSE))
      toupper(gt)
    })

    # ── Datos reproyectados ───────────────────────────────────
    sf_processed <- reactive({
      req(shared$sf_data)
      sf_obj <- shared$sf_data
      if (!is.null(input$target_crs) && input$target_crs != "original") {
        sf_obj <- sf::st_transform(sf_obj,
                                   as.integer(input$target_crs))
      }
      sf_obj
    })

    # ── Datos filtrados ───────────────────────────────────────
    sf_filtered <- reactive({
      req(sf_processed())
      sf_obj <- sf_processed()
      col    <- input$filter_col

      if (is.null(col) || col == "(ninguno)") return(sf_obj)

      col_data <- sf_obj[[col]]

      if (is.numeric(col_data) && !is.null(input$filter_range)) {
        sf_obj <- sf_obj %>%
          dplyr::filter(.data[[col]] >= input$filter_range[1],
                        .data[[col]] <= input$filter_range[2])
      } else if (!is.numeric(col_data) && !is.null(input$filter_cats)) {
        if (length(input$filter_cats) > 0)
          sf_obj <- sf_obj %>%
            dplyr::filter(.data[[col]] %in% input$filter_cats)
      }
      sf_obj
    })

    # ── Actualizar selectores ─────────────────────────────────
    observeEvent(shared$sf_data, {
      req(shared$sf_data)
      cols     <- names(shared$sf_data)
      num_cols <- cols[sapply(sf::st_drop_geometry(shared$sf_data),
                              is.numeric)]

      updateSelectInput(session, "color_col",
                        choices  = c("(ninguno)" = "", cols),
                        selected = if (length(num_cols) > 0) num_cols[1]
                        else "")
      updateSelectInput(session, "filter_col",
                        choices = c("(ninguno)" = "(ninguno)", cols))
    })

    # ── Filtro dinamico ───────────────────────────────────────
    output$filter_ui <- renderUI({
      req(sf_processed())
      col <- input$filter_col
      if (is.null(col) || col == "(ninguno)") return(NULL)
      col_data <- sf_processed()[[col]]

      if (is.numeric(col_data)) {
        rng <- range(col_data, na.rm = TRUE)
        sliderInput(ns("filter_range"), "Rango:",
                    min   = floor(rng[1]),
                    max   = ceiling(rng[2]),
                    value = rng,
                    step  = diff(rng) / 100)
      } else {
        vals <- sort(unique(as.character(col_data)))
        checkboxGroupInput(ns("filter_cats"), "Categorias:",
                           choices  = vals,
                           selected = vals)
      }
    })

    # ── Info en header del mapa ───────────────────────────────
    output$map_header_info <- renderUI({
      req(sf_processed())
      crs_name <- sf::st_crs(sf_processed())$input %||% "Sin CRS"
      gt       <- geom_type()
      tagList(
        tags$span(class = "badge bg-secondary me-1 small", gt),
        tags$span(class = "badge bg-light text-dark small", crs_name)
      )
    })

    # ── MAPA LEAFLET (interactivo) ────────────────────────────
    output$map_leaflet <- renderLeaflet({
      req(sf_filtered())
      sf_obj   <- sf::st_transform(sf_filtered(), 4326)
      color_by <- input$color_col
      pal_name <- input$color_pal %||% "viridis"
      radius   <- input$point_radius %||% 5
      opacity  <- input$fill_opacity %||% 0.7
      basemap  <- input$basemap %||% "CartoDB.Positron"

      m <- leaflet() %>%
        addProviderTiles(basemap) %>%
        addScaleBar(position = "bottomleft")

      # Construir popups
      popup_html <- build_popups(sf_obj)

      # Con o sin variable de color
      has_color <- !is.null(color_by) && color_by != "" &&
        color_by %in% names(sf_obj)

      if (has_color) {
        col_data <- sf_obj[[color_by]]
        pal <- if (is.numeric(col_data))
          colorNumeric(pal_name,  col_data, na.color = "#aaaaaa")
        else
          colorFactor(pal_name,  col_data, na.color = "#aaaaaa")

        m <- add_layer(m, sf_obj, col_data, pal, popup_html,
                       radius, opacity, color_by)
      } else {
        m <- add_layer(m, sf_obj, col_data = NULL, pal = NULL,
                       popup_html, radius, opacity)
      }

      m
    })

    # ── MAPA GGPLOT (estatico) ────────────────────────────────
    output$map_ggplot <- renderPlot({
      req(sf_filtered())
      sf_obj   <- sf_filtered()
      color_by <- input$color_col
      pal_name <- input$color_pal %||% "viridis"
      gt       <- geom_type()

      has_color <- !is.null(color_by) && color_by != "" &&
        color_by %in% names(sf_obj)

      # Base del plot
      p <- ggplot(sf_obj)

      if (has_color) {
        col_data <- sf_obj[[color_by]]

        if (grepl("POINT|MULTIPOINT", gt)) {
          p <- p + geom_sf(aes(color = .data[[color_by]]),
                           size = 2, alpha = 0.8)
        } else if (grepl("LINE|LINESTRING", gt)) {
          p <- p + geom_sf(aes(color = .data[[color_by]]),
                           linewidth = 0.8)
        } else {
          p <- p + geom_sf(aes(fill = .data[[color_by]]),
                           color = "white", linewidth = 0.2)
        }

        # Escala de color segun tipo
        if (is.numeric(col_data)) {
          scale_fn <- if (grepl("POINT|LINE", gt))
            scale_color_viridis_c(option = pal_name, name = color_by)
          else
            scale_fill_viridis_c(option = pal_name, name = color_by)
          p <- p + scale_fn
        } else {
          scale_fn <- if (grepl("POINT|LINE", gt))
            scale_color_brewer(palette = pal_name, name = color_by)
          else
            scale_fill_brewer(palette = pal_name, name = color_by)
          p <- tryCatch(p + scale_fn, error = function(e) p)
        }

      } else {
        # Sin variable: color fijo segun geometria
        if (grepl("POINT|MULTIPOINT", gt)) {
          p <- p + geom_sf(color = colores$primario, size = 2, alpha = 0.7)
        } else if (grepl("LINE|LINESTRING", gt)) {
          p <- p + geom_sf(color = colores$primario, linewidth = 0.8)
        } else {
          p <- p + geom_sf(fill = colores$secundario, color = "white",
                           linewidth = 0.3, alpha = 0.8)
        }
      }

      # Tema
      p <- p + switch(input$gg_theme %||% "minimal",
                      "minimal" = theme_minimal(),
                      "void"    = theme_void(),
                      "classic" = theme_classic(),
                      "dark"    = theme_dark(),
                      theme_minimal()
      )

      p <- p + labs(
        title    = if (has_color)
          paste("Distribucion de:", color_by)
        else paste("Datos vectoriales —", gt),
        subtitle = paste(format(nrow(sf_obj), big.mark = ","),
                         "features |",
                         sf::st_crs(sf_obj)$input %||% "Sin CRS"),
        x = NULL, y = NULL
      ) +
        theme(
          plot.title    = element_text(size = 13, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray50"),
          legend.position = "right"
        )

      p
    })

    # ── Descarga PNG ggplot ───────────────────────────────────
    output$download_plot <- downloadHandler(
      filename = function() "mapa_vectorial.png",
      content  = function(file) {
        req(sf_filtered())
        # Re-render el mismo plot
        sf_obj   <- sf_filtered()
        color_by <- input$color_col
        gt       <- geom_type()
        has_color <- !is.null(color_by) && color_by != "" &&
          color_by %in% names(sf_obj)

        p <- ggplot(sf_obj)
        if (has_color) {
          if (grepl("POINT|MULTIPOINT", gt))
            p <- p + geom_sf(aes(color = .data[[color_by]]),
                             size = 2, alpha = 0.8)
          else if (grepl("LINE", gt))
            p <- p + geom_sf(aes(color = .data[[color_by]]))
          else
            p <- p + geom_sf(aes(fill = .data[[color_by]]),
                             color = "white", linewidth = 0.2)

          if (is.numeric(sf_obj[[color_by]]))
            p <- p + scale_fill_viridis_c(option = input$color_pal,
                                          name   = color_by)
        } else {
          if (grepl("POINT|MULTIPOINT", gt))
            p <- p + geom_sf(color = colores$primario, size = 2)
          else if (grepl("LINE", gt))
            p <- p + geom_sf(color = colores$primario)
          else
            p <- p + geom_sf(fill = colores$secundario, color = "white")
        }
        p <- p + theme_minimal() +
          labs(title = paste("Mapa —", gt), x = NULL, y = NULL)

        ggplot2::ggsave(file, plot = p, width = 10, height = 8,
                        dpi = 150, bg = "white")
      }
    )

    # ── Tabla de atributos ────────────────────────────────────
    output$attr_table <- renderDT({
      req(sf_filtered())
      df <- sf::st_drop_geometry(sf_filtered())
      datatable(
        df,
        options  = list(pageLength = 10, scrollX = TRUE,
                        dom = "Bfrtip", buttons = c("csv", "excel")),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Descarga de datos ─────────────────────────────────────
    output$download_vec <- downloadHandler(
      filename = function() paste0("datos_vectoriales.",
                                   input$export_fmt),
      content  = function(file) {
        sf_out <- sf_filtered()
        if (input$export_fmt == "csv") {
          write.csv(sf::st_drop_geometry(sf_out), file,
                    row.names = FALSE)
        } else {
          sf::st_write(sf_out, file, quiet = TRUE,
                       delete_dsn = TRUE)
        }
      }
    )

  })
}

# ── Helper: construir popups HTML ────────────────────────────
build_popups <- function(sf_obj) {
  df <- sf::st_drop_geometry(sf_obj)
  # Limitar a 10 columnas para popups legibles
  cols <- names(df)[seq_len(min(10, ncol(df)))]
  df   <- df[, cols, drop = FALSE]

  lapply(seq_len(nrow(df)), function(i) {
    rows <- mapply(function(nm, val) {
      sprintf("<tr><td style='padding:2px 6px;font-weight:600;color:#555'>%s</td><td style='padding:2px 6px'>%s</td></tr>",
              nm, as.character(val))
    }, names(df), as.character(unlist(df[i, ])))
    paste0("<table style='font-size:12px;border-collapse:collapse'>",
           paste(rows, collapse = ""), "</table>")
  })
}

# ── Helper: agregar capa al mapa leaflet segun geometria ──────
add_layer <- function(m, sf_obj, col_data, pal, popups,
                      radius = 5, opacity = 0.7,
                      legend_title = NULL) {

  gt <- toupper(as.character(
    sf::st_geometry_type(sf_obj, by_geometry = FALSE)))

  has_pal <- !is.null(pal) && !is.null(col_data)

  color_vec <- if (has_pal) pal(col_data) else colores$primario
  fill_vec  <- if (has_pal) pal(col_data) else colores$secundario

  if (grepl("POINT|MULTIPOINT", gt)) {
    m <- m %>% addCircleMarkers(
      data        = sf_obj,
      radius      = radius,
      color       = color_vec,
      fillColor   = color_vec,
      fillOpacity = opacity,
      weight      = 1,
      popup       = popups
    )
  } else if (grepl("LINE|LINESTRING", gt)) {
    m <- m %>% addPolylines(
      data   = sf_obj,
      color  = color_vec,
      weight = 2,
      popup  = popups
    )
  } else {
    # Poligonos y multipoligonos
    m <- m %>% addPolygons(
      data        = sf_obj,
      fillColor   = fill_vec,
      color       = "#444444",
      fillOpacity = opacity,
      weight      = 0.8,
      popup       = popups,
      highlight   = highlightOptions(
        weight        = 2,
        color         = "#ffffff",
        fillOpacity   = opacity + 0.1,
        bringToFront  = TRUE
      )
    )
  }

  # Leyenda
  if (has_pal && !is.null(legend_title)) {
    m <- m %>% addLegend(
      "bottomright",
      pal     = pal,
      values  = col_data,
      title   = legend_title,
      opacity = 0.85
    )
  }

  m
}
