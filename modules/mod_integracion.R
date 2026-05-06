# ============================================================
# mod_integracion.R — Superposición de capas vectorial + raster
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_integracion_ui <- function(id) {
  ns <- NS(id)

  tagList(

    uiOutput(ns("datos_alert")),

    layout_sidebar(

      sidebar = sidebar(
        width = 290,
        title = "Control de capas",

        # ── Capa vectorial ──────────────────────────────────
        div(
          class = "mb-3 p-2 rounded",
          style = paste0("border-left: 4px solid ", colores$primario,
                         "; background:", colores$fondo, ";"),

          div(
            class = "d-flex align-items-center justify-content-between mb-2",
            h6(class = "fw-bold mb-0",
               style = paste0("color:", colores$primario, ";"),
               icon("draw-polygon"), " Vectorial"),
            div(class = "form-check form-switch mb-0",
                tags$input(type = "checkbox", checked = NA,
                           class = "form-check-input",
                           id = ns("vec_visible")))
          ),

          radioButtons(ns("vec_orden"), "Posici\u00f3n:",
                       choices  = c("Encima del raster" = "encima",
                                    "Debajo del raster" = "debajo"),
                       selected = "encima"),

          radioButtons(ns("vec_estilo"), "Estilo:",
                       choices  = c("Relleno s\u00f3lido" = "solido",
                                    "Solo contorno"       = "contorno"),
                       selected = "solido"),

          sliderInput(ns("vec_opacidad"), "Opacidad:",
                      min = 0, max = 1, value = 0.7, step = 0.05)
        ),

        hr(style = "margin:8px 0;"),

        # ── Capa raster ─────────────────────────────────────
        div(
          class = "mb-3 p-2 rounded",
          style = paste0("border-left: 4px solid ", colores$acento,
                         "; background:", colores$fondo, ";"),

          div(
            class = "d-flex align-items-center justify-content-between mb-2",
            h6(class = "fw-bold mb-0",
               style = paste0("color:", colores$acento, ";"),
               icon("layer-group"), " Raster"),
            div(class = "form-check form-switch mb-0",
                tags$input(type = "checkbox", checked = NA,
                           class = "form-check-input",
                           id = ns("rst_visible")))
          ),

          selectInput(ns("rst_paleta"), "Paleta de colores:",
                      choices  = c("Viridis"  = "viridis", "Magma"   = "magma",
                                   "Inferno"  = "inferno", "Plasma"  = "plasma",
                                   "Cividis"  = "cividis", "Grises"  = "grays",
                                   "Terrain"  = "terrain"),
                      selected = "viridis"),

          sliderInput(ns("rst_opacidad"), "Opacidad:",
                      min = 0, max = 1, value = 0.8, step = 0.05)
        ),

        hr(style = "margin:8px 0;"),

        selectInput(ns("basemap"), "Mapa base:",
                    choices  = c("CartoDB Claro"  = "CartoDB.Positron",
                                 "CartoDB Oscuro" = "CartoDB.DarkMatter",
                                 "OpenStreetMap"  = "OpenStreetMap",
                                 "Sat\u00e9lite" = "Esri.WorldImagery",
                                 "Sin mapa base"  = "none"),
                    selected = "CartoDB.Positron"),

        hr(style = "margin:8px 0;"),

        h6(class = "text-muted fw-bold text-uppercase small", "Exportar"),
        downloadButton(ns("descargar_mapa"), "Descargar PNG",
                       class = "btn-sm w-100",
                       style = paste0("background:", colores$primario,
                                      "; border-color:", colores$primario,
                                      "; color:#fff;"))
      ),

      # ── Mapa ─────────────────────────────────────────────
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(icon("globe"), " Integraci\u00f3n de capas"),
          uiOutput(ns("capas_badge"))
        ),
        card_body(class = "p-0", style = "min-height:560px;",
                  leafletOutput(ns("mapa"), height = "560px"))
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_integracion_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Alertas contextuales ─────────────────────────────────
    output$datos_alert <- renderUI({
      tiene_vec <- !is.null(shared$sf_data)
      tiene_rst <- !is.null(shared$raster_data)
      if (!tiene_vec && !tiene_rst) {
        div(class = "alert alert-warning d-flex align-items-center gap-2 mb-3",
            icon("triangle-exclamation"),
            "No hay datos cargados. Sub\u00ed al menos una capa en ",
            tags$strong("Datos"), ".")
      } else if (!tiene_vec) {
        div(class = "alert alert-info d-flex align-items-center gap-2 mb-3",
            icon("info-circle"),
            "Solo hay raster. Pod\u00e9s subir un vectorial en ",
            tags$strong("Datos"), " para superponerlo.")
      } else if (!tiene_rst) {
        div(class = "alert alert-info d-flex align-items-center gap-2 mb-3",
            icon("info-circle"),
            "Solo hay vectorial. Pod\u00e9s subir un raster en ",
            tags$strong("Datos"), " para superponerlo.")
      }
    })

    # ── Badge capas activas ──────────────────────────────────
    output$capas_badge <- renderUI({
      n <- sum(c(
        isTRUE(!is.null(shared$sf_data)     && isTRUE(input$vec_visible)),
        isTRUE(!is.null(shared$raster_data) && isTRUE(input$rst_visible))
      ))
      tags$span(class = "badge rounded-pill",
                style = paste0("background:", colores$primario, "; color:#fff;"),
                paste0(n, " capa(s) activa(s)"))
    })

    # ── Mapa inicial ─────────────────────────────────────────
    output$mapa <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    # ── Reactivos reproyección ───────────────────────────────
    rst_wgs84 <- reactive({
      req(shared$raster_data)
      rst <- shared$raster_data
      if (!terra::same.crs(rst, "EPSG:4326"))
        rst <- terra::project(rst, "EPSG:4326")
      rst
    })

    vec_wgs84 <- reactive({
      req(shared$sf_data)
      sf_obj <- shared$sf_data
      if (!is.na(sf::st_crs(sf_obj)) &&
          sf::st_crs(sf_obj) != sf::st_crs(4326))
        sf_obj <- sf::st_transform(sf_obj, 4326)
      sf_obj
    })

    # ── Paleta raster ────────────────────────────────────────
    get_rst_pal <- function(rst, nombre) {
      vals    <- terra::values(rst[[1]], na.rm = TRUE)
      rng     <- range(vals, na.rm = TRUE)
      pal_fun <- switch(nombre,
                        "viridis" = viridisLite::viridis,
                        "magma"   = viridisLite::magma,
                        "inferno" = viridisLite::inferno,
                        "plasma"  = viridisLite::plasma,
                        "cividis" = viridisLite::cividis,
                        "grays"   = function(n) gray.colors(n, start = 0.9, end = 0.1),
                        "terrain" = function(n) terrain.colors(n),
                        viridisLite::viridis
      )
      colorNumeric(palette = pal_fun(256), domain = rng,
                   na.color = "transparent")
    }

    # ── Observador principal ─────────────────────────────────
    observe({
      tiene_vec  <- !is.null(shared$sf_data)
      tiene_rst  <- !is.null(shared$raster_data)
      vec_vis    <- isTRUE(input$vec_visible)
      rst_vis    <- isTRUE(input$rst_visible)
      vec_orden  <- input$vec_orden    %||% "encima"
      vec_estilo <- input$vec_estilo   %||% "solido"
      vec_op     <- input$vec_opacidad %||% 0.7
      rst_op     <- input$rst_opacidad %||% 0.8
      pal_nom    <- input$rst_paleta   %||% "viridis"
      basemap    <- input$basemap      %||% "CartoDB.Positron"

      m <- leafletProxy(ns("mapa")) %>%
        clearImages() %>% clearShapes() %>%
        clearControls() %>% clearMarkers()

      if (basemap != "none")
        m <- m %>% addProviderTiles(basemap)

      # Función: agregar vectorial
      agregar_vec <- function(m) {
        if (!tiene_vec || !vec_vis) return(m)
        sf_obj  <- vec_wgs84()
        gt      <- toupper(as.character(
          sf::st_geometry_type(sf_obj, by_geometry = FALSE)))
        fill_op <- if (vec_estilo == "contorno") 0 else vec_op
        col     <- colores$primario
        w       <- if (vec_estilo == "contorno") 2 else 1

        if (grepl("POINT|MULTIPOINT", gt)) {
          m %>% addCircleMarkers(data = sf_obj, group = "Vectorial",
                                 radius = 5, color = col, fillColor = col,
                                 fillOpacity = fill_op, opacity = vec_op, weight = w)
        } else if (grepl("LINE", gt)) {
          m %>% addPolylines(data = sf_obj, group = "Vectorial",
                             color = col, weight = 2, opacity = vec_op)
        } else {
          m %>% addPolygons(data = sf_obj, group = "Vectorial",
                            fillColor = col, color = col,
                            fillOpacity = fill_op, opacity = vec_op, weight = w)
        }
      }

      # Función: agregar raster
      agregar_rst <- function(m) {
        if (!tiene_rst || !rst_vis) return(m)
        rst   <- rst_wgs84()
        pal   <- get_rst_pal(rst, pal_nom)
        rst_r <- raster::raster(rst[[1]])
        m %>%
          addRasterImage(x = rst_r, colors = pal,
                         opacity = rst_op, group = "Raster") %>%
          addLegend(position = "bottomright", pal = pal,
                    values = raster::values(rst_r),
                    title = names(rst)[1], opacity = 0.85)
      }

      # Orden de capas
      if (vec_orden == "encima") {
        m <- agregar_rst(m)
        m <- agregar_vec(m)
      } else {
        m <- agregar_vec(m)
        m <- agregar_rst(m)
      }

      # Control de capas Leaflet
      grupos <- c()
      if (tiene_vec) grupos <- c(grupos, "Vectorial")
      if (tiene_rst) grupos <- c(grupos, "Raster")
      if (length(grupos) > 0)
        m <- m %>% addLayersControl(
          overlayGroups = grupos,
          options = layersControlOptions(collapsed = FALSE))

      m %>% addScaleBar(position = "bottomleft")
    })

    # ── Ajustar bounds ───────────────────────────────────────
    observeEvent(list(shared$sf_data, shared$raster_data), {
      if (!is.null(shared$sf_data)) {
        bb <- sf::st_bbox(sf::st_transform(shared$sf_data, 4326))
        leafletProxy(ns("mapa")) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]],
                    bb[["xmax"]], bb[["ymax"]])
      } else if (!is.null(shared$raster_data)) {
        ext <- terra::ext(terra::project(shared$raster_data, "EPSG:4326"))
        leafletProxy(ns("mapa")) %>%
          fitBounds(ext$xmin, ext$ymin, ext$xmax, ext$ymax)
      }
    })

    # ── Descarga ─────────────────────────────────────────────
    output$descargar_mapa <- downloadHandler(
      filename = function() paste0("integracion_", Sys.Date(), ".png"),
      content  = function(file) {
        notify_wrn("Para exportar con todas las capas us\u00e1 el screenshot del navegador.")
        mapview::mapshot(
          leaflet() %>% addProviderTiles(input$basemap %||% "CartoDB.Positron"),
          file = file
        )
      }
    )

  })
}
