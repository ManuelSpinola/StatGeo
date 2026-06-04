# ============================================================
# mod_h3.R — Módulo de generación y visualización de grilla H3
# Usa paisaje::get_h3_grid() y leafgl para renderizado eficiente
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_h3_ui <- function(id) {
  ns <- NS(id)

  tagList(

    uiOutput(ns("aoi_alert")),

    layout_sidebar(

      sidebar = sidebar(
        width = 300,
        title = "Grilla H3",

        # ── AOI ───────────────────────────────────────────
        div(
          class = "mb-3 p-2 rounded small",
          style = paste0("background:", colores$fondo,
                         "; border-left: 3px solid ", colores$primario, ";"),
          tags$p(class = "mb-1 fw-bold", "Área de interés (AOI)"),
          tags$p(class = "mb-0 text-muted",
                 "La grilla se genera a partir del vectorial cargado en ",
                 tags$strong("Datos"), ".")
        ),

        hr(style = "margin:8px 0;"),

        # ── Resolución ────────────────────────────────────
        h6(class = "text-muted fw-bold text-uppercase small", "Resolución H3"),

        sliderInput(ns("resolucion"), "Nivel de resolución:",
                    min = 1, max = 10, value = 6, step = 1),

        uiOutput(ns("res_info")),

        numericInput(ns("expand"), "Factor de expansión del AOI:",
                     value = 0.1, min = 0, max = 1, step = 0.05),

        hr(style = "margin:8px 0;"),

        # ── Estilo ────────────────────────────────────────
        h6(class = "text-muted fw-bold text-uppercase small", "Estilo"),

        sliderInput(ns("opacidad_fill"), "Opacidad relleno:",
                    min = 0, max = 1, value = 0.4, step = 0.05),

        sliderInput(ns("opacidad_borde"), "Opacidad borde:",
                    min = 0, max = 1, value = 0.8, step = 0.05),

        selectInput(ns("basemap"), "Mapa base:",
                    choices  = c("CartoDB Claro"  = "CartoDB.Positron",
                                 "CartoDB Oscuro" = "CartoDB.DarkMatter",
                                 "OpenStreetMap"  = "OpenStreetMap",
                                 "Sat\u00e9lite"  = "Esri.WorldImagery"),
                    selected = "CartoDB.Positron"),

        hr(style = "margin:8px 0;"),

        # ── Acciones ──────────────────────────────────────
        actionButton(ns("btn_generar"), "Generar grilla",
                     class = "btn-primary w-100",
                     icon  = icon("th")),

        uiOutput(ns("h3_status")),

        hr(style = "margin:8px 0;"),

        h6(class = "text-muted fw-bold text-uppercase small", "Descargar"),

        selectInput(ns("fmt_descarga"), "Formato:",
                    choices = c("GeoPackage" = "gpkg",
                                "GeoJSON"    = "geojson")),

        downloadButton(ns("descargar"), "Descargar grilla",
                       class = "btn-outline-success w-100")
      ),

      # ── Mapa ──────────────────────────────────────────────
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(icon("th"), " Grilla H3"),
          uiOutput(ns("grilla_badge"))
        ),
        card_body(class = "p-0", style = "min-height:560px;",
                  leafletOutput(ns("mapa"), height = "560px"))
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_h3_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Grilla reactiva ──────────────────────────────────────
    h3_grid <- reactiveVal(NULL)

    # ── Alerta si no hay AOI ─────────────────────────────────
    output$aoi_alert <- renderUI({
      if (is.null(shared$sf_data) || !inherits(shared$sf_data, "sf")) {
        div(class = "alert alert-warning d-flex align-items-center gap-2 mb-3",
            icon("triangle-exclamation"),
            "No hay vectorial cargado. Sub\u00ed un archivo en ",
            tags$strong("Datos"), " para usarlo como AOI.")
      }
    })

    # ── Info de resolución ───────────────────────────────────
    output$res_info <- renderUI({
      res <- input$resolucion
      if (is.null(res)) return(NULL)

      desc <- dplyr::case_when(
        res <= 3  ~ "Muy gruesa — pocas celdas grandes",
        res <= 5  ~ "Gruesa — ideal para an\u00e1lisis regionales",
        res == 6  ~ "Media — balance entre detalle y velocidad (recomendado)",
        res == 7  ~ "Fina — buena resoluci\u00f3n, procesamiento moderado",
        res >= 8  ~ "Muy fina — muchas celdas peque\u00f1as, puede ser lento"
      )

      color <- if (res >= 8) "text-warning" else "text-muted"

      div(class = paste("small mt-1", color),
          icon(if (res >= 8) "triangle-exclamation" else "circle-info"),
          paste0(" Res. ", res, ": ", desc))
    })

    # ── Badge ────────────────────────────────────────────────
    output$grilla_badge <- renderUI({
      g <- h3_grid()
      if (is.null(g)) {
        tags$span(class = "badge bg-secondary", "Sin grilla")
      } else {
        tags$span(class = "badge bg-success",
                  paste0(format(nrow(g), big.mark = ","),
                         " hex\u00e1gonos · res. ", input$resolucion))
      }
    })

    # ── Estado ───────────────────────────────────────────────
    output$h3_status <- renderUI({
      g <- h3_grid()
      if (is.null(g)) {
        div(class = "small text-muted mt-2",
            icon("circle-info"), " Sin grilla generada.")
      } else {
        div(class = "small text-success mt-2",
            icon("check"),
            paste0(" ", format(nrow(g), big.mark = ","),
                   " hex\u00e1gonos generados."))
      }
    })

    # ── Generar grilla ───────────────────────────────────────
    observeEvent(input$btn_generar, {
      if (is.null(shared$sf_data) || !inherits(shared$sf_data, "sf")) {
        showNotification("Cargá un vectorial primero como AOI.",
                         type = "warning", duration = 4)
        return()
      }

      showNotification(
        paste0("Generando grilla H3 (res. ", input$resolucion, ")..."),
        type = "message", duration = 3
      )

      tryCatch({
        grilla <- paisaje::get_h3_grid(
          sf_object     = shared$sf_data,
          resolution    = input$resolucion,
          expand_factor = input$expand
        )
        grilla <- sf::st_transform(grilla, 4326)
        h3_grid(grilla)

        showNotification(
          paste0("\u2713 Grilla lista: ",
                 format(nrow(grilla), big.mark = ","),
                 " hex\u00e1gonos."),
          type = "message", duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Error al generar grilla:", e$message),
          type = "error", duration = 6
        )
      })
    })

    # ── Mapa inicial ─────────────────────────────────────────
    output$mapa <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    # ── Actualizar mapa al cambiar grilla o estilo ───────────
    observe({
      g       <- h3_grid()
      basemap <- input$basemap %||% "CartoDB.Positron"

      m <- leafletProxy(ns("mapa")) %>%
        leafgl::clearGlLayers() %>%
        clearControls()

      m <- m %>% addProviderTiles(basemap)

      if (!is.null(g)) {
        m <- m %>%
          leafgl::addGlPolygons(
            data        = g,
            fillColor   = colores$primario,
            fillOpacity = input$opacidad_fill  %||% 0.4,
            color       = colores$secundario,
            opacity     = input$opacidad_borde %||% 0.8,
            weight      = 0.5,
            group       = "Grilla H3"
          ) %>%
          addScaleBar(position = "bottomleft")
      }
    })

    # ── Zoom al AOI cuando se carga el vectorial ─────────────
    observeEvent(if (!is.null(shared$sf_data) &&
                     inherits(shared$sf_data, "sf"))
      nrow(shared$sf_data), {
      req(shared$sf_data)
      bb <- sf::st_bbox(sf::st_transform(shared$sf_data, 4326))
      leafletProxy(ns("mapa")) %>%
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })

    # ── Zoom a la grilla cuando se genera ────────────────────
    observeEvent(h3_grid(), {
      req(h3_grid())
      bb <- sf::st_bbox(h3_grid())
      leafletProxy(ns("mapa")) %>%
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })

    # ── Descarga ─────────────────────────────────────────────
    output$descargar <- downloadHandler(
      filename = function() {
        ext <- input$fmt_descarga
        paste0("grilla_h3_res", input$resolucion, "_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(h3_grid())
        sf::st_write(h3_grid(), file, quiet = TRUE, delete_dsn = TRUE)
      }
    )

  })
}
