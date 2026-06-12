# ============================================================
# mod_h3.R — Generación y exploración de grilla H3
#
# Lee:
#   shared$capas_vec — para elegir el AOI (cualquier vectorial)
#
# Genera hexágonos H3 sobre el AOI seleccionado,
# permite explorar la grilla y descargarla.
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_h3_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    sidebar = sidebar(
      width = 300,
      title = "Configuración H3",
      open  = TRUE,

      # ── Seleccionar AOI ────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small mb-1",
         icon("draw-polygon"), " Área de interés (AOI)"),

      uiOutput(ns("selector_aoi")),

      hr(style = "margin: 8px 0;"),

      # ── Resolución ────────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small mb-1",
         bs_icon("sliders"), " Resolución"),

      sliderInput(
        ns("h3_res"),
        label = NULL,
        min = 1, max = 10, value = 6, step = 1,
        width = "100%"
      ),

      uiOutput(ns("res_info")),

      hr(style = "margin: 8px 0;"),

      # ── Generar ───────────────────────────────────────────
      actionButton(
        ns("btn_generar"),
        label = "Generar grilla H3",
        icon  = bs_icon("hexagon-fill"),
        class = "btn-success w-100"
      ),

      uiOutput(ns("h3_status")),

      hr(style = "margin: 8px 0;"),

      # ── Descargar ─────────────────────────────────────────
      uiOutput(ns("descarga_ui"))
    ),

    # ── Panel principal ──────────────────────────────────────
    navset_card_tab(

      nav_panel(
        title = tagList(icon("map"), " Mapa"),
        card_body(
          class = "p-0",
          style = "height: 580px;",
          leafletOutput(ns("mapa_h3"), height = "100%")
        )
      ),

      nav_panel(
        title = tagList(icon("table"), " Atributos"),
        card_body(
          uiOutput(ns("tabla_ui"))
        )
      ),

      nav_panel(
        title = tagList(icon("circle-info"), " Referencia H3"),
        card_body(
          uiOutput(ns("referencia_ui"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_h3_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Estado interno ───────────────────────────────────────
    h3_grid <- reactiveVal(NULL)

    # ── Selector de AOI ─────────────────────────────────────
    output$selector_aoi <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) {
        return(div(
          class = "alert alert-warning py-2 px-3 small",
          icon("triangle-exclamation"),
          " Cargá al menos una capa vectorial en el tab ",
          tags$strong("Vectorial"), " para usarla como AOI."
        ))
      }
      nombres <- sapply(capas, function(c) c$nombre)
      selectInput(
        ns("aoi_sel"),
        label   = NULL,
        choices = setNames(names(capas), nombres),
        width   = "100%"
      )
    })

    # ── Info de resolución ───────────────────────────────────
    output$res_info <- renderUI({
      res <- input$h3_res
      if (is.null(res)) return(NULL)

      # Áreas aproximadas por resolución H3
      areas <- c(
        "1" = "~2,562,182 km²",
        "2" = "~86,745 km²",
        "3" = "~12,392 km²",
        "4" = "~1,770 km²",
        "5" = "~252 km²",
        "6" = "~36 km²  — recomendado",
        "7" = "~5 km²",
        "8" = "~0.7 km²",
        "9" = "~0.1 km²",
        "10"= "~0.015 km²  — muy fino"
      )

      desc <- dplyr::case_when(
        res <= 3  ~ "Muy gruesa — escala continental",
        res <= 5  ~ "Gruesa — escala regional",
        res == 6  ~ "Media",
        res == 7  ~ "Fina — escala local",
        res >= 8  ~ "Muy fina"
      )

      color <- if (res >= 8) "text-warning" else "text-muted"
      icono <- if (res >= 8) "triangle-exclamation" else "circle-info"

      div(
        class = paste("small mt-1", color),
        icon(icono), paste0(" Res. ", res, ": ", desc),
        div(
          class = "text-muted mt-1",
          style = "font-size:0.75rem;",
          bs_icon("hexagon-fill"),
          paste0(" Área aprox. por hexágono: ",
                 areas[as.character(res)])
        )
      )
    })

    # ── Generar grilla ───────────────────────────────────────
    observeEvent(input$btn_generar, {
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0 ||
          is.null(input$aoi_sel)) {
        notify_wrn("Seleccioná un AOI primero.")
        return()
      }

      capa <- capas[[input$aoi_sel]]
      if (is.null(capa)) {
        notify_err("Capa AOI no encontrada.")
        return()
      }

      res <- input$h3_res
      showNotification(
        paste0("Generando grilla H3 (res. ", res, ")…"),
        type = "message", duration = 3
      )

      tryCatch({
        sf_aoi <- sf::st_transform(capa$sf, 4326)
        sf_aoi <- sf::st_make_valid(sf_aoi)

        # Disolver si hay múltiples geometrías
        if (nrow(sf_aoi) > 1) {
          sf_aoi <- sf::st_union(sf_aoi) |> sf::st_as_sf()
        }

        grilla <- paisaje::get_h3_grid(
          sf_aoi,
          resolution = res
        )
        grilla <- sf::st_transform(grilla, 4326)
        grilla <- sf::st_make_valid(grilla)

        h3_grid(grilla)

        notify_ok(paste0(
          format(nrow(grilla), big.mark = ","),
          " hexágonos generados (res. ", res, ")."
        ))

        # Zoom al resultado
        bb <- sf::st_bbox(grilla)
        leafletProxy(ns("mapa_h3")) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]],
                    bb[["xmax"]], bb[["ymax"]])

      }, error = function(e) {
        notify_err(paste("Error al generar la grilla:", e$message))
      })
    })

    # ── Estado de la grilla ──────────────────────────────────
    output$h3_status <- renderUI({
      g <- h3_grid()
      if (is.null(g)) {
        div(
          class = "small text-muted mt-2",
          icon("circle-info"), " Sin grilla generada."
        )
      } else {
        div(
          class = "small text-success mt-2",
          icon("check"),
          paste0(" ", format(nrow(g), big.mark = ","),
                 " hexágonos · res. ", input$h3_res)
        )
      }
    })

    # ── UI de descarga ───────────────────────────────────────
    output$descarga_ui <- renderUI({
      if (is.null(h3_grid())) return(NULL)
      div(
        class = "d-flex gap-2",
        downloadButton(
          ns("descargar_gpkg"), "GeoPackage",
          class = "btn-sm btn-outline-primary flex-grow-1"
        ),
        downloadButton(
          ns("descargar_geojson"), "GeoJSON",
          class = "btn-sm btn-outline-primary flex-grow-1"
        )
      )
    })

    # ── Mapa H3 ──────────────────────────────────────────────
    output$mapa_h3 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    # Dibujar grilla y AOI en el mapa
    observeEvent(h3_grid(), {
      req(h3_grid())
      g <- h3_grid()

      # AOI
      capas <- shared$capas_vec
      aoi_sf <- NULL
      if (!is.null(capas) && !is.null(input$aoi_sel) &&
          !is.null(capas[[input$aoi_sel]])) {
        aoi_sf <- tryCatch(
          sf::st_transform(capas[[input$aoi_sel]]$sf, 4326),
          error = function(e) NULL
        )
      }

      # Grilla como polígonos simples
      g_poly <- tryCatch(
        suppressWarnings(sf::st_cast(g, "POLYGON")),
        error = function(e) g
      )

      m <- leafletProxy(ns("mapa_h3")) %>%
        clearShapes() %>%
        clearControls()

      # AOI de fondo
      if (!is.null(aoi_sf)) {
        m <- m %>% addPolygons(
          data        = aoi_sf,
          fillColor   = colores$primario,
          fillOpacity = 0.05,
          color       = colores$primario,
          weight      = 1.5,
          opacity     = 0.6,
          group       = "AOI"
        )
      }

      # Grilla H3
      if (nrow(g_poly) > 50000) {
        m <- tryCatch(
          m %>% leafgl::addGlPolygons(
            data      = g_poly,
            fillColor = colores$secundario,
            color     = colores$primario,
            weight    = 1,
            group     = "Grilla H3"
          ),
          error = function(e) {
            m %>% addPolygons(
              data        = g_poly,
              fillColor   = colores$secundario,
              fillOpacity = 0.3,
              color       = colores$primario,
              weight      = 1,
              opacity     = 0.7,
              group       = "Grilla H3"
            )
          }
        )
      } else {
        m <- m %>% addPolygons(
          data        = g_poly,
          fillColor   = colores$secundario,
          fillOpacity = 0.3,
          color       = colores$primario,
          weight      = 1,
          opacity     = 0.7,
          group       = "Grilla H3"
        )
      }

      grupos <- if (!is.null(aoi_sf)) c("AOI", "Grilla H3") else "Grilla H3"
      m %>%
        addLayersControl(
          overlayGroups = grupos,
          options       = layersControlOptions(collapsed = FALSE)
        ) %>%
        addScaleBar(position = "bottomleft")
    })

    # ── Tabla de atributos ───────────────────────────────────
    output$tabla_ui <- renderUI({
      if (is.null(h3_grid())) {
        return(div(
          class = "text-muted small text-center py-5",
          bs_icon("hexagon-fill",
               style = "font-size:2rem; display:block; margin-bottom:8px;"),
          "Generá una grilla H3 para ver su tabla de atributos."
        ))
      }
      DTOutput(ns("tabla_h3"))
    })

    output$tabla_h3 <- renderDT({
      req(h3_grid())
      df <- sf::st_drop_geometry(h3_grid())
      if (ncol(df) == 0) {
        df <- data.frame(
          hexagono = seq_len(nrow(h3_grid())),
          area_km2 = round(
            as.numeric(sf::st_area(h3_grid())) / 1e6, 4
          )
        )
      }
      datatable(
        df,
        options  = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class    = "table-sm table-striped table-hover"
      )
    })

    # ── Referencia H3 ────────────────────────────────────────
    output$referencia_ui <- renderUI({
      tagList(
        h6(class = "fw-bold", bs_icon("hexagon-fill"), " Sistema de indexación H3"),
        p(class = "text-muted small",
          "H3 es un sistema de indexación geoespacial jerárquico desarrollado por ",
          tags$strong("Uber Technologies"), " que divide la superficie terrestre ",
          "en hexágonos de tamaño uniforme a múltiples resoluciones."),

        tags$table(
          class = "table table-sm table-bordered small",
          tags$thead(
            tags$tr(
              tags$th("Resolución"),
              tags$th("Área aprox. por hexágono"),
              tags$th("Uso típico")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("1–3"),  tags$td("86,000–2,500,000 km²"),
                    tags$td("Escala continental")),
            tags$tr(tags$td("4–5"),  tags$td("250–1,800 km²"),
                    tags$td("Escala regional")),
            tags$tr(tags$td(class="table-success", "6"),
                    tags$td(class="table-success", "~36 km²"),
                    tags$td(class="table-success", "SDM — recomendado")),
            tags$tr(tags$td("7"),    tags$td("~5 km²"),
                    tags$td("Escala local")),
            tags$tr(tags$td("8–10"), tags$td("0.015–0.7 km²"),
                    tags$td("Muy fino — lento"))
          )
        ),

        div(
          class = "small text-muted mt-2",
          icon("link"),
          " Referencia: ",
          tags$a("h3geo.org", href = "https://h3geo.org",
                 target = "_blank"),
          " · ",
          tags$a("Uber H3 GitHub",
                 href = "https://github.com/uber/h3",
                 target = "_blank")
        )
      )
    })

    # ── Descargas ────────────────────────────────────────────
    output$descargar_gpkg <- downloadHandler(
      filename = function() {
        paste0("h3_res", input$h3_res, "_", Sys.Date(), ".gpkg")
      },
      content = function(file) {
        req(h3_grid())
        sf::st_write(h3_grid(), file, quiet = TRUE, delete_dsn = TRUE)
      }
    )

    output$descargar_geojson <- downloadHandler(
      filename = function() {
        paste0("h3_res", input$h3_res, "_", Sys.Date(), ".geojson")
      },
      content = function(file) {
        req(h3_grid())
        sf::st_write(h3_grid(), file, quiet = TRUE, delete_dsn = TRUE)
      }
    )

  })
}
