# ============================================================
# mod_mapa.R — Mapa interactivo integrado
#
# Lee:
#   shared$capas_vec     — lista de capas vectoriales
#   shared$rasters       — lista de rasters
#   shared$raster_activo — id del raster activo
#
# Permite controlar visibilidad, estilo y opacidad
# de cada capa individualmente.
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_mapa_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    sidebar = sidebar(
      width = 300,
      title = "Capas",
      open  = TRUE,

      # ── Capas vectoriales ──────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small mb-1",
         icon("map"), " Vectoriales"),
      uiOutput(ns("controles_vec")),

      hr(style = "margin: 8px 0;"),

      # ── Capa raster ───────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small mb-1",
         bs_icon("grid-3x3"), " Raster activo"),
      uiOutput(ns("controles_rst")),

      hr(style = "margin: 8px 0;"),

      # ── Mapa base ─────────────────────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small mb-1",
         icon("globe"), " Mapa base"),
      selectInput(
        ns("basemap"), NULL,
        choices = c(
          "CartoDB Claro"  = "CartoDB.Positron",
          "CartoDB Oscuro" = "CartoDB.DarkMatter",
          "OpenStreetMap"  = "OpenStreetMap",
          "Satélite"       = "Esri.WorldImagery",
          "Topo"           = "OpenTopoMap",
          "Sin mapa base"  = "none"
        ),
        selected = "CartoDB.Positron",
        width = "100%"
      ),

      hr(style = "margin: 8px 0;"),

      # ── Exportar ──────────────────────────────────────────
      downloadButton(
        ns("descargar_png"), "Exportar PNG",
        class = "btn-sm w-100",
        style = paste0("background:", colores$primario,
                       "; border-color:", colores$primario,
                       "; color:#fff;")
      )
    ),

    # ── Mapa principal ───────────────────────────────────────
    card(
      card_body(
        class = "p-0",
        style = "height: 620px;",
        leafletOutput(ns("mapa"), height = "100%")
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_mapa_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Controles vectoriales — uno por capa ─────────────────
    output$controles_vec <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) {
        return(div(
          class = "text-muted small py-1",
          icon("circle-info"),
          " Cargá capas en el tab Vectorial."
        ))
      }

      tagList(lapply(names(capas), function(id_capa) {
        capa <- capas[[id_capa]]
        div(
          class = "mb-2 p-2 rounded",
          style = paste0("border-left: 3px solid ", capa$color,
                         "; background:", colores$fondo, ";"),

          # Nombre + toggle
          div(
            class = "d-flex align-items-center justify-content-between mb-1",
            span(
              style = paste0("font-size:0.8rem; font-weight:600;",
                             "color:", capa$color, ";"),
              capa$nombre
            ),
            div(
              class = "form-check form-switch mb-0",
              checkboxInput(
                ns(paste0("vis_", id_capa)),
                label = NULL,
                value = TRUE
              )
            )
          ),

          # Colorear por variable
          {
            df   <- sf::st_drop_geometry(capa$sf)
            col_types <- vapply(df, function(x) class(x)[1], character(1))
            cols <- names(df)[col_types %in% c("numeric", "integer", "double", "character", "factor")]
            selectInput(
              ns(paste0("color_col_", id_capa)),
              label   = NULL,
              choices = c("Color sólido" = "", cols),
              width   = "100%"
            )
          },

          # Opacidad
          sliderInput(
            ns(paste0("op_", id_capa)),
            label = NULL,
            min = 0, max = 1, value = 0.7, step = 0.05
          )
        )
      }))
    })



    # ── Controles raster ─────────────────────────────────────
    output$controles_rst <- renderUI({
      if (is.null(shared$raster_activo) ||
          is.null(shared$rasters[[shared$raster_activo]])) {
        return(div(
          class = "text-muted small py-1",
          icon("circle-info"),
          " Cargá y activá un raster en el tab Raster."
        ))
      }

      rst_item <- shared$rasters[[shared$raster_activo]]
      rst      <- rst_item$rst
      n        <- terra::nlyr(rst)
      bandas   <- setNames(
        seq_len(n),
        paste0("Banda ", seq_len(n), " (", names(rst), ")")
      )

      div(
        class = "mb-2 p-2 rounded",
        style = paste0("border-left: 3px solid ", colores$acento,
                       "; background:", colores$fondo, ";"),

        div(
          class = "d-flex align-items-center justify-content-between mb-1",
          span(
            style = paste0("font-size:0.8rem; font-weight:600;",
                           "color:", colores$acento, ";"),
            rst_item$nombre
          ),
          div(
            class = "form-check form-switch mb-0",
            checkboxInput(ns("rst_visible"), label = NULL, value = TRUE)
          )
        ),

        selectInput(ns("rst_banda"), NULL,
                    choices = bandas, selected = 1, width = "100%"),

        selectInput(
          ns("rst_paleta"), NULL,
          choices  = c("Viridis" = "viridis", "Magma"   = "magma",
                       "Inferno" = "inferno", "Plasma"  = "plasma",
                       "Cividis" = "cividis", "Grises"  = "grays",
                       "Terrain" = "terrain"),
          selected = "viridis",
          width    = "100%"
        ),

        sliderInput(ns("rst_opacidad"), NULL,
                    min = 0, max = 1, value = 0.8, step = 0.05)
      )
    })

    # ── Raster activo procesado ──────────────────────────────
    rst_wgs84 <- reactive({
      req(shared$raster_activo, shared$rasters, input$rst_banda)
      rst_item <- shared$rasters[[shared$raster_activo]]
      req(rst_item)
      r <- rst_item$rst[[as.integer(input$rst_banda)]]
      if (!terra::is.lonlat(r)) r <- terra::project(r, "EPSG:4326")
      r
    })

    get_rst_pal <- function(rst, nombre) {
      vals    <- terra::values(rst, na.rm = TRUE)
      rng     <- range(vals, na.rm = TRUE)
      pal_fun <- switch(nombre,
        "viridis" = viridisLite::viridis,
        "magma"   = viridisLite::magma,
        "inferno" = viridisLite::inferno,
        "plasma"  = viridisLite::plasma,
        "cividis" = viridisLite::cividis,
        "grays"   = function(n) gray.colors(n, 0.9, 0.1),
        "terrain" = function(n) terrain.colors(n),
        viridisLite::viridis
      )
      colorNumeric(palette = pal_fun(256),
                   domain  = rng,
                   na.color = "transparent")
    }

    # ── Mapa base ────────────────────────────────────────────
    output$mapa <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    # ── Zoom automático al cargar datos ──────────────────────
    observeEvent(shared$capas_vec, {
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return()
      # Zoom a la última capa agregada
      ultima <- capas[[names(capas)[length(capas)]]]
      bb <- tryCatch(
        sf::st_bbox(sf::st_transform(ultima$sf, 4326)),
        error = function(e) NULL
      )
      if (!is.null(bb))
        leafletProxy(ns("mapa")) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]],
                    bb[["xmax"]], bb[["ymax"]])
    })

    observeEvent(shared$raster_activo, {
      req(shared$raster_activo, shared$rasters)
      rst_item <- shared$rasters[[shared$raster_activo]]
      req(rst_item)
      ext <- tryCatch(
        terra::ext(terra::project(rst_item$rst, "EPSG:4326")),
        error = function(e) NULL
      )
      if (!is.null(ext))
        leafletProxy(ns("mapa")) %>%
          fitBounds(ext$xmin, ext$ymin, ext$xmax, ext$ymax)
    })

    # ── Función principal de redibujo ────────────────────────
    redibujar_mapa <- function() {
      capas   <- shared$capas_vec
      basemap <- input$basemap %||% "CartoDB.Positron"

      tiene_rst <- !is.null(shared$raster_activo) &&
                   !is.null(shared$rasters[[shared$raster_activo]])
      rst_vis   <- tiene_rst && isTRUE(input$rst_visible)

      m <- leafletProxy(ns("mapa")) %>%
        clearImages() %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers()

      tryCatch({ m <- leafgl::clearGlLayers(m) },
               error = function(e) NULL)

      # Mapa base
      if (basemap != "none")
        m <- m %>% addProviderTiles(basemap)

      grupos <- c()

      # ── Raster primero (capa base) ────────────────────────
      if (rst_vis) {
        tryCatch({
          rst <- rst_wgs84()
          pal <- get_rst_pal(rst, input$rst_paleta %||% "viridis")
          r_raster <- raster::raster(rst)
          nombre_rst <- shared$rasters[[shared$raster_activo]]$nombre
          m <- m %>%
            addRasterImage(
              x       = r_raster,
              colors  = pal,
              opacity = input$rst_opacidad %||% 0.8,
              group   = nombre_rst
            ) %>%
            addLegend(
              position = "bottomright",
              pal      = pal,
              values   = raster::values(r_raster),
              title    = nombre_rst,
              opacity  = 0.85
            )
          grupos <- c(grupos, nombre_rst)
        }, error = function(e) NULL)
      }

      # ── Capas vectoriales encima ───────────────────────────
      if (!is.null(capas) && length(capas) > 0) {
        lapply(names(capas), function(id_capa) {
          capa    <- capas[[id_capa]]
          visible <- isTRUE(input[[paste0("vis_", id_capa)]])
          if (!visible) return(NULL)

          tryCatch({
            sf_obj   <- sf::st_transform(capa$sf, 4326)
            gt       <- toupper(as.character(
              sf::st_geometry_type(sf_obj, by_geometry = FALSE)))
            color_by <- input[[paste0("color_col_", id_capa)]] %||% ""
            opacidad <- input[[paste0("op_", id_capa)]] %||% 0.7
            has_col  <- color_by != "" && color_by %in% names(sf_obj)

            # Color de relleno
            col_fill <- capa$color
            if (has_col && is.numeric(sf_obj[[color_by]])) {
              pal_fn      <- viridisLite::viridis
              pal_leaflet <- colorNumeric(pal_fn(256), sf_obj[[color_by]])
              col_fill    <- pal_leaflet(sf_obj[[color_by]])
            }

            m <<- if (grepl("POINT|MULTIPOINT", gt)) {
              m %>% addCircleMarkers(
                data        = sf_obj,
                group       = capa$nombre,
                radius      = 5,
                stroke      = FALSE,
                fillColor   = col_fill,
                fillOpacity = opacidad,
                popup       = if (has_col)
                  as.character(sf_obj[[color_by]]) else NULL
              )
            } else if (grepl("LINE", gt)) {
              m %>% addPolylines(
                data    = sf_obj,
                group   = capa$nombre,
                color   = capa$color,
                opacity = opacidad,
                weight  = 2
              )
            } else {
              m %>% addPolygons(
                data        = sf_obj,
                group       = capa$nombre,
                fillColor   = col_fill,
                stroke      = TRUE,
                color       = "white",
                weight      = 1.5,
                opacity     = 1,
                fillOpacity = opacidad
              )
            }

            grupos <<- c(grupos, capa$nombre)

            # Leyenda si hay variable
            if (has_col && is.numeric(sf_obj[[color_by]])) {
              pal_fn      <- viridisLite::viridis
              pal_leaflet <- colorNumeric(pal_fn(256), sf_obj[[color_by]])
              m <<- m %>% addLegend(
                position = "bottomleft",
                pal      = pal_leaflet,
                values   = sf_obj[[color_by]],
                title    = color_by,
                opacity  = 0.85
              )
            }
          }, error = function(e) NULL)
        })
      }

      # Control de capas
      if (length(grupos) > 0)
        m <- m %>% addLayersControl(
          overlayGroups = grupos,
          options       = layersControlOptions(collapsed = FALSE)
        )

      m %>% addScaleBar(position = "bottomleft")
    }

    # ── Disparar redibujo ante cualquier cambio ──────────────
    observe({
      # Dependencias globales
      shared$capas_vec
      shared$raster_activo
      shared$rasters
      input$rst_visible
      input$rst_paleta
      input$rst_opacidad
      input$rst_banda
      input$basemap

      # Dependencias dinámicas por capa vectorial
      capas <- shared$capas_vec
      if (!is.null(capas) && length(capas) > 0) {
        lapply(names(capas), function(id_capa) {
          input[[paste0("vis_",       id_capa)]]
          input[[paste0("op_",        id_capa)]]
          isolate(input[[paste0("color_col_", id_capa)]])
        })
      }

      redibujar_mapa()
    })

    # Observer separado para color_col — evita loop al cambiar capa
    observe({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return()
      lapply(names(capas), function(id_capa) {
        observeEvent(input[[paste0("color_col_", id_capa)]], {
          redibujar_mapa()
        }, ignoreInit = TRUE)
      })
    })

    # ── Exportar PNG ─────────────────────────────────────────
    output$descargar_png <- downloadHandler(
      filename = function() paste0("mapa_", Sys.Date(), ".png"),
      content  = function(file) {
        mv <- NULL

        # Raster
        if (!is.null(shared$raster_activo) &&
            isTRUE(input$rst_visible)) {
          tryCatch({
            rst      <- rst_wgs84()
            pal_cols <- switch(input$rst_paleta %||% "viridis",
              "viridis" = viridisLite::viridis(256),
              "magma"   = viridisLite::magma(256),
              "inferno" = viridisLite::inferno(256),
              "plasma"  = viridisLite::plasma(256),
              "cividis" = viridisLite::cividis(256),
              "grays"   = gray.colors(256, 0.9, 0.1),
              "terrain" = terrain.colors(256),
              viridisLite::viridis(256)
            )
            nombre_rst <- shared$rasters[[shared$raster_activo]]$nombre
            mv <- mapview::mapview(
              rst,
              col.regions = pal_cols,
              alpha       = input$rst_opacidad %||% 0.8,
              layer.name  = nombre_rst
            )
          }, error = function(e) NULL)
        }

        # Vectoriales
        capas <- shared$capas_vec
        if (!is.null(capas) && length(capas) > 0) {
          lapply(names(capas), function(id_capa) {
            if (!isTRUE(input[[paste0("vis_", id_capa)]])) return()
            tryCatch({
              capa   <- capas[[id_capa]]
              mv_vec <- mapview::mapview(
                sf::st_transform(capa$sf, 4326),
                col.regions   = capa$color,
                color         = capa$color,
                alpha.regions = input[[paste0("op_", id_capa)]] %||% 0.7,
                layer.name    = capa$nombre
              )
              mv <<- if (is.null(mv)) mv_vec else mv + mv_vec
            }, error = function(e) NULL)
          })
        }

        if (is.null(mv)) {
          notify_wrn("No hay capas visibles para exportar.")
          return()
        }
        mapview::mapshot(mv, file = file)
      }
    )

  })
}
