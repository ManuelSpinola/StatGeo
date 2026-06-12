# ============================================================
# mod_raster.R — Carga y exploración de datos raster
#
# Permite cargar múltiples rasters (bio1, bio2, bio3…)
# y activar uno a la vez para visualizar.
# El raster activo se comparte con mod_mapa via shared$raster_activo.
#
# Formatos: GeoTIFF (.tif/.tiff), NetCDF (.nc), ERDAS (.img)
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_raster_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    sidebar = sidebar(
      width = 300,
      title = "Capas raster",
      open  = TRUE,

      # ── Cargar nuevo raster ────────────────────────────────
      fileInput(
        ns("rst_file"),
        label       = NULL,
        accept      = c(".tif", ".tiff", ".nc", ".img"),
        buttonLabel = tagList(icon("upload"), " Agregar raster"),
        placeholder = "Ningún archivo seleccionado"
      ),
      div(
        class = "small text-muted mt-1 mb-2",
        style = "line-height: 1.6;",
        tags$strong("Formatos aceptados:"), tags$br(),
        "GeoTIFF (.tif · .tiff)", tags$br(),
        "NetCDF (.nc) · ERDAS (.img)"
      ),

      # ── Hint dinámico ──────────────────────────────────────
      uiOutput(ns("rst_hint")),

      hr(style = "margin: 6px 0;"),

      # ── Lista de rasters cargados ──────────────────────────
      h6(
        class = "text-muted fw-bold text-uppercase small mb-2",
        icon("layer-group"), " Capas cargadas"
      ),

      div(
        style = "min-height: 60px; border: 1px solid var(--bs-border-color); border-radius: 6px; padding: 8px;",
        uiOutput(ns("lista_rasters")),
        uiOutput(ns("empty_state"))
      ),

      hr(style = "margin: 6px 0;"),

      # ── Controles de visualización ─────────────────────────
      h6(class = "text-muted fw-bold text-uppercase small mb-1",
         icon("sliders-h"), " Visualización"),
      uiOutput(ns("controles_vis"))
    ),

    # ── Panel principal ──────────────────────────────────────
    navset_card_tab(

      nav_panel(
        title = tagList(icon("map"), " Mapa"),
        card_body(
          class = "p-0",
          style = "height: 520px;",
          leafletOutput(ns("mapa_rst"), height = "100%")
        )
      ),

      nav_panel(
        title = tagList(icon("chart-bar"), " Estadísticas"),
        card_body(
          uiOutput(ns("stats_panel"))
        )
      ),

      nav_panel(
        title = tagList(icon("circle-info"), " Resumen"),
        card_body(
          uiOutput(ns("resumen_panel"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_raster_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Paleta de colores por raster ─────────────────────────
    paleta_rasters <- c(
      colores$acento,     # naranja
      colores$primario,   # azul
      colores$secundario, # azul claro
      "#C85200",          # naranja oscuro
      "#5FA2CE",          # azul medio
      "#F1CE63"           # amarillo
    )

    rv <- reactiveValues(color_idx = 0)

    # ── Helper: agregar raster a shared$rasters ──────────────
    agregar_raster <- function(rst, nombre) {
      rv$color_idx <- (rv$color_idx %% length(paleta_rasters)) + 1
      color        <- paleta_rasters[rv$color_idx]

      id_rst <- paste0("rst_", as.integer(as.numeric(Sys.time()) * 1000))

      nuevo <- list(
        id     = id_rst,
        nombre = nombre,
        rst    = rst,
        color  = color
      )

      rasters_actual         <- shared$rasters
      rasters_actual[[id_rst]] <- nuevo
      shared$rasters         <- rasters_actual

      # Activar automáticamente si es el primero
      if (is.null(shared$raster_activo)) {
        shared$raster_activo <- id_rst
      }

      notify_ok(paste0("\"", nombre, "\" cargado — ",
                       terra::nlyr(rst), " banda(s)."))
    }

    # ── Procesar archivo al subirlo ──────────────────────────
    observeEvent(input$rst_file, {
      req(input$rst_file)
      path        <- input$rst_file$datapath
      nombre_base <- tools::file_path_sans_ext(input$rst_file$name)

      tryCatch({
        rst <- terra::rast(path)
        agregar_raster(rst, nombre_base)
      }, error = function(e) {
        notify_err(paste("Error al leer el raster:", e$message))
      })
    })

    # ── Hint dinámico ────────────────────────────────────────
    output$rst_hint <- renderUI({
      req(input$rst_file)
      ext <- tolower(tools::file_ext(input$rst_file$name))
      msg <- switch(ext,
        "tif"  = ,
        "tiff" = "GeoTIFF detectado. Formato recomendado.",
        "nc"   = "NetCDF detectado. Se usará la primera variable disponible.",
        "img"  = "ERDAS Imagine detectado.",
        NULL
      )
      if (!is.null(msg)) {
        div(class = "alert alert-primary py-2 px-3 small mt-1",
            icon("check-circle"), " ", msg)
      }
    })

    # ── Lista de rasters cargados ────────────────────────────
    output$lista_rasters <- renderUI({
      rasters <- shared$rasters
      if (is.null(rasters) || length(rasters) == 0) return(NULL)

      tagList(lapply(names(rasters), function(id_rst) {
        rst_item <- rasters[[id_rst]]
        activo   <- isTRUE(shared$raster_activo == id_rst)

        div(
          class = "d-flex align-items-center gap-2 mb-2 p-2 rounded",
          style = paste0(
            "border-left: 4px solid ", rst_item$color, ";",
            "background:", if (activo) "#EEF3FA" else colores$fondo, ";",
            if (activo) paste0("box-shadow: 0 0 0 1px ", rst_item$color, ";") else ""
          ),

          # Ícono de banda
          bs_icon("grid-3x3",
               style = paste0("color:", rst_item$color, "; flex-shrink:0;")),

          # Nombre editable
          div(
            style = "flex: 1; min-width: 0;",
            tags$input(
              type     = "text",
              value    = rst_item$nombre,
              class    = "form-control form-control-sm",
              style    = "font-size:0.8rem; padding:2px 6px;",
              onchange = sprintf(
                "Shiny.setInputValue('%s', {id: '%s', nombre: this.value}, {priority: 'event'})",
                ns("renombrar_raster"), id_rst
              )
            ),
            div(
              class = "text-muted mt-1",
              style = "font-size:0.7rem;",
              paste0(terra::nlyr(rst_item$rst), " banda(s) · ",
                     terra::nrow(rst_item$rst), "\u00d7",
                     terra::ncol(rst_item$rst))
            )
          ),

          # Botón activar
          actionButton(
            ns(paste0("act_", id_rst)), NULL,
            icon  = icon(if (activo) "circle-dot" else "circle"),
            class = paste0("btn-sm p-1 ",
                           if (activo) "btn-primary" else "btn-outline-secondary"),
            title = if (activo) "Raster activo" else "Activar este raster",
            style = "flex-shrink:0;"
          ),

          # Botón eliminar
          actionButton(
            ns(paste0("del_", id_rst)), NULL,
            icon  = icon("xmark"),
            class = "btn-sm btn-outline-danger p-1",
            style = "flex-shrink:0;",
            title = "Eliminar raster"
          )
        )
      }))
    })

    # ── Estado vacío ─────────────────────────────────────────
    output$empty_state <- renderUI({
      rasters <- shared$rasters
      if (!is.null(rasters) && length(rasters) > 0) return(NULL)
      div(
        class = "text-muted small text-center py-3",
        icon("arrow-up-from-bracket",
             style = "font-size:1.5rem; display:block; margin-bottom:6px;"),
        "Subí un GeoTIFF, NetCDF o .img para agregar tu primer raster."
      )
    })

    # ── Observers dinámicos: activar y eliminar ──────────────
    observe({
      rasters <- shared$rasters
      if (is.null(rasters) || length(rasters) == 0) return()

      lapply(names(rasters), function(id_rst) {

        # Activar
        observeEvent(input[[paste0("act_", id_rst)]], {
          shared$raster_activo <- id_rst
          nombre <- shared$rasters[[id_rst]]$nombre
          notify_ok(paste0("\"", nombre, "\" activado."))
        }, ignoreInit = TRUE, once = TRUE)

        # Eliminar
        observeEvent(input[[paste0("del_", id_rst)]], {
          rasters_actual <- shared$rasters
          nombre         <- rasters_actual[[id_rst]]$nombre
          rasters_actual[[id_rst]] <- NULL
          shared$rasters <- rasters_actual

          # Si era el activo, activar el primero disponible
          if (isTRUE(shared$raster_activo == id_rst)) {
            restantes <- names(shared$rasters)
            shared$raster_activo <- if (length(restantes) > 0)
              restantes[1] else NULL
          }
          notify_wrn(paste0("\"", nombre, "\" eliminado."))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # ── Renombrar ────────────────────────────────────────────
    observeEvent(input$renombrar_raster, {
      req(input$renombrar_raster)
      info   <- input$renombrar_raster
      rasters <- shared$rasters
      if (!is.null(rasters[[info$id]])) {
        rasters[[info$id]]$nombre <- info$nombre
        shared$rasters            <- rasters
      }
    })

    # ── Raster activo como reactive ──────────────────────────
    rst_activo <- reactive({
      req(shared$raster_activo, shared$rasters)
      shared$rasters[[shared$raster_activo]]$rst
    })

    rst_activo_nombre <- reactive({
      req(shared$raster_activo, shared$rasters)
      shared$rasters[[shared$raster_activo]]$nombre
    })

    # ── Banda activa ─────────────────────────────────────────
    rst_banda <- reactive({
      req(rst_activo(), input$banda_sel)
      rst_activo()[[as.integer(input$banda_sel)]]
    })

    rst_wgs84 <- reactive({
      req(rst_banda())
      r <- rst_banda()
      if (!terra::is.lonlat(r)) r <- terra::project(r, "EPSG:4326")
      r
    })

    # ── Controles de visualización en sidebar ────────────────
    output$controles_vis <- renderUI({
      if (is.null(shared$raster_activo) ||
          is.null(shared$rasters[[shared$raster_activo]])) {
        return(div(
          class = "text-muted small",
          icon("circle-info"),
          " Activá un raster para ver los controles."
        ))
      }
      rst   <- rst_activo()
      n     <- terra::nlyr(rst)
      noms  <- names(rst)
      bandas <- setNames(seq_len(n),
                         paste0("Banda ", seq_len(n), " (", noms, ")"))
      tagList(
        selectInput(ns("banda_sel"), "Banda:",
                    choices = bandas, selected = 1, width = "100%"),
        selectInput(ns("paleta_sel"), "Paleta:",
                    choices  = c("Viridis" = "viridis", "Magma"   = "magma",
                                 "Inferno" = "inferno", "Plasma"  = "plasma",
                                 "Cividis" = "cividis", "Grises"  = "grays",
                                 "Terrain" = "terrain"),
                    selected = "viridis", width = "100%"),
        div(
          class = "d-flex gap-2 mt-2",
          downloadButton(ns("descargar_png"), "PNG",
                         class = "btn-sm btn-outline-secondary flex-grow-1"),
          downloadButton(ns("descargar_tif"), "GeoTIFF",
                         class = "btn-sm btn-outline-secondary flex-grow-1")
        )
      )
    })

    # ── Mapa leaflet interactivo ─────────────────────────────
    output$mapa_rst <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    get_rst_pal_local <- function(rst, nombre) {
      vals    <- as.vector(terra::values(rst, na.rm = TRUE))
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

    observe({
      req(rst_wgs84(), shared$raster_activo)
      tryCatch({
        rst      <- rst_wgs84()
        pal      <- get_rst_pal_local(rst, input$paleta_sel %||% "viridis")
        r_raster <- raster::raster(rst)
        nombre   <- rst_activo_nombre()

        ext <- terra::ext(rst)

        leafletProxy(ns("mapa_rst")) %>%
          clearImages() %>%
          clearControls() %>%
          addProviderTiles(input$basemap_rst %||% "CartoDB.Positron") %>%
          addRasterImage(
            x       = r_raster,
            colors  = pal,
            opacity = 0.85,
            group   = nombre
          ) %>%
          addLegend(
            position = "bottomright",
            pal      = pal,
            values   = raster::values(r_raster),
            title    = nombre,
            opacity  = 0.85
          ) %>%
          fitBounds(ext$xmin, ext$ymin, ext$xmax, ext$ymax) %>%
          addScaleBar(position = "bottomleft")
      }, error = function(e) NULL)
    })

    # ── Panel de estadísticas ────────────────────────────────
    output$stats_panel <- renderUI({
      if (is.null(shared$raster_activo)) {
        return(div(
          class = "text-muted small text-center py-5",
          "Activá un raster para ver sus estadísticas."
        ))
      }

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(icon("table"), " Estadísticas por banda"),
            card_body(tableOutput(ns("stats_tabla")))
          ),
          card(
            card_header(icon("chart-bar"), " Histograma"),
            card_body(plotOutput(ns("histograma"), height = "300px"))
          )
        )
      )
    })

    output$stats_tabla <- renderTable({
      req(rst_activo())
      rst <- rst_activo()
      n   <- terra::nlyr(rst)
      do.call(rbind, lapply(seq_len(n), function(i) {
        vals <- terra::values(rst[[i]], na.rm = TRUE)
        data.frame(
          Banda   = paste0(i, " (", names(rst)[i], ")"),
          Min     = round(min(vals),  3),
          Max     = round(max(vals),  3),
          Media   = round(mean(vals), 3),
          Mediana = round(median(vals), 3),
          DE      = round(sd(vals),   3),
          NAs     = sum(is.na(terra::values(rst[[i]])))
        )
      }))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, small = TRUE)

    output$histograma <- renderPlot({
      req(rst_banda())
      vals <- as.vector(terra::values(rst_banda(), na.rm = TRUE))
      df   <- data.frame(x = vals)
      ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(
          fill  = colores$acento,
          color = "white",
          bins  = 50,
          alpha = 0.85
        ) +
        ggplot2::labs(
          x = names(rst_banda())[1],
          y = "Frecuencia"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", color = NA)
        )
    })

    # ── Panel de resumen ─────────────────────────────────────
    output$resumen_panel <- renderUI({
      if (is.null(shared$raster_activo) ||
          is.null(shared$rasters[[shared$raster_activo]])) {
        return(div(
          class = "text-muted small text-center py-5",
          "Activá un raster para ver su resumen."
        ))
      }

      rst_item <- shared$rasters[[shared$raster_activo]]
      rst      <- rst_item$rst
      crs_desc <- tryCatch(
        terra::crs(rst, describe = TRUE)$name,
        error = function(e) "Sin CRS"
      )
      ext <- terra::ext(rst)

      tagList(
        div(
          class = "p-3 rounded mb-3",
          style = paste0("border-left: 4px solid ", rst_item$color,
                         "; background:", colores$fondo, ";"),
          h6(class = "fw-bold mb-2",
             style = paste0("color:", rst_item$color, ";"),
             bs_icon("grid-3x3"), " ", rst_item$nombre),
          tags$table(
            class = "table table-sm table-borderless small mb-0",
            info_row("Bandas",      terra::nlyr(rst)),
            info_row("Filas",       terra::nrow(rst)),
            info_row("Columnas",    terra::ncol(rst)),
            info_row("Resolución",  paste(round(terra::res(rst), 6),
                                          collapse = " \u00d7 ")),
            info_row("CRS",         crs_desc),
            info_row("Ext. Oeste",  round(ext$xmin, 5)),
            info_row("Ext. Este",   round(ext$xmax, 5)),
            info_row("Ext. Sur",    round(ext$ymin, 5)),
            info_row("Ext. Norte",  round(ext$ymax, 5))
          )
        ),

        # ── Reproyección ─────────────────────────────────────
        div(
          class = "mb-3",
          h6(class = "fw-bold small text-muted text-uppercase",
             icon("rotate"), " Reproyectar"),
          div(
            class = "d-flex gap-1",
            textInput(ns("rst_epsg"), NULL,
                      placeholder = "EPSG (ej: 4326)",
                      width = "100%"),
            actionButton(ns("btn_reproyectar"), NULL,
                         icon  = icon("rotate"),
                         class = "btn-sm btn-outline-secondary")
          ),
          helpText(
            class = "small",
            icon("circle-info"),
            " WGS84=4326 · CRTM05=5367 · Mercator=3857 ·",
            tags$a("Buscar EPSG", href = "https://epsg.io",
                   target = "_blank")
          ),
          uiOutput(ns("crs_feedback"))
        ),

        # ── Descarga ─────────────────────────────────────────
        downloadButton(
          ns("descargar_tif_resumen"), "Descargar GeoTIFF",
          class = "btn-sm btn-outline-primary w-100"
        )
      )
    })

    # ── Reproyectar raster activo ────────────────────────────
    observeEvent(input$btn_reproyectar, {
      req(shared$raster_activo)
      epsg <- suppressWarnings(as.integer(trimws(input$rst_epsg)))
      if (is.na(epsg)) {
        notify_err("EPSG inválido — debe ser un número.")
        return()
      }
      tryCatch({
        rasters  <- shared$rasters
        id_rst   <- shared$raster_activo
        rasters[[id_rst]]$rst <- terra::project(
          rasters[[id_rst]]$rst, paste0("EPSG:", epsg)
        )
        shared$rasters <- rasters
        notify_ok(paste0("Raster reproyectado a EPSG:", epsg))
      }, error = function(e) {
        notify_err(paste("Error:", e$message))
      })
    })

    output$crs_feedback <- renderUI({
      req(shared$raster_activo, shared$rasters)
      rst_item <- shared$rasters[[shared$raster_activo]]
      if (is.null(rst_item)) return(NULL)
      crs <- tryCatch(
        terra::crs(rst_item$rst, describe = TRUE)$name,
        error = function(e) "Sin CRS"
      )
      div(class = "small text-success mt-1",
          icon("globe"), paste0(" CRS actual: ", crs))
    })

    # ── Descargas ────────────────────────────────────────────
    output$descargar_png <- downloadHandler(
      filename = function() paste0(rst_activo_nombre(), "_", Sys.Date(), ".png"),
      content  = function(file) {
        req(rst_banda())
        p <- ggplot2::ggplot() +
          tidyterra::geom_spatraster(data = rst_banda()) +
          ggplot2::scale_fill_viridis_c(
            option   = input$paleta_sel %||% "viridis",
            na.value = "transparent"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = rst_activo_nombre(), x = NULL, y = NULL)
        ggplot2::ggsave(file, plot = p, width = 10, height = 8,
                        dpi = 150, bg = "white")
      }
    )

    output$descargar_tif <- downloadHandler(
      filename = function() paste0(rst_activo_nombre(), "_", Sys.Date(), ".tif"),
      content  = function(file) {
        req(rst_activo())
        terra::writeRaster(rst_activo(), file, overwrite = TRUE)
      }
    )

    output$descargar_tif_resumen <- downloadHandler(
      filename = function() paste0(rst_activo_nombre(), "_", Sys.Date(), ".tif"),
      content  = function(file) {
        req(rst_activo())
        terra::writeRaster(rst_activo(), file, overwrite = TRUE)
      }
    )

  })
}
