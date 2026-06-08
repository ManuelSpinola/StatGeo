# ============================================================
# mod_mapa.R — Panel de mapa tipo SIG
# Capas: vectorial, raster, grilla H3
# Visualización adicional: ggplot2 + tabla de atributos
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_mapa_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    sidebar = sidebar(
      width  = 310,
      title  = "Capas",
      open   = TRUE,

      # ── Capa vectorial ──────────────────────────────────
      div(
        class = "mb-2 p-2 rounded",
        style = paste0("border-left: 4px solid ", colores$primario,
                       "; background:", colores$fondo, ";"),

        div(class = "d-flex align-items-center justify-content-between mb-1",
            h6(class = "fw-bold mb-0",
               style  = paste0("color:", colores$primario, ";"),
               bs_icon("map"), " Vectorial"),
            div(class = "form-check form-switch mb-0",
                checkboxInput(ns("vec_visible"), label = NULL, value = FALSE))),

        uiOutput(ns("vec_info")),

        # Colorear por variable
        selectInput(ns("color_col"), "Colorear por:",
                    choices = c("(ninguno)" = ""), width = "100%"),

        selectInput(ns("color_pal"), "Paleta:",
                    choices  = c("viridis","magma","plasma","inferno",
                                 "RdYlGn","Blues","Spectral","Set1","Dark2"),
                    selected = "viridis", width = "100%"),

        sliderInput(ns("vec_opacidad"), "Opacidad:",
                    min = 0, max = 1, value = 0.7, step = 0.05),

        radioButtons(ns("vec_estilo"), "Estilo:",
                     choices  = c("Relleno s\u00f3lido" = "solido",
                                  "Solo contorno"       = "contorno"),
                     selected = "contorno", inline = TRUE),

      ),

      hr(style = "margin:6px 0;"),

      # ── Capa raster ─────────────────────────────────────
      div(
        class = "mb-2 p-2 rounded",
        style = paste0("border-left: 4px solid ", colores$acento,
                       "; background:", colores$fondo, ";"),

        div(class = "d-flex align-items-center justify-content-between mb-1",
            h6(class = "fw-bold mb-0",
               style  = paste0("color:", colores$acento, ";"),
               bs_icon("grid-3x3"), " Raster"),
            div(class = "form-check form-switch mb-0",
                checkboxInput(ns("rst_visible"), label = NULL, value = FALSE))),

        uiOutput(ns("rst_info")),

        selectInput(ns("rst_banda"), "Banda:",
                    choices = c("Banda 1" = "1"), selected = "1", width = "100%"),

        selectInput(ns("rst_paleta"), "Paleta:",
                    choices  = c("Viridis" = "viridis", "Magma"   = "magma",
                                 "Inferno" = "inferno", "Plasma"  = "plasma",
                                 "Cividis" = "cividis", "Grises"  = "grays",
                                 "Terrain" = "terrain"),
                    selected = "viridis", width = "100%"),

        sliderInput(ns("rst_opacidad"), "Opacidad:",
                    min = 0, max = 1, value = 0.8, step = 0.05),

      ),

      hr(style = "margin:6px 0;"),

      # ── Grilla H3 ───────────────────────────────────────
      div(
        class = "mb-2 p-2 rounded",
        style = paste0("border-left: 4px solid ", colores$secundario, "; background:", colores$fondo, ";"),

        div(class = "d-flex align-items-center justify-content-between mb-1",
            h6(class = "fw-bold mb-0", style = paste0("color:", colores$secundario, ";"),
               bs_icon("hexagon-fill"), " Grilla H3"),
            div(class = "form-check form-switch mb-0",
                checkboxInput(ns("h3_visible"), label = NULL, value = FALSE))),

        sliderInput(ns("h3_res"), "Resolución H3:",
                    min = 1, max = 10, value = 6, step = 1),

        uiOutput(ns("h3_res_info")),

        div(
          class = "d-flex gap-1",
          actionButton(ns("btn_generar_h3"), "Generar",
                       icon  = icon("th"),
                       class = "btn-sm btn-success flex-grow-1"),
          downloadButton(ns("descargar_h3"), "",
                         icon  = icon("download"),
                         class = "btn-sm btn-outline-success")
        ),

        uiOutput(ns("h3_status")),

        sliderInput(ns("h3_opacidad"), "Opacidad:",
                    min = 0, max = 1, value = 0.4, step = 0.05)
      ),

      hr(style = "margin:6px 0;"),

      # ── Mapa base y exportar ─────────────────────────────
      selectInput(ns("basemap"), "Mapa base:",
                  choices  = c("CartoDB Claro"  = "CartoDB.Positron",
                               "CartoDB Oscuro" = "CartoDB.DarkMatter",
                               "OpenStreetMap"  = "OpenStreetMap",
                               "Sat\u00e9lite"  = "Esri.WorldImagery",
                               "Sin mapa base"  = "none"),
                  selected = "CartoDB.Positron"),

      downloadButton(ns("descargar_png"), "Exportar PNG",
                     class = "btn-sm w-100 mt-1",
                     style = paste0("background:", colores$primario,
                                    "; border-color:", colores$primario,
                                    "; color:#fff;"))
    ),

    # ── Panel principal ──────────────────────────────────────
    navset_card_tab(

      nav_panel(
        "Mapa interactivo",
        card_body(class = "p-0", style = "height:580px;",
                  leafletOutput(ns("mapa"), height = "100%"))
      ),

      nav_panel(
        "Mapa est\u00e1tico",
        card_body(
          class = "p-1",
          div(class = "d-flex justify-content-end mb-1",
              downloadButton(ns("download_ggplot"), "Guardar PNG",
                             class = "btn-sm btn-outline-secondary")),
          div(class = "d-flex gap-2 mb-2 align-items-end",
              selectInput(ns("gg_theme"), "Tema:",
                          choices  = c("Minimal" = "minimal", "Void" = "void",
                                       "Classic" = "classic", "Dark" = "dark"),
                          selected = "minimal", width = "150px"),
              conditionalPanel(
                condition = paste0("input['", ns("vec_visible"), "'] == true"),
                selectInput(ns("filter_col"), "Filtrar por:",
                            choices = c("(ninguno)" = "(ninguno)"), width = "180px")
              )),
          uiOutput(ns("filter_ui")),
          plotOutput(ns("mapa_ggplot"), height = "480px")
        )
      ),

      nav_panel(
        "Atributos",
        card_body(DTOutput(ns("tabla_atributos")))
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_mapa_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ════════════════════════════════════════════════════════
    # VECTORIAL
    # ════════════════════════════════════════════════════════

    h3_grid <- reactiveVal(NULL)

    # ── Info vectorial ───────────────────────────────────────
    output$vec_info <- renderUI({
      req(shared$sf_data)
      gt  <- as.character(sf::st_geometry_type(shared$sf_data, by_geometry = FALSE))
      crs <- sf::st_crs(shared$sf_data)$Name %||% "Sin CRS"
      div(class = "small text-muted mb-1",
          tags$span(class = "badge bg-secondary me-1", toupper(gt)),
          crs)
    })

    # ── Actualizar selectores al cargar vectorial ────────────
    observeEvent(if (!is.null(shared$sf_data) &&
                     inherits(shared$sf_data, "sf"))
      nrow(shared$sf_data), {
      req(shared$sf_data)
      df       <- sf::st_drop_geometry(shared$sf_data)
      cols     <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]
      updateSelectInput(session, "color_col",
                        choices  = c("(ninguno)" = "", cols),
                        selected = if (length(num_cols) > 0) num_cols[1] else "")
      updateSelectInput(session, "filter_col",
                        choices = c("(ninguno)" = "(ninguno)", cols))
    })

    # ── Vectorial procesado (reproyectado + filtrado) ────────
    vec_processed <- reactive({
      req(shared$sf_data)
      shared$sf_data
    })

    vec_filtered <- reactive({
      req(vec_processed())
      sf_obj <- vec_processed()
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

    output$filter_ui <- renderUI({
      req(vec_processed())
      col <- input$filter_col
      if (is.null(col) || col == "(ninguno)") return(NULL)
      col_data <- vec_processed()[[col]]
      if (is.numeric(col_data)) {
        rng <- range(col_data, na.rm = TRUE)
        sliderInput(ns("filter_range"), "Rango:",
                    min = floor(rng[1]), max = ceiling(rng[2]),
                    value = rng, step = diff(rng) / 100)
      } else {
        vals <- sort(unique(as.character(col_data)))
        checkboxGroupInput(ns("filter_cats"), "Categorías:",
                           choices = vals, selected = vals)
      }
    })

    # ════════════════════════════════════════════════════════
    # RASTER
    # ════════════════════════════════════════════════════════

    # ── Info raster ──────────────────────────────────────────
    output$rst_info <- renderUI({
      req(shared$raster_data)
      rst <- shared$raster_data
      div(class = "small text-muted mb-1",
          paste0(terra::nlyr(rst), " banda(s) · ",
                 terra::nrow(rst), "\u00d7", terra::ncol(rst)))
    })

    # ── Actualizar selector de bandas ────────────────────────
    observeEvent(if (!is.null(shared$raster_data) &&
                     inherits(shared$raster_data, "SpatRaster"))
      terra::nlyr(shared$raster_data), {
      req(shared$raster_data)
      r      <- shared$raster_data
      n      <- terra::nlyr(r)
      bandas <- setNames(seq_len(n), paste0("Banda ", seq_len(n),
                                            " (", names(r), ")"))
      updateSelectInput(session, "rst_banda", choices = bandas, selected = 1)
    })

    # ── Banda raster activa ──────────────────────────────────
    rst_band <- reactive({
      req(shared$raster_data, input$rst_banda)
      shared$raster_data[[as.integer(input$rst_banda)]]
    })

    rst_wgs84 <- reactive({
      req(rst_band())
      r <- rst_band()
      if (!terra::is.lonlat(r)) r <- terra::project(r, "EPSG:4326")
      r
    })

    # ── Paleta raster ────────────────────────────────────────
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
                        viridisLite::viridis)
      colorNumeric(palette = pal_fun(256), domain = rng, na.color = "transparent")
    }

    # ════════════════════════════════════════════════════════
    # GRILLA H3
    # ════════════════════════════════════════════════════════

    output$h3_res_info <- renderUI({
      res <- input$h3_res
      if (is.null(res)) return(NULL)
      desc <- dplyr::case_when(
        res <= 3 ~ "Muy gruesa",
        res <= 5 ~ "Gruesa — regional",
        res == 6 ~ "Media — recomendado",
        res == 7 ~ "Fina",
        res >= 8 ~ "Muy fina — puede ser lento"
      )
      color <- if (res >= 8) "text-warning" else "text-muted"
      div(class = paste("small mt-1 mb-1", color),
          icon(if (res >= 8) "triangle-exclamation" else "circle-info"),
          paste0(" Res. ", res, ": ", desc))
    })

    observeEvent(input$btn_generar_h3, {
      if (is.null(shared$sf_data) || !inherits(shared$sf_data, "sf")) {
        showNotification("Cargá un vectorial primero como AOI.",
                         type = "warning", duration = 4)
        return()
      }
      showNotification(paste0("Generando grilla H3 (res. ", input$h3_res, ")..."),
                       type = "message", duration = 3)
      tryCatch({
        grilla <- paisaje::get_h3_grid(shared$sf_data, resolution = input$h3_res)
        grilla <- sf::st_transform(grilla, 4326)
        h3_grid(grilla)
        showNotification(paste0("\u2713 ", format(nrow(grilla), big.mark = ","),
                                " hex\u00e1gonos generados."),
                         type = "message", duration = 4)
      }, error = function(e)
        showNotification(paste("Error:", e$message), type = "error", duration = 6))
    })

    output$h3_status <- renderUI({
      g <- h3_grid()
      if (is.null(g)) {
        div(class = "small text-muted mt-1",
            icon("circle-info"), " Sin grilla generada.")
      } else {
        div(class = "small text-success mt-1",
            icon("check"),
            paste0(" ", format(nrow(g), big.mark = ","),
                   " hex\u00e1gonos · res. ", input$h3_res))
      }
    })

    output$descargar_h3 <- downloadHandler(
      filename = function() paste0("h3_res", input$h3_res, "_", Sys.Date(), ".gpkg"),
      content  = function(file) {
        req(h3_grid())
        sf::st_write(h3_grid(), file, quiet = TRUE, delete_dsn = TRUE)
      }
    )

    # ════════════════════════════════════════════════════════
    # MAPA INTERACTIVO
    # ════════════════════════════════════════════════════════

    output$mapa <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    # ── Función de zoom al extent de los datos ──────────────
    hacer_zoom <- function() {
      if (!is.null(shared$sf_data) && inherits(shared$sf_data, "sf")) {
        bb <- tryCatch(
          sf::st_bbox(sf::st_transform(shared$sf_data, 4326)),
          error = function(e) NULL
        )
        if (!is.null(bb))
          leafletProxy(ns("mapa")) %>%
            fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      } else if (!is.null(shared$raster_data) &&
                 inherits(shared$raster_data, "SpatRaster")) {
        ext <- tryCatch(
          terra::ext(terra::project(shared$raster_data, "EPSG:4326")),
          error = function(e) NULL
        )
        if (!is.null(ext))
          leafletProxy(ns("mapa")) %>%
            fitBounds(ext$xmin, ext$ymin, ext$xmax, ext$ymax)
      }
    }

    # Zoom al cargar vectorial
    observeEvent(if (!is.null(shared$sf_data) &&
                     inherits(shared$sf_data, "sf"))
      nrow(shared$sf_data), {
      hacer_zoom()
    })

    # Zoom al cargar raster
    observeEvent(if (!is.null(shared$raster_data) &&
                     inherits(shared$raster_data, "SpatRaster"))
      terra::ncell(shared$raster_data), {
      hacer_zoom()
    })

    # Zoom al generar grilla H3
    observeEvent(h3_grid(), {
      req(h3_grid())
      bb <- sf::st_bbox(h3_grid())
      leafletProxy(ns("mapa")) %>%
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })



    # ── Observador principal del mapa ────────────────────────
    # ── Función para redibujar todas las capas ───────────────
    redibujar_mapa <- function() {
      sf_data     <- shared$sf_data
      raster_data <- shared$raster_data
      grilla      <- h3_grid()

      tiene_vec <- !is.null(sf_data)     && inherits(sf_data, "sf")
      tiene_rst <- !is.null(raster_data) && inherits(raster_data, "SpatRaster")
      tiene_h3  <- !is.null(grilla)

      vec_vis <- isTRUE(input$vec_visible) && tiene_vec
      rst_vis <- isTRUE(input$rst_visible) && tiene_rst
      h3_vis  <- isTRUE(input$h3_visible)  && tiene_h3
      basemap <- input$basemap %||% "CartoDB.Positron"

      m <- leafletProxy(ns("mapa")) %>%
        clearImages() %>% clearShapes() %>%
        clearControls() %>% clearMarkers() %>%
        leafgl::clearGlLayers()

      if (basemap != "none")
        m <- m %>% addProviderTiles(basemap)

      # Raster primero (capa base)
      if (rst_vis) {
        banda <- input$rst_banda %||% "1"
        tryCatch({
          rst   <- rst_wgs84()
          pal   <- get_rst_pal(rst, input$rst_paleta %||% "viridis")
          rst_r <- raster::raster(rst)
          m <- m %>%
            addRasterImage(x = rst_r, colors = pal,
                           opacity = input$rst_opacidad %||% 0.8,
                           group   = "Raster") %>%
            addLegend(position = "bottomright", pal = pal,
                      values   = raster::values(rst_r),
                      title    = names(rst_band())[1],
                      opacity  = 0.85)
        }, error = function(e) NULL)
      }

      # Vectorial encima
      if (vec_vis) {
        tryCatch({
          sf_obj   <- sf::st_transform(vec_filtered(), 4326)
          gt       <- toupper(as.character(
            sf::st_geometry_type(sf_obj, by_geometry = FALSE)))
          color_by <- input$color_col
          has_col  <- !is.null(color_by) && color_by != "" &&
            color_by %in% names(sf_obj)
          fill_op  <- if (input$vec_estilo %||% "contorno" == "contorno") 0
                      else input$vec_opacidad %||% 0.7
          col_fill <- colores$primario

          if (has_col && is.numeric(sf_obj[[color_by]])) {
            pal_fn <- switch(input$color_pal %||% "viridis",
                             "viridis" = viridisLite::viridis,
                             "magma"   = viridisLite::magma,
                             "plasma"  = viridisLite::plasma,
                             "inferno" = viridisLite::inferno,
                             viridisLite::viridis)
            pal_leaflet <- colorNumeric(pal_fn(256), sf_obj[[color_by]])
            col_fill    <- pal_leaflet(sf_obj[[color_by]])
          }

          m <- if (grepl("POINT|MULTIPOINT", gt)) {
            m %>% addCircleMarkers(data = sf_obj, group = "Vectorial",
                                   radius = 5, color = colores$primario,
                                   fillColor = col_fill, fillOpacity = fill_op,
                                   opacity = input$vec_opacidad %||% 0.7)
          } else if (grepl("LINE", gt)) {
            m %>% addPolylines(data = sf_obj, group = "Vectorial",
                               color = colores$primario,
                               opacity = input$vec_opacidad %||% 0.7)
          } else {
            m %>% addPolygons(data = sf_obj, group = "Vectorial",
                              fillColor = col_fill, color = colores$primario,
                              fillOpacity = fill_op,
                              opacity = input$vec_opacidad %||% 0.7,
                              weight = 1)
          }
        }, error = function(e) NULL)
      }

      # Grilla H3
      if (h3_vis) {
        tryCatch({
          g  <- suppressWarnings(sf::st_cast(h3_grid(), "POLYGON"))
          op <- input$h3_opacidad %||% 0.4
          if (nrow(g) > 50000) {
            # Grilla grande: usar leafgl sin opacidad dinámica
            m <- m %>%
              leafgl::addGlPolygons(
                data      = g,
                fillColor = colores$secundario,
                color     = colores$primario,
                weight    = 1,
                group     = "Grilla H3"
              )
          } else {
            # Grilla normal: leaflet con opacidad dinámica
            m <- m %>%
              addPolygons(
                data        = g,
                fillColor   = colores$secundario,
                fillOpacity = op,
                color       = colores$primario,
                opacity     = min(op + 0.3, 1),
                weight      = 1,
                group       = "Grilla H3"
              )
          }
        }, error = function(e) NULL)
      }

      # Control de capas
      grupos <- c()
      if (rst_vis) grupos <- c(grupos, "Raster")
      if (vec_vis) grupos <- c(grupos, "Vectorial")
      if (h3_vis)  grupos <- c(grupos, "Grilla H3")
      if (length(grupos) > 0)
        m <- m %>% addLayersControl(
          overlayGroups = grupos,
          options       = layersControlOptions(collapsed = FALSE))

      m %>% addScaleBar(position = "bottomleft")
    }

    # ── Disparar redibujo ante cualquier cambio relevante ────
    observe({
      # Registrar dependencias
      shared$sf_data; shared$raster_data; h3_grid()
      input$vec_visible; input$rst_visible; input$h3_visible
      input$vec_estilo; input$vec_opacidad; input$color_col; input$color_pal
      input$rst_paleta; input$rst_opacidad; input$rst_banda
      input$h3_opacidad; input$basemap
      # Redibujar
      redibujar_mapa()
    })

    # ════════════════════════════════════════════════════════
    # MAPA ESTÁTICO GGPLOT
    # ════════════════════════════════════════════════════════

    output$mapa_ggplot <- renderPlot({
      tiene_vec <- !is.null(shared$sf_data) && inherits(shared$sf_data, "sf")
      tiene_rst <- !is.null(shared$raster_data) && inherits(shared$raster_data, "SpatRaster")

      if (!tiene_vec && !tiene_rst) {
        return(ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Cargá al menos una capa en Datos",
                   size = 5, color = colores$texto) +
          theme_void())
      }

      # ── Validar combinación de capas ──────────────────────
      vec_vis <- tiene_vec && isTRUE(input$vec_visible)
      rst_vis <- tiene_rst && isTRUE(input$rst_visible)
      h3_vis  <- !is.null(h3_grid()) && isTRUE(input$h3_visible)

      if (!vec_vis && !rst_vis && !h3_vis) {
        return(ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Activá al menos una capa en el panel izquierdo.",
                   size = 5, color = colores$texto) + theme_void())
      }

      if (vec_vis && rst_vis) {
        return(ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("El mapa estático muestra una capa a la vez.\n",
                                  "Desactivá el Raster para ver el mapa vectorial,\n",
                                  "o desactivá el Vectorial para ver el Raster."),
                   size = 4.5, color = colores$texto, hjust = 0.5, vjust = 0.5) +
          theme_void())
      }

      p   <- ggplot()
      pal <- input$color_pal %||% "viridis"

      # ── Solo raster ───────────────────────────────────────
      if (rst_vis) {
        r <- shared$raster_data[[as.integer(input$rst_banda %||% "1")]]
        pal_opt <- if (input$rst_paleta %||% "viridis" %in%
                       c("viridis","magma","plasma","inferno","cividis"))
          input$rst_paleta %||% "viridis" else "viridis"
        p <- p +
          tidyterra::geom_spatraster(data = r) +
          scale_fill_viridis_c(option   = pal_opt,
                               na.value = "transparent",
                               name     = "Raster",
                               alpha    = input$rst_opacidad %||% 0.8)
      }

      # ── Solo vectorial ────────────────────────────────────
      if (vec_vis) {
        sf_obj   <- vec_filtered()
        color_by <- input$color_col
        gt       <- toupper(as.character(
          sf::st_geometry_type(sf_obj, by_geometry = FALSE)))
        has_col  <- !is.null(color_by) && color_by != "" &&
          color_by %in% names(sf_obj)

        if (has_col) {
          col_data <- sf_obj[[color_by]]
          if (grepl("POINT|MULTIPOINT", gt)) {
            p <- p + geom_sf(data = sf_obj,
                             aes(color = .data[[color_by]]), size = 2)
            if (is.numeric(col_data))
              p <- p + scale_color_viridis_c(option = pal, name = color_by)
          } else if (grepl("LINE", gt)) {
            p <- p + geom_sf(data = sf_obj,
                             aes(color = .data[[color_by]]), linewidth = 0.8)
            if (is.numeric(col_data))
              p <- p + scale_color_viridis_c(option = pal, name = color_by)
          } else {
            p <- p + geom_sf(data = sf_obj,
                             aes(fill = .data[[color_by]]),
                             color = "white", linewidth = 0.2)
            if (is.numeric(col_data))
              p <- p + scale_fill_viridis_c(option = pal, name = color_by,
                                            na.value = "grey90")
            else
              p <- tryCatch(
                p + scale_fill_brewer(palette = "Set2", name = color_by),
                error = function(e) p
              )
          }
        } else {
          if (grepl("POINT|MULTIPOINT", gt))
            p <- p + geom_sf(data = sf_obj,
                             color = colores$primario, size = 2, alpha = 0.8)
          else if (grepl("LINE", gt))
            p <- p + geom_sf(data = sf_obj,
                             color = colores$primario, linewidth = 0.8)
          else
            p <- p + geom_sf(data = sf_obj,
                             fill  = colores$secundario, color = "white",
                             linewidth = 0.3, alpha = 0.8)
        }
      }

      # ── Grilla H3 encima de todo ──────────────────────────
      if (h3_vis) {
        g <- suppressWarnings(sf::st_cast(h3_grid(), "POLYGON"))
        p <- p + geom_sf(data = g, fill = NA,
                         color = colores$primario,
                         linewidth = 0.3, alpha = 0.6)
      }

      p + switch(input$gg_theme %||% "minimal",
                 "minimal" = theme_minimal(),
                 "void"    = theme_void(),
                 "classic" = theme_classic(),
                 "dark"    = theme_dark(),
                 theme_minimal()) +
        labs(x = NULL, y = NULL) +
        theme(plot.background  = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              legend.position  = "right")
    })

    output$download_ggplot <- downloadHandler(
      filename = function() paste0("mapa_", Sys.Date(), ".png"),
      content  = function(file) {
        req(vec_filtered())
        p <- ggplot(vec_filtered()) +
          geom_sf(fill = colores$secundario, color = colores$primario,
                  linewidth = 0.3, alpha = 0.8) +
          theme_minimal() +
          labs(title = "Mapa vectorial", x = NULL, y = NULL)
        ggplot2::ggsave(file, plot = p, width = 10, height = 8,
                        dpi = 150, bg = "white")
      }
    )

    # ════════════════════════════════════════════════════════
    # TABLA DE ATRIBUTOS
    # ════════════════════════════════════════════════════════

    output$tabla_atributos <- renderDT({
      if (is.null(shared$sf_data) || !inherits(shared$sf_data, "sf")) {
        return(datatable(data.frame(Mensaje = "Cargá un vectorial en Datos para ver los atributos."),
                         rownames = FALSE))
      }
      df <- sf::st_drop_geometry(vec_filtered())
      datatable(df,
                options  = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE,
                class    = "table-sm table-striped table-hover")
    })

    # ════════════════════════════════════════════════════════
    # EXPORTAR PNG
    # ════════════════════════════════════════════════════════

    output$descargar_png <- downloadHandler(
      filename = function() paste0("mapa_", Sys.Date(), ".png"),
      content  = function(file) {
        mv <- NULL

        if (!is.null(shared$raster_data) && isTRUE(input$rst_visible) && !is.null(shared$raster_data)) {
          rst      <- rst_wgs84()
          pal_cols <- switch(input$rst_paleta %||% "viridis",
                             "viridis" = viridisLite::viridis(256),
                             "magma"   = viridisLite::magma(256),
                             "inferno" = viridisLite::inferno(256),
                             "plasma"  = viridisLite::plasma(256),
                             "cividis" = viridisLite::cividis(256),
                             "grays"   = gray.colors(256, 0.9, 0.1),
                             "terrain" = terrain.colors(256),
                             viridisLite::viridis(256))
          mv <- mapview::mapview(rst, col.regions = pal_cols,
                                 alpha = input$rst_opacidad %||% 0.8,
                                 layer.name = "Raster")
        }

        if (!is.null(shared$sf_data) && isTRUE(input$vec_visible)) {
          mv_vec <- mapview::mapview(sf::st_transform(vec_filtered(), 4326),
                                    col.regions   = colores$secundario,
                                    color         = colores$primario,
                                    alpha.regions = input$vec_opacidad %||% 0.7,
                                    layer.name    = "Vectorial")
          mv <- if (is.null(mv)) mv_vec else mv + mv_vec
        }

        if (!is.null(h3_grid()) && isTRUE(input$h3_visible)) {
          mv_h3 <- mapview::mapview(sf::st_cast(h3_grid(), "POLYGON"),
                                    col.regions   = colores$secundario,
                                    color         = colores$primario,
                                    alpha.regions = input$h3_opacidad %||% 0.4,
                                    layer.name    = "Grilla H3")
          mv <- if (is.null(mv)) mv_h3 else mv + mv_h3
        }

        if (is.null(mv)) {
          showNotification("No hay capas visibles.", type = "warning", duration = 4)
          return()
        }

        mapview::mapshot(mv, file = file)
      }
    )

  })
}
