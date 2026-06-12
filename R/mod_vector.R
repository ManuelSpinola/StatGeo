# ============================================================
# mod_vector.R — Carga y exploración de capas vectoriales
#
# Soporta múltiples capas simultáneas.
# Cada capa tiene: id, nombre (editable), sf, color, visible.
# El objeto shared$capas_vec es una lista reactiva de capas.
#
# Formatos: GeoJSON, GeoPackage, Shapefile (.zip),
#           KML/KMZ, CSV/TSV, Excel (.xlsx/.xls)
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_vector_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    sidebar = sidebar(
      width = 300,
      title = "Capas vectoriales",
      open  = TRUE,

      # ── Cargar nueva capa ──────────────────────────────────
      fileInput(
        ns("vec_file"),
        label       = NULL,
        accept      = c(".geojson", ".json", ".gpkg",
                        ".zip",     ".kml",  ".kmz",
                        ".csv",     ".tsv",  ".xlsx", ".xls"),
        buttonLabel = tagList(icon("upload"), " Agregar capa"),
        placeholder = "Ningún archivo seleccionado"
      ),
      div(
        class = "small text-muted mt-1 mb-2",
        style = "line-height: 1.6;",
        tags$strong("Formatos aceptados:"), tags$br(),
        "GeoJSON \u00b7 GeoPackage \u00b7 Shapefile (.zip)", tags$br(),
        "KML / KMZ \u00b7 CSV \u00b7 TSV \u00b7 Excel (.xlsx)"
      ),

      # ── Selector de capa GeoPackage ───────────────────────
      uiOutput(ns("layer_selector_ui")),

      # ── Selector lon/lat para CSV/Excel ───────────────────
      uiOutput(ns("coord_ui")),

      hr(style = "margin: 6px 0;"),

      # ── Lista de capas cargadas ───────────────────────────
      h6(
        class = "text-muted fw-bold text-uppercase small mb-2",
        icon("layer-group"), " Capas cargadas"
      ),

      div(
        style = "min-height: 60px; border: 1px solid var(--bs-border-color); border-radius: 6px; padding: 12px; margin-top: 4px;",
        uiOutput(ns("lista_capas")),
        uiOutput(ns("empty_state"))
      )
    ),

    # ── Panel principal ──────────────────────────────────────
    navset_card_tab(

      nav_panel(
        title = tagList(icon("map"), " Mapa"),
        card_body(
          class = "p-0",
          style = "height: 520px;",
          leafletOutput(ns("mapa_preview"), height = "100%")
        )
      ),

      nav_panel(
        title = tagList(icon("table"), " Atributos"),
        card_body(
          uiOutput(ns("selector_capa_tabla")),
          DTOutput(ns("tabla_atributos"))
        )
      ),

      nav_panel(
        title = tagList(icon("circle-info"), " Resumen"),
        card_body(
          uiOutput(ns("selector_capa_resumen")),
          uiOutput(ns("resumen_capa"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_vector_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Paleta de colores por capa ───────────────────────────
    # Tableau Color Blind — rotan automáticamente
    paleta_capas <- c(
      "#1170AA", "#FC7D0B", "#5FA2CE", "#C85200",
      "#7BC8ED", "#A3ACB9", "#F1CE63", "#9F8B75"
    )

    # ── Estado interno ───────────────────────────────────────
    rv <- reactiveValues(
      raw_df      = NULL,
      file_type   = NULL,
      gpkg_layers = NULL,
      gpkg_path   = NULL,
      pending_sf  = NULL,   # sf listo para agregar (antes de nombre)
      color_idx   = 0       # índice rotativo de color
    )

    # ── Helpers de extensión ─────────────────────────────────
    file_ext_clean <- reactive({
      req(input$vec_file)
      tolower(tools::file_ext(input$vec_file$name))
    })

    is_tabular <- reactive({
      if (is.null(input$vec_file)) return(FALSE)
      file_ext_clean() %in% c("csv", "tsv", "xlsx", "xls")
    })

    is_gpkg_multi <- reactive({
      !is.null(rv$gpkg_layers) && length(rv$gpkg_layers) > 1
    })

    # ── Helper: agregar capa a shared$capas_vec ──────────────
    agregar_capa <- function(sf_obj, nombre) {
      sf_obj <- ensure_crs(sf_obj)

      rv$color_idx <- (rv$color_idx %% length(paleta_capas)) + 1
      color        <- paleta_capas[rv$color_idx]

      id_capa <- paste0("capa_", format(Sys.time(), "%Y%m%d%H%M%S"), sample(1000:9999, 1))

      nueva <- list(
        id      = id_capa,
        nombre  = nombre,
        sf      = sf_obj,
        color   = color,
        visible = TRUE
      )

      capas_actual        <- shared$capas_vec
      capas_actual[[id_capa]] <- nueva
      shared$capas_vec    <- capas_actual

      notify_ok(paste0("\"", nombre, "\" cargada — ",
                       nrow(sf_obj), " geometrías."))
    }

    # ── Selector de capa GeoPackage ──────────────────────────
    output$layer_selector_ui <- renderUI({
      req(is_gpkg_multi())
      div(
        class = "alert alert-info py-2 px-3 small mt-1",
        icon("layer-group"), " El archivo tiene varias capas.",
        selectInput(
          ns("gpkg_layer"),
          label   = "Seleccionar capa:",
          choices = rv$gpkg_layers
        ),
        actionButton(
          ns("btn_add_gpkg_layer"), "Agregar esta capa",
          class = "btn-sm btn-primary w-100 mt-1"
        )
      )
    })

    # ── Selector lon/lat ─────────────────────────────────────
    output$coord_ui <- renderUI({
      req(is_tabular(), !is.null(rv$raw_df))
      cols <- names(rv$raw_df)
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          selectInput(
            ns("lon_col"), "Longitud (X):",
            choices  = cols,
            selected = cols[grepl("^(lon|lng|x|longitud)$",
                                  cols, ignore.case = TRUE)][1]
          ),
          selectInput(
            ns("lat_col"), "Latitud (Y):",
            choices  = cols,
            selected = cols[grepl("^(lat|y|latitud)$",
                                  cols, ignore.case = TRUE)][1]
          )
        ),
        actionButton(
          ns("btn_add_tabular"), "Agregar como puntos",
          class = "btn-sm btn-primary w-100 mt-1"
        )
      )
    })

    # ── Procesar archivo al subirlo ──────────────────────────
    observeEvent(input$vec_file, {
      req(input$vec_file)
      path <- input$vec_file$datapath
      ext  <- file_ext_clean()
      nombre_base <- tools::file_path_sans_ext(input$vec_file$name)

      # Reset estado anterior
      rv$raw_df      <- NULL
      rv$gpkg_layers <- NULL
      rv$gpkg_path   <- NULL

      tryCatch({
        sf_obj <- switch(ext,

          "zip" = {
            tmp <- tempdir()
            utils::unzip(path, exdir = tmp)
            shp <- list.files(tmp, pattern = "\\.shp$",
                              full.names = TRUE, recursive = TRUE)[1]
            sf::read_sf(shp)
          },

          "gpkg" = {
            layers <- sf::st_layers(path)$name
            rv$gpkg_layers <- layers
            rv$gpkg_path   <- path
            if (length(layers) == 1) {
              sf::read_sf(path, layer = layers[1])
            } else {
              NULL  # usuario elige capa
            }
          },

          "kmz" = {
            tmp <- tempdir()
            utils::unzip(path, exdir = tmp)
            kml <- list.files(tmp, pattern = "\\.kml$",
                              full.names = TRUE, recursive = TRUE)[1]
            sf::read_sf(kml)
          },

          "csv" = {
            rv$raw_df    <- readr::read_csv(path, show_col_types = FALSE)
            rv$file_type <- "tabular"
            NULL
          },

          "tsv" = {
            rv$raw_df    <- readr::read_tsv(path, show_col_types = FALSE)
            rv$file_type <- "tabular"
            NULL
          },

          "xlsx" = , "xls" = {
            rv$raw_df    <- readxl::read_excel(path)
            rv$file_type <- "tabular"
            NULL
          },

          sf::read_sf(path)
        )

        if (!is.null(sf_obj)) {
          agregar_capa(sf_obj, nombre_base)
        }

      }, error = function(e) {
        notify_err(paste("Error al leer el archivo:", e$message))
      })
    })

    # ── Agregar capa GeoPackage elegida ──────────────────────
    observeEvent(input$btn_add_gpkg_layer, {
      req(rv$gpkg_path, input$gpkg_layer)
      tryCatch({
        sf_obj <- sf::read_sf(rv$gpkg_path, layer = input$gpkg_layer)
        agregar_capa(sf_obj, input$gpkg_layer)
      }, error = function(e) {
        notify_err(paste("Error al leer la capa:", e$message))
      })
    })

    # ── Agregar capa tabular ─────────────────────────────────
    observeEvent(input$btn_add_tabular, {
      req(rv$raw_df, input$lon_col, input$lat_col)
      nombre_base <- tools::file_path_sans_ext(input$vec_file$name)
      tryCatch({
        sf_obj <- sf::st_as_sf(
          rv$raw_df,
          coords = c(input$lon_col, input$lat_col),
          crs    = 4326,
          remove = FALSE
        )
        agregar_capa(sf_obj, nombre_base)
      }, error = function(e) {
        notify_err(paste("Error al convertir coordenadas:", e$message))
      })
    })

    # ── Lista de capas cargadas ──────────────────────────────
    output$lista_capas <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return(NULL)

      tagList(lapply(names(capas), function(id_capa) {
        capa <- capas[[id_capa]]
        gt   <- tryCatch(
          toupper(as.character(
            sf::st_geometry_type(capa$sf, by_geometry = FALSE))),
          error = function(e) "?"
        )
        icono <- switch(gt,
          "POINT"           = , "MULTIPOINT"      = "location-dot",
          "LINESTRING"      = , "MULTILINESTRING" = "route",
          "POLYGON"         = , "MULTIPOLYGON"    = "draw-polygon",
          "shapes"
        )

        div(
          class = "d-flex align-items-center gap-2 mb-2 p-2 rounded",
          style = paste0(
            "border-left: 4px solid ", capa$color, ";",
            "background:#EEF3FA;",
            "box-shadow: 0 0 0 1px ", capa$color, ";"
          ),

          icon(icono, style = paste0("color:", capa$color, "; flex-shrink:0;")),

          div(
            style = "flex: 1; min-width: 0;",
            tags$input(
              type     = "text",
              value    = capa$nombre,
              class    = "form-control form-control-sm",
              style    = "font-size:0.8rem; padding:2px 6px;",
              onchange = sprintf(
                "Shiny.setInputValue('%s', {id: '%s', nombre: this.value}, {priority: 'event'})",
                ns("renombrar_capa"), id_capa
              )
            ),
            div(
              class = "text-muted mt-1",
              style = "font-size:0.7rem;",
              paste0(format(nrow(capa$sf), big.mark = ","), " geom. · ", gt)
            )
          ),

          actionButton(
            ns(paste0("del_", id_capa)), NULL,
            icon  = icon("xmark"),
            class = "btn-sm btn-outline-danger p-1",
            style = "line-height:1; flex-shrink:0;",
            title = "Eliminar capa"
          )
        )
      }))
    })

    # ── Estado vacío ─────────────────────────────────────────
    output$empty_state <- renderUI({
      capas <- shared$capas_vec
      if (!is.null(capas) && length(capas) > 0) return(NULL)
      div(
        class = "text-muted small text-center py-3",
        icon("arrow-up-from-bracket",
             style = "font-size:1.5rem; display:block; margin-bottom:6px;"),
        "Subí un archivo para agregar tu primera capa vectorial."
      )
    })

    # ── Renombrar capa ───────────────────────────────────────
    observeEvent(input$renombrar_capa, {
      req(input$renombrar_capa)
      info    <- input$renombrar_capa
      capas   <- shared$capas_vec
      id_capa <- info$id
      if (!is.null(capas[[id_capa]])) {
        capas[[id_capa]]$nombre <- info$nombre
        shared$capas_vec        <- capas
      }
    })

    # ── Eliminar capa — observer dinámico por capa ───────────
    observe({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return()
      lapply(names(capas), function(id_capa) {
        btn_id <- paste0("del_", id_capa)
        observeEvent(input[[btn_id]], {
          capas_actual         <- shared$capas_vec
          nombre               <- capas_actual[[id_capa]]$nombre
          capas_actual[[id_capa]] <- NULL
          shared$capas_vec     <- capas_actual
          notify_wrn(paste0("\"", nombre, "\" eliminada."))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # ── Selector de capa para tabla ──────────────────────────
    output$selector_capa_tabla <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return(NULL)
      nombres <- sapply(capas, function(c) c$nombre)
      selectInput(
        ns("capa_tabla_sel"),
        label   = "Ver atributos de:",
        choices = setNames(names(capas), nombres),
        width   = "100%"
      )
    })

    # ── Tabla de atributos ───────────────────────────────────
    output$tabla_atributos <- renderDT({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0 ||
          is.null(input$capa_tabla_sel)) {
        return(datatable(
          data.frame(Mensaje = "Cargá al menos una capa vectorial."),
          rownames = FALSE
        ))
      }
      capa <- capas[[input$capa_tabla_sel]]
      if (is.null(capa)) return(NULL)
      df <- sf::st_drop_geometry(capa$sf)
      datatable(
        df,
        options  = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class    = "table-sm table-striped table-hover"
      )
    })

    # ── Selector de capa para resumen ────────────────────────
    output$selector_capa_resumen <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return(NULL)
      nombres <- sapply(capas, function(c) c$nombre)
      selectInput(
        ns("capa_resumen_sel"),
        label   = "Ver resumen de:",
        choices = setNames(names(capas), nombres),
        width   = "100%"
      )
    })

    # ── Resumen de capa ──────────────────────────────────────
    output$resumen_capa <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0 ||
          is.null(input$capa_resumen_sel)) {
        return(div(
          class = "text-muted small p-3",
          "Cargá al menos una capa vectorial para ver su resumen."
        ))
      }
      capa   <- capas[[input$capa_resumen_sel]]
      if (is.null(capa)) return(NULL)
      sf_obj <- capa$sf
      crs    <- sf::st_crs(sf_obj)
      gt     <- as.character(sf::st_geometry_type(sf_obj, by_geometry = FALSE))
      df     <- sf::st_drop_geometry(sf_obj)
      col_types <- sapply(df, function(x) class(x)[1])
      num_cols  <- sum(col_types %in% c("numeric", "integer", "double"))
      cat_cols  <- sum(col_types %in% c("character", "factor"))
      bbox      <- sf::st_bbox(sf::st_transform(sf_obj, 4326))

      tagList(
        # ── Ficha de la capa ─────────────────────────────────
        div(
          class = "p-3 rounded mb-3",
          style = paste0("border-left: 4px solid ", capa$color,
                         "; background:", colores$fondo, ";"),
          h6(class = "fw-bold mb-2",
             style = paste0("color:", capa$color, ";"),
             icon("layer-group"), " ", capa$nombre),
          tags$table(
            class = "table table-sm table-borderless small mb-0",
            info_row("Geometría",       gt),
            info_row("Número de filas", format(nrow(sf_obj), big.mark = ",")),
            info_row("Columnas totales",ncol(df)),
            info_row("  · Numéricas",   num_cols),
            info_row("  · Categóricas", cat_cols),
            info_row("CRS",             if (!is.na(crs)) crs$Name else "Sin CRS"),
            info_row("EPSG",            if (!is.na(crs$epsg)) crs$epsg else "—"),
            info_row("Ext. Oeste",      round(as.numeric(bbox["xmin"]), 5)),
            info_row("Ext. Este",       round(as.numeric(bbox["xmax"]), 5)),
            info_row("Ext. Sur",        round(as.numeric(bbox["ymin"]), 5)),
            info_row("Ext. Norte",      round(as.numeric(bbox["ymax"]), 5))
          )
        ),

        # ── Reproyección ─────────────────────────────────────
        div(
          class = "mb-3",
          h6(class = "fw-bold small text-muted text-uppercase",
             icon("rotate"), " Reproyectar"),
          div(
            class = "d-flex gap-1",
            textInput(
              ns("vec_epsg"), NULL,
              placeholder = "EPSG (ej: 4326)",
              width = "100%"
            ),
            actionButton(
              ns("btn_reproyectar"), NULL,
              icon  = icon("rotate"),
              class = "btn-sm btn-outline-secondary"
            )
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
        div(
          class = "d-flex gap-2",
          downloadButton(
            ns("descargar_geojson"), "GeoJSON",
            class = "btn-sm btn-outline-primary flex-grow-1"
          ),
          downloadButton(
            ns("descargar_gpkg"), "GeoPackage",
            class = "btn-sm btn-outline-primary flex-grow-1"
          )
        )
      )
    })

    # ── Reproyectar ──────────────────────────────────────────
    observeEvent(input$btn_reproyectar, {
      req(input$capa_resumen_sel, shared$capas_vec)
      epsg <- suppressWarnings(as.integer(trimws(input$vec_epsg)))
      if (is.na(epsg)) {
        notify_err("EPSG inválido — debe ser un número.")
        return()
      }
      tryCatch({
        capas    <- shared$capas_vec
        id_capa  <- input$capa_resumen_sel
        capas[[id_capa]]$sf <- sf::st_transform(capas[[id_capa]]$sf, epsg)
        shared$capas_vec    <- capas
        notify_ok(paste0("Reproyectado a EPSG:", epsg))
      }, error = function(e) {
        notify_err(paste("Error:", e$message))
      })
    })

    output$crs_feedback <- renderUI({
      req(input$capa_resumen_sel, shared$capas_vec)
      capa <- shared$capas_vec[[input$capa_resumen_sel]]
      if (is.null(capa)) return(NULL)
      crs <- sf::st_crs(capa$sf)$Name %||% "Sin CRS"
      div(class = "small text-success mt-1",
          icon("globe"), paste0(" CRS actual: ", crs))
    })

    # ── Descargas ────────────────────────────────────────────
    output$descargar_geojson <- downloadHandler(
      filename = function() {
        capa <- shared$capas_vec[[input$capa_resumen_sel]]
        paste0(capa$nombre %||% "vectorial", "_", Sys.Date(), ".geojson")
      },
      content = function(file) {
        req(input$capa_resumen_sel)
        capa   <- shared$capas_vec[[input$capa_resumen_sel]]
        sf_obj <- sf::st_transform(capa$sf, 4326)
        sf::st_write(sf_obj, file, quiet = TRUE, delete_dsn = TRUE)
      }
    )

    output$descargar_gpkg <- downloadHandler(
      filename = function() {
        capa <- shared$capas_vec[[input$capa_resumen_sel]]
        paste0(capa$nombre %||% "vectorial", "_", Sys.Date(), ".gpkg")
      },
      content = function(file) {
        req(input$capa_resumen_sel)
        capa <- shared$capas_vec[[input$capa_resumen_sel]]
        sf::st_write(capa$sf, file, quiet = TRUE, delete_dsn = TRUE)
      }
    )

    # ── Mapa preview ─────────────────────────────────────────
    output$mapa_preview <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -84, lat = 9.7, zoom = 7)
    })

    observe({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) return()

      m <- leafletProxy(ns("mapa_preview")) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls()

      grupos <- c()
      lapply(names(capas), function(id_capa) {
        capa <- capas[[id_capa]]
        tryCatch({
          sf_obj <- sf::st_transform(capa$sf, 4326)
          gt     <- toupper(as.character(
            sf::st_geometry_type(sf_obj, by_geometry = FALSE)))

          m <<- if (grepl("POINT|MULTIPOINT", gt)) {
            m %>% addCircleMarkers(
              data        = sf_obj,
              group       = capa$nombre,
              radius      = 5,
              stroke      = FALSE,
              fillColor   = capa$color,
              fillOpacity = 0.7
            )
          } else if (grepl("LINE", gt)) {
            m %>% addPolylines(
              data    = sf_obj,
              group   = capa$nombre,
              color   = capa$color,
              opacity = 0.9,
              weight  = 2
            )
          } else {
            m %>% addPolygons(
              data        = sf_obj,
              group       = capa$nombre,
              fillColor   = capa$color,
              stroke      = TRUE,
              color       = "white",
              weight      = 1.5,
              opacity     = 1,
              fillOpacity = 0.7
            )
          }
          grupos <<- c(grupos, capa$nombre)
        }, error = function(e) NULL)
      })

      if (length(grupos) > 0) {
        m <- m %>% addLayersControl(
          overlayGroups = grupos,
          options       = layersControlOptions(collapsed = FALSE)
        )
        # Zoom a todas las capas
        bb <- tryCatch({
          all_sf <- do.call(rbind, lapply(capas, function(c)
            sf::st_transform(c$sf, 4326)))
          sf::st_bbox(all_sf)
        }, error = function(e) NULL)
        if (!is.null(bb))
          m <- m %>% fitBounds(
            as.numeric(bb["xmin"]), as.numeric(bb["ymin"]),
            as.numeric(bb["xmax"]), as.numeric(bb["ymax"])
          )
      }

      m %>% addScaleBar(position = "bottomleft")
    })

  })
}
