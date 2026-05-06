# ============================================================
# mod_upload.R — Módulo de carga de datos (v3)
#
# Vectoriales:
#   • GeoJSON / JSON
#   • GeoPackage (.gpkg)  — con selector de capa si hay varias
#   • Shapefile (.zip con .shp + componentes)
#   • KML / KMZ
#   • CSV / TSV  — con selector de columnas lon/lat
#   • Excel (.xlsx / .xls) — con selector de columnas lon/lat
#
# Raster:
#   • GeoTIFF (.tif / .tiff)
#   • NetCDF (.nc)
#   • ERDAS Imagine (.img)
# ============================================================

library(sf)
library(terra)
library(readr)
library(readxl)
library(dplyr)

# ── UI ──────────────────────────────────────────────────────
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # ── Fila superior: vectorial + raster ────────────────────
    layout_columns(
      col_widths = c(6, 6),

      # ── Panel vectorial ──────────────────────────────────────
      card(
        card_header(
          class = "d-flex align-items-center gap-2 text-white",
          style = paste0("background:", colores$primario, ";"),
          icon("draw-polygon"), "Datos Vectoriales / Tabulares"
        ),
        card_body(
          # ── Descripción pedagógica ─────────────────────────────
          div(
            class = "mb-3 p-2 rounded small",
            style = paste0("background:", colores$fondo,
                           "; border-left: 3px solid ", colores$primario, ";"),
            tags$p(class = "mb-1 fw-bold",
                   "¿Qué son los datos vectoriales?"),
            tags$p(class = "mb-0 text-muted",
                   "Representan objetos geográficos discretos mediante ",
                   tags$strong("puntos"), ", ",
                   tags$strong("líneas"), " o ",
                   tags$strong("polígonos"), ". ",
                   "Cada objeto puede tener atributos asociados (nombre, área, población…). ",
                   "Ejemplos: estaciones de monitoreo, ríos, límites administrativos.")
          ),

          fileInput(
            ns("vec_file"),
            label       = "Subir archivo:",
            accept      = c(".geojson", ".json", ".gpkg",
                            ".zip",     ".kml",  ".kmz",
                            ".csv",     ".tsv",  ".xlsx", ".xls"),
            buttonLabel = "Examinar\u2026",
            placeholder = "GeoJSON \u00b7 GPKG \u00b7 SHP.zip \u00b7 CSV \u00b7 Excel"
          ),

          # ── Íconos ⓘ por formato vectorial ─────────────────
          div(
            class = "d-flex flex-wrap gap-2 mt-1 mb-2",

            # GeoJSON
            tags$span(
              title = "GeoJSON (.geojson, .json): formato de texto estándar para datos vectoriales. Un único archivo.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " GeoJSON"
            ),
            # GeoPackage
            tags$span(
              title = "GeoPackage (.gpkg): base de datos espacial compacta. Puede contener varias capas.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " GPKG"
            ),
            # Shapefile
            tags$span(
              title = "Shapefile (.zip): comprime los archivos .shp, .dbf, .shx y .prj en un único .zip antes de subir.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " Shapefile"
            ),
            # KML / KMZ
            tags$span(
              title = "KML / KMZ (.kml, .kmz): formato de Google Earth para datos vectoriales con estilos.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " KML/KMZ"
            ),
            # CSV / Excel
            tags$span(
              title = "CSV / Excel (.csv, .tsv, .xlsx, .xls): tabla con columnas de longitud y latitud. Se seleccionan abajo.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " CSV/Excel"
            )
          ),

          # ── Hint dinámico según tipo de archivo ─────────────
          uiOutput(ns("vec_hint")),

          # ── Selector de capa (GeoPackage con múltiples capas)
          uiOutput(ns("layer_selector_ui")),

          # ── Configuración lon/lat (CSV / Excel) ─────────────
          uiOutput(ns("coord_ui")),

          hr(),
          uiOutput(ns("vec_summary"))
        )
      ),

      # ── Panel raster ──────────────────────────────────────────
      card(
        card_header(
          class = "d-flex align-items-center gap-2 text-white",
          style = paste0("background:", colores$acento, ";"),
          icon("layer-group"), "Datos Raster"
        ),
        card_body(
          # ── Descripción pedagógica ─────────────────────────────
          div(
            class = "mb-3 p-2 rounded small",
            style = paste0("background:", colores$fondo,
                           "; border-left: 3px solid ", colores$acento, ";"),
            tags$p(class = "mb-1 fw-bold",
                   "¿Qué son los datos raster?"),
            tags$p(class = "mb-0 text-muted",
                   "Representan variables continuas mediante una ",
                   tags$strong("cuadrícula de celdas (píxeles)"), ", ",
                   "donde cada celda tiene un valor numérico. ",
                   "Son ideales para fenómenos que varían de forma continua en el espacio. ",
                   "Ejemplos: temperatura, elevación, índices de vegetación (NDVI), precipitación.")
          ),

          fileInput(
            ns("rst_file"),
            label       = "Subir archivo:",
            accept      = c(".tif", ".tiff", ".nc", ".img"),
            buttonLabel = "Examinar\u2026",
            # Placeholder más corto
            placeholder = "GeoTIFF \u00b7 NetCDF \u00b7 .img"
          ),

          # ── Íconos ⓘ por formato raster ──────────────────────
          div(
            class = "d-flex flex-wrap gap-2 mt-1 mb-2",

            # GeoTIFF
            tags$span(
              title = "GeoTIFF (.tif, .tiff): formato raster estándar con información georreferenciada incrustada. El más recomendado.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " GeoTIFF"
            ),
            # NetCDF
            tags$span(
              title = "NetCDF (.nc): formato científico multidimensional. Común en datos climáticos, oceanográficos y atmosféricos.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " NetCDF"
            ),
            # ERDAS Imagine
            tags$span(
              title = "ERDAS Imagine (.img): formato propietario de teledetección. Compatible con imágenes de satélite y sensores remotos.",
              `data-bs-toggle` = "tooltip", `data-bs-placement` = "top",
              class = "badge rounded-pill text-bg-secondary",
              icon("circle-info"), " ERDAS .img"
            )
          ),

          # ── Hint dinámico según tipo raster ──────────────────
          uiOutput(ns("rst_hint")),

          hr(),
          uiOutput(ns("rst_summary"))
        )
      )
    ),

    # ── Panel de estado global ──────────────────────────────────
    card(
      card_header(
        class = "d-flex align-items-center gap-2",
        icon("database"), "Estado de los datos cargados"
      ),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          div(
            class = "p-3 rounded",
            style = paste0("background:#EFF6FF; border-left:4px solid ", colores$primario, ";"),
            uiOutput(ns("vec_status_header")),
            uiOutput(ns("vec_status"))
          ),
          div(
            class = "p-3 rounded",
            style = paste0("background:#FFF7ED; border-left:4px solid ", colores$acento, ";"),
            h6(class = "fw-bold mb-2", style = paste0("color:", colores$acento, ";"),
               icon("layer-group"), " Raster"),
            uiOutput(ns("rst_status"))
          )
        )
      )
    ),

    # ── Script para activar tooltips Bootstrap ──────────────────
    tags$script(HTML("
      $(function () {
        var tooltipEls = document.querySelectorAll('[data-bs-toggle=\"tooltip\"]');
        tooltipEls.forEach(function(el) {
          new bootstrap.Tooltip(el, { html: false });
        });
      });
    "))
  )
}

# ── Server ──────────────────────────────────────────────────
mod_upload_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Estado interno ───────────────────────────────────────
    rv <- reactiveValues(
      raw_df       = NULL,
      raw_path     = NULL,
      file_type    = NULL,
      gpkg_layers  = NULL
    )

    # ── Detectar extensión del archivo vectorial ─────────────
    file_ext_clean <- reactive({
      req(input$vec_file)
      tolower(tools::file_ext(input$vec_file$name))
    })

    # Helpers seguros: devuelven FALSE si no hay archivo todavía
    is_tabular <- reactive({
      if (is.null(input$vec_file)) return(FALSE)
      isTRUE(file_ext_clean() %in% c("csv", "tsv", "xlsx", "xls"))
    })

    is_gpkg_multi <- reactive({
      isTRUE(!is.null(rv$gpkg_layers) && length(rv$gpkg_layers) > 1)
    })

    output$show_coord_ui       <- reactive({ isTRUE(is_tabular()) })
    output$show_layer_selector <- reactive({ isTRUE(is_gpkg_multi()) })
    outputOptions(output, "show_coord_ui",       suspendWhenHidden = FALSE)
    outputOptions(output, "show_layer_selector", suspendWhenHidden = FALSE)

    # ── Hint dinámico vectorial ──────────────────────────────
    output$vec_hint <- renderUI({
      req(input$vec_file)
      ext <- file_ext_clean()
      msg <- switch(ext,
                    "zip"   = "Shapefile detectado: se extraerá el .shp y sus componentes.",
                    "gpkg"  = "GeoPackage detectado. Se leerán las capas disponibles.",
                    "csv"   = "CSV detectado. Selecciona las columnas de coordenadas abajo.",
                    "tsv"   = "TSV detectado. Selecciona las columnas de coordenadas abajo.",
                    "xlsx"  = "Excel detectado. Selecciona las columnas de coordenadas abajo.",
                    "xls"   = "Excel detectado. Selecciona las columnas de coordenadas abajo.",
                    "kml"   = "KML detectado.",
                    "kmz"   = "KMZ detectado.",
                    "geojson" = "GeoJSON detectado.",
                    "json"    = "JSON detectado.",
                    NULL
      )
      if (!is.null(msg)) {
        div(
          class = "alert alert-success py-2 px-3 small mt-1",
          icon("check-circle"), " ", msg
        )
      }
    })

    # ── Hint dinámico raster ─────────────────────────────────
    output$rst_hint <- renderUI({
      req(input$rst_file)
      ext <- tolower(tools::file_ext(input$rst_file$name))
      msg <- switch(ext,
                    "tif"   = ,
                    "tiff"  = "GeoTIFF detectado. Formato recomendado para datos raster georreferenciados.",
                    "nc"    = "NetCDF detectado. Si el archivo contiene varias variables o tiempos, se usará la primera disponible.",
                    "img"   = "ERDAS Imagine detectado. Formato de teledetección compatible.",
                    NULL
      )
      if (!is.null(msg)) {
        div(
          class = "alert alert-primary py-2 px-3 small mt-1",
          icon("check-circle"), " ", msg
        )
      }
    })

    # ── Selector de capa para GeoPackage ────────────────────
    output$layer_selector_ui <- renderUI({
      req(is_gpkg_multi())
      div(
        class = "alert alert-info py-2 px-3 small mt-2",
        icon("layer-group"),
        " El archivo contiene varias capas.",
        selectInput(
          ns("gpkg_layer"),
          label   = "Seleccionar capa:",
          choices = rv$gpkg_layers
        )
      )
    })

    # ── Selector de coordenadas para CSV / Excel ─────────────
    output$coord_ui <- renderUI({
      req(is_tabular(), !is.null(rv$raw_df))
      cols <- names(rv$raw_df)
      layout_columns(
        col_widths = c(6, 6),
        selectInput(ns("lon_col"), "Columna Longitud (X):",
                    choices = cols,
                    selected = cols[grepl("^(lon|lng|x|longitud)$", cols, ignore.case = TRUE)][1]),
        selectInput(ns("lat_col"), "Columna Latitud (Y):",
                    choices = cols,
                    selected = cols[grepl("^(lat|y|latitud)$", cols, ignore.case = TRUE)][1])
      )
    })

    # ── Cargar archivo vectorial ─────────────────────────────
    observeEvent(input$vec_file, {
      req(input$vec_file)
      path <- input$vec_file$datapath
      ext  <- file_ext_clean()

      tryCatch({
        sf_obj <- switch(ext,

                         # Shapefile en ZIP
                         "zip" = {
                           tmp <- tempdir()
                           utils::unzip(path, exdir = tmp)
                           shp <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE,
                                             recursive = TRUE)[1]
                           sf::read_sf(shp)
                         },

                         # GeoPackage (puede tener varias capas)
                         "gpkg" = {
                           layers <- sf::st_layers(path)$name
                           rv$gpkg_layers <- layers
                           if (length(layers) == 1) {
                             sf::read_sf(path, layer = layers[1])
                           } else {
                             NULL   # se leerá cuando el usuario elija capa
                           }
                         },

                         # KMZ: descomprimir primero
                         "kmz" = {
                           tmp <- tempdir()
                           utils::unzip(path, exdir = tmp)
                           kml <- list.files(tmp, pattern = "\\.kml$", full.names = TRUE,
                                             recursive = TRUE)[1]
                           sf::read_sf(kml)
                         },

                         # Tabulares: guardar raw_df, la conversión ocurre al elegir coords
                         "csv" = {
                           rv$raw_df   <- readr::read_csv(path, show_col_types = FALSE)
                           rv$raw_path <- path
                           rv$file_type <- "tabular"
                           NULL
                         },
                         "tsv" = {
                           rv$raw_df   <- readr::read_tsv(path, show_col_types = FALSE)
                           rv$raw_path <- path
                           rv$file_type <- "tabular"
                           NULL
                         },
                         "xlsx" = {
                           rv$raw_df   <- readxl::read_excel(path)
                           rv$raw_path <- path
                           rv$file_type <- "tabular"
                           NULL
                         },
                         "xls" = {
                           rv$raw_df   <- readxl::read_excel(path)
                           rv$raw_path <- path
                           rv$file_type <- "tabular"
                           NULL
                         },

                         # GeoJSON, JSON, KML, etc.
                         sf::read_sf(path)
        )

        if (!is.null(sf_obj)) {
          sf_obj         <- ensure_crs(sf_obj)
          shared$sf_data <- sf_obj
          notify_ok("Archivo vectorial cargado correctamente.")
        }

      }, error = function(e) {
        notify_err(paste("Error al leer el archivo:", e$message))
      })
    })

    # ── Leer capa elegida en GeoPackage ─────────────────────
    observeEvent(input$gpkg_layer, {
      req(input$vec_file, input$gpkg_layer)
      tryCatch({
        sf_obj         <- sf::read_sf(input$vec_file$datapath,
                                      layer = input$gpkg_layer)
        sf_obj         <- ensure_crs(sf_obj)
        shared$sf_data <- sf_obj
        notify_ok(paste("Capa cargada:", input$gpkg_layer))
      }, error = function(e) {
        notify_err(paste("Error al leer la capa:", e$message))
      })
    })

    # ── Convertir tabla a sf al elegir coordenadas ───────────
    observeEvent(list(input$lon_col, input$lat_col), {
      req(is_tabular(), !is.null(rv$raw_df),
          !is.null(input$lon_col), !is.null(input$lat_col))
      tryCatch({
        df     <- rv$raw_df
        sf_obj <- sf::st_as_sf(
          df,
          coords = c(input$lon_col, input$lat_col),
          crs    = 4326,
          remove = FALSE
        )
        shared$sf_data <- sf_obj
        notify_ok("Puntos creados desde coordenadas.")
      }, error = function(e) {
        notify_err(paste("Error al convertir coordenadas:", e$message))
      })
    })

    # ── Cargar archivo raster ────────────────────────────────
    observeEvent(input$rst_file, {
      req(input$rst_file)
      tryCatch({
        rst              <- terra::rast(input$rst_file$datapath)
        shared$raster_data <- rst
        notify_ok("Archivo raster cargado correctamente.")
      }, error = function(e) {
        notify_err(paste("Error al leer el raster:", e$message))
      })
    })

    # ── Resumen vectorial ────────────────────────────────────
    output$vec_summary <- renderUI({
      req(shared$sf_data)
      sf_obj <- shared$sf_data
      crs    <- sf::st_crs(sf_obj)
      tags$table(
        class = "table table-sm table-borderless small mb-0",
        info_row("Filas",      nrow(sf_obj)),
        info_row("Columnas",   ncol(sf_obj) - 1),
        info_row("Geometría",  as.character(sf::st_geometry_type(sf_obj,
                                                                 by_geometry = FALSE))),
        info_row("CRS",        if (!is.na(crs)) crs$Name else "Sin CRS")
      )
    })

    # ── Resumen raster ───────────────────────────────────────
    output$rst_summary <- renderUI({
      req(shared$raster_data)
      rst <- shared$raster_data
      tags$table(
        class = "table table-sm table-borderless small mb-0",
        info_row("Capas",      terra::nlyr(rst)),
        info_row("Filas",      terra::nrow(rst)),
        info_row("Columnas",   terra::ncol(rst)),
        info_row("Resolución", paste(round(terra::res(rst), 5), collapse = " × ")),
        info_row("CRS",        terra::crs(rst, describe = TRUE)$name)
      )
    })

    # ── Header dinámico del panel vectorial ─────────────────
    output$vec_status_header <- renderUI({
      if (is.null(shared$sf_data)) {
        h6(class = "fw-bold mb-2", style = paste0("color:", colores$primario, ";"),
           icon("draw-polygon"), " Vectorial")
      } else {
        gt <- as.character(sf::st_geometry_type(shared$sf_data,
                                                by_geometry = FALSE))
        info <- switch(gt,
                       "POINT"              = ,
                       "MULTIPOINT"         = list(ic = "location-dot", label = "Puntos"),
                       "LINESTRING"         = ,
                       "MULTILINESTRING"    = list(ic = "route",        label = "L\u00edneas"),
                       "POLYGON"            = ,
                       "MULTIPOLYGON"       = list(ic = "draw-polygon", label = "Pol\u00edgonos"),
                       "GEOMETRYCOLLECTION" = list(ic = "shapes",       label = "Geometr\u00eda mixta"),
                       list(ic = "draw-polygon", label = gt)
        )
        h6(class = "fw-bold mb-2", style = paste0("color:", colores$primario, ";"),
           icon(info$ic), paste0(" ", info$label))
      }
    })

    # ── Estado (chips de estado global) ─────────────────────
    output$vec_status <- renderUI({
      if (!is.null(shared$sf_data)) {
        n <- nrow(shared$sf_data)
        g <- as.character(sf::st_geometry_type(shared$sf_data,
                                               by_geometry = FALSE))
        div(
          class = "small",
          tags$span(class = "badge me-1", style = paste0("background:", colores$primario, "; color:#fff;"), icon("check"), " Cargado"),
          paste0(n, " geometrías · ", g)
        )
      } else {
        div(class = "text-muted small", icon("circle-xmark"), " Sin datos")
      }
    })

    output$rst_status <- renderUI({
      if (!is.null(shared$raster_data)) {
        rst <- shared$raster_data
        div(
          class = "small",
          tags$span(class = "badge me-1", style = paste0("background:", colores$acento, "; color:#fff;"), icon("check"), " Cargado"),
          paste0(terra::nlyr(rst), " capa(s) · ",
                 terra::nrow(rst), "\u00d7", terra::ncol(rst))
        )
      } else {
        div(class = "text-muted small", icon("circle-xmark"), " Sin datos")
      }
    })

  })
}
