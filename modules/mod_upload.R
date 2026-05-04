# ============================================================
# mod_upload.R — Módulo de carga de datos (v2)
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


# ── UI ──────────────────────────────────────────────────────
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # ── Fila superior: vectorial + raster ────────────────────
    layout_columns(
      col_widths = c(6, 6),

      # ── Panel vectorial ────────────────────────────────────
      card(
        card_header(
          class = "d-flex align-items-center gap-2 text-white",
          style = paste0("background:", colores$primario, ";"),
          icon("draw-polygon"), "Datos Vectoriales / Tabulares"
        ),
        card_body(
          fileInput(
            ns("vec_file"),
            label       = "Subir archivo:",
            accept      = c(".geojson", ".json", ".gpkg",
                            ".zip",     ".kml",  ".kmz",
                            ".csv",     ".tsv",  ".xlsx", ".xls"),
            buttonLabel = "Examinar\u2026",
            placeholder = "GeoJSON \u00b7 GPKG \u00b7 SHP.zip \u00b7 CSV \u00b7 Excel"
          ),

          # Hint dinamico segun tipo de archivo
          uiOutput(ns("vec_hint")),

          # ── Selector de capa (GeoPackage con multiples capas)
          uiOutput(ns("layer_selector_ui")),

          # ── Configuracion lon/lat (CSV / Excel) ─────────────
          uiOutput(ns("coord_ui")),

          hr(),
          uiOutput(ns("vec_summary"))
        )
      ),

      # ── Panel raster ───────────────────────────────────────
      card(
        card_header(
          class = "d-flex align-items-center gap-2 text-white",
          style = paste0("background:", colores$acento, ";"),
          icon("layer-group"), "Datos Raster"
        ),
        card_body(
          fileInput(
            ns("rst_file"),
            label       = "Subir archivo raster:",
            accept      = c(".tif", ".tiff", ".nc", ".img"),
            buttonLabel = "Examinar\u2026",
            placeholder = "GeoTIFF \u00b7 NetCDF \u00b7 ERDAS .img"
          ),
          helpText(icon("info-circle"),
                   "GeoTIFF (.tif), NetCDF (.nc), ERDAS Imagine (.img)"),
          hr(),
          uiOutput(ns("rst_summary"))
        )
      )
    ),

    # ── Panel de estado global ─────────────────────────────
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
            style = paste0("background:", colores$fondo, "; border-left:4px solid ", colores$primario, ";"),
            h6(class = "text-success fw-bold mb-2",
               icon("draw-polygon"), " Vector / Puntos"),
            uiOutput(ns("vec_status"))
          ),
          div(
            class = "p-3 rounded",
            style = paste0("background:", colores$fondo, "; border-left:4px solid ", colores$acento, ";"),
            h6(class = "text-primary fw-bold mb-2",
               icon("layer-group"), " Raster"),
            uiOutput(ns("rst_status"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_upload_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Estado interno ───────────────────────────────────────
    rv <- reactiveValues(
      raw_df      = NULL,   # data.frame crudo de CSV/Excel
      file_type   = NULL,   # "geo" | "tabular"
      gpkg_layers = NULL,   # capas disponibles en GPKG
      gpkg_path   = NULL    # ruta temporal del GPKG
    )

    # ── Extension limpia ────────────────────────────────────
    file_ext_clean <- reactive({
      req(input$vec_file)
      tolower(tools::file_ext(input$vec_file$name))
    })

    # ── Hint dinamico ────────────────────────────────────────
    output$vec_hint <- renderUI({
      req(input$vec_file)
      ext <- file_ext_clean()
      msg <- switch(ext,
                    "zip"  = "Shapefile detectado: se extraera el .shp y sus componentes.",
                    "gpkg" = "GeoPackage detectado. Se listaran las capas disponibles.",
                    "csv"  = "CSV detectado. Selecciona las columnas de coordenadas.",
                    "tsv"  = "TSV detectado. Selecciona las columnas de coordenadas.",
                    "xlsx" = "Excel detectado. Selecciona las columnas de coordenadas.",
                    "xls"  = "Excel detectado. Selecciona las columnas de coordenadas.",
                    "kmz"  = "KMZ detectado (KML comprimido).",
                    NULL
      )
      if (!is.null(msg))
        div(class = "text-muted small mt-1 mb-2",
            icon("info-circle"), " ", msg)
    })

    # ── Selector de capa GPKG (dinamico) ─────────────────────
    output$layer_selector_ui <- renderUI({
      req(rv$gpkg_layers, length(rv$gpkg_layers) > 1)
      tagList(
        div(class = "alert alert-info py-2 px-3 small",
            icon("layer-group"),
            paste0(" El GPKG tiene ", length(rv$gpkg_layers),
                   " capas. Selecciona una:")),
        selectInput(ns("gpkg_layer"), NULL,
                    choices  = rv$gpkg_layers,
                    selected = rv$gpkg_layers[1])
      )
    })

    # ── UI de coordenadas para CSV/Excel ─────────────────────
    output$coord_ui <- renderUI({
      req(rv$raw_df)
      cols <- names(rv$raw_df)

      # Heuristica para sugerir columnas
      lon_guess <- cols[grepl("^(lon|long|longitude|x|easting)",
                              cols, ignore.case = TRUE)][1]
      lat_guess <- cols[grepl("^(lat|latitude|y|northing)",
                              cols, ignore.case = TRUE)][1]

      tagList(
        div(class = "alert alert-warning py-2 px-3 small",
            icon("map-pin"),
            " Selecciona las columnas de coordenadas:"),

        layout_columns(
          col_widths = c(6, 6),
          selectInput(ns("lon_col"), "Longitud (X):",
                      choices  = cols,
                      selected = if (!is.na(lon_guess %||% NA)) lon_guess else cols[1]),
          selectInput(ns("lat_col"), "Latitud (Y):",
                      choices  = cols,
                      selected = if (!is.na(lat_guess %||% NA)) lat_guess else
                        if (length(cols) >= 2) cols[2] else cols[1])
        ),

        selectInput(ns("csv_crs"), "CRS de los puntos:",
                    choices = c(
                      "WGS84 \u2014 EPSG:4326"    = "4326",
                      "CRTM05 \u2014 EPSG:5367"   = "5367",
                      "Mercator \u2014 EPSG:3857"  = "3857",
                      "NAD83 \u2014 EPSG:4269"     = "4269"
                    )),

        # Vista previa
        div(
          class = "mt-2",
          style = "max-height:160px; overflow-y:auto; font-size:0.78rem;",
          h6(class = "text-muted small fw-bold mb-1",
             "Vista previa (primeras 5 filas):"),
          tableOutput(ns("csv_preview"))
        ),

        actionButton(ns("confirm_coords"),
                     "Confirmar y crear puntos espaciales",
                     class = "btn-success btn-sm w-100 mt-2",
                     icon  = icon("check"))
      )
    })

    # ── Vista previa CSV ─────────────────────────────────────
    output$csv_preview <- renderTable({
      req(rv$raw_df)
      head(rv$raw_df, 5)
    }, striped = TRUE, bordered = TRUE, small = TRUE, rownames = FALSE)

    # ── Leer archivo al subirlo ───────────────────────────────
    observeEvent(input$vec_file, {
      req(input$vec_file)

      path <- input$vec_file$datapath
      ext  <- file_ext_clean()

      # Resetear
      rv$raw_df      <- NULL
      rv$gpkg_layers <- NULL
      rv$gpkg_path   <- NULL
      rv$file_type   <- NULL

      # ── CSV / TSV ──────────────────────────────────────────
      if (ext %in% c("csv", "tsv")) {
        tryCatch({
          delim <- if (ext == "tsv") "\t" else ","
          df <- readr::read_delim(path, delim = delim,
                                  show_col_types = FALSE,
                                  locale = readr::locale(encoding = "UTF-8"))
          rv$raw_df    <- as.data.frame(df)
          rv$file_type <- "tabular"
          showNotification(
            paste0("CSV leido: ", nrow(rv$raw_df), " filas, ",
                   ncol(rv$raw_df), " columnas"),
            type = "message"
          )
        }, error = function(e) {
          showNotification(paste("Error leyendo CSV:", e$message), type = "error")
        })
        return()
      }

      # ── Excel ──────────────────────────────────────────────
      if (ext %in% c("xlsx", "xls")) {
        tryCatch({
          tmp_xl <- paste0(path, ".", ext)
          file.copy(path, tmp_xl, overwrite = TRUE)
          sheets <- readxl::excel_sheets(tmp_xl)
          df     <- readxl::read_excel(tmp_xl, sheet = 1)
          if (length(sheets) > 1)
            showNotification(
              paste0("Excel con ", length(sheets),
                     " hojas. Usando hoja 1: '", sheets[1], "'"),
              type = "warning", duration = 6
            )
          rv$raw_df    <- as.data.frame(df)
          rv$file_type <- "tabular"
          showNotification(
            paste0("Excel leido: ", nrow(rv$raw_df), " filas, ",
                   ncol(rv$raw_df), " columnas"),
            type = "message"
          )
        }, error = function(e) {
          showNotification(paste("Error leyendo Excel:", e$message), type = "error")
        })
        return()
      }

      # ── GeoPackage ─────────────────────────────────────────
      if (ext == "gpkg") {
        tryCatch({
          # Copiar a tmp con extension correcta
          tmp_gpkg <- paste0(path, ".gpkg")
          file.copy(path, tmp_gpkg, overwrite = TRUE)
          layers <- sf::st_layers(tmp_gpkg)$name

          if (length(layers) == 0) stop("El GPKG no contiene capas.")

          rv$gpkg_layers <- layers
          rv$gpkg_path   <- tmp_gpkg

          if (length(layers) == 1) {
            load_sf_from_path(tmp_gpkg)
          }
          # Si >1 capa: layer_selector_ui se activa via rv$gpkg_layers
        }, error = function(e) {
          showNotification(paste("Error en GeoPackage:", e$message), type = "error")
        })
        return()
      }

      # ── Shapefile .zip ─────────────────────────────────────
      if (ext == "zip") {
        tryCatch({
          tmp_dir <- file.path(tempdir(),
                               paste0("shp_", as.integer(Sys.time())))
          dir.create(tmp_dir, showWarnings = FALSE)
          unzip(path, exdir = tmp_dir)

          shp_files <- list.files(tmp_dir, pattern = "\\.shp$",
                                  full.names = TRUE, recursive = TRUE)
          if (length(shp_files) == 0)
            stop("No se encontro ningun .shp dentro del zip.")
          if (length(shp_files) > 1)
            showNotification(
              paste0("Multiples .shp: usando '",
                     basename(shp_files[1]), "'"),
              type = "warning"
            )

          base  <- tools::file_path_sans_ext(shp_files[1])
          falta <- c(".dbf", ".shx")[
            !file.exists(paste0(base, c(".dbf", ".shx")))]
          if (length(falta) > 0)
            showNotification(
              paste0("Faltan componentes: ",
                     paste(falta, collapse = ", ")),
              type = "warning"
            )

          load_sf_from_path(shp_files[1])
        }, error = function(e) {
          showNotification(paste("Error en Shapefile:", e$message), type = "error")
        })
        return()
      }

      # ── KMZ (zip con .kml) ─────────────────────────────────
      if (ext == "kmz") {
        tryCatch({
          tmp_dir <- file.path(tempdir(),
                               paste0("kmz_", as.integer(Sys.time())))
          dir.create(tmp_dir, showWarnings = FALSE)
          unzip(path, exdir = tmp_dir)
          kml_f <- list.files(tmp_dir, pattern = "\\.kml$",
                              full.names = TRUE, recursive = TRUE)[1]
          if (is.na(kml_f)) stop("No se encontro .kml dentro del KMZ.")
          load_sf_from_path(kml_f)
        }, error = function(e) {
          showNotification(paste("Error en KMZ:", e$message), type = "error")
        })
        return()
      }

      # ── GeoJSON, KML y otros reconocidos por sf ────────────
      tryCatch({
        load_sf_from_path(path)
      }, error = function(e) {
        showNotification(paste("Error al cargar:", e$message), type = "error")
      })
    })

    # ── Cambio de capa en GPKG ────────────────────────────────
    observeEvent(input$gpkg_layer, {
      req(rv$gpkg_path, input$gpkg_layer)
      load_sf_from_path(rv$gpkg_path, layer = input$gpkg_layer)
    }, ignoreInit = TRUE)

    # ── Confirmar coordenadas CSV/Excel ───────────────────────
    observeEvent(input$confirm_coords, {
      req(rv$raw_df, input$lon_col, input$lat_col)

      tryCatch({
        df    <- rv$raw_df
        lon_c <- input$lon_col
        lat_c <- input$lat_col
        epsg  <- as.integer(input$csv_crs)

        for (col in c(lon_c, lat_c)) {
          if (!col %in% names(df))
            stop(paste("Columna no encontrada:", col))
          if (!is.numeric(df[[col]])) {
            df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
            if (all(is.na(df[[col]])))
              stop(paste("La columna", col,
                         "no puede convertirse a numerico."))
          }
        }

        n_antes <- nrow(df)
        df <- df[!is.na(df[[lon_c]]) & !is.na(df[[lat_c]]), ]
        n_drop  <- n_antes - nrow(df)

        sf_obj <- sf::st_as_sf(df,
                               coords = c(lon_c, lat_c),
                               crs    = epsg,
                               remove = FALSE)

        shared$sf_data  <- sf_obj
        shared$crs_info <- sf::st_crs(sf_obj)$input

        msg <- paste0(nrow(sf_obj), " puntos creados")
        if (n_drop > 0)
          msg <- paste0(msg, " (", n_drop, " filas con NA ignoradas)")
        showNotification(paste0("Conversion exitosa: ", msg),
                         type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Error al crear puntos:", e$message),
                         type = "error")
      })
    })

    # ── Carga raster ─────────────────────────────────────────
    observeEvent(input$rst_file, {
      req(input$rst_file)
      tryCatch({
        r <- terra::rast(input$rst_file$datapath)
        shared$raster_data <- r
        showNotification("Raster cargado correctamente.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error al cargar raster:", e$message),
                         type = "error")
      })
    })

    # ── Resumen vectorial ─────────────────────────────────────
    output$vec_summary <- renderUI({
      req(shared$sf_data)
      sf_obj <- shared$sf_data
      tagList(
        h6(class = "text-muted small fw-bold text-uppercase mt-1",
           "Resumen"),
        tags$table(
          class = "table table-sm table-striped small mb-0",
          tags$tbody(
            info_row("Geometria",
                     class(sf::st_geometry(sf_obj))[1]),
            info_row("Features",
                     format(nrow(sf_obj), big.mark = ",")),
            info_row("Atributos",  ncol(sf_obj) - 1),
            info_row("CRS",
                     sf::st_crs(sf_obj)$input %||% "Sin CRS"),
            info_row("Bbox X",
                     paste(round(sf::st_bbox(sf_obj)[c(1,3)], 4),
                           collapse = " \u2192 ")),
            info_row("Bbox Y",
                     paste(round(sf::st_bbox(sf_obj)[c(2,4)], 4),
                           collapse = " \u2192 "))
          )
        )
      )
    })

    # ── Resumen raster ────────────────────────────────────────
    output$rst_summary <- renderUI({
      req(shared$raster_data)
      r <- shared$raster_data
      tagList(
        h6(class = "text-muted small fw-bold text-uppercase mt-1",
           "Resumen"),
        tags$table(
          class = "table table-sm table-striped small mb-0",
          tags$tbody(
            info_row("Bandas",     terra::nlyr(r)),
            info_row("Filas",      terra::nrow(r)),
            info_row("Columnas",   terra::ncol(r)),
            info_row("Resolucion",
                     paste(round(terra::res(r), 6), collapse = " \u00d7 ")),
            info_row("CRS",
                     terra::crs(r, describe = TRUE)$name %||% "Sin CRS"),
            info_row("Valores NA",
                     sum(is.na(terra::values(r[[1]]))))
          )
        )
      )
    })

    # ── Badges de estado ──────────────────────────────────────
    output$vec_status <- renderUI({
      if (is.null(shared$sf_data)) {
        tags$span(class = "text-muted",
                  icon("circle-xmark"), " Sin datos")
      } else {
        sf_obj <- shared$sf_data
        geom_t <- class(sf::st_geometry(sf_obj))[1]
        tagList(
          tags$span(class = "badge bg-success me-1",
                    paste(format(nrow(sf_obj), big.mark = ","),
                          "features")),
          tags$span(class = "badge bg-secondary", geom_t),
          tags$br(),
          tags$small(class = "text-muted",
                     sf::st_crs(sf_obj)$input %||% "Sin CRS")
        )
      }
    })

    output$rst_status <- renderUI({
      if (is.null(shared$raster_data)) {
        tags$span(class = "text-muted",
                  icon("circle-xmark"), " Sin datos")
      } else {
        r <- shared$raster_data
        tagList(
          tags$span(class = "badge bg-primary me-1",
                    paste(terra::nlyr(r), "bandas")),
          tags$span(class = "badge bg-secondary",
                    paste(terra::nrow(r), "\u00d7", terra::ncol(r)))
        )
      }
    })

    # ── Helper: cargar sf desde ruta ─────────────────────────
    load_sf_from_path <- function(path, layer = NULL, quiet = TRUE) {
      args <- list(dsn = path, quiet = quiet)
      if (!is.null(layer)) args$layer <- layer

      sf_obj <- do.call(sf::st_read, args)

      if (is.na(sf::st_crs(sf_obj))) {
        showNotification(
          "Sin CRS definido \u2014 se asigna WGS84 (EPSG:4326).",
          type = "warning"
        )
        sf_obj <- sf::st_set_crs(sf_obj, 4326)
      }

      shared$sf_data  <- sf_obj
      shared$crs_info <- sf::st_crs(sf_obj)$input
      rv$file_type    <- "geo"

      lbl <- if (!is.null(layer)) paste0(" (capa: ", layer, ")") else ""
      showNotification(
        paste0(nrow(sf_obj), " features cargados",
               lbl, " \u2014 ",
               class(sf::st_geometry(sf_obj))[1]),
        type = "message"
      )
    }

  })
}
