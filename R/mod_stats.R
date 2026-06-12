# ============================================================
# mod_stats.R — Módulo de estadísticas descriptivas
# Usa: easystats (report, parameters, performance, see)
#      tidyverse (dplyr, ggplot2, tidyr)
#
# Lee:
#   shared$capas_vec     — múltiples vectoriales
#   shared$rasters       — múltiples rasters
#   shared$raster_activo — raster activo
# ============================================================

# easystats (se carga con precaución — disponible si instalado)
safe_library <- function(pkg) {
  suppressPackageStartupMessages(
    tryCatch(library(pkg, character.only = TRUE), error = function(e) NULL)
  )
}
safe_library("parameters")
safe_library("report")
safe_library("see")
safe_library("correlation")
safe_library("datawizard")

# ── UI ──────────────────────────────────────────────────────
mod_stats_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Análisis",

      h6(class = "text-muted fw-bold text-uppercase small", "Fuente de datos"),

      radioButtons(
        ns("data_source"), NULL,
        choices  = c("Vectorial (atributos)" = "vector",
                     "Raster (por banda)"    = "raster"),
        selected = "vector"
      ),

      # ── Selector de capa vectorial ─────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'vector'", ns("data_source")),
        uiOutput(ns("selector_capa_vec"))
      ),

      # ── Selector de raster y banda ─────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'raster'", ns("data_source")),
        uiOutput(ns("selector_raster")),
        selectInput(ns("raster_band_stat"), "Banda:", choices = NULL)
      ),

      hr(),

      h6(class = "text-muted fw-bold text-uppercase small",
         "Estadísticas descriptivas"),

      uiOutput(ns("var_selector")),

      hr(),

      h6(class = "text-muted fw-bold text-uppercase small", "Visualización"),

      selectInput(
        ns("plot_type"), "Tipo de gráfico:",
        choices = list(
          "── Una variable ──" = list(
            "Histograma + densidad" = "hist",
            "Densidad"              = "density",
            "Boxplot"               = "box",
            "Violin + puntos"       = "violin"
          ),
          "── Dos o más variables ──" = list(
            "Dispersión (X vs Y)"      = "scatter",
            "Matriz de correlaciones"  = "corr"
          )
        )
      ),



      conditionalPanel(
        condition = sprintf("input['%s'] == 'scatter'", ns("plot_type")),
        uiOutput(ns("var2_selector"))
      ),

      actionButton(
        ns("run_analysis"), "Ejecutar análisis",
        class = "btn-success w-100 mt-2"
      )
    ),

    # ── Panel principal ──────────────────────────────────────
    navset_card_tab(

      nav_panel(
        title = tagList(icon("table-list"), " Estadísticas"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(icon("table-list"), " Descriptivas"),
            card_body(tableOutput(ns("stats_output")))
          ),
          card(
            card_header(icon("chart-area"), " Visualización"),
            card_body(plotOutput(ns("main_plot"), height = "400px"))
          )
        )
      ),

      nav_panel(
        title = tagList(icon("file-alt"), " Interpretación"),
        card(
          card_body(
            tags$style(HTML(sprintf(
              "#%s { white-space: pre-wrap; word-wrap: break-word; }",
              NS(id)("auto_report")
            ))),
            verbatimTextOutput(ns("auto_report"))
          )
        )
      ),

      nav_panel(
        title = tagList(bs_icon("code-slash"), " Código R"),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tagList(bs_icon("code-slash"), " Código R reproducible"),
            downloadButton(
              ns("descargar_script"),
              label = "Descargar .R",
              icon  = bs_icon("download"),
              class = "btn-sm btn-outline-primary"
            )
          ),
          card_body(
            p("Script que reproduce este análisis con tus datos geoespaciales.",
              class = "text-muted small mb-2"),
            verbatimTextOutput(ns("codigo_r"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_stats_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    # ── Selector de capa vectorial ───────────────────────────
    output$selector_capa_vec <- renderUI({
      capas <- shared$capas_vec
      if (is.null(capas) || length(capas) == 0) {
        return(div(
          class = "text-muted small",
          icon("circle-info"),
          " Cargá una capa en el tab Vectorial."
        ))
      }
      nombres <- sapply(capas, function(c) c$nombre)
      selectInput(
        session$ns("capa_vec_sel"),
        label   = "Capa:",
        choices = setNames(names(capas), nombres),
        width   = "100%"
      )
    })

    # ── Selector de raster ───────────────────────────────────
    output$selector_raster <- renderUI({
      rasters <- shared$rasters
      if (is.null(rasters) || length(rasters) == 0) {
        return(div(
          class = "text-muted small",
          icon("circle-info"),
          " Cargá un raster en el tab Raster."
        ))
      }
      nombres <- sapply(rasters, function(r) r$nombre)
      selectInput(
        session$ns("raster_sel"),
        label   = "Raster:",
        choices = setNames(names(rasters), nombres),
        width   = "100%"
      )
    })

    # ── Actualizar bandas al cambiar raster ──────────────────
    observeEvent(input$raster_sel, {
      req(input$raster_sel, shared$rasters)
      rst_item <- shared$rasters[[input$raster_sel]]
      if (is.null(rst_item)) return()
      n      <- terra::nlyr(rst_item$rst)
      noms   <- names(rst_item$rst)
      bandas <- setNames(seq_len(n),
                         paste0("Banda ", seq_len(n), " (", noms, ")"))
      updateSelectInput(session, "raster_band_stat",
                        choices = bandas, selected = 1)
    })

    # También actualizar si cambia la lista de rasters
    observeEvent(shared$rasters, {
      req(input$data_source == "raster")
      rasters <- shared$rasters
      if (is.null(rasters) || length(rasters) == 0) return()
      # Usar el raster activo como default si está disponible
      sel <- shared$raster_activo %||% names(rasters)[1]
      if (!is.null(sel) && !is.null(rasters[[sel]])) {
        rst_item <- rasters[[sel]]
        n      <- terra::nlyr(rst_item$rst)
        noms   <- names(rst_item$rst)
        bandas <- setNames(seq_len(n),
                           paste0("Banda ", seq_len(n), " (", noms, ")"))
        updateSelectInput(session, "raster_band_stat",
                          choices = bandas, selected = 1)
      }
    })

    # ── Datos según fuente seleccionada ──────────────────────
    active_data <- reactive({
      if (input$data_source == "vector") {
        capas <- shared$capas_vec
        req(capas, input$capa_vec_sel)
        capa <- capas[[input$capa_vec_sel]]
        req(capa)
        sf::st_drop_geometry(capa$sf) %>%
          dplyr::select(where(is.numeric))
      } else {
        rasters <- shared$rasters
        req(rasters, input$raster_sel, input$raster_band_stat)
        rst_item <- rasters[[input$raster_sel]]
        req(rst_item)
        r    <- rst_item$rst[[as.integer(input$raster_band_stat)]]
        vals <- terra::values(r, na.rm = TRUE)
        data.frame(valor = as.numeric(vals))
      }
    })

    # ── Selector de variable 1 ───────────────────────────────
    output$var_selector <- renderUI({
      req(active_data())
      cols <- names(active_data())
      if (length(cols) == 0)
        return(helpText("Sin variables numéricas disponibles."))
      selectInput(session$ns("var1"), "Variable:",
                  choices = cols, selected = cols[1])
    })

    # ── Selector de variable 2 (scatter) ────────────────────
    output$var2_selector <- renderUI({
      req(active_data())
      cols <- names(active_data())
      if (length(cols) < 2)
        return(helpText("Se necesitan al menos 2 variables."))
      selectInput(session$ns("var2"), "Variable Y:",
                  choices  = cols,
                  selected = if (length(cols) >= 2) cols[2] else cols[1])
    })

    # ── Estadísticas descriptivas ────────────────────────────
    stats_df <- eventReactive(input$run_analysis, {
      req(active_data(), input$var1)
      df  <- active_data()
      col <- df[[input$var1]]
      x   <- col[!is.na(col)]
      n   <- length(x)
      se  <- sd(x) / sqrt(n)
      ic  <- t.test(x, conf.level = 0.95)$conf.int
      moda <- as.numeric(names(sort(table(x), decreasing = TRUE))[1])

      data.frame(
        Estadístico = c(
          "N", "NAs",
          "Media", "Error estándar",
          "IC 95% — límite inferior", "IC 95% — límite superior",
          "Mediana", "Moda",
          "Desv. estándar", "Varianza", "CV (%)",
          "Mínimo", "Máximo",
          "Q1", "Q3", "IQR",
          "Asimetría", "Curtosis"
        ),
        Descripción = c(
          "Observaciones válidas", "Valores perdidos",
          "Tendencia central", "Precisión de la media",
          "Límite inferior del intervalo de confianza",
          "Límite superior del intervalo de confianza",
          "Centro resistente a valores extremos", "Valor más frecuente",
          "Dispersión promedio respecto a la media",
          "Dispersión al cuadrado", "Dispersión relativa a la media",
          "Valor más bajo observado", "Valor más alto observado",
          "25% de los datos están por debajo",
          "75% de los datos están por debajo", "Rango del 50% central",
          "Forma de la distribución (0 = simétrica)",
          "Peso de las colas (0 = normal)"
        ),
        Valor = round(c(
          n, sum(is.na(col)),
          mean(x), se,
          ic[1], ic[2],
          median(x), moda,
          sd(x), var(x), sd(x) / mean(x) * 100,
          min(x), max(x),
          quantile(x, 0.25), quantile(x, 0.75), IQR(x),
          moments_skewness(x), moments_kurtosis(x)
        ), 4)
      )
    })

    output$stats_output <- renderTable({
      req(stats_df())
      stats_df()
    }, striped = TRUE, bordered = TRUE, width = "100%")

    # ── Gráfico principal ────────────────────────────────────
    output$main_plot <- renderPlot({
      req(input$run_analysis > 0)
      req(active_data(), input$var1)

      df   <- active_data()
      var1 <- input$var1
      col1 <- df[[var1]]
      type <- input$plot_type

      base_theme <- theme_minimal(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = colores$fondo, color = NA),
          panel.grid.minor = element_blank()
        )

      if (type == "hist") {
        ggplot(df, aes(x = .data[[var1]])) +
          geom_histogram(aes(y = after_stat(density)),
                         bins = 30, fill = colores$secundario,
                         alpha = 0.8, color = "white") +
          geom_density(color = colores$primario, linewidth = 1) +
          base_theme +
          labs(title = paste("Distribución de", var1),
               x = var1, y = "Densidad")

      } else if (type == "density") {
        ggplot(df, aes(x = .data[[var1]])) +
          geom_density(fill = colores$secundario, alpha = 0.5,
                       color = colores$primario, linewidth = 1.2) +
          base_theme +
          labs(title = paste("Densidad de", var1),
               x = var1, y = "Densidad")

      } else if (type == "box") {
        ggplot(df, aes(y = .data[[var1]])) +
          geom_boxplot(fill = colores$secundario, alpha = 0.7,
                       color = colores$primario,
                       outlier.color = colores$peligro) +
          base_theme +
          labs(title = paste("Boxplot de", var1), y = var1)

      } else if (type == "violin") {
        ggplot(df, aes(x = "", y = .data[[var1]])) +
          geom_violin(fill = colores$secundario, color = colores$primario,
                      alpha = 0.6, linewidth = 0.8) +
          geom_jitter(color = colores$primario, width = 0.08,
                      alpha = 0.5, size = 1.8) +
          stat_summary(fun = mean, geom = "point", shape = 18,
                       size = 4, color = colores$acento) +
          base_theme +
          labs(title = paste("Violin de", var1),
               x = NULL, y = var1,
               caption = "\u25c6 = media  |  puntos = observaciones individuales")

      } else if (type == "scatter") {
        req(input$var2)
        var2 <- input$var2
        ggplot(df, aes(x = .data[[var1]], y = .data[[var2]])) +
          geom_point(alpha = 0.5, color = colores$primario, size = 1.5) +
          geom_smooth(method = "lm", color = colores$peligro,
                      fill = colores$advertencia, alpha = 0.2) +
          base_theme +
          labs(title = paste(var1, "vs", var2), x = var1, y = var2)

      } else if (type == "corr") {
        num_df  <- df %>% dplyr::select(where(is.numeric))
        cor_mat <- cor(num_df, use = "complete.obs")
        cor_long <- as.data.frame(as.table(cor_mat)) %>%
          dplyr::rename(Var1 = Var1, Var2 = Var2, Correlacion = Freq)

        ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlacion)) +
          geom_tile(color = "white") +
          geom_text(aes(label = round(Correlacion, 2)), size = 3) +
          scale_fill_gradient2(
            low = colores$peligro, mid = "white", high = colores$primario,
            midpoint = 0, limits = c(-1, 1)
          ) +
          base_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Matriz de correlaciones", x = NULL, y = NULL)
      }
    })

    # ── Reporte automático ───────────────────────────────────
    output$auto_report <- renderText({
      req(input$run_analysis > 0)
      req(active_data(), input$var1)

      df   <- active_data()
      col  <- df[[input$var1]]
      x    <- col[!is.na(col)]
      n    <- length(x)
      mn   <- mean(x)
      med  <- median(x)
      de   <- sd(x)
      se   <- de / sqrt(n)
      cv   <- de / mn * 100
      ic   <- t.test(x, conf.level = 0.95)$conf.int
      asim <- moments_skewness(x)
      kurt <- moments_kurtosis(x)
      var  <- input$var1

      cv_interp <- dplyr::case_when(
        cv < 15  ~ "baja variabilidad",
        cv < 30  ~ "variabilidad moderada",
        cv < 50  ~ "variabilidad alta",
        TRUE     ~ "variabilidad muy alta"
      )
      p1 <- sprintf(
        "La variable '%s' tiene una media de %.4f (DE: %.4f, CV: %.1f%% - %s) con un error estándar de %.4f. El intervalo de confianza al 95%% sugiere que el valor poblacional real se encuentra entre %.4f y %.4f.",
        var, mn, de, cv, cv_interp, se, ic[1], ic[2]
      )

      p2 <- if (abs(mn - med) / de < 0.1) {
        sprintf(
          "La media (%.4f) y la mediana (%.4f) son muy similares, lo que sugiere que los datos son aproximadamente simétricos y no hay valores extremos que distorsionen la media.",
          mn, med
        )
      } else if (mn > med) {
        sprintf(
          "La mediana (%.4f) es menor que la media (%.4f), una primera se\u00f1al de asimetría positiva - algunos valores altos están jalando la media hacia arriba.",
          med, mn
        )
      } else {
        sprintf(
          "La mediana (%.4f) es mayor que la media (%.4f), una primera se\u00f1al de asimetría negativa - algunos valores bajos están jalando la media hacia abajo.",
          med, mn
        )
      }

      p3 <- dplyr::case_when(
        asim >  1   ~ sprintf("La distribución presenta asimetría positiva fuerte (g\u2081 = %.4f): los valores se concentran en la parte baja de la escala con una cola hacia valores altos. En variables ecológicas esto es común (abundancias, áreas, concentraciones). Se recomienda evaluar una transformación logarítmica antes de aplicar modelos paramétricos.", asim),
        asim >  0.5 ~ sprintf("La distribución presenta asimetría positiva moderada (g\u2081 = %.4f): hay una tendencia hacia valores bajos con cola hacia la derecha.", asim),
        asim < -1   ~ sprintf("La distribución presenta asimetría negativa fuerte (g\u2081 = %.4f): los valores se concentran en la parte alta de la escala con una cola hacia valores bajos.", asim),
        asim < -0.5 ~ sprintf("La distribución presenta asimetría negativa moderada (g\u2081 = %.4f): hay una tendencia hacia valores altos con cola hacia la izquierda.", asim),
        TRUE        ~ sprintf("La distribución es aproximadamente simétrica (g\u2081 = %.4f): los valores se distribuyen de forma equilibrada alrededor de la media.", asim)
      )

      p4 <- dplyr::case_when(
        kurt >  1 ~ sprintf("La curtosis es alta - leptocúrtica (g\u2082 = %.4f): hay más valores extremos de lo esperado en una distribución normal. Revisá posibles valores atípicos antes de modelar.", kurt),
        kurt < -1 ~ sprintf("La curtosis es baja - platicúrtica (g\u2082 = %.4f): la distribución es más plana que la normal, con menos valores extremos.", kurt),
        TRUE      ~ sprintf("La curtosis es cercana a la normal - mesocúrtica (g\u2082 = %.4f): las colas tienen un peso similar al esperado en una distribución normal.", kurt)
      )

      p5 <- tryCatch({
        if (n < 3) {
          "Prueba de normalidad: se necesitan al menos 3 observaciones."
        } else if (n > 5000) {
          "Prueba de normalidad (Shapiro-Wilk): no aplicable con más de 5000 observaciones. Usá el gráfico Q-Q o la prueba de Kolmogorov-Smirnov."
        } else {
          sw <- shapiro.test(x)
          if (sw$p.value < 0.05) {
            sprintf(
              "Prueba de Shapiro-Wilk: W = %.4f, p = %.4f \u2192 Hay evidencia estadística de no normalidad (p < 0.05)",
              sw$statistic, sw$p.value
            )
          } else {
            sprintf(
              "Prueba de Shapiro-Wilk: W = %.4f, p = %.4f \u2192 No hay evidencia suficiente para rechazar la normalidad (p \u2265 0.05).",
              sw$statistic, sw$p.value
            )
          }
        }
      }, error = function(e) paste("Shapiro-Wilk: no disponible -", e$message))

      paste(p1, p2, p3, p4, p5, sep = "\n\n")
    })

    # ── Código R reproducible ────────────────────────────────
    codigo_generado <- eventReactive(input$run_analysis, {
      req(active_data(), input$var1)

      fuente <- input$data_source
      var1   <- input$var1
      tipo   <- input$plot_type

      encabezado <- encabezado_script("StatGeo", "Estadísticas")

      carga <- if (fuente == "vector") {
        nombre_capa <- if (!is.null(input$capa_vec_sel) &&
                           !is.null(shared$capas_vec[[input$capa_vec_sel]]))
          shared$capas_vec[[input$capa_vec_sel]]$nombre else "capa"
        paste0(
          "library(sf)\nlibrary(tidyverse)\n\n",
          "# Cargar capa vectorial: ", nombre_capa, "\n",
          "datos <- st_read(\"tu_archivo.gpkg\")\n\n",
          "# Extraer atributos numéricos\n",
          "df <- st_drop_geometry(datos) |> select(where(is.numeric))\n\n",
          "col <- df$`", var1, "`\n\n"
        )
      } else {
        nombre_rst <- if (!is.null(input$raster_sel) &&
                          !is.null(shared$rasters[[input$raster_sel]]))
          shared$rasters[[input$raster_sel]]$nombre else "raster"
        banda <- input$raster_band_stat %||% "1"
        paste0(
          "library(terra)\nlibrary(tidyverse)\n\n",
          "# Cargar raster: ", nombre_rst, "\n",
          "r   <- rast(\"tu_archivo.tif\")\n",
          "col <- values(r[[", banda, "]], na.rm = TRUE) |> as.numeric()\n\n"
        )
      }

      stats_bloque <- paste0(
        "# ── Estadísticas descriptivas ──\n",
        "summary(col)\n",
        "sd(col, na.rm = TRUE)\n",
        "IQR(col, na.rm = TRUE)\n\n",
        "# Asimetría y curtosis\n",
        "n  <- sum(!is.na(col))\n",
        "mu <- mean(col, na.rm = TRUE)\n",
        "s  <- sd(col, na.rm = TRUE)\n",
        "asimetria <- (sum((col - mu)^3, na.rm = TRUE) / n) / s^3\n",
        "curtosis  <- (sum((col - mu)^4, na.rm = TRUE) / n) / s^4 - 3\n\n"
      )

      grafico <- if (tipo == "hist") {
        paste0(
          "# ── Histograma con curva de densidad ──\n",
          "ggplot(data.frame(x = col), aes(x = x)) +\n",
          "  geom_histogram(aes(y = after_stat(density)),\n",
          "                 bins = 30, fill = \"#5FA2CE\",\n",
          "                 alpha = 0.8, color = \"white\") +\n",
          "  geom_density(color = \"#1170AA\", linewidth = 1) +\n",
          "  labs(title = \"Distribución de ", var1, "\",\n",
          "       x = \"", var1, "\", y = \"Densidad\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "density") {
        paste0(
          "# ── Densidad ──\n",
          "ggplot(data.frame(x = col), aes(x = x)) +\n",
          "  geom_density(fill = \"#5FA2CE\", alpha = 0.5,\n",
          "               color = \"#1170AA\", linewidth = 1.2) +\n",
          "  labs(title = \"Densidad de ", var1, "\",\n",
          "       x = \"", var1, "\", y = \"Densidad\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "box") {
        paste0(
          "# ── Boxplot ──\n",
          "ggplot(data.frame(x = col), aes(y = x)) +\n",
          "  geom_boxplot(fill = \"#5FA2CE\", alpha = 0.7,\n",
          "               color = \"#1170AA\",\n",
          "               outlier.color = \"#C85200\") +\n",
          "  labs(title = \"Boxplot de ", var1, "\", y = \"", var1, "\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "violin") {
        paste0(
          "# ── Violin ──\n",
          "ggplot(data.frame(x = col), aes(x = \"\", y = x)) +\n",
          "  geom_violin(fill = \"#5FA2CE\", color = \"#1170AA\",\n",
          "              alpha = 0.6, linewidth = 0.8) +\n",
          "  geom_jitter(color = \"#1170AA\", width = 0.08,\n",
          "              alpha = 0.5, size = 1.8) +\n",
          "  stat_summary(fun = mean, geom = \"point\", shape = 18,\n",
          "               size = 4, color = \"#FC7D0B\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "scatter") {
        var2 <- if (!is.null(input$var2)) input$var2 else "var2"
        paste0(
          "# ── Dispersión ──\n",
          "ggplot(df, aes(x = `", var1, "`, y = `", var2, "`)) +\n",
          "  geom_point(alpha = 0.5, color = \"#1170AA\", size = 1.5) +\n",
          "  geom_smooth(method = \"lm\", color = \"#C85200\",\n",
          "              fill = \"#F1CE63\", alpha = 0.2) +\n",
          "  labs(title = \"", var1, " vs ", var2, "\",\n",
          "       x = \"", var1, "\", y = \"", var2, "\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "corr") {
        paste0(
          "# ── Matriz de correlaciones ──\n",
          "cor_mat  <- cor(df, use = \"complete.obs\")\n",
          "cor_long <- as.data.frame(as.table(cor_mat))\n",
          "names(cor_long) <- c(\"Var1\", \"Var2\", \"Correlacion\")\n\n",
          "ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlacion)) +\n",
          "  geom_tile(color = \"white\") +\n",
          "  geom_text(aes(label = round(Correlacion, 2)), size = 3) +\n",
          "  scale_fill_gradient2(low = \"#C85200\", mid = \"white\",\n",
          "                       high = \"#1170AA\", midpoint = 0,\n",
          "                       limits = c(-1, 1)) +\n",
          "  theme_minimal() +\n",
          "  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +\n",
          "  labs(title = \"Matriz de correlaciones\", x = NULL, y = NULL)\n"
        )
      }

      paste0(encabezado, carga, stats_bloque, grafico)
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function() paste0("statgeo_stats_",
                                   format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )
  })
}

# ── Helpers numéricos ────────────────────────────────────────
moments_skewness <- function(x) {
  x  <- x[!is.na(x)]
  n  <- length(x)
  mu <- mean(x)
  s  <- sd(x)
  if (s == 0) return(NA)
  (sum((x - mu)^3) / n) / s^3
}

moments_kurtosis <- function(x) {
  x  <- x[!is.na(x)]
  n  <- length(x)
  mu <- mean(x)
  s  <- sd(x)
  if (s == 0) return(NA)
  (sum((x - mu)^4) / n) / s^4 - 3
}
