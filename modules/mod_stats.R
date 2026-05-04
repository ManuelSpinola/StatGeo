# ============================================================
# mod_stats.R — Módulo de estadísticas descriptivas
# Usa: easystats (report, parameters, performance, see)
#      tidyverse (dplyr, ggplot2, tidyr)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(DT)

# easystats (se carga con precaución — disponible si instalado)
safe_library <- function(pkg) {
  suppressPackageStartupMessages(
    tryCatch(library(pkg, character.only = TRUE), error = function(e) NULL)
  )
}
safe_library("easystats")
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
      
      radioButtons(ns("data_source"), NULL,
                   choices  = c("Vectorial (atributos)" = "vector",
                                "Raster (por banda)"    = "raster"),
                   selected = "vector"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'raster'", ns("data_source")),
        selectInput(ns("raster_band_stat"), "Banda:", choices = NULL)
      ),
      
      hr(),
      
      h6(class = "text-muted fw-bold text-uppercase small", "Estadísticas descriptivas"),
      
      uiOutput(ns("var_selector")),
      
      hr(),
      
      h6(class = "text-muted fw-bold text-uppercase small", "Visualización"),
      
      selectInput(ns("plot_type"), "Tipo de gráfico:",
                  choices = c(
                    "Histograma"       = "hist",
                    "Boxplot"          = "box",
                    "Densidad"         = "density",
                    "Dispersión (2 vars)" = "scatter",
                    "Correlaciones"    = "corr"
                  )),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'scatter'", ns("plot_type")),
        uiOutput(ns("var2_selector"))
      ),
      
      actionButton(ns("run_analysis"), "Ejecutar análisis",
                   class = "btn-success w-100 mt-2")
    ),
    
    # ── Panel principal ──────────────────────────────────────
    layout_columns(
      col_widths = c(6, 6),
      
      # Estadísticas descriptivas
      card(
        card_header(icon("table-list"), " Estadísticas descriptivas"),
        card_body(
          uiOutput(ns("stats_output"))
        )
      ),
      
      # Gráfico
      card(
        card_header(icon("chart-area"), " Visualización"),
        card_body(
          plotOutput(ns("main_plot"), height = "400px")
        )
      ),
      
      # Reporte en texto (easystats::report)
      card(
        col_widths = 12,
        card_header(icon("file-alt"), " Reporte automático (easystats)"),
        card_body(
          verbatimTextOutput(ns("auto_report"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_stats_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    # ── Datos según fuente seleccionada ───────────────────────
    active_data <- reactive({
      if (input$data_source == "vector") {
        req(shared$sf_data)
        sf::st_drop_geometry(shared$sf_data) %>%
          dplyr::select(where(is.numeric))
      } else {
        req(shared$raster_data, input$raster_band_stat)
        r    <- shared$raster_data[[as.integer(input$raster_band_stat)]]
        vals <- terra::values(r, na.rm = TRUE)
        data.frame(valor = as.numeric(vals))
      }
    })
    
    # ── Actualizar bandas raster ───────────────────────────────
    observeEvent(shared$raster_data, {
      req(shared$raster_data)
      n <- terra::nlyr(shared$raster_data)
      updateSelectInput(session, "raster_band_stat",
                        choices  = setNames(seq_len(n), paste("Banda", seq_len(n))),
                        selected = 1)
    })
    
    # ── Selector de variable 1 ────────────────────────────────
    output$var_selector <- renderUI({
      req(active_data())
      cols <- names(active_data())
      if (length(cols) == 0) return(helpText("Sin variables numéricas disponibles."))
      selectInput(session$ns("var1"), "Variable:", choices = cols, selected = cols[1])
    })
    
    # ── Selector de variable 2 (scatter) ──────────────────────
    output$var2_selector <- renderUI({
      req(active_data())
      cols <- names(active_data())
      if (length(cols) < 2) return(helpText("Se necesitan al menos 2 variables."))
      selectInput(session$ns("var2"), "Variable Y:", choices = cols,
                  selected = if (length(cols) >= 2) cols[2] else cols[1])
    })
    
    # ── Estadísticas descriptivas ─────────────────────────────
    stats_df <- eventReactive(input$run_analysis, {
      req(active_data(), input$var1)
      df  <- active_data()
      col <- df[[input$var1]]
      
      data.frame(
        Estadística = c("N", "NAs", "Mínimo", "Q1", "Mediana",
                        "Media", "Q3", "Máximo", "Desv. Est.",
                        "Asimetría", "Curtosis"),
        Valor = round(c(
          sum(!is.na(col)),
          sum(is.na(col)),
          min(col, na.rm = TRUE),
          quantile(col, 0.25, na.rm = TRUE),
          median(col, na.rm = TRUE),
          mean(col, na.rm = TRUE),
          quantile(col, 0.75, na.rm = TRUE),
          max(col, na.rm = TRUE),
          sd(col, na.rm = TRUE),
          moments_skewness(col),
          moments_kurtosis(col)
        ), 4)
      )
    })
    
    output$stats_output <- renderUI({
      req(stats_df())
      
      renderTable(stats_df(), striped = TRUE, bordered = TRUE,
                  small = TRUE, width = "100%")()
    })
    
    # ── Gráfico principal ─────────────────────────────────────
    output$main_plot <- renderPlot({
      input$run_analysis
      req(active_data(), input$var1)
      
      df   <- active_data()
      var1 <- input$var1
      col1 <- df[[var1]]
      type <- input$plot_type
      
      base_theme <- theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = "#F8FAF9", color = NA),
              panel.grid.minor = element_blank())
      
      if (type == "hist") {
        ggplot(df, aes(x = .data[[var1]])) +
          geom_histogram(aes(y = after_stat(density)),
                         bins = 30, fill = "#52B788", alpha = 0.8, color = "white") +
          geom_density(color = "#2D6A4F", linewidth = 1) +
          base_theme +
          labs(title = paste("Distribución de", var1), x = var1, y = "Densidad")
        
      } else if (type == "box") {
        ggplot(df, aes(y = .data[[var1]])) +
          geom_boxplot(fill = "#52B788", alpha = 0.7, color = "#2D6A4F",
                       outlier.color = "#B7472A") +
          base_theme +
          labs(title = paste("Boxplot de", var1), y = var1)
        
      } else if (type == "density") {
        ggplot(df, aes(x = .data[[var1]])) +
          geom_density(fill = "#52B788", alpha = 0.5, color = "#2D6A4F", linewidth = 1.2) +
          base_theme +
          labs(title = paste("Densidad de", var1), x = var1, y = "Densidad")
        
      } else if (type == "scatter") {
        req(input$var2)
        var2 <- input$var2
        
        ggplot(df, aes(x = .data[[var1]], y = .data[[var2]])) +
          geom_point(alpha = 0.5, color = "#2D6A4F", size = 1.5) +
          geom_smooth(method = "lm", color = "#B7472A", fill = "#f4a58a", alpha = 0.2) +
          base_theme +
          labs(title = paste(var1, "vs", var2), x = var1, y = var2)
        
      } else if (type == "corr") {
        # Matriz de correlación simple
        num_df  <- df %>% dplyr::select(where(is.numeric))
        cor_mat <- cor(num_df, use = "complete.obs")
        
        cor_long <- as.data.frame(as.table(cor_mat)) %>%
          rename(Var1 = Var1, Var2 = Var2, Correlacion = Freq)
        
        ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlacion)) +
          geom_tile(color = "white") +
          geom_text(aes(label = round(Correlacion, 2)), size = 3) +
          scale_fill_gradient2(low = "#B7472A", mid = "white", high = "#2D6A4F",
                               midpoint = 0, limits = c(-1, 1)) +
          base_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Matriz de correlaciones", x = NULL, y = NULL)
      }
    })
    
    # ── Reporte automático (easystats) ────────────────────────
    output$auto_report <- renderText({
      input$run_analysis
      req(active_data(), input$var1)
      
      df  <- active_data()
      col <- df[[input$var1]]
      
      tryCatch({
        # Intentar con easystats::report si está disponible
        if (exists("report") && requireNamespace("report", quietly = TRUE)) {
          r <- report::report(col)
          as.character(r)
        } else {
          # Reporte manual
          sprintf(
            "Variable: %s\nN = %d | NAs = %d\nMedia = %.4f | Mediana = %.4f\nDesv. Est. = %.4f\nRango: [%.4f, %.4f]\nAsimetría = %.4f | Curtosis = %.4f",
            input$var1,
            sum(!is.na(col)), sum(is.na(col)),
            mean(col, na.rm = TRUE), median(col, na.rm = TRUE),
            sd(col, na.rm = TRUE),
            min(col, na.rm = TRUE), max(col, na.rm = TRUE),
            moments_skewness(col), moments_kurtosis(col)
          )
        }
      }, error = function(e) {
        paste("Error al generar reporte:", e$message)
      })
    })
    
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
