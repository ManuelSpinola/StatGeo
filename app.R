# ============================================================
# app.R — Punto de entrada de StatGeo
#
# Este archivo SOLO:
#   1. Carga librerías y helpers compartidos
#   2. Carga los módulos
#   3. Define ui y server ensamblando los módulos
#
# La lógica de cada pestaña vive en modules/mod_*.R
# Las funciones compartidas viven en R/helpers.R
# ============================================================

# ── 1. Librerías y helpers ─────────────────────────────────
source("R/helpers.R")

# ── 2. Módulos ─────────────────────────────────────────────
source("modules/mod_upload.R")
source("modules/mod_vector.R")
source("modules/mod_raster.R")
source("modules/mod_integracion.R")
source("modules/mod_stats.R")
source("modules/mod_acerca_de.R")

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
    img(src = "hexsticker_StatGeo.png", height = "38px"),
    span("StatGeo", style = "font-weight: 600;")
  ),
  theme  = tema_app,
  lang   = "es",
  footer = div(
    class = "text-center text-muted small py-2",
    style = paste0("border-top: 1px solid ", colores$borde, ";"),
    "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
  ),

  nav_panel(title = "Datos",        icon = bs_icon("folder2-open"),  mod_upload_ui("upload")),
  nav_panel(title = "Vectorial",    icon = bs_icon("map"),            mod_vector_ui("vector")),
  nav_panel(title = "Raster",       icon = bs_icon("grid-3x3"),       mod_raster_ui("raster")),
  nav_panel(title = "Integración",  icon = bs_icon("layers"),         mod_integracion_ui("integracion")),
  nav_panel(title = "Estadísticas", icon = bs_icon("bar-chart"),      mod_stats_ui("stats")),
  nav_panel(title = "Acerca de",    icon = bs_icon("info-circle"),    mod_acerca_de_ui("acerca_de")),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "StatGeo v1.1"))
)

# ── 4. Server ──────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive values compartidos entre módulos
  shared <- reactiveValues(
    sf_data     = NULL,   # Datos vectoriales (sf object)
    raster_data = NULL,   # Datos raster (SpatRaster)
    crs_info    = NULL    # CRS activo
  )

  # Módulo de carga — produce datos compartidos
  mod_upload_server("upload", shared = shared)

  # Módulos de exploración — consumen shared
  mod_vector_server("vector",           shared = shared)
  mod_raster_server("raster",           shared = shared)
  mod_integracion_server("integracion", shared = shared)
  mod_stats_server("stats",             shared = shared)
  mod_acerca_de_server("acerca_de")
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
