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

library(shiny)
library(bslib)

# ── 2. Módulos ─────────────────────────────────────────────
source("modules/mod_upload.R")
source("modules/mod_vector.R")
source("modules/mod_raster.R")
source("modules/mod_stats.R")

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
    img(
      src    = "hexsticker_StatGeo.png",
      height = "38px"
    ),
    span("StatGeo", style = "font-weight: 600;")
  ),
  theme  = tema_app,
  lang   = "es",
  footer = div(
    class = "text-center text-muted small py-2",
    style = paste0("border-top: 1px solid ", colores$borde, ";"),
    "Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
  ),

  nav_panel("📁 Datos",        mod_upload_ui("upload")),
  nav_panel("🗺️ Vectorial",    mod_vector_ui("vector")),
  nav_panel("🏔️ Raster",       mod_raster_ui("raster")),
  nav_panel("📊 Estadísticas", mod_stats_ui("stats")),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "StatGeo v1.0"))
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

  # Módulos de visualización — consumen datos compartidos
  mod_vector_server("vector", shared = shared)
  mod_raster_server("raster", shared = shared)
  mod_stats_server("stats",  shared = shared)
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
