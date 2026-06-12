#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # ── Reactive values compartidos entre módulos ──────────────
  shared <- shiny::reactiveValues(
    capas_vec     = list(),  # Lista de capas vectoriales
    rasters       = list(),  # Lista de rasters cargados
    raster_activo = NULL     # id del raster activo para visualizar
  )

  # ── Módulos ────────────────────────────────────────────────
  mod_vector_server("vector", shared = shared)
  mod_raster_server("raster", shared = shared)
  mod_mapa_server("mapa",     shared = shared)
  mod_h3_server("h3",         shared = shared)
  mod_stats_server("stats",   shared = shared)
  mod_acerca_de_server("acerca_de")
}
