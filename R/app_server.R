#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # Reactive values compartidos entre módulos
  shared <- shiny::reactiveValues(
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
