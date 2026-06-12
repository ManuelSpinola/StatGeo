#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatGeo")
  )

  page_navbar(
    title = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatGeo.png", height = "38px"),
      span("StatGeo", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),

    nav_panel(title = "Vectorial",     icon = bs_icon("map"),          mod_vector_ui("vector")),
    nav_panel(title = "Raster",        icon = bs_icon("grid-3x3"),     mod_raster_ui("raster")),
    nav_panel(title = "Mapa",          icon = bs_icon("layers"),       mod_mapa_ui("mapa")),
    nav_panel(title = "Grilla H3",     icon = bs_icon("hexagon-fill"), mod_h3_ui("h3")),
    nav_panel(title = "Estad\u00edsticas", icon = bs_icon("bar-chart"),mod_stats_ui("stats")),
    nav_panel(title = "Acerca de",     icon = bs_icon("info-circle"),  mod_acerca_de_ui("acerca_de")),

    nav_spacer(),
    nav_item(tags$span(class = "text-muted small", "StatGeo v2.0"))
  )
}
