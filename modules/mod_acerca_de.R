# ============================================================
# mod_acerca_de.R — Módulo "Acerca de" para StatGeo
#
# Muestra información sobre R y los paquetes utilizados,
# así como las citas para referencia académica.
#
# StatSuite:
#   StatDesign  — Diseño de estudios y muestreo
#   StatFlow    — Primeros análisis y visualización
#   StatGeo     — Para trabajar con mapas (SIG)  ← esta app
#   StatModels  — Modelos avanzados (próximamente)
# ============================================================

library(bsicons)

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      col_widths = c(12),
      gap        = "1rem",

      # ── Desarrollado con R ────────────────────────────────
      card(
        card_header(
          bs_icon("code-slash"), " Desarrollado con R"
        ),
        card_body(
          p("StatGeo fue desarrollada con R y los siguientes paquetes de código abierto."),

          # Entorno R
          p(class = "text-muted small fw-bold mt-3 mb-1", "ENTORNO DE DESARROLLO"),
          div(
            style = "display: flex; align-items: center; gap: 12px; background: var(--bs-secondary-bg); border-radius: 8px; padding: 10px 14px;",
            bs_icon("r-circle", size = "1.2em"),
            tags$strong("R Project for Statistical Computing"),
            tags$span(
              style = "color: gray; font-size: 0.85em;",
              paste0("v", R.version$major, ".", R.version$minor)
            ),
            tags$a(
              "r-project.org",
              href   = "https://www.r-project.org",
              target = "_blank",
              style  = "margin-left: auto; font-size: 0.85em;"
            )
          ),

          # Paquetes
          p(class = "text-muted small fw-bold mt-3 mb-2", "PAQUETES"),
          tags$table(
            class = "table table-sm table-hover",
            style = "font-size: 0.85em;",
            tags$thead(
              tags$tr(
                tags$th("Paquete"),
                tags$th("Versión"),
                tags$th("Referencia")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$code("shiny")),
                tags$td(paste0("v", packageVersion("shiny"))),
                tags$td(tags$a("shiny.posit.co", href = "https://shiny.posit.co", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("bslib")),
                tags$td(paste0("v", packageVersion("bslib"))),
                tags$td(tags$a("rstudio.github.io/bslib", href = "https://rstudio.github.io/bslib", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("bsicons")),
                tags$td(paste0("v", packageVersion("bsicons"))),
                tags$td(tags$a("github.com/rstudio/bsicons", href = "https://github.com/rstudio/bsicons", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("sf")),
                tags$td(paste0("v", packageVersion("sf"))),
                tags$td(tags$a("r-spatial.github.io/sf", href = "https://r-spatial.github.io/sf", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("terra")),
                tags$td(paste0("v", packageVersion("terra"))),
                tags$td(tags$a("rspatial.org/terra", href = "https://rspatial.org/terra", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("tidyterra")),
                tags$td(paste0("v", packageVersion("tidyterra"))),
                tags$td(tags$a("dieghernan.github.io/tidyterra", href = "https://dieghernan.github.io/tidyterra", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("raster")),
                tags$td(paste0("v", packageVersion("raster"))),
                tags$td(tags$a("rspatial.org/raster", href = "https://rspatial.org/raster", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("leaflet")),
                tags$td(paste0("v", packageVersion("leaflet"))),
                tags$td(tags$a("rstudio.github.io/leaflet", href = "https://rstudio.github.io/leaflet", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("mapview")),
                tags$td(paste0("v", packageVersion("mapview"))),
                tags$td(tags$a("r-spatial.github.io/mapview", href = "https://r-spatial.github.io/mapview", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("tidyverse")),
                tags$td(paste0("v", packageVersion("tidyverse"))),
                tags$td(tags$a("tidyverse.org", href = "https://www.tidyverse.org", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("readxl")),
                tags$td(paste0("v", packageVersion("readxl"))),
                tags$td(tags$a("readxl.tidyverse.org", href = "https://readxl.tidyverse.org", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("scales")),
                tags$td(paste0("v", packageVersion("scales"))),
                tags$td(tags$a("scales.r-lib.org", href = "https://scales.r-lib.org", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("DT")),
                tags$td(paste0("v", packageVersion("DT"))),
                tags$td(tags$a("rstudio.github.io/DT", href = "https://rstudio.github.io/DT", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("parameters")),
                tags$td(paste0("v", packageVersion("parameters"))),
                tags$td(tags$a("easystats.github.io/parameters", href = "https://easystats.github.io/parameters", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("report")),
                tags$td(paste0("v", packageVersion("report"))),
                tags$td(tags$a("easystats.github.io/report", href = "https://easystats.github.io/report", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("correlation")),
                tags$td(paste0("v", packageVersion("correlation"))),
                tags$td(tags$a("easystats.github.io/correlation", href = "https://easystats.github.io/correlation", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("datawizard")),
                tags$td(paste0("v", packageVersion("datawizard"))),
                tags$td(tags$a("easystats.github.io/datawizard", href = "https://easystats.github.io/datawizard", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("viridisLite")),
                tags$td(paste0("v", packageVersion("viridisLite"))),
                tags$td(tags$a("sjmgarnier.github.io/viridisLite", href = "https://sjmgarnier.github.io/viridisLite", target = "_blank"))
              )
            )
          )
        )
      ),

      # ── Citas ─────────────────────────────────────────────
      card(
        card_header(
          bs_icon("journal-text"), " Citas"
        ),
        card_body(
          p("Si utilizás StatGeo en tu investigación, por favor citá R y esta aplicación."),

          p(class = "text-muted small fw-bold mt-3 mb-1", "CÓMO CITAR R"),
          tags$pre(
            style = "background: var(--bs-secondary-bg); border-left: 3px solid #6c757d; border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 0.8em; white-space: pre-wrap;",
            "R Core Team (2026). R: A Language and Environment for Statistical
Computing. R Foundation for Statistical Computing, Vienna, Austria.
https://www.R-project.org/"
          ),

          p(class = "text-muted small fw-bold mt-3 mb-1", "CÓMO CITAR ESTA APLICACIÓN"),
          tags$pre(
            style = "background: var(--bs-secondary-bg); border-left: 3px solid #6c757d; border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 0.8em; white-space: pre-wrap;",
            "Spínola, M. (2026). StatGeo: Análisis y visualización espacial
[Aplicación web]. StatSuite. https://statsuite.netlify.app"
          )
        )
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sin lógica de servidor por ahora
  })
}
