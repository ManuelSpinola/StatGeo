# ============================================================
# PAQUETES REQUERIDOS — instalar con:
#   install.packages(pkgs)
# o con renv:
#   renv::restore()
# ============================================================

pkgs <- c(
  # Framework Shiny
  "shiny",
  "bslib",
  
  # Datos espaciales vectoriales
  "sf",
  
  # Datos espaciales raster
  "terra",
  "raster",          # Requerido por leaflet para raster addRasterImage
  
  # tidyterra (geom_spatraster para ggplot2)
  "tidyterra",
  
  # Visualización interactiva de mapas
  "leaflet",
  "mapview",
  
  # Tablas interactivas
  "DT",
  
  # Tidyverse
  "dplyr",
  "tidyr",
  "ggplot2",
  "purrr",
  "stringr",
  "forcats",
  "scales",
  
  # easystats
  "easystats",    # instala: parameters, performance, report, see,
                  #          correlation, bayestestR, datawizard, effectsize
  
  # Lectura de archivos tabulares
  "readr",         # CSV / TSV
  "readxl",        # Excel (.xlsx / .xls)

  # Utilidades
  "viridisLite",
  "tools"          # base, pero por claridad
)

# Para instalar todo de una vez:
# install.packages(pkgs)

message("Paquetes necesarios listados. Ejecuta install.packages(pkgs) para instalarlos.")
