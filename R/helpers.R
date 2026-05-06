# ============================================================
# R/helpers.R — Funciones auxiliares globales
# ============================================================

library(shiny)
library(bslib)
library(bsicons)
library(sf)
library(terra)
library(tidyterra)
library(raster)
library(leaflet)
library(mapview)
library(tidyverse)
library(readxl)
library(scales)
library(DT)
library(parameters)
library(report)
library(correlation)
library(datawizard)

# Operador null-coalescing
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "") a else b

# ── Helper: fila de tabla de resumen ─────────────────────────
info_row <- function(label, value) {
  tags$tr(
    tags$td(class = "fw-semibold text-muted pe-3", label),
    tags$td(as.character(value))
  )
}

# ── Helper: notificación con icono ───────────────────────────
notify_ok  <- function(msg) showNotification(paste("✅", msg), type = "message", duration = 3)
notify_err <- function(msg) showNotification(paste("❌", msg), type = "error",   duration = 5)
notify_wrn <- function(msg) showNotification(paste("⚠️", msg), type = "warning", duration = 4)

# ── Helper: verificar que sf tenga CRS ───────────────────────
ensure_crs <- function(sf_obj, default_epsg = 4326) {
  if (is.na(sf::st_crs(sf_obj))) {
    warning("Sin CRS — asignando EPSG:", default_epsg)
    sf_obj <- sf::st_set_crs(sf_obj, default_epsg)
  }
  sf_obj
}

# ── Helper: columnas numéricas de un sf ──────────────────────
numeric_cols <- function(sf_obj) {
  df   <- sf::st_drop_geometry(sf_obj)
  cols <- names(df)[sapply(df, is.numeric)]
  cols
}

# ── Helper: formato de número grande ─────────────────────────
fmt_num <- function(x, digits = 3) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}

# ── Paleta de colores StatGeo ──────────────────────────────
# Tableau Color Blind — accesible y profesional
colores <- list(
  fondo       = "#F4F7FB",  # azul muy pálido — fondo general
  primario    = "#1170AA",  # azul oscuro — navbar, encabezados
  acento      = "#FC7D0B",  # naranja — botones, íconos activos
  secundario  = "#5FA2CE",  # azul claro — elementos secundarios
  texto       = "#57606C",  # gris oscuro — texto principal
  exito       = "#5FA2CE",  # azul claro — valores positivos
  advertencia = "#F1CE63",  # amarillo — badge categórica, valores medios
  peligro     = "#C85200",  # naranja oscuro — errores, outliers
  borde       = "#C8D9EC",  # azul muy pálido — bordes y separadores
  # Paleta completa Tableau Color Blind para gráficos
  tableau = c(
    "#1170AA", # azul oscuro
    "#FC7D0B", # naranja
    "#A3ACB9", # gris medio
    "#57606C", # gris oscuro
    "#C85200", # naranja oscuro
    "#7BC8ED", # azul cielo
    "#5FA2CE", # azul claro
    "#F1CE63", # amarillo
    "#9F8B75", # marrón
    "#B85A0D"  # marrón naranja
  )
)

# ── Tema visual ────────────────────────────────────────────
tema_app <- bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-brand, .nav-link { color: #ffffff !important; }
  .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .navbar-brand { display: flex !important; align-items: center !important; padding-top: 0 !important; padding-bottom: 0 !important; }
")

# ── Escala de color para gráficos (ggplot2) ───────────────
# Uso: + scale_fill_tableau_cb() o + scale_color_tableau_cb()
scale_fill_tableau_cb <- function(...) {
  scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  scale_color_manual(values = colores$tableau, ...)
}
