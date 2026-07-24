# ─────────────────────────────────────────────────────────────────────────────
# 8. POPULATION INDICATORS — Reclutamiento, Densidad, CPUE (coquina, por region)
# ─────────────────────────────────────────────────────────────────────────────
# Requiere objetos ya creados en COMPARA_RECLAM.R:
#   tallas_co, pal_area, theme_reclam, archivo_cadiz, archivo_valencia,
#   fig_dpi, here(), y las librerias tidyverse/readxl/janitor/patchwork ya cargadas.

col_cadiz    <- unname(pal_area["Cadiz"])
col_valencia <- unname(pal_area["Valencia"])

## 8.1 Reclutamiento — proporcion mensual de individuos <= 10.8 mm ------------
## (mismo umbral usado en el resto del paper: is_recruit en la seccion 2)

recr_idx <- tallas_co |>
  group_by(Area, fecha = FECHA, year, month) |>
  summarise(
    N_total   = n(),
    N_recruit = sum(TALLA <= 10.8, na.rm = TRUE),
    RI        = N_recruit / N_total,
    .groups   = "drop"
  )

## 8.2 Cruce Captura_coquina x bitacora — base para Densidad y CPUE -----------
# Densidad = Nº individuos (Captura_coquina) / area barrida (bitacora)
#   area_m2 = Distancia (m) * ancho del arte (0.85 m)
# CPUE     = Submuestra comercial (g) (Captura_coquina) expandida a 1 hora,
#   usando Tiempo de pesca (minutos) (bitacora)
# Cruce por Fecha y por Punto (Captura_coquina) = Replica (bitacora) --
# verificado en los datos: Captura_coquina$Punto usa codigos tipo "5P1"/"D1NC"
# que coinciden con bitacora$Replica, no con bitacora$Punto (que es solo la
# estacion, ej. "5"/"D").

# readxl a veces devuelve Fecha como texto -- filas basura en bitacora Cadiz
# (fila de tipos "<chr>/<dbl>/<S3: POSIXct>" + fila de cabecera repetida)
# fuerzan la columna entera a character. parse_fecha() cubre Date, POSIXct,
# numerico (serial Excel), texto-serial y texto con formato; lo no reconocible
# (filas basura) queda NA y se descarta despues con filter(!is.na(fecha)).
parse_fecha <- function(x) {
  if (inherits(x, "Date"))    return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (is.numeric(x))          return(as.Date(x, origin = "1899-12-30"))

  x_num <- suppressWarnings(as.numeric(x))
  out   <- as.Date(x_num, origin = "1899-12-30")
  falta <- is.na(out)
  if (any(falta)) {
    out[falta] <- suppressWarnings(
      dplyr::coalesce(
        as.Date(x[falta], format = "%Y-%m-%d"),
        as.Date(x[falta], format = "%d/%m/%Y"),
        as.Date(x[falta], format = "%d-%m-%Y")
      )
    )
  }
  out
}

ANCHO_ARTE_M <- 0.85   # ancho del arte de pesca (85 cm), fijo para ambas regiones

# Esfuerzo (tiempo de pesca) y distancia recorrida viven en 'bitacora'
read_bitacora <- function(archivo, area_lbl) {
  read_excel(archivo, sheet = "bitacora") |>
    clean_names() |>
    filter(especie == "CO") |>
    mutate(
      fecha                   = parse_fecha(fecha),
      tiempo_de_pesca_minutos = suppressWarnings(as.numeric(tiempo_de_pesca_minutos)),
      distancia_m             = suppressWarnings(as.numeric(distancia_m))
    ) |>
    filter(!is.na(fecha)) |>                         # descarta filas basura de la hoja
    transmute(Area = area_lbl, punto = trimws(replica), fecha,
              tiempo_de_pesca_minutos, distancia_m)
}

# Numero de individuos y submuestra comercial viven en 'Captura_coquina'
read_captura <- function(archivo, area_lbl) {
  df <- read_excel(archivo, sheet = "Captura_coquina") |> clean_names()
  # "Nº individuos" transcribe de forma inconsistente segun el motor de
  # limpieza de nombres (la ordinal "º" no siempre transliteral igual);
  # se localiza por patron en vez de asumir el nombre exacto resultante
  names(df)[str_detect(names(df), "individuo")] <- "n_individuos"
  df |>
    transmute(
      Area                   = area_lbl,
      punto                  = trimws(punto),
      fecha                  = parse_fecha(fecha),
      n_individuos           = as.numeric(n_individuos),
      submuestra_comercial_g = as.numeric(submuestra_comercial_g)
    ) |>
    filter(!is.na(fecha))
}

cap_coq <- bind_rows(
  read_captura(archivo_cadiz, "Cadiz") |>
    left_join(read_bitacora(archivo_cadiz, "Cadiz"), by = c("Area", "punto", "fecha")),
  read_captura(archivo_valencia, "Valencia") |>
    left_join(read_bitacora(archivo_valencia, "Valencia"), by = c("Area", "punto", "fecha"))
) |>
  mutate(
    # Protocolo Cadiz: tiempo de pesca fijo = 5 min para toda la serie
    # (bitacora tiene huecos de registro; el valor real no varia entre
    # replicas). No aplica a Valencia, que tiene el dato bien registrado.
    tiempo_de_pesca_minutos = if_else(Area == "Cadiz",
                                      coalesce(tiempo_de_pesca_minutos, 5),
                                      tiempo_de_pesca_minutos),
    year        = year(fecha),
    month       = month(fecha),
    area_m2     = distancia_m * ANCHO_ARTE_M,
    dens_ind_m2 = n_individuos / area_m2,
    cpue_gh     = submuestra_comercial_g / (tiempo_de_pesca_minutos / 60)
  )

# QC: capturas sin esfuerzo/distancia asociada tras el cruce -- revisar bitacora
# antes de seguir si esta lista no esta vacia (punto sin replica homologa).
cap_coq |>
  filter(is.na(distancia_m) | is.na(tiempo_de_pesca_minutos)) |>
  count(Area, year, month) |>
  print(n = Inf)

## 8.3 Densidad — individuos / m^2, por mes y region ---------------------------

density_ts <- cap_coq |>
  filter(!is.na(dens_ind_m2), is.finite(dens_ind_m2)) |>
  group_by(Area, year, month) |>
  summarise(
    fecha     = min(fecha),
    dens_mean = mean(dens_ind_m2, na.rm = TRUE),
    dens_se   = sd(dens_ind_m2,   na.rm = TRUE) / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  )

## 8.4 CPUE — submuestra comercial expandida a 1 h, por mes y region -----------

cpue_monthly <- cap_coq |>
  filter(!is.na(cpue_gh), is.finite(cpue_gh), cpue_gh >= 0) |>
  mutate(cpue_kg_h = cpue_gh / 1000) |>
  group_by(Area, year, month) |>
  summarise(
    fecha     = min(fecha),
    cpue_mean = mean(cpue_kg_h, na.rm = TRUE),
    cpue_se   = sd(cpue_kg_h,   na.rm = TRUE) / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  )

## 8.5 Panel 2 columnas (Cadiz | Valencia) x 3 filas ---------------------------
## Reclutamiento / Densidad / CPUE, mismo patron de dos columnas que Fig6
## Ejes compartidos entre Cadiz y Valencia (calculados sobre ambas regiones)
## para que las columnas sean comparables visualmente.

fecha_rng    <- range(c(recr_idx$fecha, density_ts$fecha, cpue_monthly$fecha), na.rm = TRUE)
recruit_ymax <- max(recr_idx$RI * 100, na.rm = TRUE) * 1.08
dens_ymax    <- max(density_ts$dens_mean + density_ts$dens_se, na.rm = TRUE) * 1.08
cpue_ymax    <- max(cpue_monthly$cpue_mean + cpue_monthly$cpue_se, na.rm = TRUE) * 1.08

make_recruit_area <- function(area_nm, col) {
  df <- recr_idx |> filter(Area == area_nm)
  ggplot(df, aes(x = fecha, y = RI * 100)) +
    geom_col(fill = col, width = 12) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = fecha_rng) +
    coord_cartesian(ylim = c(0, recruit_ymax)) +
    labs(x = NULL, y = "Recruitment (%)", title = area_nm) +
    theme_reclam +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(face = "bold", hjust = 0.5))
}

make_dens_area <- function(area_nm, col) {
  df <- density_ts |> filter(Area == area_nm)
  ggplot(df, aes(x = fecha, y = dens_mean)) +
    geom_ribbon(aes(ymin = pmax(dens_mean - dens_se, 0), ymax = dens_mean + dens_se),
                fill = col, alpha = 0.2) +
    geom_line(colour = col, linewidth = 0.9) +
    geom_point(colour = col, size = 2) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = fecha_rng) +
    coord_cartesian(ylim = c(0, dens_ymax)) +
    labs(x = NULL, y = expression("Density (ind" %.% "m"^{-2}*")")) +
    theme_reclam + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_cpue_area <- function(area_nm, col) {
  df <- cpue_monthly |> filter(Area == area_nm)
  ggplot(df, aes(x = fecha, y = cpue_mean)) +
    geom_ribbon(aes(ymin = pmax(cpue_mean - cpue_se, 0), ymax = cpue_mean + cpue_se),
                fill = col, alpha = 0.2) +
    geom_line(colour = col, linewidth = 0.9) +
    geom_point(colour = col, size = 2) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = fecha_rng) +
    coord_cartesian(ylim = c(0, cpue_ymax)) +
    labs(x = NULL, y = expression("CPUE (kg" %.% "h"^{-1}*")")) +
    theme_reclam + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

panel_cadiz <- make_recruit_area("Cadiz", col_cadiz) /
               make_dens_area("Cadiz", col_cadiz)    /
               make_cpue_area("Cadiz", col_cadiz)

panel_valencia <- make_recruit_area("Valencia", col_valencia) /
                   make_dens_area("Valencia", col_valencia)    /
                   make_cpue_area("Valencia", col_valencia)

fig7 <- (panel_cadiz | panel_valencia) +
  plot_annotation(
    tag_levels = "a")

# inspeccionar
fig7

# guardar
ggsave(here("FIG", "Fig7_PopIndicators_ByRegion.jpeg"), fig7,
       width = 7, height = 9, dpi = fig_dpi)
