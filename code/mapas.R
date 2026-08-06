library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(marmap)        # batimetría
library(patchwork)

# ============================================================
# 1. DATOS BASE
# ============================================================

# Costa mundial (bordes RNaturalEarth)
world <- ne_countries(scale = "medium", returnclass = "sf")
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# ============================================================
# 2. BATIMETRÍA (NOAA via marmap)
# ============================================================

# Golfo de Cádiz
bat_cadiz <- getNOAA.bathy(
  lon1 = -9.5, lon2 = -5.5,
  lat1 = 35.5, lat2 = 38.0,
  resolution = 2
)

# Valencia
bat_valencia <- getNOAA.bathy(
  lon1 = -1.5, lon2 = 1.0,
  lat1 = 38.5, lat2 = 40.5,
  resolution = 2
)

# Convertir a data.frame para ggplot
bathy_to_df <- function(bat) {
  as.xyz(bat) |>
    as_tibble() |>
    setNames(c("lon", "lat", "depth")) |>
    filter(depth < 0)   # solo mar
}

df_cadiz   <- bathy_to_df(bat_cadiz)
df_valencia <- bathy_to_df(bat_valencia)

# ============================================================
# 3. RÍOS
# ============================================================
# Cádiz (Guadalquivir, Guadiana, Tinto, Odiel): TODOS desde la red
# hidrográfica local de la Junta de Andalucía (red_hidrografica.shp).
# Ya no se llama a rivers_global (rivers_lake_centerlines).
#
# Valencia (Júcar, Turia): esa cuenca no está cubierta por el
# shapefile de Andalucía, así que se mantiene el suplemento regional
# "rivers_europe" (10m) de Natural Earth solo para esos dos ríos.

rivers_europe <- ne_download(scale = 10, type = "rivers_europe",
                              category = "physical", returnclass = "sf")

rios_valencia <- rivers_europe |>
  dplyr::select(name) |>
  dplyr::filter(str_detect(name, "Jucar|Júcar|Turia"))

# ============================================================
# 3c. RÍOS LOCALES (Junta de Andalucía) — Guadalquivir, Guadiana, Tinto, Odiel
# ============================================================

hidro_and <- st_read("~/IEO/Maps/Shapefiles_Andalucia/red_hidrografica.shp") |>
  st_transform(4326)

# NOMBRE confirmado como campo de nombre (diagnóstico previo para
# Tinto/Odiel). Para Guadalquivir/Guadiana aún no está confirmado el
# string exacto -- si el filtro de abajo no trae nada (o trae de más),
# corre esto y pégame el resultado:
#
# hidro_and |>
#   sf::st_drop_geometry() |>
#   dplyr::pull(NOMBRE) |>
#   unique() |>
#   (\(x) x[str_detect(x, "(?i)guadalquivir|guadiana")])() |>
#   print()

rios_cadiz_excluir <- c("Barranco de Guadiana", "Guadiana Menor", "Antiguo Cauce")

rios_cadiz <- hidro_and |>
  dplyr::filter(str_detect(NOMBRE, regex("Guadalquivir|Guadiana|Río Odiel|Río Tinto", ignore_case = TRUE))) |>
  dplyr::filter(!str_detect(NOMBRE, str_c(rios_cadiz_excluir, collapse = "|"))) |>
  dplyr::rename(name = NOMBRE) |>
  dplyr::select(name)

# Punto etiqueta por río: se toma el punto medio de cada línea
# (útil para ubicar el nombre del río con geom_label_repel).
# st_line_sample no funciona en coordenadas geográficas (lon/lat) ni
# sobre MULTILINESTRING, así que se: (1) proyecta a ETRS89-LAEA
# (EPSG:3035), (2) se "explota" a LINESTRING simple (st_cast), (3) se
# muestrea el punto medio de cada segmento, y (4) se conserva un solo
# punto por río (el del segmento más largo) para la etiqueta.

label_por_rio <- function(rios_sf) {
  rios_sf |>
    st_transform(3035) |>
    st_cast("MULTILINESTRING") |>
    st_cast("LINESTRING") |>
    dplyr::mutate(largo = st_length(geometry)) |>
    dplyr::group_by(name) |>
    dplyr::slice_max(largo, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(geometry = st_line_sample(geometry, sample = 0.5) |> st_cast("POINT")) |>
    dplyr::select(name) |>
    st_transform(4326)
}

rios_cadiz_lbl    <- label_por_rio(rios_cadiz)
rios_valencia_lbl <- label_por_rio(rios_valencia)

# ============================================================
# 3b. PUNTOS DE MUESTREO
# ============================================================
# Cádiz: Puntos 1, 3 y 5 (coordenadas en grados-minutos decimales).
# Valencia: sitios B y D (coordenadas en grados-minutos-segundos;
# se promedian las réplicas NC/C -> en este caso son idénticas,
# por lo que el promedio equivale al valor único de cada sitio).

dm_to_dec <- function(deg, min) deg + min / 60
dms_to_dec <- function(deg, min, sec) deg + min / 60 + sec / 3600

puntos_cadiz <- tibble(
  Zona  = "Cadiz",
  Punto = c("1", "3", "5"),
  Nombre = c("Punta del Cabo", "Zalabar", "Torre Carbonero"),
  lat = c(dm_to_dec(36, 47.986), dm_to_dec(36, 52.231), dm_to_dec(36, 55.032)),
  lon = -c(dm_to_dec(6, 23.703), dm_to_dec(6, 25.801), dm_to_dec(6, 27.512))
)

# Valencia: promedio de réplicas por sitio (idénticas en este caso)
valencia_raw <- tibble(
  Zona  = "Valencia",
  Punto = c("D", "D", "D", "B", "B", "B"),
  lat_dms = list(c(38, 58, 55.49), c(38, 58, 55.49), c(38, 58, 55.49),
                 c(39, 6, 45.15), c(39, 6, 45.15), c(39, 6, 45.15)),
  lon_dms = list(c(0, 8, 44.39), c(0, 8, 44.39), c(0, 8, 44.39),
                 c(0, 13, 33.22), c(0, 13, 33.22), c(0, 13, 33.22))
) |>
  dplyr::mutate(
    lat = purrr::map_dbl(lat_dms, ~ dms_to_dec(.x[1], .x[2], .x[3])),
    lon = -purrr::map_dbl(lon_dms, ~ dms_to_dec(.x[1], .x[2], .x[3]))
  )

puntos_valencia <- valencia_raw |>
  dplyr::group_by(Zona, Punto) |>
  dplyr::summarise(lat = mean(lat), lon = mean(lon), .groups = "drop")

puntos_cadiz_sf    <- st_as_sf(puntos_cadiz, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
puntos_valencia_sf <- st_as_sf(puntos_valencia, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# ============================================================
# 4. POLÍGONOS DE ESTUDIO
# ============================================================

# Golfo de Cádiz — zona de coquina
poly_cadiz <- st_polygon(list(matrix(c(
  -7.5, 36.5,
  -6.0, 36.5,
  -6.0, 37.25,
  -7.5, 37.25,
  -7.5, 36.5
), ncol = 2, byrow = TRUE))) |>
  st_sfc(crs = 4326) |>
  st_sf()

# Valencia — zona de coquina
poly_valencia <- st_polygon(list(matrix(c(
  -0.5, 38.8,
  0.3, 38.8,
  0.3, 39.6,
  -0.5, 39.6,
  -0.5, 38.8
), ncol = 2, byrow = TRUE))) |>
  st_sfc(crs = 4326) |>
  st_sf()

# ============================================================
# 5. MAPA GENERAL (inset)
# ============================================================

map_general <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey40", linewidth = 0.3) +
  # Rectángulos de referencia
  annotate("rect",
           xmin = -9.5, xmax = -5.5, ymin = 35.5, ymax = 38.0,
           fill = NA, color = "black", linewidth = 0.6) +
  annotate("rect",
           xmin = -1.5, xmax = 1.0, ymin = 38.5, ymax = 40.5,
           fill = NA, color = "black", linewidth = 0.6) +
  coord_sf(xlim = c(-10, 5), ylim = c(34, 45)) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# ============================================================
# 6. PANEL GOLFO DE CÁDIZ
# ============================================================

isobaths_cadiz <- c(-25, -50, -100, -200)

panel_cadiz <- ggplot() +
  # Batimetría en azul
  geom_contour(data = df_cadiz,
               aes(x = lon, y = lat, z = depth),
               breaks = isobaths_cadiz,
               color = "steelblue4", linewidth = 0.3) +
  # Costa
  geom_sf(data = world, fill = "grey70", color = "grey40", linewidth = 0.4, alpha = 0.4) +
  # Ríos analizados (Guadalquivir, Guadiana, Tinto, Odiel)
  geom_sf(data = rios_cadiz, color = "steelblue", linewidth = 0.5) +
  ggrepel::geom_text_repel(
    data = rios_cadiz_lbl,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.3, colour = "steelblue4", fontface = "italic",
    segment.colour = "steelblue4", segment.size = 0.2,
    box.padding = 0.4, max.overlaps = 20
  ) +
  # Polígono de estudio
  geom_sf(data = poly_cadiz, fill = NA, color = "black",
          linewidth = 0.7, linetype = "solid") +
  # Puntos de muestreo (1, 3, 5)
  geom_sf(data = puntos_cadiz_sf, shape = 21, fill = "red",
          color = "black", size = 2.2, stroke = 0.4) +
  ggrepel::geom_label_repel(
    data = puntos_cadiz_sf,
    aes(label = Punto, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5, colour = "black", segment.colour = "black",
    box.padding = 0.6, max.overlaps = 20
  ) +
  # Etiquetas isobatas
  metR::geom_text_contour(data = df_cadiz,
                          aes(x = lon, y = lat, z = depth),
                          breaks = isobaths_cadiz,
                          size = 2.5, color = "steelblue4", skip = 0) +
  coord_sf(xlim = c(-9.5, -5.5), ylim = c(35.5, 38.0)) +
  labs(x = NULL, y = NULL, title = "Gulf of Cádiz") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#E6F3FA", color = NA),
    plot.title = element_text(size = 9, face = "bold")
  )

# ============================================================
# 7. PANEL VALENCIA
# ============================================================

panel_valencia <- ggplot() +
  geom_contour(data = df_valencia,
               aes(x = lon, y = lat, z = depth),
               breaks = isobaths_cadiz,
               color = "steelblue4", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey70", color = "grey40", linewidth = 0.4, alpha = 0.4) +
  # Ríos analizados (Júcar, Turia)
  geom_sf(data = rios_valencia, color = "steelblue", linewidth = 0.5) +
  ggrepel::geom_text_repel(
    data = rios_valencia_lbl,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.3, colour = "steelblue4", fontface = "italic",
    segment.colour = "steelblue4", segment.size = 0.2,
    box.padding = 0.4, max.overlaps = 20
  ) +
  geom_sf(data = poly_valencia, fill = NA, color = "black",
          linewidth = 0.7, linetype = "solid") +
  # Puntos de muestreo (B, D)
  geom_sf(data = puntos_valencia_sf, shape = 21, fill = "red",
          color = "black", size = 2.2, stroke = 0.4) +
  ggrepel::geom_label_repel(
    data = puntos_valencia_sf,
    aes(label = Punto, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5, colour = "black", segment.colour = "black",
    box.padding = 0.6, max.overlaps = 20
  ) +
  metR::geom_text_contour(data = df_valencia,
                          aes(x = lon, y = lat, z = depth),
                          breaks = isobaths_cadiz,
                          size = 2.5, color = "steelblue4", skip = 0) +
  coord_sf(xlim = c(-1.5, 1.0), ylim = c(38.5, 40.5)) +
  labs(x = NULL, y = NULL, title = "Valencia") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#E6F3FA", color = NA),
    plot.title = element_text(size = 9, face = "bold")
  )

# ============================================================
# 8. COMPOSICIÓN FINAL
# ============================================================

final_map <- (panel_cadiz | panel_valencia) /
  wrap_elements(map_general) +
  plot_layout(heights = c(3, 1))


# Guardar
ggsave(
  plot     = final_map,
  filename = "FIG/map_study_areas.png",
  width    = 8,
  height   = 6,
  dpi      = 300
)
