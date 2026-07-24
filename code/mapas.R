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

# Costa mundial
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
# 3. POLÍGONOS DE ESTUDIO
# ============================================================

# Golfo de Cádiz — zona de coquina
poly_cadiz <- st_polygon(list(matrix(c(
  -7.5, 36.8,
  -6.0, 36.8,
  -6.0, 37.5,
  -7.5, 37.5,
  -7.5, 36.8
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
# 4. SÍMBOLO COQUINA (polígono simple tipo almeja)
# ============================================================

donax_shape <- function(cx, cy, size = 0.15) {
  # Silueta estilizada de Donax trunculus (triángulo redondeado)
  theta <- seq(0, pi, length.out = 50)
  x <- cx + size * cos(theta) * 0.8
  y <- cy + size * sin(theta) * 0.5
  # Cierra la forma
  x <- c(x, cx - size * 0.8, cx + size * 0.8)
  y <- c(y, cy, cy)
  data.frame(x = x, y = y)
}

# Posiciones de los símbolos (ajusta según tus zonas)
donax_cadiz   <- donax_shape(-6.8, 36.95, size = 0.12)
donax_valencia <- donax_shape(-0.1, 39.1,  size = 0.12)

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
  # Batimetría en grises
  geom_contour(data = df_cadiz,
               aes(x = lon, y = lat, z = depth),
               breaks = isobaths_cadiz,
               color = "grey60", linewidth = 0.3) +
  # Costa
  geom_sf(data = world, fill = "grey85", color = "grey30", linewidth = 0.4) +
  # Polígono de estudio
  geom_sf(data = poly_cadiz, fill = NA, color = "black",
          linewidth = 0.7, linetype = "solid") +
  # Símbolo coquina
  geom_polygon(data = donax_cadiz, aes(x = x, y = y),
               fill = "grey20", color = "black", linewidth = 0.3) +
  # Etiquetas isobatas
  metR::geom_text_contour(data = df_cadiz,
                          aes(x = lon, y = lat, z = depth),
                          breaks = isobaths_cadiz,
                          size = 2.5, color = "grey50", skip = 0) +
  coord_sf(xlim = c(-9.5, -5.5), ylim = c(35.5, 38.0)) +
  labs(x = NULL, y = NULL, title = "Gulf of Cádiz") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 9, face = "bold")
  )

# ============================================================
# 7. PANEL VALENCIA
# ============================================================

panel_valencia <- ggplot() +
  geom_contour(data = df_valencia,
               aes(x = lon, y = lat, z = depth),
               breaks = isobaths_cadiz,
               color = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey85", color = "grey30", linewidth = 0.4) +
  geom_sf(data = poly_valencia, fill = NA, color = "black",
          linewidth = 0.7, linetype = "solid") +
  geom_polygon(data = donax_valencia, aes(x = x, y = y),
               fill = "grey20", color = "black", linewidth = 0.3) +
  metR::geom_text_contour(data = df_valencia,
                          aes(x = lon, y = lat, z = depth),
                          breaks = isobaths_cadiz,
                          size = 2.5, color = "grey50", skip = 0) +
  coord_sf(xlim = c(-1.5, 1.0), ylim = c(38.5, 40.5)) +
  labs(x = NULL, y = NULL, title = "Valencia") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
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
