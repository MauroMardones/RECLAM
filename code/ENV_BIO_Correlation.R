# ENV_BIO_Correlation.R
# Environmental drivers of Donax trunculus population indicators
# Gulf of Cádiz (GoC) vs. Gulf of Valencia (GoV)
# Project RECLAM — IEO-CSIC / UPV
#
# Analyses:
#   0.  Setup
#   1.  Biological data loading + monthly aggregation (substrate: COMPARA_RECLAM.R)
#   2.  Environmental data loading (env_monthly_all.csv, 2014–2025)
#   3.  Join + temporal alignment (overlap dinamico, ver OVERLAP_END en seccion 3)
#   4.  Historical context: env climatology + sampling window
#   5.  Spearman correlations at lag 0, 1, 2 + FDR correction
#   6.  PCA of environmental space + biological overlay
#   7.  GAM drivers of population indicators, por region
#   8.  Integrated time series figure (env + bio concurrent panel)
#   9.  Export tables and figures
#
# NOTE: Cobertura de datos (ver mensajes de consola al correr el script para
#       el rango exacto vigente):
#       Biological data: Cadiz Jul 2024 – Sep 2025 (sin huecos); Valencia
#       Jul 2024 – Sep 2025 (huecos: mar-2025, ago-2025).
#       Environmental data: Jan 2014 – Dic 2025.
#       OVERLAP_END = min(ultimo mes bio, ultimo mes env) -- se recalcula
#       solo, no esta hardcodeado. n por region puede diferir (ver Cadiz vs.
#       Valencia) por los huecos de muestreo bio.
#       Todas las correlaciones siguen siendo EXPLORATORIAS -- interpretar
#       con cautela, aunque el n mejoro sustancialmente vs. la version
#       original (Jul-Dic 2024 solamente, n=5-6/region).
#
# Author  : Mauricio Mardones
# Updated : 2026-07

# ─────────────────────────────────────────────────────────────────────────────
# 0. SETUP -------
# ─────────────────────────────────────────────────────────────────────────────

rm(list = ls())
options(scipen = 999)
set.seed(2025)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(here)
  library(janitor)
  library(patchwork)
  library(viridis)
  library(ggrepel)
  library(corrplot)    # correlation matrix visualization
  library(FactoMineR)  # PCA
  library(factoextra)  # PCA visualization
  library(mgcv)        # GAM
  library(vegan)       # PERMANOVA / PERMDISP (instalar con install.packages("vegan") si falta)
})

theme_reclam <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "grey92", colour = NA),
    legend.position   = "bottom",
    legend.key.size   = unit(0.4, "cm"),
    plot.title        = element_text(face = "bold", size = 11),
    axis.title        = element_text(size = 10)
  )

pal_area <- c(Cadiz = "#1B7FC4", Valencia = "#D94F00")

# Etiquetas legibles para indicadores poblacionales (usado en PCA 6c y en
# efectos parciales GAM, seccion 7.3)
bio_label_lookup <- c(Lmean = "L̄", L_p90 = "L_P90", RI = "RI",
                      CPUE = "CPUE", Densidad = "Densidad")

dir.create(here("FIG"),     showWarnings = FALSE)
dir.create(here("RESULTS"), showWarnings = FALSE)

# Temporal overlap constants
# OVERLAP_START es fijo (inicio del diseño de muestreo). OVERLAP_END NO se
# hardcodea -- se calcula en la seccion 3 como el minimo entre el ultimo mes
# con dato bio y el ultimo mes con dato ambiental disponibles, para que el
# script se ajuste solo a medida que se agreguen mas meses de datos (ver
# historial: "2024-12-01" excluia diciembre 2024 por error; luego se
# actualizo env_monthly_all.csv con datos hasta 2025 y el bio ya cubre hasta
# sep-2025 -- fijar la fecha a mano quedaria obsoleto de nuevo).
OVERLAP_START <- as.Date("2024-07-01")


# ─────────────────────────────────────────────────────────────────────────────
# 1. BIOLOGICAL DATA — monthly aggregation
#    Source: Template_Reclam_Cadiz.xlsx / Template_Reclam_Valencia.xlsx
#    Substrate: data loading conventions from COMPARA_RECLAM.R
# ─────────────────────────────────────────────────────────────────────────────

archivo_cadiz    <- here("DATA", "Data_Final", "Template_Reclam_Cadiz.xlsx")
archivo_valencia <- here("DATA", "Data_Final", "Template_Reclam_Valencia.xlsx")

stopifnot(
  "Template Cadiz not found"    = file.exists(archivo_cadiz),
  "Template Valencia not found" = file.exists(archivo_valencia)
)

## 1.1 Size data (coquina only) ------------------------------------------------

tallac <- read_excel(archivo_cadiz, sheet = "tallas") |>
  dplyr::select(-any_of("ZONA")) |>
  filter(!is.na(TALLA)) |>
  mutate(TALLA = as.numeric(TALLA),
         PUNTO = as.character(PUNTO),
         FECHA = as.Date(FECHA, origin = "1899-12-30"),
         Area  = "Cadiz")

tallav <- read_excel(archivo_valencia, sheet = "tallas") |>
  mutate(TALLA = as.numeric(TALLA),
         FECHA = as.Date(FECHA),
         Area  = "Valencia")

tallas <- bind_rows(tallac, tallav) |>
  filter(!is.na(TALLA), TALLA > 0) |>
  filter(tolower(especie) == "co") |>              # coquina only
  mutate(year  = year(FECHA),
         month = month(FECHA))

## 1.2 Monthly biological indicators ------------------------------------------
# Lmean, L_p90, RI — N ya no se usa como indicador (era proxy de densidad
# sin estandarizar por area; reemplazado por Densidad real en 1.3). Se
# conserva la columna solo como tamaño de muestra para el panel de la
# seccion 7 (n de individuos medidos detras de cada Lmean mensual).

bio_monthly <- tallas |>
  group_by(Area, year, month) |>
  summarise(
    Lmean = mean(TALLA, na.rm = TRUE),
    L_p90 = quantile(TALLA, 0.90, na.rm = TRUE),
    RI    = mean(TALLA <= 25, na.rm = TRUE),   # proporción reclutas (SL ≤ 8 mm)
    N     = n(),                               # tamaño de muestra (no indicador de densidad)
    .groups = "drop"
  ) |>
  mutate(date   = as.Date(paste(year, month, "15", sep = "-")),
         region = tolower(Area))

cat(sprintf("Bio data: %d region-months (Cadiz: %d, Valencia: %d)\n",
            nrow(bio_monthly),
            sum(bio_monthly$Area == "Cadiz"),
            sum(bio_monthly$Area == "Valencia")))

## 1.3 CPUE y Densidad (calculados en COMPARA_RECLAM2.R, seccion 8.6) ---------
# Requiere haber corrido COMPARA_RECLAM2.R antes -- genera
# RESULTS/Table_Density_CPUE_Monthly.csv con Area/year/month/dens_mean/cpue_mean

indicators_path <- here("RESULTS", "Table_Density_CPUE_Monthly.csv")
stopifnot("Table_Density_CPUE_Monthly.csv not found -- correr COMPARA_RECLAM2.R primero" =
            file.exists(indicators_path))

cpue_dens_monthly <- read_csv(indicators_path, show_col_types = FALSE,
                               col_types = cols(dens_mean = col_double(),
                                                 dens_se   = col_double(),
                                                 cpue_mean = col_double(),
                                                 cpue_se   = col_double())) |>
  mutate(region = tolower(Area)) |>
  dplyr::select(region, year, month, Densidad = dens_mean, CPUE = cpue_mean)

bio_monthly <- bio_monthly |>
  left_join(cpue_dens_monthly, by = c("region", "year", "month"))

cat(sprintf("CPUE/Densidad unidos: %d/%d meses con CPUE, %d/%d meses con Densidad\n",
            sum(!is.na(bio_monthly$CPUE)),     nrow(bio_monthly),
            sum(!is.na(bio_monthly$Densidad)), nrow(bio_monthly)))


# ─────────────────────────────────────────────────────────────────────────────
# 2. ENVIRONMENTAL DATA
# ─────────────────────────────────────────────────────────────────────────────

env_path <- here("DATA", "Environmental_Data", "env_monthly_all.csv")
stopifnot("env_monthly_all.csv not found" = file.exists(env_path))

env <- read_csv(env_path, show_col_types = FALSE) |>
  mutate(date   = as.Date(date),
         region = tolower(region))   # harmonise to lowercase

env_vars <- c("sst_mean", "sst_anom", "wind_speed", "wind_anom",
              "runoff_mm", "runoff_anom", "mhw_days")

# Variante sin anomalias -- usada en las secciones 5b y 6b (Spearman/PCA
# solo con la variable base, sin sst_anom/wind_anom/runoff_anom)
env_vars_noanom <- c("sst_mean", "wind_speed", "runoff_mm", "mhw_days")

cat(sprintf("Env data: %d rows | %d–%d | regions: %s\n",
            nrow(env), min(env$year), max(env$year),
            paste(unique(env$region), collapse = ", ")))


# ─────────────────────────────────────────────────────────────────────────────
# 3. JOIN + TEMPORAL ALIGNMENT
# ─────────────────────────────────────────────────────────────────────────────

# Full join (all bio months — env sera NA en los meses donde no haya
# cobertura ambiental)
joined_full <- bio_monthly |>
  left_join(env |> dplyr::select(-date), by = c("region", "year", "month"))

# OVERLAP_END dinamico: el mas restrictivo entre el ultimo mes bio y el
# ultimo mes ambiental disponibles (evita hardcodear una fecha que quede
# obsoleta cuando se agreguen nuevos meses a cualquiera de los dos datasets)
OVERLAP_END <- min(max(bio_monthly$date, na.rm = TRUE),
                    max(env$date, na.rm = TRUE))

cat(sprintf("Ventana de overlap: %s a %s (bio hasta %s, env hasta %s)\n",
            OVERLAP_START, OVERLAP_END,
            max(bio_monthly$date, na.rm = TRUE), max(env$date, na.rm = TRUE)))

# Concurrent overlap (n puede diferir por region si hay meses bio faltantes,
# ej. Valencia con huecos -- ver mensaje de conteo mas abajo)
joined_overlap <- joined_full |>
  filter(!is.na(sst_mean)) |>
  filter(date >= OVERLAP_START, date <= OVERLAP_END)

cat(sprintf("\nConcurrent overlap: %s to %s\n",
            OVERLAP_START, OVERLAP_END))
cat(sprintf("n per region: %s\n",
            paste(table(joined_overlap$Area), collapse = " / ")))

# Lagged environmental variables (lag 1 and 2 months)
# Env leads bio by 1 or 2 months: match bio[month t] with env[month t-1] or t-2

add_env_lags <- function(env_df, bio_df, lag_months) {
  env_lagged <- env_df |>
    mutate(month_bio = month + lag_months,
           year_bio  = year + (month_bio > 12),
           month_bio = if_else(month_bio > 12, month_bio - 12, month_bio)) |>
    dplyr::select(region, year_bio, month_bio, all_of(env_vars)) |>  # nombres originales
    rename(year = year_bio, month = month_bio) |>
    rename_with(~ paste0(., "_lag", lag_months), all_of(env_vars))   # añade sufijo

  bio_df |> left_join(env_lagged, by = c("region", "year", "month"))
}

joined_lag1 <- add_env_lags(env, joined_overlap, 1)
joined_lag2 <- add_env_lags(env, joined_overlap, 2)

# Master lag dataset
joined_lags <- joined_overlap |>
  left_join(
    joined_lag1 |> dplyr::select(region, year, month,
                                   ends_with("_lag1")),
    by = c("region", "year", "month")
  ) |>
  left_join(
    joined_lag2 |> dplyr::select(region, year, month,
                                   ends_with("_lag2")),
    by = c("region", "year", "month")
  )



sampling_years  <- unique(c(year(OVERLAP_START), year(OVERLAP_END)))
sampling_label  <- paste0(format(OVERLAP_START, "%b %Y"), "–", format(OVERLAP_END, "%b %Y"))

env_context <- env |>
  mutate(Area = str_to_title(region))

## 4.1 SST — serie completa con ventana de muestreo resaltada -----------------

fig_context_sst <- ggplot(env_context, aes(x = date, y = sst_mean, colour = Area)) +
  geom_rect(data = tibble(xmin = OVERLAP_START, xmax = OVERLAP_END,
                           ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  scale_colour_manual(values = pal_area) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "SST (°C)", colour = "Region",
       subtitle = sprintf("Sombreado = ventana de muestreo (%s)", sampling_label)) +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## 4.2 MHW days — serie completa -----------------------------------------------

fig_context_mhw <- ggplot(env_context, aes(x = date, y = mhw_days)) +
  geom_rect(data = tibble(xmin = OVERLAP_START, xmax = OVERLAP_END,
                           ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_col(aes(fill = Area), position = "identity", alpha = 0.6, width = 25) +
  scale_fill_manual(values = pal_area) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "MHW days / month", fill = "Region") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## 4.3 Panel combinado ----------------------------------------------------------

fig_context <- fig_context_sst / fig_context_mhw +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(here("FIG", "FigS_ENV_Context.jpeg"), fig_context, width = 13, height = 8, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 5. SPEARMAN CORRELATIONS — lag 0, 1, 2
#    IMPORTANT: n por region ahora depende del overlap dinamico (ver
#    seccion 3, OVERLAP_END) -- puede diferir entre Cadiz y Valencia si hay
#    huecos de muestreo bio. El desfase de 1-2 meses no reduce n porque
#    env_monthly_all.csv cubre 2014-2025, mas atras que cualquier bio mes.
#    Todos los resultados son EXPLORATORIOS — reportar rho y p pero
#    interpretar con cautela
# ─────────────────────────────────────────────────────────────────────────────

bio_indicators <- c("Lmean", "L_p90", "RI", "CPUE", "Densidad")

## 5.1 Spearman rho function ---------------------------------------------------

spearman_safe <- function(x, y) {
  idx <- complete.cases(x, y)
  if (sum(idx) < 4) return(tibble(rho = NA_real_, p = NA_real_, n = sum(idx)))
  ct <- cor.test(x[idx], y[idx], method = "spearman", exact = FALSE)
  tibble(rho = ct$estimate, p = ct$p.value, n = sum(idx))
}

## 5.2 Run correlations across lags, regions, var pairs -----------------------

run_corr_block <- function(data_df, lag_suffix, area_nm) {
  ev <- if (lag_suffix == "lag0") env_vars else paste0(env_vars, "_", lag_suffix)
  ev_names <- env_vars  # human-readable label always uses base name

  map_dfr(bio_indicators, function(bio_v) {
    map_dfr(seq_along(ev), function(i) {
      x <- data_df[[ev[i]]]
      y <- data_df[[bio_v]]
      spearman_safe(x, y) |>
        mutate(bio_var = bio_v,
               env_var = ev_names[i],
               lag     = lag_suffix,
               Area    = area_nm)
    })
  })
}

corr_results <- bind_rows(
  # Cádiz
  run_corr_block(joined_lags |> filter(Area == "Cadiz"),  "lag0",  "Cadiz"),
  run_corr_block(joined_lag1 |> filter(Area == "Cadiz"),  "lag1",  "Cadiz"),
  run_corr_block(joined_lag2 |> filter(Area == "Cadiz"),  "lag2",  "Cadiz"),
  # Valencia
  run_corr_block(joined_lags |> filter(Area == "Valencia"), "lag0", "Valencia"),
  run_corr_block(joined_lag1 |> filter(Area == "Valencia"), "lag1", "Valencia"),
  run_corr_block(joined_lag2 |> filter(Area == "Valencia"), "lag2", "Valencia")
)

## 5.3 FDR correction (Benjamini-Hochberg) ------------------------------------

corr_results <- corr_results |>
  group_by(Area, lag) |>
  mutate(p_adj = p.adjust(p, method = "BH")) |>
  ungroup() |>
  mutate(
    sig = case_when(
      is.na(p_adj)    ~ "",
      p_adj < 0.05    ~ "*",
      p_adj < 0.10    ~ "†",
      TRUE            ~ ""
    ),
    rho_label = sprintf("%.2f%s", rho, sig)
  )

write_csv(corr_results, here("RESULTS", "Table_Spearman_ENV_BIO.csv"))

## 5.4 Heatmap — rho by env_var × bio_var, facet Area × lag ------------------

fig_corr_heat <- corr_results |>
  # ya no se filtran los NA -- se mantienen para dibujar la celda gris + "n/a"
  # (indeterminado por falta de varianza en la ventana de lag, no dato faltante)
  mutate(
    bio_var = factor(bio_var, levels = bio_indicators,
                     labels = c("L̄ (mm)", "L_P90 (mm)", "RI (%)",
                                "CPUE (kg·h⁻¹)", "Densidad (ind·m⁻²)")),
    env_var = factor(env_var, levels = env_vars,
                     labels = c("SST", "SST_anom", "Wind", "Wind_anom",
                                "Runoff", "Runoff_anom", "MHW_days")),
    lag     = factor(lag, levels = c("lag0", "lag1", "lag2"),
                     labels = c("Lag 0", "Lag 1 mo", "Lag 2 mo")),
    label_display = if_else(is.na(rho), "n/a", rho_label)
  ) |>
  ggplot(aes(x = bio_var, y = env_var, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label_display), size = 2.8, colour = "grey10") +
  facet_grid(lag ~ Area) +
  scale_fill_gradient2(low  = "#d8b365", mid = "white", high = "#5ab4ac",
                       midpoint = 0, limits = c(-1, 1), na.value = "grey85",
                       name = "Spearman ρ") +
  labs(x = "", y = "Environmental variable",
       subtitle = "n/a = varianza insuficiente en la ventana de lag (correlación indeterminada)") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right")

ggsave(here("FIG", "Fig_Spearman_Heatmap.jpeg"),
       fig_corr_heat, width = 8, height = 8, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 5b. SPEARMAN CORRELATIONS — SIN ANOMALIAS (solo variable base)
#     Mismo procedimiento de 5.1-5.4 pero con env_vars_noanom.
# ─────────────────────────────────────────────────────────────────────────────

run_corr_block_noanom <- function(data_df, lag_suffix, area_nm) {
  ev <- if (lag_suffix == "lag0") env_vars_noanom else paste0(env_vars_noanom, "_", lag_suffix)
  ev_names <- env_vars_noanom

  map_dfr(bio_indicators, function(bio_v) {
    map_dfr(seq_along(ev), function(i) {
      x <- data_df[[ev[i]]]
      y <- data_df[[bio_v]]
      spearman_safe(x, y) |>
        mutate(bio_var = bio_v,
               env_var = ev_names[i],
               lag     = lag_suffix,
               Area    = area_nm)
    })
  })
}

corr_results_noanom <- bind_rows(
  run_corr_block_noanom(joined_lags |> filter(Area == "Cadiz"),  "lag0",  "Cadiz"),
  run_corr_block_noanom(joined_lag1 |> filter(Area == "Cadiz"),  "lag1",  "Cadiz"),
  run_corr_block_noanom(joined_lag2 |> filter(Area == "Cadiz"),  "lag2",  "Cadiz"),
  run_corr_block_noanom(joined_lags |> filter(Area == "Valencia"), "lag0", "Valencia"),
  run_corr_block_noanom(joined_lag1 |> filter(Area == "Valencia"), "lag1", "Valencia"),
  run_corr_block_noanom(joined_lag2 |> filter(Area == "Valencia"), "lag2", "Valencia")
) |>
  group_by(Area, lag) |>
  mutate(p_adj = p.adjust(p, method = "BH")) |>
  ungroup() |>
  mutate(
    sig = case_when(
      is.na(p_adj)    ~ "",
      p_adj < 0.05    ~ "*",
      p_adj < 0.10    ~ "†",
      TRUE            ~ ""
    ),
    rho_label = sprintf("%.2f%s", rho, sig)
  )

write_csv(corr_results_noanom, here("RESULTS", "Table_Spearman_ENV_BIO_SinAnom.csv"))

fig_corr_heat_noanom <- corr_results_noanom |>
  mutate(
    bio_var = factor(bio_var, levels = bio_indicators,
                     labels = c("L̄ (mm)", "L_P90 (mm)", "RI (%)",
                                "CPUE (kg·h⁻¹)", "Densidad (ind·m⁻²)")),
    env_var = factor(env_var, levels = env_vars_noanom,
                     labels = c("SST", "Wind", "Runoff", "MHW_days")),
    lag     = factor(lag, levels = c("lag0", "lag1", "lag2"),
                     labels = c("Lag 0", "Lag 1 mo", "Lag 2 mo")),
    label_display = if_else(is.na(rho), "n/a", rho_label)
  ) |>
  ggplot(aes(x = bio_var, y = env_var, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label_display), size = 2.8, colour = "grey10") +
  facet_grid(lag ~ Area) +
  scale_fill_gradient2(low  = "#d8b365", mid = "white", high = "#5ab4ac",
                       midpoint = 0, limits = c(-1, 1), na.value = "grey85",
                       name = "Spearman ρ") +
  labs(x = "", y = "Environmental variable",
       subtitle = "Sin anomalias | n/a = varianza insuficiente en la ventana de lag") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right")

ggsave(here("FIG", "Fig_Spearman_Heatmap_SinAnom.jpeg"),
       fig_corr_heat_noanom, width = 8, height = 8, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 6. PCA — ENVIRONMENTAL SPACE (env_monthly_all.csv completo) + SAMPLING OVERLAY
# ─────────────────────────────────────────────────────────────────────────────
# PCA usa todos los anios de env disponibles para definir el espacio.
# El periodo de muestreo bio (OVERLAP_START-OVERLAP_END, dinamico, puede
# abarcar 2+ anios calendario) se proyecta como puntos suplementarios.
# NOTA: antes se marcaba "Sampling" solo con year==2024 -- con el muestreo
# extendido hasta sep-2025 eso dejaba afuera mas de la mitad de los puntos
# reales. Se corrigio a comparar contra la fecha real de overlap.

## 6.1 Prepare env matrix ------------------------------------------------------

env_pca_mat <- env |>
  dplyr::select(region, year, month, date, all_of(env_vars)) |>
  drop_na() |>
  mutate(period = if_else(date >= OVERLAP_START & date <= OVERLAP_END, "Sampling", "Background"),
         Area   = str_to_title(region),
         label  = paste0(str_sub(month.abb[month], 1, 1), str_sub(year, 3, 4)))

pca_vars <- env_pca_mat |> dplyr::select(all_of(env_vars))

## 6.2 PCA (prcomp, scaled) ----------------------------------------------------

pca_fit <- prcomp(pca_vars, scale. = TRUE, center = TRUE)

pca_scores <- as_tibble(pca_fit$x[, 1:3]) |>
  bind_cols(env_pca_mat |> dplyr::select(region, year, month, period, Area, label))

var_exp <- round(summary(pca_fit)$importance[2, 1:3] * 100, 1)

## 6.3 Add biological indicators to sampling points ---------------------------

pca_bio <- pca_scores |>
  filter(period == "Sampling") |>
  left_join(bio_monthly |> dplyr::select(region, year, month, Lmean, RI),
            by = c("region", "year", "month"))

## 6.4 Biplot PC1 × PC2 -------------------------------------------------------

loadings_df <- as_tibble(pca_fit$rotation[, 1:2],
                          rownames = "variable") |>
  mutate(variable_short = c("SST", "SST_an", "Wind", "Wind_an",
                             "Run", "Run_an", "MHW_d"))

scale_arrow <- 3   # scale arrows to data range

fig_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  # Background cloud (10 yr)
  geom_point(data = . %>% filter(period == "Background"),
             aes(colour = Area), alpha = 0.3, size = 1.5) +
  # Sampling period — larger + filled
  geom_point(data = pca_bio,
             aes(colour = Area, size = Lmean), alpha = 0.9, shape = 21,
             fill = "white", stroke = 1.5) +
  geom_text_repel(data = pca_bio,
                  aes(label = label, colour = Area),
                  size = 3, max.overlaps = 20) +
  # Loadings arrows
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_arrow,
                   yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_df,
            aes(x = PC1 * scale_arrow * 1.15,
                y = PC2 * scale_arrow * 1.15,
                label = variable_short),
            size = 3, colour = "grey20") +
  scale_colour_manual(values = pal_area) +
  scale_size_continuous(name = "L̄ (mm)", range = c(3, 8)) +
  labs(x = sprintf("PC1 (%s%%)", var_exp[1]),
       y = sprintf("PC2 (%s%%)", var_exp[2]),
       colour = "Region",
       title  = sprintf("PCA environmental space — %d–%d", min(env$year), max(env$year)),
       subtitle = paste0("Large circles = sampling period (", sampling_label, "); size = mean shell length")) +
  theme_reclam

ggsave(here("FIG", "Fig_PCA_ENV.jpeg"),
       fig_pca, width = 9, height = 7, dpi = 300)

## 6.5 PCA loadings table ------------------------------------------------------

pca_load_tab <- as_tibble(pca_fit$rotation[, 1:3], rownames = "env_variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

write_csv(pca_load_tab, here("RESULTS", "Table_PCA_Loadings.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 6b. PCA — ENVIRONMENTAL SPACE — SIN ANOMALIAS (solo variable base)
#     Mismo procedimiento de 6.1-6.5 pero con env_vars_noanom.
# ─────────────────────────────────────────────────────────────────────────────

env_pca_mat_noanom <- env |>
  dplyr::select(region, year, month, date, all_of(env_vars_noanom)) |>
  drop_na() |>
  mutate(period = if_else(date >= OVERLAP_START & date <= OVERLAP_END, "Sampling", "Background"),
         Area   = str_to_title(region),
         label  = paste0(str_sub(month.abb[month], 1, 1), str_sub(year, 3, 4)))

pca_vars_noanom <- env_pca_mat_noanom |> dplyr::select(all_of(env_vars_noanom))

pca_fit_noanom <- prcomp(pca_vars_noanom, scale. = TRUE, center = TRUE)

pca_scores_noanom <- as_tibble(pca_fit_noanom$x[, 1:3]) |>
  bind_cols(env_pca_mat_noanom |> dplyr::select(region, year, month, period, Area, label))

var_exp_noanom <- round(summary(pca_fit_noanom)$importance[2, 1:3] * 100, 1)

pca_bio_noanom <- pca_scores_noanom |>
  filter(period == "Sampling") |>
  left_join(bio_monthly |> dplyr::select(region, year, month, Lmean, RI),
            by = c("region", "year", "month"))

loadings_df_noanom <- as_tibble(pca_fit_noanom$rotation[, 1:2],
                                 rownames = "variable") |>
  mutate(variable_short = c("SST", "Wind", "Run", "MHW_d"))

fig_pca_noanom <- ggplot(pca_scores_noanom, aes(x = PC1, y = PC2)) +
  geom_point(data = . %>% filter(period == "Background"),
             aes(colour = Area), alpha = 0.3, size = 1.5) +
  geom_point(data = pca_bio_noanom,
             aes(colour = Area, size = Lmean), alpha = 0.9, shape = 21,
             fill = "white", stroke = 1.5) +
  geom_text_repel(data = pca_bio_noanom,
                  aes(label = label, colour = Area),
                  size = 3, max.overlaps = 20) +
  geom_segment(data = loadings_df_noanom,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_arrow,
                   yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_df_noanom,
            aes(x = PC1 * scale_arrow * 1.15,
                y = PC2 * scale_arrow * 1.15,
                label = variable_short),
            size = 3, colour = "grey20") +
  scale_colour_manual(values = pal_area) +
  scale_size_continuous(name = "L̄ (mm)", range = c(3, 8)) +
  labs(x = sprintf("PC1 (%s%%)", var_exp_noanom[1]),
       y = sprintf("PC2 (%s%%)", var_exp_noanom[2]),
       colour = "Region",
       title  = sprintf("PCA environmental space, sin anomalias — %d–%d", min(env$year), max(env$year)),
       subtitle = paste0("Large circles = sampling period (", sampling_label, "); size = mean shell length")) +
  theme_reclam

ggsave(here("FIG", "Fig_PCA_ENV_SinAnom.jpeg"),
       fig_pca_noanom, width = 9, height = 7, dpi = 300)

pca_load_tab_noanom <- as_tibble(pca_fit_noanom$rotation[, 1:3], rownames = "env_variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

write_csv(pca_load_tab_noanom, here("RESULTS", "Table_PCA_Loadings_SinAnom.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 6c. PCA — BIOLOGICAL INDICATOR SPACE (RI, CPUE, Densidad juntas, SIN ambiente)
#     Reestructurado: la version anterior mezclaba cada variable poblacional
#     con el ambiente en el mismo PCA. Eso confunde "las regiones difieren"
#     con "el ambiente de las regiones difiere" -- prueba concreta: el
#     PERMANOVA de Densidad+ambiente daba significativo (p_adj=0.002) pero el
#     Wilcoxon de Densidad sola NO (p_adj=0.44) -- la separacion la generaba
#     el ambiente, no Densidad. Este PCA usa SOLO las 3 variables
#     poblacionales como activas, sin ambiente, para responder limpiamente
#     "¿las poblaciones (no su habitat) difieren entre regiones?". La
#     relacion bio~ambiente se responde en las secciones 5 (Spearman) y 7
#     (GAM), no aca.
# ─────────────────────────────────────────────────────────────────────────────

bio_pca_vars_list <- c("RI", "CPUE", "Densidad")

bio_pca_mat <- bio_monthly |>
  dplyr::select(Area, region, year, month, all_of(bio_pca_vars_list)) |>
  drop_na(all_of(bio_pca_vars_list)) |>
  mutate(label = paste0(str_sub(month.abb[month], 1, 1), str_sub(year, 3, 4)))

cat(sprintf("PCA bio (RI/CPUE/Densidad, sin ambiente): %d region-meses (Cadiz: %d, Valencia: %d)\n",
            nrow(bio_pca_mat),
            sum(bio_pca_mat$Area == "Cadiz"),
            sum(bio_pca_mat$Area == "Valencia")))

pca_bio_vars <- bio_pca_mat |> dplyr::select(all_of(bio_pca_vars_list))
pca_fit_bio  <- prcomp(pca_bio_vars, scale. = TRUE, center = TRUE)

scores_bio <- as_tibble(pca_fit_bio$x[, 1:2]) |>
  bind_cols(bio_pca_mat |> dplyr::select(Area, region, year, month, label))

var_exp_bio <- round(summary(pca_fit_bio)$importance[2, 1:2] * 100, 1)

hull_bio <- scores_bio |>
  group_by(Area) |>
  slice(chull(PC1, PC2)) |>
  ungroup()

loadings_bio <- as_tibble(pca_fit_bio$rotation[, 1:2], rownames = "variable") |>
  mutate(variable_short = c("RI", "CPUE", "Densidad"))

scale_arrow <- 3

fig_pca_bio <- ggplot(scores_bio, aes(x = PC1, y = PC2)) +
  geom_polygon(data = hull_bio, aes(fill = Area, colour = Area),
               alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(colour = Area), size = 3, alpha = 0.9) +
  geom_text_repel(aes(label = label, colour = Area), size = 3, max.overlaps = 20) +
  geom_segment(data = loadings_bio,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_bio,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3.2, colour = "grey20") +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values = pal_area, guide = "none") +
  labs(x = sprintf("PC1 (%s%%)", var_exp_bio[1]),
       y = sprintf("PC2 (%s%%)", var_exp_bio[2]),
       colour = "Region",
       title    = "PCA biological indicator space",
       subtitle = "Variables activas: RI, CPUE, Densidad (sin ambiente)") +
  theme_reclam

ggsave(here("FIG", "Fig_PCA_BIO.jpeg"), fig_pca_bio, width = 8, height = 6.5, dpi = 300)

pca_load_tab_bio <- as_tibble(pca_fit_bio$rotation[, 1:2], rownames = "variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write_csv(pca_load_tab_bio, here("RESULTS", "Table_PCA_Loadings_BIO.csv"))

## 6c.1 PERMANOVA — Cadiz vs Valencia, espacio bio-solo (RI+CPUE+Densidad) ---
# Version limpia del PERMANOVA: sin ambiente mezclado. Complementa el
# Wilcoxon univariado (6c.2) con la version multivariada conjunta.

set.seed(2025)

vars_scaled_bio <- scale(pca_bio_vars)
dist_bio        <- vegan::vegdist(vars_scaled_bio, method = "euclidean")

permanova_bio <- vegan::adonis2(dist_bio ~ Area, data = bio_pca_mat, permutations = 999)
disp_bio      <- vegan::betadisper(dist_bio, bio_pca_mat$Area)
permdisp_bio  <- vegan::permutest(disp_bio, permutations = 999)

permanova_bio_result <- tibble(
  espacio     = "RI + CPUE + Densidad (sin ambiente)",
  R2          = round(permanova_bio$R2[1], 3),
  F_permanova = round(permanova_bio$F[1], 2),
  p_permanova = permanova_bio$`Pr(>F)`[1],
  F_permdisp  = round(permdisp_bio$tab$F[1], 2),
  p_permdisp  = permdisp_bio$tab$`Pr(>F)`[1],
  n_cadiz     = sum(bio_pca_mat$Area == "Cadiz"),
  n_valencia  = sum(bio_pca_mat$Area == "Valencia")
)

print(permanova_bio_result)

write_csv(permanova_bio_result, here("RESULTS", "Table_PERMANOVA_BIO.csv"))

## 6c.2 Wilcoxon — Cadiz vs Valencia, variable poblacional SOLA --------------
# El test mas limpio de los tres: univariado, no usa el ambiente para nada.
# Compara directamente RI/CPUE/Densidad entre regiones. bio_monthly completo
# (no restringido al overlap bio-env) porque no hace falta dato ambiental.

run_wilcox <- function(bio_v) {
  df <- bio_monthly |> dplyr::select(Area, all_of(bio_v)) |> drop_na()
  form <- as.formula(paste0(bio_v, " ~ Area"))
  wt <- wilcox.test(form, data = df)

  tibble(
    bio_var         = bio_v,
    W               = unname(wt$statistic),
    p               = wt$p.value,
    median_cadiz    = round(median(df[[bio_v]][df$Area == "Cadiz"],    na.rm = TRUE), 3),
    median_valencia = round(median(df[[bio_v]][df$Area == "Valencia"], na.rm = TRUE), 3),
    n_cadiz         = sum(df$Area == "Cadiz"),
    n_valencia      = sum(df$Area == "Valencia")
  )
}

wilcox_results <- bind_rows(
  run_wilcox("RI"),
  run_wilcox("CPUE"),
  run_wilcox("Densidad")
) |>
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    sig = case_when(
      is.na(p_adj) ~ "",
      p_adj < 0.05 ~ "*",
      p_adj < 0.10 ~ "†",
      TRUE         ~ ""
    )
  )

print(wilcox_results)

write_csv(wilcox_results, here("RESULTS", "Table_Wilcoxon_byVariable.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 6d. PCA POR VARIABLE POBLACIONAL + AMBIENTE — SUPLEMENTARIO / EXPLORATORIO
#     ADVERTENCIA: estos PCA mezclan la variable poblacional con el ambiente
#     como variables activas. NO usar el PERMANOVA de esta seccion (6d.5)
#     como evidencia de que la variable poblacional difiere entre regiones
#     -- puede estar dominado por la diferencia ambiental Atlantico vs
#     Mediterraneo (asi paso con Densidad, ver 6c). Sirve para explorar
#     visualmente como se posiciona cada variable poblacional respecto al
#     gradiente ambiental, no para testear diferencias poblacionales (para
#     eso: 6c.1 PERMANOVA bio-solo, o 6c.2 Wilcoxon).
# ─────────────────────────────────────────────────────────────────────────────

# Etiquetas cortas para las flechas de loadings (variables activas: la bio y
# las 4 ambientales base, todas a lag0)
env_short <- c(sst_mean = "SST", wind_speed = "Wind",
              runoff_mm = "Runoff", mhw_days = "MHW_days")

## 6d.1 PCA — RI + ambiente (suplementario) ----------------------------------

active_vars_ri <- c("RI", env_vars_noanom)

mat_ri <- joined_lags |>
  dplyr::select(Area, region, year, month, all_of(active_vars_ri)) |>
  drop_na(all_of(active_vars_ri)) |>
  mutate(label = paste0(str_sub(month.abb[month], 1, 1), str_sub(year, 3, 4)))

cat(sprintf("PCA RI + ambiente (suplementario): %d region-meses (Cadiz: %d, Valencia: %d)\n",
            nrow(mat_ri), sum(mat_ri$Area == "Cadiz"), sum(mat_ri$Area == "Valencia")))

pca_vars_ri <- mat_ri |> dplyr::select(all_of(active_vars_ri))
pca_fit_ri  <- prcomp(pca_vars_ri, scale. = TRUE, center = TRUE)

scores_ri <- as_tibble(pca_fit_ri$x[, 1:2]) |>
  bind_cols(mat_ri |> dplyr::select(Area, region, year, month, label))

var_exp_ri <- round(summary(pca_fit_ri)$importance[2, 1:2] * 100, 1)

hull_ri <- scores_ri |>
  group_by(Area) |>
  slice(chull(PC1, PC2)) |>
  ungroup()

loadings_ri <- as_tibble(pca_fit_ri$rotation[, 1:2], rownames = "variable") |>
  mutate(is_bio = variable == "RI",
         variable_short = if_else(is_bio, "RI", env_short[variable]))

loadings_ri_env <- loadings_ri |> filter(!is_bio)
loadings_ri_bio <- loadings_ri |> filter(is_bio)

fig_pca_ri <- ggplot(scores_ri, aes(x = PC1, y = PC2)) +
  geom_polygon(data = hull_ri, aes(fill = Area, colour = Area),
               alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(colour = Area), size = 3, alpha = 0.9) +
  geom_text_repel(aes(label = label, colour = Area), size = 3, max.overlaps = 20) +
  geom_segment(data = loadings_ri_env,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_ri_env,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3, colour = "grey20") +
  geom_segment(data = loadings_ri_bio,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.25, "cm")), colour = "firebrick", linewidth = 1) +
  geom_text(data = loadings_ri_bio,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3.6, colour = "firebrick", fontface = "bold") +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values = pal_area, guide = "none") +
  labs(x = sprintf("PC1 (%s%%)", var_exp_ri[1]),
       y = sprintf("PC2 (%s%%)", var_exp_ri[2]),
       colour = "Region")+
      theme_reclam

ggsave(here("FIG", "Fig_PCA_RI_ENV.jpeg"), fig_pca_ri, width = 8.5, height = 6.5, dpi = 300)

pca_load_tab_ri <- as_tibble(pca_fit_ri$rotation[, 1:2], rownames = "variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write_csv(pca_load_tab_ri, here("RESULTS", "Table_PCA_Loadings_RI_ENV.csv"))

## 6d.2 PCA — CPUE + ambiente (suplementario) --------------------------------

active_vars_cpue <- c("CPUE", env_vars_noanom)

mat_cpue <- joined_lags |>
  dplyr::select(Area, region, year, month, all_of(active_vars_cpue)) |>
  drop_na(all_of(active_vars_cpue)) |>
  mutate(label = paste0(str_sub(month.abb[month], 1, 1), str_sub(year, 3, 4)))

cat(sprintf("PCA CPUE + ambiente (suplementario): %d region-meses (Cadiz: %d, Valencia: %d)\n",
            nrow(mat_cpue), sum(mat_cpue$Area == "Cadiz"), sum(mat_cpue$Area == "Valencia")))

pca_vars_cpue <- mat_cpue |> dplyr::select(all_of(active_vars_cpue))
pca_fit_cpue  <- prcomp(pca_vars_cpue, scale. = TRUE, center = TRUE)

scores_cpue <- as_tibble(pca_fit_cpue$x[, 1:2]) |>
  bind_cols(mat_cpue |> dplyr::select(Area, region, year, month, label))

var_exp_cpue <- round(summary(pca_fit_cpue)$importance[2, 1:2] * 100, 1)

hull_cpue <- scores_cpue |>
  group_by(Area) |>
  slice(chull(PC1, PC2)) |>
  ungroup()

loadings_cpue <- as_tibble(pca_fit_cpue$rotation[, 1:2], rownames = "variable") |>
  mutate(is_bio = variable == "CPUE",
         variable_short = if_else(is_bio, "CPUE", env_short[variable]))

loadings_cpue_env <- loadings_cpue |> filter(!is_bio)
loadings_cpue_bio <- loadings_cpue |> filter(is_bio)

fig_pca_cpue <- ggplot(scores_cpue, aes(x = PC1, y = PC2)) +
  geom_polygon(data = hull_cpue, aes(fill = Area, colour = Area),
               alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(colour = Area), size = 3, alpha = 0.9) +
  geom_text_repel(aes(label = label, colour = Area), size = 3, max.overlaps = 20) +
  geom_segment(data = loadings_cpue_env,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_cpue_env,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3, colour = "grey20") +
  geom_segment(data = loadings_cpue_bio,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.25, "cm")), colour = "firebrick", linewidth = 1) +
  geom_text(data = loadings_cpue_bio,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3.6, colour = "firebrick", fontface = "bold") +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values = pal_area, guide = "none") +
  labs(x = sprintf("PC1 (%s%%)", var_exp_cpue[1]),
       y = sprintf("PC2 (%s%%)", var_exp_cpue[2]),
       colour = "Region")+
       theme_reclam
       
ggsave(here("FIG", "Fig_PCA_CPUE_ENV.jpeg"), fig_pca_cpue, width = 8.5, height = 6.5, dpi = 300)

pca_load_tab_cpue <- as_tibble(pca_fit_cpue$rotation[, 1:2], rownames = "variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write_csv(pca_load_tab_cpue, here("RESULTS", "Table_PCA_Loadings_CPUE_ENV.csv"))

## 6d.3 PCA — Densidad + ambiente (suplementario) ----------------------------

active_vars_dens <- c("Densidad", env_vars_noanom)

mat_dens <- joined_lags |>
  dplyr::select(Area, region, year, month, all_of(active_vars_dens)) |>
  drop_na(all_of(active_vars_dens)) |>
  mutate(label = paste0(str_sub(month.abb[month], 1, 1), str_sub(year, 3, 4)))

cat(sprintf("PCA Densidad + ambiente (suplementario): %d region-meses (Cadiz: %d, Valencia: %d)\n",
            nrow(mat_dens), sum(mat_dens$Area == "Cadiz"), sum(mat_dens$Area == "Valencia")))

pca_vars_dens <- mat_dens |> dplyr::select(all_of(active_vars_dens))
pca_fit_dens  <- prcomp(pca_vars_dens, scale. = TRUE, center = TRUE)

scores_dens <- as_tibble(pca_fit_dens$x[, 1:2]) |>
  bind_cols(mat_dens |> dplyr::select(Area, region, year, month, label))

var_exp_dens <- round(summary(pca_fit_dens)$importance[2, 1:2] * 100, 1)

hull_dens <- scores_dens |>
  group_by(Area) |>
  slice(chull(PC1, PC2)) |>
  ungroup()

loadings_dens <- as_tibble(pca_fit_dens$rotation[, 1:2], rownames = "variable") |>
  mutate(is_bio = variable == "Densidad",
         variable_short = if_else(is_bio, "Densidad", env_short[variable]))

loadings_dens_env <- loadings_dens |> filter(!is_bio)
loadings_dens_bio <- loadings_dens |> filter(is_bio)

fig_pca_dens <- ggplot(scores_dens, aes(x = PC1, y = PC2)) +
  geom_polygon(data = hull_dens, aes(fill = Area, colour = Area),
               alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(colour = Area), size = 3, alpha = 0.9) +
  geom_text_repel(aes(label = label, colour = Area), size = 3, max.overlaps = 20) +
  geom_segment(data = loadings_dens_env,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_dens_env,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3, colour = "grey20") +
  geom_segment(data = loadings_dens_bio,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.25, "cm")), colour = "firebrick", linewidth = 1) +
  geom_text(data = loadings_dens_bio,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15, label = variable_short),
            size = 3.6, colour = "firebrick", fontface = "bold") +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values = pal_area, guide = "none") +
  labs(x = sprintf("PC1 (%s%%)", var_exp_dens[1]),
       y = sprintf("PC2 (%s%%)", var_exp_dens[2]),
       colour = "Region")+
  theme_reclam

ggsave(here("FIG", "Fig_PCA_Densidad_ENV.jpeg"), fig_pca_dens, width = 8.5, height = 6.5, dpi = 300)

pca_load_tab_dens <- as_tibble(pca_fit_dens$rotation[, 1:2], rownames = "variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write_csv(pca_load_tab_dens, here("RESULTS", "Table_PCA_Loadings_Densidad_ENV.csv"))

## 6d.4 Panel combinado — los 3 PCA bio+ambiente juntos ----------------------

fig_pca_bio_panel <- (fig_pca_ri | fig_pca_cpue | fig_pca_dens) +
  plot_annotation(tag_levels = "a")+
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(here("FIG", "Fig_PCA_BIO_panel.jpeg"),
       fig_pca_bio_panel, width = 10, height = 5, dpi = 300)

## 6d.5 PERMANOVA — espacio bio + ambiente (EXPLORATORIO, ver advertencia) ---
# Repetido de una version anterior. Un R2/p significativo aca describe que
# las dos regiones ocupan zonas distintas del espacio [variable + ambiente]
# conjunto -- consistente con que Cadiz (Atlantico) y Valencia (Mediterraneo)
# tienen regimenes de SST/viento distintos, NO necesariamente con que la
# variable poblacional difiera (para eso ver 6c.1/6c.2 arriba).

run_permanova_env <- function(pca_vars, mat, bio_v) {
  vars_scaled <- scale(pca_vars)
  dist_mat    <- vegan::vegdist(vars_scaled, method = "euclidean")

  pmv  <- vegan::adonis2(dist_mat ~ Area, data = mat, permutations = 999)
  disp <- vegan::betadisper(dist_mat, mat$Area)
  pdsp <- vegan::permutest(disp, permutations = 999)

  tibble(
    bio_var      = bio_v,
    R2           = round(pmv$R2[1], 3),
    F_permanova  = round(pmv$F[1], 2),
    p_permanova  = pmv$`Pr(>F)`[1],
    F_permdisp   = round(pdsp$tab$F[1], 2),
    p_permdisp   = pdsp$tab$`Pr(>F)`[1],
    n_cadiz      = sum(mat$Area == "Cadiz"),
    n_valencia   = sum(mat$Area == "Valencia")
  )
}

permanova_env_results <- bind_rows(
  run_permanova_env(pca_vars_ri,   mat_ri,   "RI"),
  run_permanova_env(pca_vars_cpue, mat_cpue, "CPUE"),
  run_permanova_env(pca_vars_dens, mat_dens, "Densidad")
) |>
  mutate(
    p_permanova_adj = p.adjust(p_permanova, method = "BH"),
    sig = case_when(
      is.na(p_permanova_adj)   ~ "",
      p_permanova_adj < 0.05   ~ "*",
      p_permanova_adj < 0.10   ~ "†",
      TRUE                     ~ ""
    )
  )

print(permanova_env_results)

write_csv(permanova_env_results, here("RESULTS", "Table_PERMANOVA_byVariable_ENV_supl.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 7. GAM — DRIVERS OF POPULATION INDICATORS, POR REGION
#    Identifica que variables ambientales explican la variacion de cada
#    indicador poblacional (Lmean, L_p90, RI, CPUE, Densidad), Cadiz y
#    Valencia POR SEPARADO (sin agrupar/pooled).
#    Modelo: bio ~ s(env, k = 3), una region a la vez.
#    Variables ambientales: env_vars_noanom (sin anomalias, sin MHW_int_*),
#    a lag 0, 1 y 2 meses -- mismos datasets que la seccion 5.
#    CAUTELA: n por region depende del overlap dinamico (ver Data_BIO_ENV_
#    overlap.csv al correr el script -- Cadiz y Valencia pueden diferir por
#    huecos de muestreo bio). Con n todavia moderado y k=3, interpretar con
#    cuidado; ver Table_GAM_ENV_BIO_byRegion.csv para el n real de cada celda.
# ─────────────────────────────────────────────────────────────────────────────

## 7.1 GAM univariado por region (un smooth a la vez) + FDR -------------------

gam_safe_region <- function(data_df, bio_v, env_v) {
  df <- data_df |> dplyr::select(all_of(c(bio_v, env_v))) |> drop_na()
  n  <- nrow(df)

  if (n < 5 || length(unique(df[[env_v]])) < 3) {
    return(tibble(edf = NA_real_, Fstat = NA_real_, p = NA_real_,
                  dev_expl = NA_real_, r2 = NA_real_, n = n))
  }

  form <- as.formula(paste0(bio_v, " ~ s(", env_v, ", k = 3)"))
  m <- tryCatch(mgcv::gam(form, data = df, method = "REML"),
                error = function(e) NULL)

  if (is.null(m)) {
    return(tibble(edf = NA_real_, Fstat = NA_real_, p = NA_real_,
                  dev_expl = NA_real_, r2 = NA_real_, n = n))
  }

  s_tab <- summary(m)$s.table
  tibble(edf      = unname(s_tab[1, "edf"]),
         Fstat    = unname(s_tab[1, "F"]),
         p        = unname(s_tab[1, "p-value"]),
         dev_expl = summary(m)$dev.expl,
         r2       = summary(m)$r.sq,
         n        = n)
}

run_gam_block_region <- function(data_df, lag_suffix, area_nm) {
  ev <- if (lag_suffix == "lag0") env_vars_noanom else paste0(env_vars_noanom, "_", lag_suffix)
  ev_names <- env_vars_noanom

  map_dfr(bio_indicators, function(bio_v) {
    map_dfr(seq_along(ev), function(i) {
      gam_safe_region(data_df, bio_v, ev[i]) |>
        mutate(bio_var = bio_v, env_var = ev_names[i], lag = lag_suffix, Area = area_nm)
    })
  })
}

gam_results_region <- bind_rows(
  run_gam_block_region(joined_lags |> filter(Area == "Cadiz"),    "lag0", "Cadiz"),
  run_gam_block_region(joined_lag1 |> filter(Area == "Cadiz"),    "lag1", "Cadiz"),
  run_gam_block_region(joined_lag2 |> filter(Area == "Cadiz"),    "lag2", "Cadiz"),
  run_gam_block_region(joined_lags |> filter(Area == "Valencia"), "lag0", "Valencia"),
  run_gam_block_region(joined_lag1 |> filter(Area == "Valencia"), "lag1", "Valencia"),
  run_gam_block_region(joined_lag2 |> filter(Area == "Valencia"), "lag2", "Valencia")
) |>
  group_by(Area, lag) |>
  mutate(p_adj = p.adjust(p, method = "BH")) |>
  ungroup() |>
  mutate(
    sig = case_when(
      is.na(p_adj) ~ "",
      p_adj < 0.05 ~ "*",
      p_adj < 0.10 ~ "†",
      TRUE         ~ ""
    )
  )

write_csv(gam_results_region, here("RESULTS", "Table_GAM_ENV_BIO_byRegion.csv"))

## 7.2 Heatmap — deviance explicada por env_var x bio_var, facet lag x Area ---

fig_gam_heat_region <- gam_results_region |>
  mutate(
    bio_var = factor(bio_var, levels = bio_indicators,
                     labels = c("L̄ (mm)", "L_P90 (mm)", "RI (%)",
                                "CPUE (kg·h⁻¹)", "Densidad (ind·m⁻²)")),
    env_var = factor(env_var, levels = env_vars_noanom,
                     labels = c("SST", "Wind", "Runoff", "MHW_days")),
    lag     = factor(lag, levels = c("lag0", "lag1", "lag2"),
                     labels = c("Lag 0", "Lag 1 mo", "Lag 2 mo")),
    label_display = if_else(is.na(dev_expl), "n/a",
                             sprintf("%.0f%%%s", dev_expl * 100, sig))
  ) |>
  ggplot(aes(x = bio_var, y = env_var, fill = dev_expl)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label_display), size = 2.8, colour = "grey10") +
  facet_grid(lag ~ Area) +
  scale_fill_viridis_c(option = "G",
                       alpha= 0.5,
                       limits = c(0, 1), na.value = "grey85",
                       name = "Deviance") +
  labs(x = "", y = "") +
       theme_reclam +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right")

ggsave(here("FIG", "Fig_GAM_Heatmap_byRegion.jpeg"),
       fig_gam_heat_region, width = 9, height = 9, dpi = 300)

## 7.3 Efectos parciales (smooths) por region — ggplot, sin for() -------------
# .build_gam_partial(): ajusta bio ~ s(env, k=3) para UNA combinacion y
# devuelve el ggplot (equivalente a plot.gam pero con theme_reclam). No hace
# I/O -- es el bloque reusable.
# plot_gam_partial(): wrapper de uso suelto, ademas guarda (ggsave) un panel
# individual.
# plot_gam_partial_grid(): arma TODOS los paneles bio_indicators x
# env_vars_noanom para una region/lag, vía purrr::pmap (no for()) +
# patchwork, y guarda un unico jpeg -- reemplaza el panel completo que
# antes se generaba con jpeg()/par(mfrow)/plot.gam.

.build_gam_partial <- function(bio_v, env_v, area_nm, lag_suffix = "lag0") {

  data_src <- switch(lag_suffix,
                      lag0 = joined_lags,
                      lag1 = joined_lag1,
                      lag2 = joined_lag2,
                      stop("lag_suffix debe ser 'lag0', 'lag1' o 'lag2'"))

  env_col <- if (lag_suffix == "lag0") env_v else paste0(env_v, "_", lag_suffix)

  df <- data_src |>
    filter(Area == area_nm) |>
    dplyr::select(all_of(c(bio_v, env_col))) |>
    drop_na()

  stopifnot("Muy pocos datos para ajustar el GAM (n < 5)" = nrow(df) >= 5,
            "Variable ambiental sin variacion suficiente (< 3 valores unicos)" =
              length(unique(df[[env_col]])) >= 3)

  form <- as.formula(paste0(bio_v, " ~ s(", env_col, ", k = 3)"))
  m <- mgcv::gam(form, data = df, method = "REML")

  grid <- tibble(x_seq = seq(min(df[[env_col]]), max(df[[env_col]]), length.out = 200))
  names(grid) <- env_col

  pred <- predict(m, newdata = grid, type = "terms", se.fit = TRUE)

  grid <- grid |>
    mutate(fit = as.numeric(pred$fit[, 1]),
           se  = as.numeric(pred$se.fit[, 1]),
           lwr = fit - 1.96 * se,
           upr = fit + 1.96 * se)

  # Residuales parciales en los puntos observados (component + residual):
  # efecto parcial evaluado en cada x observado + el residuo de la
  # observacion -- es lo que muestra los datos reales alrededor de la curva
  # (equivalente a plot.gam(..., residuals = TRUE)).
  pred_obs <- predict(m, newdata = df, type = "terms")
  df <- df |>
    mutate(partial_resid = as.numeric(pred_obs[, 1]) + residuals(m, type = "response"))

  s_tab <- summary(m)$s.table
  edf   <- round(unname(s_tab[1, "edf"]), 2)
  pval  <- signif(unname(s_tab[1, "p-value"]), 2)

  ggplot(grid, aes(x = .data[[env_col]], y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = pal_area[[area_nm]], alpha = 0.2) +
    geom_line(colour = pal_area[[area_nm]], linewidth = 1) +
    geom_point(data = df, aes(x = .data[[env_col]], y = partial_resid),
               colour = "black", size = 0.8, alpha = 0.8) +
    labs(x = env_col, y = paste0("s(", bio_v, ")"),
         subtitle = sprintf("edf = %.2f | p = %s | n = %d (%s)",
                            edf, pval, nrow(df), lag_suffix)) +
    theme_reclam +
    theme(plot.subtitle = element_text(size = 7))
}

plot_gam_partial <- function(bio_v, env_v, area_nm, lag_suffix = "lag0") {
  p <- .build_gam_partial(bio_v, env_v, area_nm, lag_suffix)
  env_col <- if (lag_suffix == "lag0") env_v else paste0(env_v, "_", lag_suffix)
  fname <- paste0("Fig_GAM_partial_", bio_v, "_", env_col, "_", area_nm, ".jpeg")
  ggsave(here("FIG", fname), p, width = 5.5, height = 4.5, dpi = 300)
  message("Guardado: ", fname)
  p
}

plot_gam_partial_grid <- function(area_nm, lag_suffix = "lag0") {

  combos <- tidyr::expand_grid(bio_v = bio_indicators, env_v = env_vars_noanom)

  plots <- purrr::pmap(combos, function(bio_v, env_v) {
    tryCatch(
      .build_gam_partial(bio_v, env_v, area_nm, lag_suffix),
      error = function(e) {
        ggplot() + theme_void() +
          labs(subtitle = "n insuf. / sin ajuste") +
          theme(plot.subtitle = element_text(size = 7))
      }
    )
  })

  panel <- patchwork::wrap_plots(plots, ncol = length(env_vars_noanom))

  fname <- paste0("Fig_GAM_Smooths_", lag_suffix, "_", area_nm, ".jpeg")
  ggsave(here("FIG", fname), panel,
         width = 2 * length(env_vars_noanom), height = 2 * length(bio_indicators), dpi = 300)
  message("Guardado: ", fname)
  panel
}

# Paneles completos por region (reemplaza el jpeg()/par(mfrow) anterior):
plot_gam_partial_grid("Cadiz",    "lag0")
plot_gam_partial_grid("Valencia", "lag0")

# Combinacion suelta especifica (opcional, ad hoc):
# plot_gam_partial("L_p90", "wind_speed", "Valencia", "lag0")

# ─────────────────────────────────────────────────────────────────────────────
# 9. EXPORT SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

write_csv(joined_lags,  here("RESULTS", "Data_BIO_ENV_overlap.csv"))
write_csv(corr_results, here("RESULTS", "Table_Spearman_ENV_BIO.csv"))
write_csv(pca_load_tab, here("RESULTS", "Table_PCA_Loadings.csv"))
write_csv(bio_monthly,  here("RESULTS", "Data_BIO_monthly.csv"))
write_csv(gam_results_region, here("RESULTS", "Table_GAM_ENV_BIO_byRegion.csv"))

message("\n========================================")
message(" ENV_BIO_Correlation.R — done.")
message(" Figures in : ", here("FIG"))
message(" Results in : ", here("RESULTS"))
message("  · FigS_ENV_Context.jpeg     — historical context (SST + MHW)")
message("  · Fig_Spearman_Heatmap.jpeg — Spearman rho heatmap (lag 0–2)")
message("  · Fig_PCA_ENV.jpeg          — PCA env space + bio overlay")
message("  · Fig_TS_ENV_BIO.jpeg       — integrated time series panel")
message("  · Table_Spearman_ENV_BIO.csv")
message("  · Table_PCA_Loadings.csv")
message("  · Data_BIO_ENV_overlap.csv")
message(" --- Variante sin anomalias (5b/6b) ---")
message("  · Fig_Spearman_Heatmap_SinAnom.jpeg")
message("  · Fig_PCA_ENV_SinAnom.jpeg")
message("  · Table_Spearman_ENV_BIO_SinAnom.csv")
message("  · Table_PCA_Loadings_SinAnom.csv")
message(" --- PCA biologico solo, RI+CPUE+Densidad, sin ambiente (6c) ---")
message("  · Fig_PCA_BIO.jpeg               — PCA con hull por region")
message("  · Table_PCA_Loadings_BIO.csv")
message("  · Table_PERMANOVA_BIO.csv        — PERMANOVA+PERMDISP, espacio bio-solo")
message("  · Table_Wilcoxon_byVariable.csv  — test limpio, RI/CPUE/Densidad por separado")
message(" --- PCA por variable + ambiente — SUPLEMENTARIO/EXPLORATORIO (6d) ---")
message("  · Fig_PCA_RI_ENV.jpeg / Fig_PCA_CPUE_ENV.jpeg / Fig_PCA_Densidad_ENV.jpeg")
message("  · Fig_PCA_BIO_panel.jpeg — los 3 PCA juntos, lado a lado")
message("  · Table_PCA_Loadings_RI_ENV.csv / _CPUE_ENV.csv / _Densidad_ENV.csv")
message("  · Table_PERMANOVA_byVariable_ENV_supl.csv — NO usar como evidencia de dif. poblacional")
message(" --- GAM drivers poblacionales por region (7, n dinamico por region) ---")
message("  · Fig_GAM_Heatmap_byRegion.jpeg          — deviance explicada, bio x env x lag x Area")
message("  · Fig_GAM_Smooths_lag0_Cadiz.jpeg        — panel completo efectos parciales, Cadiz")
message("  · Fig_GAM_Smooths_lag0_Valencia.jpeg     — panel completo efectos parciales, Valencia")
message("  · plot_gam_partial(bio, env, Area, lag)  — combinacion suelta a demanda (ggsave individual)")
message("  · Table_GAM_ENV_BIO_byRegion.csv         — edf, F, p, p_adj, dev_expl, r2, n")
message("========================================\n")
