# =============================================================================
# ENV_BIO_Correlation.R
# Environmental drivers of Donax trunculus population indicators
# Gulf of Cádiz (GoC) vs. Gulf of Valencia (GoV)
# Project RECLAM — IEO-CSIC / UPV
#
# Analyses:
#   0.  Setup
#   1.  Biological data loading + monthly aggregation (substrate: COMPARA_RECLAM.R)
#   2.  Environmental data loading (env_monthly_all.csv, 2014–2024)
#   3.  Join + temporal alignment (overlap: Jul–Dec 2024, n=6/region)
#   4.  Historical context: env climatology + sampling window
#   5.  Spearman correlations at lag 0, 1, 2 + FDR correction
#   6.  PCA of environmental space + biological overlay
#   7.  Integrated time series figure (env + bio concurrent panel)
#   8.  Export tables and figures
#
# NOTE: Biological data covers Jul 2024 – Jun 2025.
#       Environmental data covers Jan 2014 – Dec 2024.
#       Concurrent overlap = Jul–Dec 2024 (n = 6 months per region).
#       All correlations are EXPLORATORY — interpret with caution (low n).
#
# Author  : Mauricio Mardones / Alberto García
# Updated : 2025-05
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 0. SETUP
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

dir.create(here("FIG"),     showWarnings = FALSE)
dir.create(here("RESULTS"), showWarnings = FALSE)

# Temporal overlap constants
OVERLAP_START <- as.Date("2024-07-01")
OVERLAP_END   <- as.Date("2024-12-01")


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
# Lmean, L_p90, RI, N — no CPUE (denominador de esfuerzo pendiente)

bio_monthly <- tallas |>
  group_by(Area, year, month) |>
  summarise(
    Lmean = mean(TALLA, na.rm = TRUE),
    L_p90 = quantile(TALLA, 0.90, na.rm = TRUE),
    RI    = mean(TALLA <= 25, na.rm = TRUE),   # proporción reclutas (SL ≤ 8 mm)
    N     = n(),                               # densidad relativa
    .groups = "drop"
  ) |>
  mutate(date   = as.Date(paste(year, month, "15", sep = "-")),
         region = tolower(Area))

cat(sprintf("Bio data: %d region-months (Cadiz: %d, Valencia: %d)\n",
            nrow(bio_monthly),
            sum(bio_monthly$Area == "Cadiz"),
            sum(bio_monthly$Area == "Valencia")))


# ─────────────────────────────────────────────────────────────────────────────
# 2. ENVIRONMENTAL DATA
# ─────────────────────────────────────────────────────────────────────────────

env_path <- here("DATA", "Environmental_Data", "env_monthly_all.csv")
stopifnot("env_monthly_all.csv not found" = file.exists(env_path))

env <- read_csv(env_path, show_col_types = FALSE) |>
  mutate(date   = as.Date(date),
         region = tolower(region))   # harmonise to lowercase

env_vars <- c("sst_mean", "sst_anom", "wind_speed", "wind_anom",
              "runoff_mm", "runoff_anom", "mhw_days", "mhw_int_mean", "mhw_int_max")

cat(sprintf("Env data: %d rows | %d–%d | regions: %s\n",
            nrow(env), min(env$year), max(env$year),
            paste(unique(env$region), collapse = ", ")))


# ─────────────────────────────────────────────────────────────────────────────
# 3. JOIN + TEMPORAL ALIGNMENT
# ─────────────────────────────────────────────────────────────────────────────

# Full join (all bio months — env será NA para meses después de dic 2024)
joined_full <- bio_monthly |>
  left_join(env |> dplyr::select(-date), by = c("region", "year", "month"))

# Concurrent overlap only (Jul–Dec 2024, n=6 per region)
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


# ─────────────────────────────────────────────────────────────────────────────
# 4. HISTORICAL CONTEXT: env climatology + sampling window
# ─────────────────────────────────────────────────────────────────────────────
# Shows where the 2024 sampling period falls in the 10-yr distribution

## 4.1 SST climatology --------------------------------------------------------

env_clim <- env |>
  group_by(region, month) |>
  summarise(
    sst_clim_mean = mean(sst_mean, na.rm = TRUE),
    sst_clim_lo   = quantile(sst_mean, 0.10, na.rm = TRUE),
    sst_clim_hi   = quantile(sst_mean, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(Area = str_to_title(region))

env_2024 <- env |>
  filter(year == 2024) |>
  mutate(Area = str_to_title(region))

fig_context_sst <- ggplot(env_clim, aes(x = month)) +
  geom_ribbon(aes(ymin = sst_clim_lo, ymax = sst_clim_hi, fill = Area),
              alpha = 0.2) +
  geom_line(aes(y = sst_clim_mean, colour = Area), linewidth = 0.9,
            linetype = "dashed") +
  geom_line(data = env_2024, aes(y = sst_mean, colour = Area),
            linewidth = 1.2) +
  geom_point(data = env_2024, aes(y = sst_mean, colour = Area), size = 2) +
  geom_vline(xintercept = c(7, 12), linetype = "dotted",
             colour = "grey40", linewidth = 0.7) +
  annotate("rect", xmin = 7, xmax = 12, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.2) +
  annotate("text", x = 9.5, y = Inf, label = "Sampling window",
           vjust = 1.5, size = 3, colour = "grey30") +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values   = pal_area) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = NULL, y = "SST (°C)", colour = "Region", fill = "Region",
       title = "SST 2024 vs. climatology 2014–2023",
       subtitle = "Dashed = climatological mean; ribbon = P10–P90; solid = 2024") +
  theme_reclam

## 4.2 MHW days context -------------------------------------------------------

fig_context_mhw <- env |>
  mutate(Area = str_to_title(region)) |>
  group_by(Area, year) |>
  summarise(mhw_annual = sum(mhw_days, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = mhw_annual, fill = Area)) +
  geom_col(position = "dodge", width = 0.7, colour = "grey20", alpha = 0.85) +
  scale_fill_manual(values = pal_area) +
  geom_vline(xintercept = 2024, linetype = "dashed",
             colour = "grey30", linewidth = 0.8) +
  annotate("text", x = 2024.1, y = Inf, label = "2024\n(sampling)",
           hjust = 0, vjust = 1.2, size = 3, colour = "grey30") +
  labs(x = NULL, y = "MHW days per year", fill = "Region",
       title = "Annual MHW days 2014–2024") +
  theme_reclam

fig_context <- (fig_context_sst / fig_context_mhw) +
  plot_annotation(tag_levels = "a",
                  title = "Environmental context — D. trunculus sampling period")

ggsave(here("FIG", "FigS_ENV_Context.jpeg"),
       fig_context, width = 10, height = 8, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 5. SPEARMAN CORRELATIONS — lag 0, 1, 2
#    IMPORTANT: n = 6 per region at lag 0; n = 5 at lag 1; n = 4 at lag 2
#    All results are EXPLORATORY — report rho and p but interpret cautiously
# ─────────────────────────────────────────────────────────────────────────────

bio_indicators <- c("Lmean", "L_p90", "RI", "N")

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
  filter(!is.na(rho)) |>
  mutate(
    bio_var = factor(bio_var, levels = bio_indicators,
                     labels = c("L̄ (mm)", "L_P90 (mm)", "RI (%)", "N")),
    env_var = factor(env_var, levels = env_vars,
                     labels = c("SST", "SST_anom", "Wind", "Wind_anom",
                                "Runoff", "Runoff_anom",
                                "MHW_days", "MHW_int_mean", "MHW_int_max")),
    lag     = factor(lag, levels = c("lag0", "lag1", "lag2"),
                     labels = c("Lag 0", "Lag 1 mo", "Lag 2 mo"))
  ) |>
  ggplot(aes(x = bio_var, y = env_var, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = rho_label), size = 2.8, colour = "grey10") +
  facet_grid(lag ~ Area) +
  scale_fill_gradient2(low  = "#1B7FC4", mid = "white", high = "#D94F00",
                       midpoint = 0, limits = c(-1, 1), name = "Spearman ρ") +
  labs(x = "Biological indicator", y = "Environmental variable",
       title = expression("Spearman ρ — " * italic("D. trunculus") *
                            " × environment"),
       subtitle = "* FDR p < 0.05; † FDR p < 0.10 | n = 6 (lag0), 5 (lag1), 4 (lag2) per region") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right")

ggsave(here("FIG", "Fig_Spearman_Heatmap.jpeg"),
       fig_corr_heat, width = 12, height = 9, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 6. PCA — ENVIRONMENTAL SPACE (10 years) + SAMPLING PERIOD OVERLAY
# ─────────────────────────────────────────────────────────────────────────────
# PCA uses all 10 years of monthly env data to define the environmental space.
# Bio sampling period (Jul–Dec 2024) is projected as supplementary points.

## 6.1 Prepare env matrix ------------------------------------------------------

env_pca_mat <- env |>
  dplyr::select(region, year, month, all_of(env_vars)) |>
  drop_na() |>
  mutate(period = if_else(year == 2024 & month >= 7, "Sampling", "Background"),
         Area   = str_to_title(region),
         label  = paste0(str_sub(month.abb[month], 1, 1), year))

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
                             "Run", "Run_an",
                             "MHW_d", "MHW_im", "MHW_ix"))

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
       title  = "PCA environmental space — 2014–2024",
       subtitle = "Large circles = sampling period (Jul–Dec 2024); size = mean shell length") +
  theme_reclam

ggsave(here("FIG", "Fig_PCA_ENV.jpeg"),
       fig_pca, width = 9, height = 7, dpi = 300)

## 6.5 PCA loadings table ------------------------------------------------------

pca_load_tab <- as_tibble(pca_fit$rotation[, 1:3], rownames = "env_variable") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

write_csv(pca_load_tab, here("RESULTS", "Table_PCA_Loadings.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 7. INTEGRATED TIME SERIES FIGURE
#    Concurrent panel: SST + MHW + Lmean + RI per region
# ─────────────────────────────────────────────────────────────────────────────

ts_data <- joined_full |>
  filter(!is.na(sst_mean)) |>                      # solo meses con env disponible
  mutate(date_plot = as.Date(paste(year, month, "15", sep = "-")))

# Panel A: SST mean + anomaly
p_sst <- ggplot(ts_data, aes(x = date_plot, colour = Area)) +
  geom_line(aes(y = sst_mean), linewidth = 1) +
  geom_point(aes(y = sst_mean), size = 2.5) +
  geom_col(aes(y = sst_anom * 0.5 + 19, fill = Area),  # scaled overlay
           alpha = 0.25, width = 20, position = "identity") +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values = pal_area) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "SST (°C)", colour = NULL, fill = NULL,
       title = "SST") +
  theme_reclam

# Panel B: MHW intensity
p_mhw <- ggplot(ts_data |> filter(mhw_days > 0),
                aes(x = date_plot, y = mhw_int_mean, fill = Area)) +
  geom_col(position = "dodge", width = 15, alpha = 0.85) +
  scale_fill_manual(values = pal_area) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = range(ts_data$date_plot)) +
  labs(x = NULL, y = "MHW intensity (°C)", fill = NULL,
       title = "MHW mean intensity") +
  theme_reclam

# Panel C: Mean shell length
p_lmean <- ggplot(ts_data, aes(x = date_plot, y = Lmean, colour = Area)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = N), alpha = 0.85) +
  scale_colour_manual(values = pal_area) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "L̄ (mm)", colour = NULL, size = "n ind.",
       title = "Mean shell length") +
  theme_reclam

# Panel D: Recruitment index
p_ri <- ggplot(ts_data, aes(x = date_plot, y = RI * 100, colour = Area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = pal_area) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "RI (%)", colour = NULL,
       title = "Recruitment index (SL ≤ 8 mm)") +
  theme_reclam

fig_ts_panel <- (p_sst / p_mhw / p_lmean / p_ri) +
  plot_annotation(
    tag_levels = "a",
    title      = expression(italic("D. trunculus") ~
                               "— environmental & biological time series (Jul–Dec 2024)")
  )

ggsave(here("FIG", "Fig_TS_ENV_BIO.jpeg"),
       fig_ts_panel, width = 10, height = 14, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 8. EXPORT SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

write_csv(joined_lags,  here("RESULTS", "Data_BIO_ENV_overlap.csv"))
write_csv(corr_results, here("RESULTS", "Table_Spearman_ENV_BIO.csv"))
write_csv(pca_load_tab, here("RESULTS", "Table_PCA_Loadings.csv"))
write_csv(bio_monthly,  here("RESULTS", "Data_BIO_monthly.csv"))

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
message("========================================\n")
