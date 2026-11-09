# ENV_BIO_Correlation.R
# Environmental drivers of Donax trunculus population indicators
# Gulf of Cádiz (GoC) vs. Gulf of Valencia (GoV)
# Project RECLAM — IEO-CSIC / UPV
#
# Analyses:
#   0.  Setup
#   1.  Biological data loading + monthly aggregation (conventions from COMPARA_RECLAM.R)
#   2.  Environmental data loading (env_monthly_all_2021_2025.csv, 2021–2025)
#   3.  Join + temporal alignment (dynamic overlap, see OVERLAP_END in section 3)
#   4.  Environmental context: monthly series + sampling window
#   5.  Spearman correlations at lags 0, 1, 2 + FDR correction
#   6.  PCA of environmental space + biological overlay
#   7.  GAM drivers of population indicators, by region
#   7b. Pooled GAM (both regions) with region x environment interaction
#   8.  Export tables
#
# NOTE: Data coverage (check console messages for the exact current range):
#       Biological data: Cádiz Jul 2024 – Sep 2025 (no gaps); Valencia
#       Jul 2024 – Sep 2025 (gaps: Mar 2025, Aug 2025).
#       Environmental data: Jan 2021 – Dec 2025 (env_monthly_all_2021_2025.csv,
#       60 months per region). Anomalies (*_anom) in this file are NOT used
#       (the 2021–2025 climatology is too short to be a meaningful baseline);
#       all analyses use the base variables SST, wind speed, runoff and
#       MHW days.
#       OVERLAP_END = min(last biological month, last environmental month),
#       computed automatically. n per region may differ (Cádiz vs. Valencia)
#       because of gaps in biological sampling.
#       All correlations are EXPLORATORY and should be interpreted with caution.
#
# Figures: no titles or subtitles (captions go in the manuscript), except the
#          GAM partial-effect panels, whose subtitle reports edf | p | n.
#          Default size FIG_W x FIG_H = 6 x 7 in; GAM panels GAM_W x GAM_H = 5 x 6 in.
#
# Author  : Mauricio Mardones
# Updated : 2026-10

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
  library(corrplot)    # correlation matrix visualisation
  library(FactoMineR)  # PCA
  library(factoextra)  # PCA visualisation
  library(mgcv)        # GAM
  library(vegan)       # PERMANOVA / PERMDISP
})

theme_reclam <- theme_bw(base_size = 10) +
  theme(
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "grey92", colour = NA),
    legend.position   = "bottom",
    legend.key.size   = unit(0.4, "cm"),
    axis.title        = element_text(size = 9)
  )

# Figure size (inches) and resolution for all exported figures
FIG_W   <- 5
FIG_H   <- 6
FIG_DPI <- 300

# GAM partial-effect panels (section 7.3)
GAM_W <- 5
GAM_H <- 6

save_fig <- function(file, plot, width = FIG_W, height = FIG_H) {
  ggsave(here("FIG", file), plot, width = width, height = height, dpi = FIG_DPI)
}

# Region colours and display labels (legends and facet strips)
pal_area      <- c(Cadiz = "#1B7FC4", Valencia = "#D94F00")
region_labels <- c(Cadiz = "Gulf of Cádiz", Valencia = "Gulf of Valencia")

# Population indicators: internal names and display labels
bio_indicators <- c("Lmean", "L_p90", "RI", "CPUE", "Density")

bio_short <- c(Lmean = "Mean length", L_p90 = "L90", RI = "RI",
               CPUE = "CPUE", Density = "Density")

# Plotmath axis labels (avoid unicode superscripts that do not render in
# some graphic devices). Order must match bio_indicators.
bio_axis_labels <- c(
  expression(bar(L) ~ "(mm)"),
  expression(L[90] ~ "(mm)"),
  expression("RI"),
  expression("CPUE (kg" ~ h^{-1} * ")"),
  expression("Density (ind" ~ m^{-2} * ")")
)

# Environmental variables: display labels
env_labels <- c(sst_mean    = "SST",
                wind_speed  = "Wind speed",
                runoff_mm   = "Runoff",
                mhw_days    = "MHW days")

lag_labels <- c(lag0 = "Lag 0", lag1 = "Lag 1 month", lag2 = "Lag 2 months")

# Month labels for points (e.g. "Jul-24"); three-letter month avoids the
# ambiguity of single-letter labels (J = Jan/Jun/Jul, M = Mar/May, A = Apr/Aug)
month_label <- function(month, year) {
  paste0(month.abb[month], "-", str_sub(year, 3, 4))
}

dir.create(here("FIG"),     showWarnings = FALSE)
dir.create(here("RESULTS"), showWarnings = FALSE)

# Temporal overlap constants
# OVERLAP_START is fixed (start of the sampling design). OVERLAP_END is not
# hard-coded: it is computed in section 3 as the minimum of the last month
# with biological data and the last month with environmental data.
OVERLAP_START <- as.Date("2024-07-01")


# ─────────────────────────────────────────────────────────────────────────────
# 1. BIOLOGICAL DATA — monthly aggregation
#    Source: Template_Reclam_Cadiz.xlsx / Template_Reclam_Valencia.xlsx
# ─────────────────────────────────────────────────────────────────────────────

archivo_cadiz    <- here("DATA", "Data_Final", "Template_Reclam_Cadiz.xlsx")
archivo_valencia <- here("DATA", "Data_Final", "Template_Reclam_Valencia.xlsx")

stopifnot(
  "Template Cadiz not found"    = file.exists(archivo_cadiz),
  "Template Valencia not found" = file.exists(archivo_valencia)
)

## 1.1 Size data (wedge clam only) --------------------------------------------

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
  filter(tolower(especie) == "co") |>              # D. trunculus only
  mutate(year  = year(FECHA),
         month = month(FECHA))

## 1.2 Monthly biological indicators ------------------------------------------
# Lmean, L_p90, RI. N is kept only as the sample size behind each monthly
# Lmean (it is not used as an abundance indicator; see Density in 1.3).
# RECRUIT_THRESHOLD: shell length (mm) defining a recruit for RI.
# CHECK: COMPARA_RECLAM.R and the manuscript use L50 = 10.8 mm.

RECRUIT_THRESHOLD <- 10.8

bio_monthly <- tallas |>
  group_by(Area, year, month) |>
  summarise(
    Lmean = mean(TALLA, na.rm = TRUE),
    L_p90 = quantile(TALLA, 0.90, na.rm = TRUE),
    RI    = mean(TALLA <= RECRUIT_THRESHOLD, na.rm = TRUE),  # proportion of recruits
    N     = n(),                                             # sample size
    .groups = "drop"
  ) |>
  mutate(date   = as.Date(paste(year, month, "15", sep = "-")),
         region = tolower(Area))

cat(sprintf("Biological data: %d region-months (Cadiz: %d, Valencia: %d)\n",
            nrow(bio_monthly),
            sum(bio_monthly$Area == "Cadiz"),
            sum(bio_monthly$Area == "Valencia")))

## 1.3 CPUE and Density (computed in COMPARA_RECLAM.R) -------------------------
# Requires running COMPARA_RECLAM.R first; it writes
# RESULTS/Table_Density_CPUE_Monthly.csv with Area/year/month/dens_mean/cpue_mean

indicators_path <- here("RESULTS", "Table_Density_CPUE_Monthly.csv")
stopifnot("Table_Density_CPUE_Monthly.csv not found -- run COMPARA_RECLAM.R first" =
            file.exists(indicators_path))

cpue_dens_monthly <- read_csv(indicators_path, show_col_types = FALSE,
                               col_types = cols(dens_mean = col_double(),
                                                 dens_se   = col_double(),
                                                 cpue_mean = col_double(),
                                                 cpue_se   = col_double())) |>
  mutate(region = tolower(Area)) |>
  dplyr::select(region, year, month, Density = dens_mean, CPUE = cpue_mean)

bio_monthly <- bio_monthly |>
  left_join(cpue_dens_monthly, by = c("region", "year", "month"))

cat(sprintf("CPUE/Density joined: %d/%d months with CPUE, %d/%d months with Density\n",
            sum(!is.na(bio_monthly$CPUE)),    nrow(bio_monthly),
            sum(!is.na(bio_monthly$Density)), nrow(bio_monthly)))


# ─────────────────────────────────────────────────────────────────────────────
# 2. ENVIRONMENTAL DATA
# ─────────────────────────────────────────────────────────────────────────────
# 2021–2025 series (formerly env_monthly_all.csv, 2014–2025). It also
# includes chl_mean/chl_anom and q_total/q_total_anom (with some NA months);
# these are NOT included in env_vars, so results remain comparable with the
# previous version.

env_path <- here("DATA", "Environmental_Data", "env_monthly_all_2021_2025.csv")
stopifnot("env_monthly_all_2021_2025.csv not found" = file.exists(env_path))

env <- read_csv(env_path, show_col_types = FALSE) |>
  mutate(date   = as.Date(date),
         region = tolower(region))   # harmonise to lowercase

# Lags of 1–2 months require environmental data from OVERLAP_START - 2 months
stopifnot("Environmental series does not cover OVERLAP_START - 2 months (lags)" =
            min(env$date, na.rm = TRUE) <= (OVERLAP_START %m-% months(2)))

# Base variables only. Anomalies (*_anom) are present in the file but are
# not used: their 2021–2025 climatology is too short to be a reference.
env_vars <- c("sst_mean", "wind_speed", "runoff_mm", "mhw_days")

cat(sprintf("Environmental data: %d rows | %d–%d | regions: %s\n",
            nrow(env), min(env$year), max(env$year),
            paste(unique(env$region), collapse = ", ")))


# ─────────────────────────────────────────────────────────────────────────────
# 3. JOIN + TEMPORAL ALIGNMENT
# ─────────────────────────────────────────────────────────────────────────────

# Full join (all biological months; environment is NA where not covered)
joined_full <- bio_monthly |>
  left_join(env |> dplyr::select(-date), by = c("region", "year", "month"))

# Dynamic OVERLAP_END: the most restrictive of the last biological month and
# the last environmental month
OVERLAP_END <- min(max(bio_monthly$date, na.rm = TRUE),
                   max(env$date, na.rm = TRUE))

cat(sprintf("Overlap window: %s to %s (biological data to %s, environmental data to %s)\n",
            OVERLAP_START, OVERLAP_END,
            max(bio_monthly$date, na.rm = TRUE), max(env$date, na.rm = TRUE)))

# Concurrent overlap (n may differ by region because of biological gaps)
joined_overlap <- joined_full |>
  filter(!is.na(sst_mean)) |>
  filter(date >= OVERLAP_START, date <= OVERLAP_END)

cat(sprintf("n per region (Cadiz / Valencia): %s\n",
            paste(table(joined_overlap$Area), collapse = " / ")))

# Lagged environmental variables (1 and 2 months)
# Environment leads biology: bio[month t] is matched with env[month t-1] or t-2

add_env_lags <- function(env_df, bio_df, lag_months) {
  env_lagged <- env_df |>
    mutate(month_bio = month + lag_months,
           year_bio  = year + (month_bio > 12),
           month_bio = if_else(month_bio > 12, month_bio - 12, month_bio)) |>
    dplyr::select(region, year_bio, month_bio, all_of(env_vars)) |>
    rename(year = year_bio, month = month_bio) |>
    rename_with(~ paste0(., "_lag", lag_months), all_of(env_vars))

  bio_df |> left_join(env_lagged, by = c("region", "year", "month"))
}

joined_lag1 <- add_env_lags(env, joined_overlap, 1)
joined_lag2 <- add_env_lags(env, joined_overlap, 2)

# Master lag dataset
joined_lags <- joined_overlap |>
  left_join(
    joined_lag1 |> dplyr::select(region, year, month, ends_with("_lag1")),
    by = c("region", "year", "month")
  ) |>
  left_join(
    joined_lag2 |> dplyr::select(region, year, month, ends_with("_lag2")),
    by = c("region", "year", "month")
  )


# ─────────────────────────────────────────────────────────────────────────────
# 4. ENVIRONMENTAL CONTEXT — full series with sampling window shaded
# ─────────────────────────────────────────────────────────────────────────────

env_context <- env |>
  mutate(Area = str_to_title(region))

sampling_rect <- tibble(xmin = OVERLAP_START, xmax = OVERLAP_END,
                        ymin = -Inf, ymax = Inf)

## 4.1 SST ---------------------------------------------------------------------

fig_context_sst <- ggplot(env_context, aes(x = date, y = sst_mean, colour = Area)) +
  geom_rect(data = sampling_rect,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  scale_colour_manual(values = pal_area, labels = region_labels) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "SST (°C)", colour = "Region") +
  theme_reclam

## 4.2 MHW days ----------------------------------------------------------------

fig_context_mhw <- ggplot(env_context, aes(x = date, y = mhw_days)) +
  geom_rect(data = sampling_rect,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_col(aes(fill = Area), position = "identity", alpha = 0.6, width = 25) +
  scale_fill_manual(values = pal_area, labels = region_labels) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "MHW days per month", fill = "Region") +
  theme_reclam

## 4.3 Combined panel ----------------------------------------------------------

fig_context <- (fig_context_sst / fig_context_mhw) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

save_fig("FigS_ENV_Context.jpeg", fig_context)


# ─────────────────────────────────────────────────────────────────────────────
# 5. SPEARMAN CORRELATIONS — lags 0, 1, 2
#    n per region depends on the dynamic overlap (section 3). Lags of 1–2
#    months do not reduce n because the environmental series starts in 2021.
#    All results are EXPLORATORY.
# ─────────────────────────────────────────────────────────────────────────────

## 5.1 Spearman rho function ---------------------------------------------------

spearman_safe <- function(x, y) {
  idx <- complete.cases(x, y)
  if (sum(idx) < 4) return(tibble(rho = NA_real_, p = NA_real_, n = sum(idx)))
  ct <- cor.test(x[idx], y[idx], method = "spearman", exact = FALSE)
  tibble(rho = unname(ct$estimate), p = ct$p.value, n = sum(idx))
}

## 5.2 Correlations across lags, regions and variable pairs -------------------

run_corr_block <- function(data_df, lag_suffix, area_nm, vars = env_vars) {
  ev <- if (lag_suffix == "lag0") vars else paste0(vars, "_", lag_suffix)

  map_dfr(bio_indicators, function(bio_v) {
    map_dfr(seq_along(ev), function(i) {
      spearman_safe(data_df[[ev[i]]], data_df[[bio_v]]) |>
        mutate(bio_var = bio_v,
               env_var = vars[i],
               lag     = lag_suffix,
               Area    = area_nm)
    })
  })
}

run_corr_all <- function(vars) {
  bind_rows(
    run_corr_block(joined_lags |> filter(Area == "Cadiz"),    "lag0", "Cadiz",    vars),
    run_corr_block(joined_lag1 |> filter(Area == "Cadiz"),    "lag1", "Cadiz",    vars),
    run_corr_block(joined_lag2 |> filter(Area == "Cadiz"),    "lag2", "Cadiz",    vars),
    run_corr_block(joined_lags |> filter(Area == "Valencia"), "lag0", "Valencia", vars),
    run_corr_block(joined_lag1 |> filter(Area == "Valencia"), "lag1", "Valencia", vars),
    run_corr_block(joined_lag2 |> filter(Area == "Valencia"), "lag2", "Valencia", vars)
  ) |>
    # FDR correction (Benjamini–Hochberg) within region x lag
    group_by(Area, lag) |>
    mutate(p_adj = p.adjust(p, method = "BH")) |>
    ungroup() |>
    mutate(
      sig = case_when(
        is.na(p_adj) ~ "",
        p_adj < 0.05 ~ "*",
        p_adj < 0.10 ~ "†",
        TRUE         ~ ""
      ),
      rho_label = sprintf("%.2f%s", rho, sig)
    )
}

corr_results <- run_corr_all(env_vars)
write_csv(corr_results, here("RESULTS", "Table_Spearman_ENV_BIO.csv"))

## 5.3 Heatmap function — rho by env_var x bio_var, faceted lag x region ------
# Grey cells with "n/a": correlation undefined (insufficient variance)

plot_corr_heatmap <- function(corr_df, vars) {
  corr_df |>
    mutate(
      bio_var = factor(bio_var, levels = bio_indicators),
      env_var = factor(env_var, levels = vars, labels = env_labels[vars]),
      lag     = factor(lag, levels = names(lag_labels), labels = lag_labels),
      label_display = if_else(is.na(rho), "n/a", rho_label)
    ) |>
    ggplot(aes(x = bio_var, y = env_var, fill = rho)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = label_display), size = 2.4, colour = "grey10") +
    facet_grid(lag ~ Area, labeller = labeller(Area = region_labels)) +
    scale_x_discrete(labels = bio_axis_labels) +
    scale_fill_gradient2(low  = "#d8b365", mid = "white", high = "#5ab4ac",
                         midpoint = 0, limits = c(-1, 1), na.value = "grey85",
                         name = expression("Spearman" ~ rho)) +
    labs(x = NULL, y = NULL) +
    theme_reclam +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "right")
}

fig_corr_heat <- plot_corr_heatmap(corr_results, env_vars)
save_fig("Fig_Spearman_Heatmap.jpeg", fig_corr_heat)


# ─────────────────────────────────────────────────────────────────────────────
# 6. PCA — ENVIRONMENTAL SPACE (full 2021–2025 series) + SAMPLING OVERLAY
# ─────────────────────────────────────────────────────────────────────────────
# The PCA uses all available environmental months (2021–2025, 60 per region)
# to define the space; sampling months (OVERLAP_START–OVERLAP_END) are
# highlighted. With the short series the sampling window represents ~25% of
# the points, so the "Background" is not a long-term reference.

scale_arrow <- 3   # scale loading arrows to the score range

## 6.1–6.4 Environmental PCA function -----------------------------------------

run_env_pca <- function(vars) {
  mat <- env |>
    dplyr::select(region, year, month, date, all_of(vars)) |>
    drop_na() |>
    mutate(period = if_else(date >= OVERLAP_START & date <= OVERLAP_END,
                            "Sampling", "Background"),
           Area   = str_to_title(region),
           label  = month_label(month, year))

  fit <- prcomp(mat |> dplyr::select(all_of(vars)), scale. = TRUE, center = TRUE)

  scores <- as_tibble(fit$x[, 1:3]) |>
    bind_cols(mat |> dplyr::select(region, year, month, period, Area, label))

  var_exp <- round(summary(fit)$importance[2, 1:3] * 100, 1)

  scores_bio <- scores |>
    filter(period == "Sampling") |>
    left_join(bio_monthly |> dplyr::select(region, year, month, Lmean, RI),
              by = c("region", "year", "month"))

  loadings <- as_tibble(fit$rotation[, 1:2], rownames = "variable") |>
    mutate(variable_short = env_labels[variable])

  fig <- ggplot(scores, aes(x = PC1, y = PC2)) +
    geom_point(data = scores |> filter(period == "Background"),
               aes(colour = Area), alpha = 0.3, size = 1.2) +
    geom_point(data = scores_bio,
               aes(colour = Area, size = Lmean), alpha = 0.9, shape = 21,
               fill = "white", stroke = 1.2) +
    geom_text_repel(data = scores_bio,
                    aes(label = label, colour = Area),
                    size = 2.3, max.overlaps = 20, show.legend = FALSE) +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
                 arrow = arrow(length = unit(0.2, "cm")),
                 colour = "grey30", linewidth = 0.6) +
    geom_text(data = loadings,
              aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15,
                  label = variable_short),
              size = 2.8, colour = "grey20") +
    scale_colour_manual(values = pal_area, labels = region_labels) +
    scale_size_continuous(name = expression(bar(L) ~ "(mm)"), range = c(2, 6)) +
    labs(x = sprintf("PC1 (%s%%)", var_exp[1]),
         y = sprintf("PC2 (%s%%)", var_exp[2]),
         colour = "Region") +
    theme_reclam +
    theme(legend.box = "vertical")

  load_tab <- as_tibble(fit$rotation[, 1:3], rownames = "env_variable") |>
    mutate(across(where(is.numeric), \(x) round(x, 3)))

  list(fit = fit, fig = fig, loadings = load_tab)
}

## 6.5 Environmental PCA (base variables) --------------------------------------

pca_env <- run_env_pca(env_vars)
pca_load_tab <- pca_env$loadings
save_fig("Fig_PCA_ENV.jpeg", pca_env$fig)
write_csv(pca_load_tab, here("RESULTS", "Table_PCA_Loadings.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 6c. PCA — POPULATION INDICATOR SPACE (RI, CPUE, Density; no environment)
#     Only the three population indicators are active variables, so the test
#     answers "do the populations (not their habitat) differ between
#     regions?". Mixing indicators with environment confounds regional
#     environmental differences with population differences (e.g. the
#     Density + environment PERMANOVA was significant whereas the Wilcoxon
#     test on Density alone was not). Bio–environment relationships are
#     addressed in sections 5 (Spearman) and 7 (GAM).
# ─────────────────────────────────────────────────────────────────────────────

bio_pca_vars_list <- c("RI", "CPUE", "Density")

bio_pca_mat <- bio_monthly |>
  dplyr::select(Area, region, year, month, all_of(bio_pca_vars_list)) |>
  drop_na(all_of(bio_pca_vars_list)) |>
  mutate(label = month_label(month, year))

cat(sprintf("Population PCA (RI/CPUE/Density, no environment): %d region-months (Cadiz: %d, Valencia: %d)\n",
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
  mutate(variable_short = bio_short[variable])

fig_pca_bio <- ggplot(scores_bio, aes(x = PC1, y = PC2)) +
  geom_polygon(data = hull_bio, aes(fill = Area, colour = Area),
               alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(colour = Area), size = 2.2, alpha = 0.9) +
  geom_text_repel(aes(label = label, colour = Area), size = 2.3,
                  max.overlaps = 20, show.legend = FALSE) +
  geom_segment(data = loadings_bio,
               aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey30", linewidth = 0.6) +
  geom_text(data = loadings_bio,
            aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15,
                label = variable_short),
            size = 3, colour = "grey20") +
  scale_colour_manual(values = pal_area, labels = region_labels) +
  scale_fill_manual(values = pal_area, guide = "none") +
  labs(x = sprintf("PC1 (%s%%)", var_exp_bio[1]),
       y = sprintf("PC2 (%s%%)", var_exp_bio[2]),
       colour = "Region") +
  theme_reclam

save_fig("Fig_PCA_BIO.jpeg", fig_pca_bio)

pca_load_tab_bio <- as_tibble(pca_fit_bio$rotation[, 1:2], rownames = "variable") |>
  mutate(variable = bio_short[variable],
         across(where(is.numeric), \(x) round(x, 3)))
write_csv(pca_load_tab_bio, here("RESULTS", "Table_PCA_Loadings_BIO.csv"))

## 6c.1 PERMANOVA — Cádiz vs Valencia, population space only -----------------

set.seed(2025)

vars_scaled_bio <- scale(pca_bio_vars)
dist_bio        <- vegan::vegdist(vars_scaled_bio, method = "euclidean")

permanova_bio <- vegan::adonis2(dist_bio ~ Area, data = bio_pca_mat, permutations = 999)
disp_bio      <- vegan::betadisper(dist_bio, bio_pca_mat$Area)
permdisp_bio  <- vegan::permutest(disp_bio, permutations = 999)

permanova_bio_result <- tibble(
  space       = "RI + CPUE + Density (no environment)",
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

## 6c.2 Wilcoxon — Cádiz vs Valencia, one indicator at a time -----------------
# Uses the full bio_monthly (no environmental data needed).

run_wilcox <- function(bio_v) {
  df <- bio_monthly |> dplyr::select(Area, all_of(bio_v)) |> drop_na()
  wt <- wilcox.test(as.formula(paste0(bio_v, " ~ Area")), data = df)

  tibble(
    bio_var         = bio_short[[bio_v]],
    W               = unname(wt$statistic),
    p               = wt$p.value,
    median_cadiz    = round(median(df[[bio_v]][df$Area == "Cadiz"],    na.rm = TRUE), 3),
    median_valencia = round(median(df[[bio_v]][df$Area == "Valencia"], na.rm = TRUE), 3),
    n_cadiz         = sum(df$Area == "Cadiz"),
    n_valencia      = sum(df$Area == "Valencia")
  )
}

wilcox_results <- map_dfr(bio_pca_vars_list, run_wilcox) |>
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
# 6d. PCA PER POPULATION INDICATOR + ENVIRONMENT — SUPPLEMENTARY / EXPLORATORY
#     WARNING: these PCAs use the indicator and the environment as active
#     variables. Do NOT use the PERMANOVA in 6d.5 as evidence that the
#     indicator differs between regions; it can be dominated by the
#     Atlantic vs. Mediterranean environmental contrast. Use 6c.1/6c.2 for
#     that test.
# ─────────────────────────────────────────────────────────────────────────────

run_bio_env_pca <- function(bio_v) {
  active_vars <- c(bio_v, env_vars)

  mat <- joined_lags |>
    dplyr::select(Area, region, year, month, all_of(active_vars)) |>
    drop_na(all_of(active_vars)) |>
    mutate(label = month_label(month, year))

  cat(sprintf("PCA %s + environment (supplementary): %d region-months (Cadiz: %d, Valencia: %d)\n",
              bio_v, nrow(mat), sum(mat$Area == "Cadiz"), sum(mat$Area == "Valencia")))

  pca_vars <- mat |> dplyr::select(all_of(active_vars))
  fit      <- prcomp(pca_vars, scale. = TRUE, center = TRUE)

  scores <- as_tibble(fit$x[, 1:2]) |>
    bind_cols(mat |> dplyr::select(Area, region, year, month, label))

  var_exp <- round(summary(fit)$importance[2, 1:2] * 100, 1)

  hull <- scores |>
    group_by(Area) |>
    slice(chull(PC1, PC2)) |>
    ungroup()

  loadings <- as_tibble(fit$rotation[, 1:2], rownames = "variable") |>
    mutate(is_bio = variable == bio_v,
           variable_short = if_else(is_bio, bio_short[variable], env_labels[variable]))

  fig <- ggplot(scores, aes(x = PC1, y = PC2)) +
    geom_polygon(data = hull, aes(fill = Area, colour = Area),
                 alpha = 0.12, linewidth = 0.4) +
    geom_point(aes(colour = Area), size = 1.8, alpha = 0.9) +
    geom_text_repel(aes(label = label, colour = Area), size = 2,
                    max.overlaps = 20, show.legend = FALSE) +
    geom_segment(data = loadings |> filter(!is_bio),
                 aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
                 arrow = arrow(length = unit(0.15, "cm")), colour = "grey30", linewidth = 0.5) +
    geom_text(data = loadings |> filter(!is_bio),
              aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15,
                  label = variable_short),
              size = 2.3, colour = "grey20") +
    geom_segment(data = loadings |> filter(is_bio),
                 aes(x = 0, y = 0, xend = PC1 * scale_arrow, yend = PC2 * scale_arrow),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "firebrick", linewidth = 0.9) +
    geom_text(data = loadings |> filter(is_bio),
              aes(x = PC1 * scale_arrow * 1.15, y = PC2 * scale_arrow * 1.15,
                  label = variable_short),
              size = 2.8, colour = "firebrick", fontface = "bold") +
    scale_colour_manual(values = pal_area, labels = region_labels) +
    scale_fill_manual(values = pal_area, guide = "none") +
    labs(x = sprintf("PC1 (%s%%)", var_exp[1]),
         y = sprintf("PC2 (%s%%)", var_exp[2]),
         colour = "Region") +
    theme_reclam

  load_tab <- as_tibble(fit$rotation[, 1:2], rownames = "variable") |>
    mutate(variable = if_else(variable == bio_v, bio_short[variable], env_labels[variable]),
           across(where(is.numeric), \(x) round(x, 3)))

  list(fig = fig, loadings = load_tab, pca_vars = pca_vars, mat = mat)
}

## 6d.1–6d.3 RI, CPUE, Density + environment ----------------------------------

pca_ri   <- run_bio_env_pca("RI")
pca_cpue <- run_bio_env_pca("CPUE")
pca_dens <- run_bio_env_pca("Density")

save_fig("Fig_PCA_RI_ENV.jpeg",      pca_ri$fig)
save_fig("Fig_PCA_CPUE_ENV.jpeg",    pca_cpue$fig)
save_fig("Fig_PCA_Densidad_ENV.jpeg", pca_dens$fig)

write_csv(pca_ri$loadings,   here("RESULTS", "Table_PCA_Loadings_RI_ENV.csv"))
write_csv(pca_cpue$loadings, here("RESULTS", "Table_PCA_Loadings_CPUE_ENV.csv"))
write_csv(pca_dens$loadings, here("RESULTS", "Table_PCA_Loadings_Densidad_ENV.csv"))

## 6d.4 Combined panel (a–c) ---------------------------------------------------

fig_pca_bio_panel <- (pca_ri$fig + pca_cpue$fig + pca_dens$fig) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a")

save_fig("Fig_PCA_BIO_panel.jpeg", fig_pca_bio_panel)

## 6d.5 PERMANOVA — indicator + environment space (EXPLORATORY) ---------------
# A significant result here indicates that regions occupy different parts of
# the [indicator + environment] space, consistent with distinct Atlantic and
# Mediterranean SST/wind regimes; it does NOT imply that the indicator itself
# differs (see 6c.1/6c.2).

run_permanova_env <- function(pca_vars, mat, bio_v) {
  dist_mat <- vegan::vegdist(scale(pca_vars), method = "euclidean")

  pmv  <- vegan::adonis2(dist_mat ~ Area, data = mat, permutations = 999)
  disp <- vegan::betadisper(dist_mat, mat$Area)
  pdsp <- vegan::permutest(disp, permutations = 999)

  tibble(
    bio_var      = bio_short[[bio_v]],
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
  run_permanova_env(pca_ri$pca_vars,   pca_ri$mat,   "RI"),
  run_permanova_env(pca_cpue$pca_vars, pca_cpue$mat, "CPUE"),
  run_permanova_env(pca_dens$pca_vars, pca_dens$mat, "Density")
) |>
  mutate(
    p_permanova_adj = p.adjust(p_permanova, method = "BH"),
    sig = case_when(
      is.na(p_permanova_adj) ~ "",
      p_permanova_adj < 0.05 ~ "*",
      p_permanova_adj < 0.10 ~ "†",
      TRUE                   ~ ""
    )
  )

print(permanova_env_results)
write_csv(permanova_env_results, here("RESULTS", "Table_PERMANOVA_byVariable_ENV_supl.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 7. GAM — DRIVERS OF POPULATION INDICATORS, BY REGION
#    Univariate models bio ~ s(env, k = 3), fitted separately for Cádiz and
#    Valencia (not pooled). Environmental variables: env_vars at lags
#    0, 1 and 2 months (same datasets as section 5).
#    CAUTION: n per region is moderate; see Table_GAM_ENV_BIO_byRegion.csv
#    for the n of each model.
# ─────────────────────────────────────────────────────────────────────────────

## 7.1 Univariate GAM by region + FDR -------------------------------------------

gam_safe_region <- function(data_df, bio_v, env_v) {
  df <- data_df |> dplyr::select(all_of(c(bio_v, env_v))) |> drop_na()
  n  <- nrow(df)

  empty <- tibble(edf = NA_real_, Fstat = NA_real_, p = NA_real_,
                  dev_expl = NA_real_, r2 = NA_real_, n = n)

  if (n < 5 || length(unique(df[[env_v]])) < 3) return(empty)

  form <- as.formula(paste0(bio_v, " ~ s(", env_v, ", k = 3)"))
  m <- tryCatch(mgcv::gam(form, data = df, method = "REML"),
                error = function(e) NULL)
  if (is.null(m)) return(empty)

  s_tab <- summary(m)$s.table
  tibble(edf      = unname(s_tab[1, "edf"]),
         Fstat    = unname(s_tab[1, "F"]),
         p        = unname(s_tab[1, "p-value"]),
         dev_expl = summary(m)$dev.expl,
         r2       = summary(m)$r.sq,
         n        = n)
}

run_gam_block_region <- function(data_df, lag_suffix, area_nm) {
  ev <- if (lag_suffix == "lag0") env_vars else paste0(env_vars, "_", lag_suffix)

  map_dfr(bio_indicators, function(bio_v) {
    map_dfr(seq_along(ev), function(i) {
      gam_safe_region(data_df, bio_v, ev[i]) |>
        mutate(bio_var = bio_v, env_var = env_vars[i],
               lag = lag_suffix, Area = area_nm)
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

## 7.2 Heatmap — deviance explained by env_var x bio_var, faceted lag x region

fig_gam_heat_region <- gam_results_region |>
  mutate(
    bio_var = factor(bio_var, levels = bio_indicators),
    env_var = factor(env_var, levels = env_vars,
                     labels = env_labels[env_vars]),
    lag     = factor(lag, levels = names(lag_labels), labels = lag_labels),
    label_display = if_else(is.na(dev_expl), "n/a",
                            sprintf("%.0f%%%s", dev_expl * 100, sig))
  ) |>
  ggplot(aes(x = bio_var, y = env_var, fill = dev_expl)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label_display), size = 2.4, colour = "grey10") +
  facet_grid(lag ~ Area, labeller = labeller(Area = region_labels)) +
  scale_x_discrete(labels = bio_axis_labels) +
  scale_fill_viridis_c(option = "G", alpha = 0.5,
                       limits = c(0, 1), na.value = "grey85",
                       name = "Deviance\nexplained") +
  labs(x = NULL, y = NULL) +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "right")

save_fig("Fig_GAM_Heatmap_byRegion.jpeg", fig_gam_heat_region)

## 7.3 Partial effects (smooths) by region ------------------------------------
# .build_gam_partial(): fits bio ~ s(env, k = 3) for one combination and
#   returns the ggplot (equivalent to plot.gam(..., residuals = TRUE)).
# plot_gam_partial(): single-panel wrapper that also saves the figure.
# plot_gam_partial_grid(): all bio_indicators x env_vars panels for one
#   region/lag; axis titles only on the outer row/column.

.build_gam_partial <- function(bio_v, env_v, area_nm, lag_suffix = "lag0",
                               show_x = TRUE, show_y = TRUE) {

  data_src <- switch(lag_suffix,
                     lag0 = joined_lags,
                     lag1 = joined_lag1,
                     lag2 = joined_lag2,
                     stop("lag_suffix must be 'lag0', 'lag1' or 'lag2'"))

  env_col <- if (lag_suffix == "lag0") env_v else paste0(env_v, "_", lag_suffix)

  df <- data_src |>
    filter(Area == area_nm) |>
    dplyr::select(all_of(c(bio_v, env_col))) |>
    drop_na()

  stopifnot("Too few observations to fit the GAM (n < 5)" = nrow(df) >= 5,
            "Environmental variable with insufficient variation (< 3 unique values)" =
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

  # Partial residuals at observed points (component + residual)
  pred_obs <- predict(m, newdata = df, type = "terms")
  df <- df |>
    mutate(partial_resid = as.numeric(pred_obs[, 1]) + residuals(m, type = "response"))

  # edf, p-value and n of the smooth, shown as panel subtitle
  s_tab    <- summary(m)$s.table
  edf      <- unname(s_tab[1, "edf"])
  pval     <- unname(s_tab[1, "p-value"])
  p_txt    <- if (pval < 0.001) "p<0.001" else sprintf("p=%.3f", pval)
  lag_txt  <- if (lag_suffix == "lag0") "" else paste0(" (", tolower(lag_labels[[lag_suffix]]), ")")
  stat_lab <- sprintf("edf=%.2f | %s | n=%d%s", edf, p_txt, nrow(df), lag_txt)

  x_lab <- if (lag_suffix == "lag0") env_labels[[env_v]] else
    paste0(env_labels[[env_v]], " (", tolower(lag_labels[[lag_suffix]]), ")")

  ggplot(grid, aes(x = .data[[env_col]], y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = pal_area[[area_nm]], alpha = 0.2) +
    geom_line(colour = pal_area[[area_nm]], linewidth = 0.9) +
    geom_point(data = df, aes(x = .data[[env_col]], y = partial_resid),
               colour = "black", size = 1.2, alpha = 0.8) +
    labs(x = if (show_x) x_lab else NULL,
         y = if (!show_y) NULL else if (bio_v == "Lmean") expression(s(bar(L)))
             else paste0("s(", bio_short[[bio_v]], ")"),
         subtitle = stat_lab) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    theme_reclam +
    theme(axis.text     = element_text(size = 7),
          axis.title    = element_text(size = 8),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 6, hjust = 0.5,
                                       face = if (pval < 0.05) "bold" else "plain",
                                       margin = margin(b = 2)),
          plot.margin   = margin(3, 4, 3, 3))
}

plot_gam_partial <- function(bio_v, env_v, area_nm, lag_suffix = "lag0") {
  p <- .build_gam_partial(bio_v, env_v, area_nm, lag_suffix)
  env_col <- if (lag_suffix == "lag0") env_v else paste0(env_v, "_", lag_suffix)
  fname <- paste0("Fig_GAM_partial_", bio_v, "_", env_col, "_", area_nm, ".jpeg")
  save_fig(fname, p)
  message("Saved: ", fname)
  p
}

plot_gam_partial_grid <- function(area_nm, lag_suffix = "lag0") {

  combos <- tidyr::expand_grid(bio_v = bio_indicators, env_v = env_vars)

  plots <- purrr::pmap(combos, function(bio_v, env_v) {
    tryCatch(
      .build_gam_partial(bio_v, env_v, area_nm, lag_suffix),
      error = function(e) {
        ggplot() + theme_void() +
          annotate("text", x = 0, y = 0, label = "n/a", size = 2.5, colour = "grey50")
      }
    )
  })

  panel <- patchwork::wrap_plots(plots, ncol = length(env_vars))

  fname <- paste0("Fig_GAM_Smooths_", lag_suffix, "_", area_nm, ".jpeg")
  save_fig(fname, panel, width = GAM_W, height = GAM_H)
  message("Saved: ", fname)
  panel
}

# Full panels by region
plot_gam_partial_grid("Cadiz",    "lag0")
plot_gam_partial_grid("Valencia", "lag0")

# Optional single combination:
# plot_gam_partial("L_p90", "wind_speed", "Valencia", "lag0")


# ─────────────────────────────────────────────────────────────────────────────
# 7b. POOLED GAM (both regions) — REGION x ENVIRONMENT INTERACTION
#     Same data as sections 5 and 7 (joined_lags: sampling period, lags 0–2),
#     but both regions fitted together. Three nested models per
#     indicator x environmental variable x lag:
#       M0: y ~ Area                         (regional mean, no environment)
#       M1: y ~ Area + s(env)                (common environmental effect)
#       M2: y ~ Area + s(env, by = Area)     (region-specific effect)
#     Support rule (ΔAIC, ML fits): environment supported if ΔAIC < -2 vs M0;
#     region-specific if M2 beats M1 by > 2 AIC units.
# ─────────────────────────────────────────────────────────────────────────────

dat_pooled <- joined_lags |>
  mutate(Area     = factor(Area),
         t        = (year - min(year)) * 12 + month,          # time index
         RI_logit = qlogis(pmin(pmax(RI, 0.005), 0.995)))     # logit of recruit proportion

bio_lab <- bio_short[bio_indicators]   # "Lmean" -> "Mean length", ...
env_lab <- env_labels[env_vars]        # "sst_mean" -> "SST", ...

## 7b.1 Response specification: family by indicator ---------------------------
# Lengths: Gaussian | RI (proportion 0-1): Gaussian on logit(RI)
# CPUE and Density: Tweedie (positive, skewed, allows zeros)
# Note: a binomial on recruits/measured is badly overdispersed here (hundreds
# of individuals per month) and makes every term "significant"; the logit
# transform keeps one observation per month as the unit of replication.

resp_spec <- function(bio_v) {
  switch(bio_v,
         Lmean   = list(lhs = "Lmean",    fam = gaussian()),
         L_p90   = list(lhs = "L_p90",    fam = gaussian()),
         RI      = list(lhs = "RI_logit", fam = gaussian()),
         CPUE    = list(lhs = "CPUE",     fam = tw()),
         Density = list(lhs = "Density",  fam = tw()))
}

pooled_data <- function(bio_v, env_col) {
  dat_pooled |>
    dplyr::select(Area, t, any_of(c(bio_v, "RI_logit")), all_of(env_col)) |>
    drop_na()
}

## 7b.2 Three nested models per indicator x env variable x lag ----------------

fit_set <- function(bio_v, env_v, lag) {
  env_col <- if (lag == 0) env_v else paste0(env_v, "_lag", lag)
  sp <- resp_spec(bio_v)
  d  <- pooled_data(bio_v, env_col)

  if (nrow(d) < 10) return(NULL)

  f0 <- as.formula(paste(sp$lhs, "~ Area"))
  f1 <- as.formula(paste0(sp$lhs, " ~ Area + s(", env_col, ", k = 3)"))
  f2 <- as.formula(paste0(sp$lhs, " ~ Area + s(", env_col, ", by = Area, k = 3)"))

  m0 <- gam(f0, family = sp$fam, data = d, method = "ML")
  m1 <- gam(f1, family = sp$fam, data = d, method = "ML")
  m2 <- gam(f2, family = sp$fam, data = d, method = "ML")

  aic  <- c(M0 = AIC(m0), M1 = AIC(m1), M2 = AIC(m2))
  best <- names(which.min(aic))

  tibble(bio_var = bio_v, env_var = env_v, lag = lag, n = nrow(d),
         dev_expl_M1 = summary(m1)$dev.expl,
         dev_expl_M2 = summary(m2)$dev.expl,
         p_M1    = summary(m1)$s.table[1, "p-value"],
         dAIC_M1 = aic[["M1"]] - aic[["M0"]],      # < -2: environment improves the model
         dAIC_M2 = aic[["M2"]] - aic[["M1"]],      # < -2: effect differs between regions
         best_model = best)
}

pooled_grid <- expand_grid(bio_v = bio_indicators, env_v = env_vars, lag = 0:2)

res_pooled <- pmap_dfr(pooled_grid, fit_set) |>
  mutate(across(c(dev_expl_M1, dev_expl_M2, dAIC_M1, dAIC_M2), \(x) round(x, 2)),
         p_M1    = signif(p_M1, 3),
         bio_var = bio_lab[bio_var],
         env_var = env_lab[env_var])

print(res_pooled, n = Inf)
write_csv(res_pooled, here("RESULTS", "Table_GAM_pooled_region_interaction.csv"))

## 7b.3 Best environmental model per indicator (lowest AIC across env x lag) --

best_per_bio <- res_pooled |>
  filter(best_model != "M0") |>
  group_by(bio_var) |>
  slice_min(dAIC_M1, n = 1, with_ties = FALSE) |>
  ungroup()
print(best_per_bio)

## 7b.4 Residual autocorrelation check for models of interest -----------------

check_acf <- function(bio_v, env_v, lag, by_region = FALSE) {
  env_col <- if (lag == 0) env_v else paste0(env_v, "_lag", lag)
  sp <- resp_spec(bio_v)
  d  <- pooled_data(bio_v, env_col) |> arrange(Area, t)
  rhs <- if (by_region) paste0("Area + s(", env_col, ", by = Area, k = 3)")
         else paste0("Area + s(", env_col, ", k = 3)")
  m <- gam(as.formula(paste(sp$lhs, "~", rhs)), family = sp$fam, data = d, method = "REML")
  d$res <- residuals(m, type = "deviance")
  d |> group_by(Area) |>
    summarise(lag1_acf = round(acf(res, plot = FALSE, lag.max = 1)$acf[2], 2),
              .groups = "drop")
}

print(check_acf("Lmean",   "sst_mean", lag = 1))
print(check_acf("Density", "sst_mean", lag = 0, by_region = TRUE))

## 7b.5 Shared plotting helpers ------------------------------------------------

y_lab_pooled <- list(Lmean   = expression(bar(L) ~ "(mm)"),
                     L_p90   = expression(L[90] ~ "(mm)"),
                     RI      = "RI",
                     CPUE    = expression("CPUE (kg" ~ h^{-1} * ")"),
                     Density = expression("Density (ind" ~ m^{-2} * ")"))

lab_y_partial <- c(Lmean = "s(Mean length)", L_p90 = "s(L90)", RI = "s(logit RI)",
                   CPUE = "s(CPUE)", Density = "s(Density)")

lag_suffix_txt <- c("", " (lag 1 month)", " (lag 2 months)")

fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# Model support per cell: "R" region-specific, "C" common, "–" no support
support <- res_pooled |>
  mutate(bio_v    = names(bio_lab)[match(bio_var, bio_lab)],
         env_v    = names(env_lab)[match(env_var, env_lab)],
         dAIC_env = pmin(dAIC_M1, dAIC_M1 + dAIC_M2),     # best env model vs M0
         code     = case_when(dAIC_env > -2 ~ "–",
                              dAIC_M2  < -2 ~ "R",
                              TRUE          ~ "C")) |>
  dplyr::select(bio_v, env_v, lag, code, dAIC_env)

# Fit the model chosen by the support rule (REML, for plotting)
fit_pooled_plot <- function(bio_v, env_col, by_reg) {
  sp <- resp_spec(bio_v)
  d  <- pooled_data(bio_v, env_col)
  rhs <- if (by_reg) paste0("Area + s(", env_col, ", by = Area, k = 3)")
         else paste0("Area + s(", env_col, ", k = 3)")
  m <- gam(as.formula(paste(sp$lhs, "~", rhs)), family = sp$fam, data = d, method = "REML")
  list(m = m, d = d)
}

# Response-scale prediction grid within the observed range of each region
pred_response <- function(m, d, env_col, bio_v) {
  nd <- d |>
    group_by(Area) |>
    summarise(x = list(seq(min(.data[[env_col]]), max(.data[[env_col]]), length.out = 100)),
              .groups = "drop") |>
    unnest(x) |>
    rename(!!env_col := x)

  pr  <- predict(m, newdata = nd, type = "link", se.fit = TRUE)
  inv <- if (bio_v == "RI") plogis else m$family$linkinv   # back to response scale
  nd |>
    mutate(fit = inv(pr$fit),
           lwr = inv(pr$fit - 1.96 * pr$se.fit),
           upr = inv(pr$fit + 1.96 * pr$se.fit))
}

## 7b.6 Fitted curves of the best model per indicator (response scale) --------

plot_best <- function(row) {
  bio_v   <- names(bio_lab)[match(row$bio_var, bio_lab)]
  env_v   <- names(env_lab)[match(row$env_var, env_lab)]
  env_col <- if (row$lag == 0) env_v else paste0(env_v, "_lag", row$lag)
  by_reg  <- row$dAIC_M2 < -2      # region-specific only if M2 beats M1 by > 2 AIC

  fp <- fit_pooled_plot(bio_v, env_col, by_reg)
  m  <- fp$m; d <- fp$d
  nd <- pred_response(m, d, env_col, bio_v)

  p_min <- min(summary(m)$s.table[, "p-value"])
  stat_lab <- sprintf("%s | dev.expl=%.0f%% | %s",
                      if (by_reg) "region-specific" else "common smooth",
                      100 * summary(m)$dev.expl,
                      if (p_min < 0.001) "p<0.001" else sprintf("p=%.3f", p_min))

  ggplot(nd, aes(x = .data[[env_col]], y = fit, colour = Area, fill = Area)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(data = d, aes(x = .data[[env_col]], y = .data[[bio_v]]),
               size = 1.6, alpha = 0.85, inherit.aes = FALSE,
               colour = pal_area[as.character(d$Area)]) +
    scale_colour_manual(values = pal_area, labels = region_labels) +
    scale_fill_manual(values = pal_area, labels = region_labels) +
    labs(x = paste0(row$env_var, lag_suffix_txt[row$lag + 1]), y = y_lab_pooled[[bio_v]],
         colour = "Region", fill = "Region", subtitle = stat_lab) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 7,
                                       face = if (p_min < 0.05) "bold" else "plain"),
          legend.position = "bottom")
}

best_plots <- best_per_bio |>
  mutate(bio_var = factor(bio_var, levels = bio_lab)) |>
  arrange(bio_var) |>
  mutate(bio_var = as.character(bio_var)) |>
  split(seq_len(nrow(best_per_bio))) |>
  map(plot_best)

fig_gam_pooled <- wrap_plots(best_plots, ncol = 2) + guide_area() +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.direction = "vertical")

save_fig("Fig_GAM_pooled_best.jpeg", fig_gam_pooled, width = 6, height = 7)

## 7b.7 Heatmap of model support (ΔAIC) for every indicator x variable x lag --
# Fill = ΔAIC of the best environmental model (M1 or M2) vs. region only (M0)
# Text: "–" no support (ΔAIC > -2); "C" common effect; "R" region-specific

fig_daic <- res_pooled |>
  mutate(bio_var  = factor(bio_var, levels = bio_lab),
         env_var  = factor(env_var, levels = env_lab),
         lag      = factor(lag, levels = 0:2,
                           labels = c("Lag 0", "Lag 1 month", "Lag 2 months")),
         dAIC_env = pmin(dAIC_M1, dAIC_M1 + dAIC_M2),
         best_txt = case_when(dAIC_env > -2 ~ "–",
                              dAIC_M2  < -2 ~ "R",
                              TRUE          ~ "C"),
         dAIC_plot = pmax(dAIC_env, -30)) |>              # cap for colour scale
  ggplot(aes(x = bio_var, y = env_var, fill = dAIC_plot)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = best_txt), size = 3, colour = "grey10") +
  facet_wrap(~ lag, ncol = 1, strip.position = "right") +
  scale_fill_gradient2(low = "#2c7fb8", mid = "white", high = "grey70",
                       midpoint = 0, limits = c(-30, 5),
                       name = expression(Delta * "AIC (env vs region)")) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", legend.key.width = unit(1.2, "cm"))

save_fig("Fig_GAM_pooled_dAIC.jpeg", fig_daic, width = 6, height = 7)

## 7b.8 Full panel, response scale: 4 env (rows) x 5 indicators (cols) --------
# One figure per lag; each panel follows the ΔAIC support rule (subtitle code).

plot_cell <- function(bio_v, env_v, lag) {
  env_col <- if (lag == 0) env_v else paste0(env_v, "_lag", lag)
  sup     <- support |> filter(bio_v == !!bio_v, env_v == !!env_v, lag == !!lag)
  by_reg  <- sup$code == "R"

  fp <- fit_pooled_plot(bio_v, env_col, by_reg)
  m  <- fp$m; d <- fp$d
  nd <- pred_response(m, d, env_col, bio_v)

  p_min <- min(summary(m)$s.table[, "p-value"])
  stat_lab <- sprintf("%s | %.0f%% | %s", sup$code, 100 * summary(m)$dev.expl,
                      if (p_min < 0.001) "p<0.001" else sprintf("p=%.3f", p_min))

  ggplot(nd, aes(x = .data[[env_col]], y = fit, colour = Area, fill = Area)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, colour = NA,
                show.legend = FALSE) +
    geom_line(linewidth = 0.7, show.legend = FALSE) +
    geom_point(data = d, aes(x = .data[[env_col]], y = .data[[bio_v]], colour = Area),
               size = 0.9, alpha = 0.8, inherit.aes = FALSE) +
    scale_colour_manual(values = pal_area, labels = region_labels) +
    scale_fill_manual(values = pal_area, labels = region_labels) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    guides(fill = "none", colour = guide_legend(override.aes = list(size = 2.5))) +
    labs(x = env_lab[[env_v]], y = y_lab_pooled[[bio_v]],
         colour = "Region", subtitle = stat_lab) +
    theme_bw(base_size = 8) +
    theme(panel.grid.minor = element_blank(),
          axis.text  = element_text(size = 6.5),
          axis.title = element_text(size = 7.5),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 6, hjust = 0.5, face = "bold",
                                       margin = margin(b = 2)),
          plot.margin = margin(3, 4, 3, 3))
}

plot_pooled_grid <- function(lag = 0, width = 7, height = 5.5) {
  combos <- expand_grid(env_v = names(env_lab), bio_v = names(bio_lab))  # rows = env, cols = bio
  plots  <- pmap(combos, function(env_v, bio_v) plot_cell(bio_v, env_v, lag))

  fig <- wrap_plots(plots, ncol = length(bio_lab), guides = "collect") &
    theme(legend.position = "bottom")

  fname <- paste0("Fig_GAM_pooled_grid_lag", lag, ".jpeg")
  save_fig(fname, fig, width = width, height = height)
  message("Saved: ", fname)
  fig
}

walk(0:2, plot_pooled_grid)

## 7b.9 Partial effects of the pooled GAM: 4 env (rows) x 5 indicators (cols) -
# y-axis = partial effect s(env) on the link scale (centred, as plot.gam);
# points = partial residuals (term + working residual).
# Subtitle: code | edf | p | n  (for "R" models: Cádiz / Valencia values)

plot_partial_cell <- function(bio_v, env_v, lag) {
  env_col <- if (lag == 0) env_v else paste0(env_v, "_lag", lag)
  sup     <- support |> filter(bio_v == !!bio_v, env_v == !!env_v, lag == !!lag)
  by_reg  <- sup$code == "R"

  fp <- fit_pooled_plot(bio_v, env_col, by_reg)
  m  <- fp$m; d <- fp$d
  s_tab <- summary(m)$s.table

  # Grid: per region if region-specific, otherwise one common range
  nd <- (if (by_reg) d |> group_by(Area)
         else d |> mutate(Area = levels(d$Area)[1]) |> group_by(Area)) |>
    summarise(x = list(seq(min(.data[[env_col]]), max(.data[[env_col]]), length.out = 100)),
              .groups = "drop") |>
    unnest(x) |>
    rename(!!env_col := x) |>
    mutate(Area = factor(Area, levels = levels(d$Area)))

  pr <- predict(m, newdata = nd, type = "terms", se.fit = TRUE)
  sm <- grep("^s\\(", colnames(pr$fit))                      # smooth columns
  nd <- nd |>
    mutate(fit = rowSums(pr$fit[, sm, drop = FALSE]),
           se  = sqrt(rowSums(pr$se.fit[, sm, drop = FALSE]^2)),
           lwr = fit - 1.96 * se,
           upr = fit + 1.96 * se,
           grp = if (by_reg) as.character(Area) else "common")

  # Partial residuals at observed points
  po <- predict(m, newdata = d, type = "terms")
  d  <- d |>
    mutate(partial = rowSums(po[, grep("^s\\(", colnames(po)), drop = FALSE]) +
             residuals(m, type = "working"))

  stat_lab <- if (by_reg) {
    sprintf("R | edf=%.2f/%.2f | p: %s/%s | n=%d",
            s_tab[1, "edf"], s_tab[2, "edf"],
            fmt_p(s_tab[1, "p-value"]), fmt_p(s_tab[2, "p-value"]), nrow(d))
  } else {
    sprintf("%s | edf=%.2f | p: %s | n=%d", sup$code,
            s_tab[1, "edf"], fmt_p(s_tab[1, "p-value"]), nrow(d))
  }

  g <- ggplot(nd, aes(x = .data[[env_col]], y = fit, group = grp))
  g <- if (by_reg) g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Area), alpha = 0.18,
                                   show.legend = FALSE)
       else g + geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey60", alpha = 0.25)
  g <- if (by_reg) g + geom_line(aes(colour = Area), linewidth = 0.7, show.legend = FALSE)
       else g + geom_line(colour = "grey20", linewidth = 0.7)

  g +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_point(data = d, aes(x = .data[[env_col]], y = partial, colour = Area),
               size = 0.9, alpha = 0.8, inherit.aes = FALSE) +
    scale_colour_manual(values = pal_area, labels = region_labels) +
    scale_fill_manual(values = pal_area, labels = region_labels) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    guides(fill = "none", colour = guide_legend(override.aes = list(size = 2.5))) +
    labs(x = env_lab[[env_v]], y = lab_y_partial[[bio_v]],
         colour = "Region", subtitle = stat_lab) +
    theme_bw(base_size = 8) +
    theme(panel.grid.minor = element_blank(),
          axis.text  = element_text(size = 6.5),
          axis.title = element_text(size = 7.5),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 5.5, hjust = 0.5, face = "bold",
                                       margin = margin(b = 2)),
          plot.margin = margin(3, 4, 3, 3))
}

plot_partial_grid <- function(lag = 0, width = 7, height = 5.5) {
  combos <- expand_grid(env_v = names(env_lab), bio_v = names(bio_lab))
  plots  <- pmap(combos, function(env_v, bio_v) plot_partial_cell(bio_v, env_v, lag))

  fig <- wrap_plots(plots, ncol = length(bio_lab), guides = "collect") &
    theme(legend.position = "bottom")

  fname <- paste0("Fig_GAM_pooled_partial_lag", lag, ".jpeg")
  save_fig(fname, fig, width = width, height = height)
  message("Saved: ", fname)
  fig
}

walk(0:2, plot_partial_grid)

## 7b.10 How much of the regional difference does SST explain? ---------------
# For each indicator: regional effect (Valencia vs Cadiz, link scale) in
#   M0: y ~ Area                       (raw regional difference)
#   M1: y ~ Area + s(SST_lag, k = 3)   (difference after accounting for SST)
# Reduction (%) = share of the regional difference explained by SST.

area_shrink <- function(bio_v, env_col) {
  sp <- resp_spec(bio_v)
  d  <- pooled_data(bio_v, env_col)
  m0 <- gam(as.formula(paste(sp$lhs, "~ Area")), family = sp$fam, data = d, method = "REML")
  m1 <- gam(as.formula(paste0(sp$lhs, " ~ Area + s(", env_col, ", k = 3)")),
            family = sp$fam, data = d, method = "REML")
  b0 <- summary(m0)$p.table["AreaValencia", ]
  b1 <- summary(m1)$p.table["AreaValencia", ]
  tibble(bio_var = bio_lab[[bio_v]], env = env_col, n = nrow(d),
         area_effect_raw = round(b0[["Estimate"]], 3),
         p_raw           = signif(b0[[4]], 3),
         area_effect_adj = round(b1[["Estimate"]], 3),
         p_adj           = signif(b1[[4]], 3),
         reduction_pct   = round(100 * (1 - b1[["Estimate"]] / b0[["Estimate"]]), 1))
}

tab_shrink <- map_dfr(c("sst_mean", "sst_mean_lag1", "sst_mean_lag2"), function(ec)
  map_dfr(bio_indicators, \(b) area_shrink(b, ec)))

print(tab_shrink, n = Inf)
write_csv(tab_shrink, here("RESULTS", "Table_RegionalDifference_explained_by_SST.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 8. EXPORT SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

write_csv(joined_lags,  here("RESULTS", "Data_BIO_ENV_overlap.csv"))
write_csv(bio_monthly,  here("RESULTS", "Data_BIO_monthly.csv"))

message("\n========================================")
message(" ENV_BIO_Correlation.R — done.")
message(" Figures in : ", here("FIG"), "  (", FIG_W, " x ", FIG_H, " in, ", FIG_DPI, " dpi)")
message(" Results in : ", here("RESULTS"))
message("  · FigS_ENV_Context.jpeg               — environmental context (SST + MHW days)")
message("  · Fig_Spearman_Heatmap.jpeg           — Spearman rho, lags 0–2")
message("  · Fig_PCA_ENV.jpeg                    — environmental PCA + sampling months")
message("  · Fig_PCA_BIO.jpeg                    — population-indicator PCA (RI, CPUE, Density)")
message("  · Fig_PCA_BIO_panel.jpeg              — indicator + environment PCAs (a–c, supplementary)")
message("  · Fig_GAM_Heatmap_byRegion.jpeg       — GAM deviance explained, by region and lag")
message("  · Fig_GAM_Smooths_lag0_Cadiz.jpeg / _Valencia.jpeg — GAM partial effects")
message("  · Fig_GAM_pooled_best.jpeg / Fig_GAM_pooled_dAIC.jpeg — pooled GAM (region x env)")
message("  · Fig_GAM_pooled_grid_lag0-2.jpeg / Fig_GAM_pooled_partial_lag0-2.jpeg")
message("  · Tables: Table_Spearman_ENV_BIO.csv, Table_PCA_Loadings*.csv,")
message("            Table_PERMANOVA_BIO.csv, Table_Wilcoxon_byVariable.csv,")
message("            Table_PERMANOVA_byVariable_ENV_supl.csv, Table_GAM_ENV_BIO_byRegion.csv,")
message("            Table_GAM_pooled_region_interaction.csv,")
message("            Table_RegionalDifference_explained_by_SST.csv,")
message("            Data_BIO_ENV_overlap.csv, Data_BIO_monthly.csv")
message("========================================\n")
