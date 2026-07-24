# =============================================================================
# COMPARA_RECLAM.R
# Comparative Population Dynamics of Donax trunculus
# Gulf of Cádiz (GoC) vs. Gulf of Valencia (GoV)
# Project RECLAM — IEO-CSIC / UPV
#
# Analyses:
#   0.  Setup & helpers
#   1.  Data loading (size, L-W) — coquina only (Especie == "CO")
#   2.  Size-frequency distributions (LFQ)
#   3.  von Bertalanffy growth (ELEFAN_GA + NLS + comparison)
#   4.  Growth performance index (Φ')
#   5.  Natural mortality — Pauly(1980), Hoenig(1983), Lorenzen(1996)
#   6.  Length-weight relationship (allometric)
#   7.  Mass-specific growth rate (MGR) — Brey (2001)
#   9.  Statistical comparisons (Wilcoxon, Cohen's d, Kruskal-Wallis)
#  10.  Linear mixed-effects models (LMM)
#  11.  Publication-ready composite figures (patchwork)
#  12.  Summary tables (flextable)
#
# NOTE: Environmental analysis moved to ENV_BIO_Correlation.R
#
# Author  : Mauricio Mardones / Alberto García
# Updated : 2025-04
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 0. SETUP
# ─────────────────────────────────────────────────────────────────────────────

rm(list = ls())
options(bitmapType = "cairo", scipen = 999)
set.seed(2025)

## 0.1 Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  # Core
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(here)
  library(janitor)

  # Fisheries
  library(TropFishR)     # ELEFAN_GA, lfqRestructure, mortality estimators
  library(LBSPR)         # Length-Based SPR (supplementary check)

  # Statistical modelling
  library(nlme)          # lme — LMM with AR(1) correlation structures
  library(lme4)          # lmer  (alternative fitting)
  library(lmerTest)      # p-values for lmer
  library(mgcv)          # GAM
  library(gratia)        # GAM diagnostics & draws
  library(broom.mixed)   # tidy() for nlme / lme4 objects
  library(effsize)       # Cohen's d

  # Visualization
  library(ggridges)      # Ridge plots
  library(patchwork)     # Multi-panel layout
  library(viridis)       # Colour-blind palettes
  library(ggpubr)        # stat_compare_means
  library(car)

  # Tables
  library(flextable)
})



## 0.2 Publication theme -------------------------------------------------------

theme_reclam <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    strip.background   = element_rect(fill = "grey92", colour = NA),
    legend.position    = "bottom",
    legend.key.size    = unit(0.4, "cm"),
    plot.title         = element_text(face = "bold", size = 11),
    axis.title         = element_text(size = 10)
  )

## 0.3 Colour palette (colour-blind safe) -------------------------------------

pal_area <- c(Cadiz = "#1B7FC4", Valencia = "#D94F00")

## 0.4 Output directories ------------------------------------------------------

dir.create(here("FIG"),     showWarnings = FALSE)
dir.create(here("RESULTS"), showWarnings = FALSE)

## 0.5 Figure quality (set 300 for final publication) -------------------------
fig_dpi <- 150


# ─────────────────────────────────────────────────────────────────────────────
# 1. DATA LOADING
# ─────────────────────────────────────────────────────────────────────────────

archivo_cadiz    <- here("DATA", "Data_Final", "Template_Reclam_Cadiz.xlsx")
archivo_valencia <- here("DATA", "Data_Final", "Template_Reclam_Valencia.xlsx")

stopifnot(
  "Template Cadiz not found"    = file.exists(archivo_cadiz),
  "Template Valencia not found" = file.exists(archivo_valencia)
)

## 1.1 Size-frequency (tallas) -------------------------------------------------

tallac <- read_excel(archivo_cadiz, sheet = "tallas") |>
  dplyr::select(-any_of("ZONA")) |>
  filter(!is.na(TALLA)) |>
  mutate(
    TALLA       = as.numeric(TALLA),
    PUNTO       = as.character(PUNTO),
    FECHA       = as.Date(FECHA, origin = "1899-12-30"),
    Area        = "Cadiz",
    especie     = tolower(especie),                    # normalizar a minúsculas
    # Cádiz: REPLICA contiene "P" → Poblacional, "C" → Comercial
    tipo_rastro = if_else(grepl("P", REPLICA, ignore.case = FALSE),
                          "Poblacional", "Comercial")
  )

tallav <- read_excel(archivo_valencia, sheet = "tallas") |>
  filter(!is.na(TALLA)) |>
  mutate(
    TALLA       = as.numeric(TALLA),
    FECHA       = as.Date(FECHA),
    Area        = "Valencia",
    especie     = tolower(especie),                    # normalizar a minúsculas
    # Valencia: REPLICA contiene "NC" → Poblacional, "C" (sin NC) → Comercial
    tipo_rastro = if_else(grepl("NC", REPLICA, ignore.case = FALSE),
                          "Poblacional", "Comercial")
  )

tallas <- bind_rows(tallac, tallav) |>
  filter(!is.na(TALLA), TALLA > 0, !is.na(especie)) |>
  mutate(
    year        = year(FECHA),
    month       = month(FECHA),
    talla_cls   = floor(TALLA),                       # 1-mm class
    size_class  = ifelse(TALLA <= 8, "Recruit", "Adult"),
    tipo_rastro = factor(tipo_rastro, levels = c("Poblacional", "Comercial")),
    # Unificar códigos de especie: "chs" → "ch" (chirla), "co" = coquina
    especie     = recode(especie, "chs" = "ch"),
    especie     = factor(especie, levels = c("co", "ch"),
                         labels = c("co" = "Coquina", "ch" = "Chirla"))
  )

# Subconjunto coquina — usado en análisis de crecimiento y mortalidad
tallas_co <- tallas |> filter(especie == "Coquina",
                              tipo_rastro == "Poblacional")



# ─────────────────────────────────────────────────────────────────────────────
# 2. SIZE-FREQUENCY DISTRIBUTIONS (LFQ)
# ─────────────────────────────────────────────────────────────────────────────

## 2.1 Build TropFishR-compatible LFQ objects ----------------------------------

build_lfq <- function(df) {
  freq <- df |>
    dplyr::filter(!is.na(TALLA)) |>
    dplyr::mutate(cls = floor(TALLA)) |>
    dplyr::group_by(year, month, cls) |>
    summarise(N = n(), .groups = "drop")

  wide <- freq |>
    pivot_wider(names_from = cls, values_from = N, values_fill = 0L) |>
    arrange(year, month)

  fechas <- as.Date(paste(wide$year, wide$month, "15", sep = "-"))
  mat    <- as.matrix(wide[, -(1:2)])
  ord    <- order(as.numeric(colnames(mat)))
  mat    <- mat[, ord]
  clases <- as.numeric(colnames(mat)[ord])

  list(dates = fechas, midLengths = clases, catch = t(mat))
}

# ELEFAN usa solo coquina
lfq_c <- lfqRestructure(build_lfq(tallas_co |> filter(Area == "Cadiz",
                                                      tipo_rastro == "Poblacional")),    MA = 5)
lfq_v <- lfqRestructure(build_lfq(tallas_co |> filter(Area == "Valencia",
                                                      tipo_rastro == "Poblacional")), MA = 5)

## 2.2 Base data — LFQ por especie x area x rastro ----------------------------

ni_plot <- tallas |>
  group_by(especie, Area, tipo_rastro, year, month, talla_cls) |>
  summarise(Ni = n(), .groups = "drop") |>
  mutate(
    fecha_label = paste(year, sprintf("%02d", month), sep = "-"),
    is_recruit  = talla_cls <= 10.8
  )

## 2.3 LFQ barras por especie x area -------------------------------------
make_lfq_plot <- function(sp, area_nm, col) {
  df <- ni_plot |>
    filter(especie == sp, Area == area_nm)
  if (nrow(df) == 0) return(NULL)
  ggplot(df, aes(x = talla_cls, y = Ni, fill = is_recruit)) +
    geom_col(width = 0.9) +
    geom_vline(xintercept = 25, linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    facet_wrap(~ fecha_label, ncol = 4, scales = "free_y") +
    scale_fill_manual(
      values = c("TRUE" = "#FFB703", "FALSE" = col),
      labels = c("TRUE" = "Recruit (≤10.8 mm)", "FALSE" = "Adult")
    ) +
    labs(
      title = bquote(.(toupper(sp)) ~ "—" ~ .(area_nm)),
      x = "Shell length (mm)", y = expression(N[i]), fill = NULL
    ) +
    theme_reclam
}

# Generar una figura por cada combinación especie x area
combos <- tallas |>
  distinct(especie, Area) |>
  arrange(especie, Area)

plots_lfq <- map(seq_len(nrow(combos)), function(i) {
  sp <- as.character(combos$especie[i])
  ar <- combos$Area[i]
  col <- pal_area[ar]
  make_lfq_plot(sp, ar, col)
}) |>
  set_names(sprintf("%s_%s", combos$especie, combos$Area))

# inspeccionar antes de guardar
plots_lfq[["coquina_Cadiz"]]

# guardar todos
iwalk(plots_lfq, function(p, nm) {
  if (!is.null(p)) {
    ggsave(here("FIG", sprintf("Fig2_LFQ_%s.jpeg", nm)), p,
           width = 14, height = 10, dpi = fig_dpi)
  }
})
## 2.4 LFQ densidad por especie x area x rastro --------------------------------

dens_data <- tallas |>
  mutate(fecha_label = paste(year, sprintf("%02d", month), sep = "-")) |>
  group_by(especie, Area, fecha_label) |>
  filter(n() >= 5) |>
  summarise(
    d = list(density(TALLA, n = 512)),
    .groups = "drop"
  ) |>
  mutate(x = map(d, "x"), y = map(d, "y")) |>
  dplyr::select(-d) |>
  unnest(c(x, y)) |>
  mutate(size_class = if_else(x <= 10.8, "Recruit", "Adult"))

plots_dens <- map(levels(tallas$especie), function(sp) {
  df <- dens_data |> filter(especie == sp)
  if (nrow(df) == 0) return(NULL)
  ggplot(df, aes(x = x, y = y, colour = Area, fill = Area)) +
    geom_area(aes(alpha = size_class), position = "identity") +
    geom_line(linewidth = 0.7) +
    geom_vline(aes(xintercept = 10.8, linetype = "L50"),
               colour = "grey30", linewidth = 0.5) +
    facet_wrap(~ fecha_label, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = pal_area) +
    scale_fill_manual(values   = pal_area) +
    scale_alpha_manual(values  = c("Recruit" = 0.6, "Adult" = 0.15)) +
    scale_linetype_manual(values = c("L50" = "dashed")) +
    guides(alpha = "none") +
    labs(x = "Shell length (mm)", y = "Density",
         colour = "Region", fill = "Region", linetype = NULL) +
    theme_reclam +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x  = element_text(angle = 45, hjust = 1, size = 6))
}) |>
  set_names(levels(tallas$especie))

# inspeccionar
plots_dens[["coquina"]]

# guardar
iwalk(plots_dens, function(p, sp) {
  if (!is.null(p)) {
    ggsave(here("FIG", sprintf("Fig2_LFQ_density_%s.jpeg", sp)),
           p, width = 8, height = 6, dpi = fig_dpi)
  }
})

## 2.5 LFQ heatmap por especie x rastro x area ---------------------------------
# meses en orden descendente
p_heatmap_coq <- {
  df <- ni_plot   # nivel poblacional (coquina), sin split por especie ni rastro
  ggplot(df %>% 
           filter(especie == "Coquina"), aes(x = talla_cls, y = factor(fecha_label), fill = Ni)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    facet_wrap(~ Area) +
    scale_fill_viridis_c(option = "G", name = expression(N[i]),
                         trans = "log1p", na.value = "grey90",
                         limits = c(0, quantile(df$Ni, 0.9, na.rm = TRUE)),
                         oob = scales::squish) +
    #scale_y_discrete(labels = month.abb) +
    labs(x = "Shell length (mm)", y = "") +
    theme_reclam + theme(legend.position = "none",
                         aspect.ratio = 0.5)
}

# inspeccionar
p_heatmap_coq

# guardar
ggsave(here("FIG", "Fig2c_LFQ_heatmap_coquina.jpeg"),
       p_heatmap_coq, width = 7, height = 9, dpi = fig_dpi)


## 8.5 Monthly violin × region (size distribution) ----------------------------

fig_violin <- ggplot(tallas_co,   # ya viene filtrado a Poblacional, sin refiltrar
                     aes(x = factor(month), y = TALLA,
                         fill = Area, colour = Area)) +
  geom_violin(alpha = 0.35, scale = "width", trim = TRUE,
              position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.75,
               position = position_dodge(width = 0.9)) +
  ggpubr::stat_compare_means(aes(group = Area), method = "wilcox.test",
                              label = "p.signif",
                              label.y = max(tallas_co$TALLA, na.rm = TRUE) * 1.08,
                              size = 4) +
  scale_fill_manual(values   = pal_area) +
  scale_colour_manual(values = pal_area) +
  scale_x_discrete(labels = month.abb) +
  labs(x = "Month", y = "Shell length (mm)",
       fill = "Region", colour = "Region",
       title = expression("Monthly size distribution \u2014 " * italic("D. trunculus"))) +
  theme_reclam

# inspeccionar
fig_violin

# guardar
ggsave(here("FIG", "FigS3_SizeViolin.jpeg"), fig_violin, width = 14, height = 5, dpi = fig_dpi)

## 9.4 Comparison boxplot (Figure 5) ------------------------------------------

fig5 <- ggplot(tallas_co, aes(x = Area, y = TALLA, fill = Area)) +
  geom_violin(alpha = 0.5, trim = TRUE) +
  geom_boxplot(width = 0.10, outlier.shape = 21, outlier.size = 0.8,
               colour = "grey20") +
  ggpubr::stat_compare_means(method = "wilcox.test", label = "p.format",
                             label.x = 1.4, size = 3.5) +
  scale_fill_manual(values = pal_area) +
  labs(x = NULL, y = "Shell length (mm)", fill = NULL,
       title = expression("Size comparison \u2014 " * italic("D. trunculus"))) +
  theme_reclam + theme(legend.position = "none")

ggsave(here("FIG", "Fig5_SizeComparison.jpeg"), fig5, width = 6, height = 5, dpi = fig_dpi)


# 3. VON BERTALANFFY GROWTH PARAMETERS
# ─────────────────────────────────────────────────────────────────────────────

## 3.1 ELEFAN_GA ---------------------------------------------------------------
# Increase popSize/maxiter for final publication runs

run_elefan <- function(lfq_obj, label,
                       Linf_lo = 20,  Linf_hi = 55,
                       K_lo    = 0.1, K_hi    = 3.0,
                       popSize = 60,  maxiter = 120) {
  message("[ELEFAN_GA] ", label, " ...")
  fit <- ELEFAN_GA(
    lfq_obj,
    low_par  = list(Linf = Linf_lo, K = K_lo,  t_anchor = 0),
    up_par   = list(Linf = Linf_hi, K = K_hi,  t_anchor = 1),
    popSize  = popSize,
    maxiter  = maxiter,
    run      = 10,
    MA       = 5,
    parallel = FALSE,
    monitor  = FALSE
  )
  message("  Linf=", round(fit$par$Linf, 2),
          "  K=",    round(fit$par$K, 3),
          "  Rn=",   round(fit$Rn_max, 4))
  fit
}

fit_eg_c <- run_elefan(lfq_c, "Cadiz")
# [ELEFAN_GA] Cadiz ...
# Genetic algorithm is running. This might take some time.
# Linf=24.07  K=1.267  Rn=0.1602
fit_eg_v <- run_elefan(lfq_v, "Valencia")
# [ELEFAN_GA] Valencia ...
# Genetic algorithm is running. This might take some time.
#  Linf=36.49  K=0.281  Rn=0.1866

## 3.2 NLS on mean monthly length (independent validation) --------------------

mean_monthly <- tallas_co |>
  filter(tipo_rastro == "Poblacional") |>
  group_by(Area, FECHA) |>
  summarise(Lmean = mean(TALLA, na.rm = TRUE), .groups = "drop") |>
  group_by(Area) |>
  mutate(t_rel = decimal_date(FECHA) - min(decimal_date(FECHA))) |>
  ungroup()

fit_nls_area <- function(df_area) {
  Lmax <- max(df_area$Lmean, na.rm = TRUE)
  nls(
    Lmean ~ Linf * (1 - exp(-K * (t_rel - t0))),
    data      = df_area,
    start     = list(Linf = Lmax * 1.1, K = 0.6, t0 = 0),
    algorithm = "port",
    lower     = c(Linf = Lmax * 0.9, K = 0.05, t0 = -3),
    upper     = c(Linf = 60,         K = 3.5,  t0 =  3),
    control   = nls.control(maxiter = 300)
  )
}

fit_nls_c <- fit_nls_area(mean_monthly |> filter(Area == "Cadiz"))
fit_nls_v <- fit_nls_area(mean_monthly |> filter(Area == "Valencia"))

## 3.3 VBGF parameter table (Table 2 in paper) --------------------------------

extract_vbgf_row <- function(eg_fit, nls_fit, area) {
  tibble(
    Area      = area,
    Method    = c("ELEFAN_GA", "NLS"),
    Linf_mm   = c(eg_fit$par$Linf,    coef(nls_fit)["Linf"]),
    K_yr      = c(eg_fit$par$K,       coef(nls_fit)["K"]),
    t0_yr     = c(eg_fit$par$t_anchor, coef(nls_fit)["t0"]),
    Phi_prime = log10(K_yr) + 2 * log10(Linf_mm),
    Rn_max    = c(eg_fit$Rn_max, NA_real_)
  )
}

tab_vbgf <- bind_rows(
  extract_vbgf_row(fit_eg_c, fit_nls_c, "Cadiz"),
  extract_vbgf_row(fit_eg_v, fit_nls_v, "Valencia")
) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(tab_vbgf)
write_csv(tab_vbgf, here("RESULTS", "Table2_VBGF.csv"))

## 3.4 VBGF growth curves plot (Figure 3a) ------------------------------------

age_seq <- seq(0, 7, by = 0.02)

vbgf_pred <- function(Linf, K, t0, area, method) {
  tibble(age    = age_seq,
         length = Linf * (1 - exp(-K * (age_seq - t0))),
         Area   = area,
         Method = method)
}

vbgf_curves <- bind_rows(
  vbgf_pred(fit_eg_c$par$Linf, fit_eg_c$par$K, fit_eg_c$par$t_anchor, "Cadiz",   "ELEFAN_GA"),
  vbgf_pred(coef(fit_nls_c)["Linf"], coef(fit_nls_c)["K"], coef(fit_nls_c)["t0"], "Cadiz",   "NLS"),
  vbgf_pred(fit_eg_v$par$Linf, fit_eg_v$par$K, fit_eg_v$par$t_anchor, "Valencia","ELEFAN_GA"),
  vbgf_pred(coef(fit_nls_v)["Linf"], coef(fit_nls_v)["K"], coef(fit_nls_v)["t0"], "Valencia","NLS")
)

fig3a <- ggplot(vbgf_curves, aes(x = age, y = length,
                                  colour = Area, linetype = Method)) +
  geom_line(linewidth = 1) +
  geom_hline(data = tab_vbgf |> filter(Method == "ELEFAN_GA"),
             aes(yintercept = Linf_mm, colour = Area),
             linetype = "dotted", linewidth = 0.5, alpha = 0.6) +
  scale_colour_manual(values = pal_area) +
  scale_linetype_manual(values = c("ELEFAN_GA" = "solid", "NLS" = "dashed")) +
  labs(x = "Age (years)", y = "Shell length (mm)",
       colour = "Region", linetype = "Method") +
  theme_reclam


# ─────────────────────────────────────────────────────────────────────────────
# 4. GROWTH PERFORMANCE INDEX (Φ')
# ─────────────────────────────────────────────────────────────────────────────
# Φ' = log₁₀(K) + 2·log₁₀(L∞)   [Pauly & Munro 1984]

phi_df <- tab_vbgf |>
  filter(Method == "ELEFAN_GA") |>
  dplyr::select(Area, Linf_mm, K_yr, Phi_prime)

# Comparison with published values for D. trunculus
phi_lit <- tibble(
  Reference   = c("Ramajal et al. 2020 (GoC)",
                  "Moreira et al. 2006",
                  "This study — Cádiz",
                  "This study — Valencia"),
  Linf        = c(34.5, 32.1,
                  filter(phi_df, Area == "Cadiz")$Linf_mm,
                  filter(phi_df, Area == "Valencia")$Linf_mm),
  K           = c(0.98, 1.12,
                  filter(phi_df, Area == "Cadiz")$K_yr,
                  filter(phi_df, Area == "Valencia")$K_yr)
) |>
  mutate(Phi_prime = round(log10(K) + 2 * log10(Linf), 3),
         Source    = c(rep("Literature", 2), rep("This study", 2)))

write_csv(phi_lit, here("RESULTS", "Table_Phi_literature.csv"))
print(phi_lit)

fig3b <- ggplot(phi_df, aes(x = Area, y = Phi_prime, fill = Area)) +
  geom_col(width = 0.5, colour = "grey20",
           alpha=0.5) +
  geom_text(aes(label = round(Phi_prime, 2)), 
            vjust = -0.5, size = 4, 
            fontface = "bold") +
  scale_fill_manual(values = pal_area) +
  ylim(0, max(phi_df$Phi_prime) * 1.2) +
  labs(x = NULL,
       y = expression(phi*"' = log"[10]*"(K) + 2\u00b7log"[10]*"(L"[infinity]*")")) +
  theme_reclam + theme(legend.position = "none")

fig3 <- (fig3a | fig3b) +
  plot_annotation(tag_levels = "a")

ggsave(here("FIG", "Fig3_VBGF_panel.jpeg"), fig3, width = 12, height = 5, dpi = fig_dpi)


# ─────────────────────────────────────────────────────────────────────────────
# 5. NATURAL MORTALITY — THREE ESTIMATORS
# ─────────────────────────────────────────────────────────────────────────────

# 5.1 Required ancillary data ─────────────────────────────────────────────────
# Mean annual SST (°C) — replace with CMEMS values when available
T_c <- 19.2    # GoC annual mean SST (°C)
T_v <- 20.5    # GoV annual mean SST (°C)

# Maximum age from VBGF (age when L = 0.95·L∞)
tmax <- function(Linf, K, t0) t0 - log(1 - 0.95) / K

tmax_c <- tmax(fit_eg_c$par$Linf, fit_eg_c$par$K, fit_eg_c$par$t_anchor)
tmax_v <- tmax(fit_eg_v$par$Linf, fit_eg_v$par$K, fit_eg_v$par$t_anchor)

# 5.2 Method I — Pauly (1980) ──────────────────────────────────────────────────
# log(M) = -0.0152 - 0.279·log(L∞) + 0.6543·log(K) + 0.4634·log(T)
M_pauly <- function(Linf, K, T) {
  exp(-0.0152 - 0.279 * log(Linf) + 0.6543 * log(K) + 0.4634 * log(T))
}

M_p_c <- M_pauly(fit_eg_c$par$Linf, fit_eg_c$par$K, T_c)
M_p_v <- M_pauly(fit_eg_v$par$Linf, fit_eg_v$par$K, T_v)

# 5.3 Method II — Hoenig (1983) ────────────────────────────────────────────────
# log(M) = 1.44 - 0.984·log(t_max)
M_hoenig <- function(tmax) exp(1.44 - 0.984 * log(tmax))

M_h_c <- M_hoenig(tmax_c)
M_h_v <- M_hoenig(tmax_v)

# 5.4 Method III — Lorenzen (1996) ─────────────────────────────────────────────
# M ≈ 3.0·W̄^(-0.25)  (requires mean body weight in g WW)
# Placeholder W̄; updated in Section 6 after LW fitting
Wbar_c <- 1.8   # g WW — placeholder
Wbar_v <- 1.2   # g WW — placeholder

M_lorenzen <- function(Wbar) 3.0 * Wbar^(-0.25)
M_l_c <- M_lorenzen(Wbar_c)
M_l_v <- M_lorenzen(Wbar_v)

# 5.5 Mortality summary table (Table 2 in paper) ──────────────────────────────

tab_mort <- tibble(
  Area                  = c("Cadiz", "Valencia"),
  `t_max (yr)`          = c(tmax_c, tmax_v),
  `T_mean (C)`          = c(T_c, T_v),
  `M_Pauly (yr-1)`      = c(M_p_c, M_p_v),
  `M_Hoenig (yr-1)`     = c(M_h_c, M_h_v),
  `M_Lorenzen (yr-1)`   = c(M_l_c, M_l_v),
  `M_mean (yr-1)`       = c(mean(c(M_p_c, M_h_c, M_l_c)),
                             mean(c(M_p_v, M_h_v, M_l_v)))
) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(tab_mort)
write_csv(tab_mort, here("RESULTS", "Table2_Mortality.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 6. LENGTH-WEIGHT RELATIONSHIP  W = a · L^b
# ─────────────────────────────────────────────────────────────────────────────

## 1.2 Length-weight (talla_peso) ----------------------------------------------

lpesoc <- read_excel(archivo_cadiz,    sheet = "talla_peso") |>
  filter(tolower(Especie) == "co") |>                 # coquina only
  mutate(area = "Cadiz")
lpesov <- read_excel(archivo_valencia, sheet = "talla_peso") |>
  filter(tolower(Especie) == "co") |>                 # coquina only
  mutate(area = "Valencia")

lp <- bind_rows(lpesoc, lpesov) |>
  dplyr::rename_with(toupper) |>
  dplyr::filter(!is.na(LONGITUD), !is.na(PESO), LONGITUD > 0, PESO > 0)

## 6.1 Log-linear regression per region ----------------------------------------

lw_fit <- function(df, area_label) {
  fit  <- lm(log(PESO) ~ log(LONGITUD), data = df)
  ci_b <- confint(fit)[2, ]
  tibble(
    area      = area_label,
    n         = nrow(df),
    a         = exp(coef(fit)[1]),
    b         = coef(fit)[2],
    R2        = summary(fit)$r.squared,
    b_CI_lo   = ci_b[1],
    b_CI_hi   = ci_b[2],
    isometric = between(3, ci_b[1], ci_b[2])
  )
}

tab_lw <- bind_rows(
  lw_fit(lp |> filter(AREA == "Cadiz"),    "Cadiz"),
  lw_fit(lp |> filter(AREA == "Valencia"), "Valencia")
) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

print(tab_lw)
write_csv(tab_lw, here("RESULTS", "Table_LW.csv"))

## 6.2 Plot --------------------------------------------------------------------

# Predicted curves from fitted a, b (avoids unstable geom_smooth + nls)
lw_curves <- tab_lw |>
  rowwise() |>
  reframe(
    LONGITUD = exp(seq(log(min(lp$LONGITUD, na.rm = TRUE)),
                       log(max(lp$LONGITUD, na.rm = TRUE)),
                       length.out = 200)),
    PESO     = a * LONGITUD^b,
    AREA     = area
  )

fig_lw <- ggplot(lp, aes(x = LONGITUD,
                         y = PESO,
                         colour = AREA)) +
  geom_point(alpha = 0.1, size = 1.2) +
  geom_line(data = lw_curves, linewidth = 1) +
  scale_colour_manual(values = pal_area) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.3) +
  labs(x = "Shell length (mm)", y = "Wet weight (g)",
       colour = "Region") +
  theme_reclam + theme(legend.position = "none")

ggsave(here("FIG", "Fig_LW.jpeg"), 
       fig_lw, width = 7, 
       height = 5,
       dpi = fig_dpi)

## 6.3 Update Wbar and Lorenzen M with fitted a, b ----------------------------

a_c  <- tab_lw |> filter(area == "Cadiz")    |> pull(a)
b_c  <- tab_lw |> filter(area == "Cadiz")    |> pull(b)
a_v  <- tab_lw |> filter(area == "Valencia") |> pull(a)
b_v  <- tab_lw |> filter(area == "Valencia") |> pull(b)

Lmean_c <- mean(tallas_co |> filter(Area == "Cadiz")    |>
                  pull(TALLA), na.rm = TRUE)
Lmean_v <- mean(tallas_co |> filter(Area == "Valencia") |> 
                  pull(TALLA), na.rm = TRUE)

Wbar_c <- a_c * Lmean_c^b_c
Wbar_v <- a_v * Lmean_v^b_v

# Re-calculate Lorenzen M with updated Wbar
M_l_c  <- M_lorenzen(Wbar_c)
M_l_v  <- M_lorenzen(Wbar_v)

M_mean_c <- mean(c(M_p_c, M_h_c, M_l_c))
M_mean_v <- mean(c(M_p_v, M_h_v, M_l_v))

message("Updated Lorenzen M  GoC: ", round(M_l_c, 3),
        "  GoV: ", round(M_l_v, 3))


## 7. PRODUCCION SECUNDARIA RECLAM GOV versus GOC (Marina) ----------

# Bioanalogical model for bivalves (Brey 2001):
#   log₁₀(P/B) = 0.8067 − 0.1883·log₁₀(AFDW) + 0.6918·log₁₀(T) − 0.3697·log₁₀(M)
#
# AFDW ≈ 0.065 · WW  (conversion factor for Donax; update with lab ash data)
# MGR reported as: mg AFDW · g AFDW⁻¹ · d⁻¹

# leer datos 

Data_produccion <- read_csv2(here("DATA", 
                                  "Data_produccion.csv"))
# Datos

# 1. AJUSTAR MODELO MIXTO
# (1|Estacion) le dice a R que Estacion es el factor aleatorio que agrupa las réplicas
datos <- Data_produccion |>
  mutate(P = as.numeric(P),
         B = as.numeric(B),
         Ratio =Ratio/100)

# revisar si aparecieron NA por coercion (valores no numericos ocultos)
datos |> filter(is.na(B) | is.na(P))

modelo_mixto <- lmer(B ~ Localizacion + (1 | Estacion), data = datos)

# Ver el resultado del análisis
summary(modelo_mixto)

# Obtener la tabla ANOVA clásica con los p-valores para la Localización
anova(modelo_mixto)

#COMO no me da la p, hago esto: Consiste en comparar tu modelo con un "modelo nulo" (un modelo que no tiene la variable Localizacion). La diferencia entre ambos te dará la \(p\) real mediante una distribución de Chi-cuadrado (\(\chi ^{2}\)).

# 2. El modelo nulo (sin Localizacion, solo con el efecto aleatorio de las estaciones)
modelo_nulo <- lmer(B ~ 1 + (1 | Estacion), data = datos, REML = FALSE)

# 3. Comparar ambos modelos para obtener la p (ya me da la p)
anova(modelo_nulo, modelo_mixto)

# 4. Asunciones para comprobar su aplicación:
#4.1 Gráfico de residuos vs. valores ajustados Linealidad y Homocedasticidad (Homogeneidad de varianzas)
plot(modelo_mixto)

#4.2 Gráfico Q-Q para ver la alineación de los puntos(Normalidad residuos)
qqnorm(residuals(modelo_mixto))
qqline(residuals(modelo_mixto), col = "red")

# Test estadístico (buscamos un p-valor mayor a 0.05)
shapiro.test(residuals(modelo_mixto))

#4.3 Independencia de las observaciones (Garantizada por el diseño)

#Cómo se cumple: Al haber incluido (1 | Estacion) en la fórmula de tu modelo mixto, ya has corregido este supuesto matemáticamente. El modelo asume esa dependencia y la controla.

#_________________________________________________________________________________________
##BOXPLOT
# Crear el boxplot por localización


pal_area <- c(Cadiz = "#1B7FC4", Valencia = "#D94F00")

datos <- datos |>
  mutate(P = as.numeric(P),
         B = as.numeric(B))

make_panel <- function(var, ylab) {
  ggplot(datos, aes(x = Localizacion, y = .data[[var]], fill = Localizacion)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6, 
                 width = 0.5,
                 color = "black") +
    geom_jitter(aes(color = Estacion), width = 0.15, size = 2.5) +
    scale_fill_manual(values = pal_area) +
    scale_color_viridis_d(option = "D") +
    guides(fill = "none") +
    labs(x = "", y = ylab, color = "Stations") +
    theme_bw() +
    theme(
      axis.title    = element_text(face = "bold", size = 12),
      axis.text     = element_text(size = 11),
      legend.title  = element_text(face = "bold")
    )
}

p_B <- make_panel("B",     "Biomass")
p_P <- make_panel("P",     "Production")
p_R <- make_panel("Ratio", "P / B")

panel_PB <- (p_B | p_P | p_R) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")

# inspeccionar
panel_PB

# guardar
ggsave(here("FIG", "FigX_P_B_Ratio.jpeg"), panel_PB,
       width = 8, height = 4, dpi = fig_dpi)





# ─────────────────────────────────────────────────────────────────────────────
# 9. STATISTICAL COMPARISONS
# ─────────────────────────────────────────────────────────────────────────────

## 9.1 Wilcoxon rank-sum tests + Benjamini-Hochberg correction ─────────────────

run_wilcox <- function(var_cadiz, var_valencia, label) {
  w <- wilcox.test(var_cadiz, var_valencia, conf.int = TRUE, exact = FALSE)
  tibble(
    Variable    = label,
    n_Cadiz     = length(var_cadiz),
    n_Valencia  = length(var_valencia),
    W           = w$statistic,
    p_raw       = w$p.value,
    Estimate    = w$estimate,
    CI_lo       = w$conf.int[1],
    CI_hi       = w$conf.int[2]
  )
}

tab_wilcox <- bind_rows(
  run_wilcox(
    tallas_co |> filter(Area == "Cadiz",
                        tipo_rastro == "Poblacional")    |> pull(TALLA),
    tallas_co |> filter(Area == "Valencia",
                        tipo_rastro == "Poblacional") |> pull(TALLA),
    "Shell length (mm)"
  ),
  run_wilcox(
    recr_idx |> filter(Area == "Cadiz")    |> pull(RI),
    recr_idx |> filter(Area == "Valencia") |> pull(RI),
    "Recruitment index"
  )#,
  # run_wilcox(
  #   cpue_monthly |> filter(Area == "Cadiz")    |> pull(cpue_mean),
  #   cpue_monthly |> filter(Area == "Valencia") |> pull(cpue_mean),
  #   "CPUE (g/min)"
  # )
) |>
  mutate(p_adj = p.adjust(p_raw, method = "BH"),
         Sig   = case_when(p_adj < 0.001 ~ "***",
                           p_adj < 0.01  ~ "**",
                           p_adj < 0.05  ~ "*",
                           TRUE          ~ "ns"),
         across(where(is.numeric), \(x) round(x, 4)))

print(tab_wilcox)
write_csv(tab_wilcox, here("RESULTS", "Table3_Wilcoxon.csv"))

## 9.2 Cohen's d effect sizes --------------------------------------------------

d_size <- cohen.d(
  tallas_co |> filter(Area == "Cadiz")    |> pull(TALLA),
  tallas_co |> filter(Area == "Valencia") |> pull(TALLA)
)
d_ri <- cohen.d(
  recr_idx |> filter(Area == "Cadiz")    |> pull(RI),
  recr_idx |> filter(Area == "Valencia") |> pull(RI)
)

cat(sprintf("\nCohen d — Shell length : %.3f (%s)\n", d_size$estimate, d_size$magnitude))
cat(sprintf("Cohen d — Recruit. idx : %.3f (%s)\n",  d_ri$estimate,   d_ri$magnitude))

## 9.3 Kruskal-Wallis — seasonal variation within each region -----------------

kw_c <- kruskal.test(TALLA ~ factor(month), data = tallas |> filter(Area == "Cadiz"))
kw_v <- kruskal.test(TALLA ~ factor(month), data = tallas |> filter(Area == "Valencia"))

cat(sprintf("KW Cadiz   : H = %.2f, df = %d, p = %.4f\n",
            kw_c$statistic, kw_c$parameter, kw_c$p.value))
cat(sprintf("KW Valencia: H = %.2f, df = %d, p = %.4f\n",
            kw_v$statistic, kw_v$parameter, kw_v$p.value))



# ─────────────────────────────────────────────────────────────────────────────
# 10. LINEAR MIXED-EFFECTS MODELS (LMM)
# ─────────────────────────────────────────────────────────────────────────────
# Response  : mean shell length at station × month level
# Fixed     : Area + month (seasonal signal)
# Random    : PUNTO (station identity)
# Correlation: AR(1) along time index within each station

## 10.1 Station-level dataset --------------------------------------------------

lmm_data <- tallas_co |>
  filter(tipo_rastro == "Poblacional") |>
  group_by(Area, PUNTO, year, month, FECHA) |>
  summarise(
    Lmean  = mean(TALLA, na.rm = TRUE),
    N      = n(),
    RI     = mean(TALLA <= 8, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(Lmean), N >= 5) |>
  mutate(
    month_f  = factor(month, levels = 1:12, labels = month.abb),
    time_idx = as.numeric(FECHA),
    Area_f   = factor(Area, levels = c("Cadiz", "Valencia"))
  )

## 10.2 LMM with AR(1) error structure (nlme) ----------------------------------

lmm_lmean <- lme(
  Lmean ~ Area_f + month_f,
  random      = ~ 1 | PUNTO,
  correlation = corAR1(form = ~ time_idx | PUNTO),
  data        = lmm_data,
  method      = "REML",
  control     = lmeControl(opt = "optim", maxIter = 300, tolerance = 1e-6)
)

summary(lmm_lmean)

## Export fixed effects
tab_lmm <- broom.mixed::tidy(lmm_lmean, effects = "fixed", conf.int = TRUE) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))
write_csv(tab_lmm, here("RESULTS", "Table4_LMM_fixed.csv"))

## 10.3 Model comparison: with vs. without AR(1) ───────────────────────────────

lmm_noAR <- lme(
  Lmean ~ Area_f + month_f,
  random = ~ 1 | PUNTO,
  data   = lmm_data,
  method = "ML"
)

lmm_AR <- update(lmm_lmean, method = "ML")
print(anova(lmm_noAR, lmm_AR))

## 10.4 LMM also for recruitment index ----------------------------------------

lmm_ri <- lme(
  RI ~ Area_f + month_f,
  random      = ~ 1 | PUNTO,
  correlation = corAR1(form = ~ time_idx | PUNTO),
  data        = lmm_data,
  method      = "REML",
  control     = lmeControl(opt = "optim")
)
summary(lmm_ri)

## 10.5 Diagnostic plot ────────────────────────────────────────────────────────

diag_df <- data.frame(
  fitted    = fitted(lmm_lmean),
  resid_std = residuals(lmm_lmean, type = "normalized"),
  Area      = lmm_data$Area_f
)

fig_diag <- ggplot(diag_df, aes(x = fitted, y = resid_std, colour = Area)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.7) +
  scale_colour_manual(values = pal_area) +
  labs(x = "Fitted values (mm)", y = "Normalized residuals",
       title = "LMM diagnostics \u2014 residuals vs. fitted") +
  theme_reclam

ggsave(here("FIG", "FigS1_LMM_diag.jpeg"), fig_diag, width = 7, height = 5, dpi = fig_dpi)
# ─────────────────────────────────────────────────────────────────────────────
# 11. PUBLICATION-READY COMPOSITE FIGURES
# ─────────────────────────────────────────────────────────────────────────────
# Figura resumen, 3 filas:
#   1. LFQ heatmap por region (Cadiz | Valencia)
#   2. Indice de performance de crecimiento (fig3b, Phi', seccion 4)
#   3. Produccion — B, P, P/B por region (panel_PB, seccion anterior)

col_cadiz    <- unname(pal_area["Cadiz"])
col_valencia <- unname(pal_area["Valencia"])

## 11.1 Fila 1 — LFQ density ridge por region
# usa dens_data ya calculado en la seccion 2.4
make_ridge_area <- function(area_nm, col) {
  df <- dens_data |>
    filter(especie == "Coquina", Area == area_nm) |>
    mutate(fecha_label = factor(fecha_label, levels = sort(unique(fecha_label))))
  lvls <- levels(df$fecha_label)
  ggplot(df, aes(x = x, y = fecha_label, height = y)) +
    geom_ridgeline(fill = col, colour = col, alpha = 0.6,
                   scale = 15, linewidth = 0.4) +
    geom_vline(xintercept = 10.8, linetype = "dashed",
               colour = "grey30", linewidth = 0.5) +
    scale_y_discrete(breaks = lvls[seq(1, length(lvls), by = 2)]) +
    labs(x = "Shell length (mm)", y = NULL, title = area_nm) +
    theme_reclam +
    theme(axis.text.y = element_text(size = 7),
          plot.title  = element_text(face = "bold", hjust = 0.5))
}

row_heat <- make_ridge_area("Cadiz", col_cadiz) | make_ridge_area("Valencia", col_valencia)

## 11.2 Fila 2 — Phi' (fig3b, seccion 4) ----------------------------------------
## 11.3 Fila 3 — produccion B / P / P·B-1 (panel_PB, p_B|p_P|p_R) --------------
# fig3b y p_B/p_P/p_R ya existen (secciones 4 y previa a esta) -- se reusan tal cual,
# sin su propio plot_annotation para que el tag_levels quede unico a nivel de fig6

row_prod <- (p_B | p_P | p_R) + 
  plot_layout(guides = "collect")


## 11.4 Ensamblar panel 3 filas --------------------------------------------------
## fig3b y fig_lw (seccion 6.2, talla-peso) van lado a lado en la fila 2

fig6 <- row_heat / (fig3b | fig_lw) / row_prod +
  plot_annotation(
    tag_levels = "a")

# inspeccionar
fig6

# guardar
ggsave(here("FIG", "Fig6_Composite_ByRegion.jpeg"), fig6,
       width = 10, height = 10, dpi = fig_dpi)
# somatico con condicion corporal, relevante si el paper discute produccion (P/B).


# ─────────────────────────────────────────────────────────────────────────────
# 12. MASTER PARAMETER TABLE (flextable)
# ─────────────────────────────────────────────────────────────────────────────

# promedios de P, B y Ratio por region, a partir de datos (Data_produccion)
tab_prod <- datos |>
  group_by(Area = Localizacion) |>
  summarise(
    P_mean     = mean(P,     na.rm = TRUE),
    B_mean     = mean(B,     na.rm = TRUE),
    Ratio_mean = mean(Ratio/100, na.rm = TRUE),
    .groups    = "drop"
  )

tab_master <- tab_vbgf |>
  filter(Method == "ELEFAN_GA") |>
  dplyr::select(Area, Linf_mm, K_yr, Phi_prime) |>
  left_join(
    tab_mort |> dplyr::select(Area,
                              `M_Pauly (yr-1)`,
                              `M_Hoenig (yr-1)`,
                              `M_Lorenzen (yr-1)`,
                              `M_mean (yr-1)`),
    by = "Area"
  ) |>
  left_join(tab_prod, by = "Area")

ft <- tab_master |>
  flextable() |>
  set_header_labels(
    Area                = "Region",
    Linf_mm             = "L∞ (mm)",
    K_yr                = "K (yr⁻¹)",
    Phi_prime           = "Φ′",
    `M_Pauly (yr-1)`    = "M Pauly",
    `M_Hoenig (yr-1)`   = "M Hoenig",
    `M_Lorenzen (yr-1)` = "M Lorenzen",
    `M_mean (yr-1)`     = "M̄ (yr⁻¹)",
    P_mean              = "P",
    B_mean              = "B",
    Ratio_mean          = "P/B"
  ) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  border_outer(border = officer::fp_border(color = "grey40", width = 1)) |>
  autofit()

save_as_docx(ft, path = here("RESULTS", "Table2_Master.docx"))
write_csv(tab_master, here("RESULTS", "Table2_Master.csv"))

# ─────────────────────────────────────────────────────────────────────────────

