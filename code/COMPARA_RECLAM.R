# =============================================================================
# COMPARA_RECLAM.R
# Comparative Population Dynamics of Donax trunculus
# Gulf of Cádiz (GoC) vs. Gulf of Valencia (GoV)
# Project RECLAM — IEO-CSIC / UPV
#
# Analyses:
#   0.  Setup & helpers
#   1.  Data loading (size, L-W, CPUE, catch)
#   2.  Size-frequency distributions (LFQ)
#   3.  von Bertalanffy growth (ELEFAN_GA + NLS + comparison)
#   4.  Growth performance index (Φ')
#   5.  Natural mortality — Pauly(1980), Hoenig(1983), Lorenzen(1996)
#   6.  Length-weight relationship (allometric)
#   7.  Mass-specific growth rate (MGR) — Brey (2001)
#   8.  Population indicators: density, biomass, CPUE, recruitment index
#   9.  Statistical comparisons (Wilcoxon, Cohen's d, Kruskal-Wallis)
#  10.  Linear mixed-effects models (LMM)
#  11.  Environmental drivers — SST anomaly, MHW, GAM/GLM
#  12.  Publication-ready composite figures (patchwork)
#  13.  Summary tables (flextable)
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

  # Environmental
  library(heatwaveR)     # Marine heatwave detection (Hobday et al. 2016)

  # Visualization
  library(ggridges)      # Ridge plots
  library(patchwork)     # Multi-panel layout
  library(viridis)       # Colour-blind palettes
  library(ggpubr)        # stat_compare_means

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
    TALLA = as.numeric(TALLA),
    PUNTO = as.character(PUNTO),
    FECHA = as.Date(FECHA, origin = "1899-12-30"),
    Area  = "Cadiz"
  )

tallav <- read_excel(archivo_valencia, sheet = "tallas") |>
  mutate(
    TALLA = as.numeric(TALLA),
    FECHA = as.Date(FECHA),
    Area  = "Valencia"
  )

tallas <- bind_rows(tallac, tallav) |>
  filter(!is.na(TALLA), TALLA > 0) |>
  mutate(
    year      = year(FECHA),
    month     = month(FECHA),
    talla_cls = floor(TALLA),                         # 1-mm class
    size_class = ifelse(TALLA <= 8, "Recruit", "Adult")
  )

## 1.2 Length-weight (talla_peso) ----------------------------------------------

lpesoc <- read_excel(archivo_cadiz,    sheet = "talla_peso") |>
  mutate(area = "Cadiz")
lpesov <- read_excel(archivo_valencia, sheet = "talla_peso") |>
  mutate(area = "Valencia")

lp <- bind_rows(lpesoc, lpesov) |>
  dplyr::rename_with(toupper) |>
  dplyr::filter(!is.na(LONGITUD), !is.na(PESO), LONGITUD > 0, PESO > 0)

## 1.3 Coquina catch / CPUE ---------------------------------------------------

harmonise_cap <- function(df) {
  df |>
    mutate(
      across(any_of(c("peso_total_con_cascajo_g",
                       "peso_muestreado_total_con_cascajo_g")), as.numeric),
      observaciones = as.character(observaciones),
      fecha         = as.Date(fecha)
    )
}

cap_coq_c <- read_excel(archivo_cadiz,    sheet = "Captura_coquina") |>
  mutate(area = "Cadiz")    |> clean_names() |> harmonise_cap()
cap_coq_v <- read_excel(archivo_valencia, sheet = "Captura_coquina") |>
  mutate(area = "Valencia") |> clean_names() |> harmonise_cap()

cap_coq <- bind_rows(cap_coq_c, cap_coq_v) |>
  dplyr::mutate(
    cpue_g_min = peso_total_con_cascajo_g / 1,
    year       = year(fecha),
    month      = month(fecha)
  )

## 1.4 Environmental SST (optional — add your CSV) -----------------------------
# Expected columns:  date (Date), sst (°C), area ("Cadiz" | "Valencia")

sst_path <- here("DATA", "SST_GoC_GoV.csv")
sst <- if (file.exists(sst_path)) {
  read_csv(sst_path, show_col_types = FALSE) |>
    mutate(date = as.Date(date))
} else {
  message("[INFO] SST file not found — Section 11 (environmental drivers) skipped.")
  NULL
}


# ─────────────────────────────────────────────────────────────────────────────
# 2. SIZE-FREQUENCY DISTRIBUTIONS (LFQ)
# ─────────────────────────────────────────────────────────────────────────────

## 2.1 Build TropFishR-compatible LFQ objects ----------------------------------

build_lfq <- function(df) {
  freq <- df |>
    filter(!is.na(TALLA)) |>
    mutate(cls = floor(TALLA)) |>
    group_by(year, month, cls) |>
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

lfq_c <- lfqRestructure(build_lfq(tallac), MA = 5)
lfq_v <- lfqRestructure(build_lfq(tallav), MA = 5)

## 2.2 Monthly LFQ plots (Figure 2 in paper) -----------------------------------

ni_plot <- tallas |>
  group_by(Area, year, month, talla_cls) |>
  summarise(Ni = n(), .groups = "drop") |>
  mutate(
    fecha_label = paste(year, sprintf("%02d", month), sep = "-"),
    is_recruit  = talla_cls <= 8
  )

make_lfq_plot <- function(area_nm, col) {
  ggplot(ni_plot |> filter(Area == area_nm),
         aes(x = talla_cls, y = Ni, fill = is_recruit)) +
    geom_col(width = 0.9) +
    facet_wrap(~ fecha_label, ncol = 4, scales = "free_y") +
    scale_fill_manual(
      values = c("TRUE" = "#FFB703", "FALSE" = col),
      labels = c("TRUE" = "Recruit (\u22648 mm)", "FALSE" = "Adult")
    ) +
    labs(
      title = bquote(italic("D. trunculus") ~ "\u2014" ~ .(area_nm)),
      x = "Shell length (mm)", y = expression(N[i]), fill = NULL
    ) +
    theme_reclam
}

fig2a <- make_lfq_plot("Cadiz",   pal_area["Cadiz"])
fig2b <- make_lfq_plot("Valencia", pal_area["Valencia"])

ggsave(here("FIG", "Fig2a_LFQ_Cadiz.jpeg"),   fig2a, width = 14, height = 10, dpi = 300)
ggsave(here("FIG", "Fig2b_LFQ_Valencia.jpeg"), fig2b, width = 14, height = 10, dpi = 300)

## 2.3 LFQ heatmap (month × size class) ----------------------------------------

fig2c <- ggplot(ni_plot, aes(x = talla_cls, y = factor(month), fill = Ni)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  facet_wrap(~ Area) +
  scale_fill_viridis_c(option = "H", name = expression(N[i]),
                       trans = "sqrt", na.value = "grey90") +
  scale_y_discrete(labels = month.abb) +
  labs(x = "Shell length (mm)", y = "Month") +
  theme_reclam + theme(legend.position = "right")

ggsave(here("FIG", "Fig2c_LFQ_heatmap.jpeg"), fig2c, width = 10, height = 6, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
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
fit_eg_v <- run_elefan(lfq_v, "Valencia")

## 3.2 NLS on mean monthly length (independent validation) --------------------

mean_monthly <- tallas |>
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

age_seq <- seq(0, 5, by = 0.02)

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
       colour = "Region", linetype = "Method",
       title = expression("von Bertalanffy growth")) +
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
  geom_col(width = 0.5, colour = "grey20") +
  geom_text(aes(label = round(Phi_prime, 2)), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = pal_area) +
  ylim(0, max(phi_df$Phi_prime) * 1.2) +
  labs(x = NULL,
       y = expression(phi*"' = log"[10]*"(K) + 2\u00b7log"[10]*"(L"[infinity]*")"),
       title = "Growth performance index (\u03a6\u2032)") +
  theme_reclam + theme(legend.position = "none")

fig3 <- (fig3a | fig3b) +
  plot_annotation(tag_levels = "a",
                  title = expression(italic("D. trunculus") ~ "\u2014 Growth parameters"))

ggsave(here("FIG", "Fig3_VBGF_panel.jpeg"), fig3, width = 12, height = 5, dpi = 300)


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

## 6.1 Log-linear regression per region ----------------------------------------

lw_fit <- function(df, area_label) {
  fit  <- lm(log(PESO) ~ log(TALLA), data = df)
  ci_b <- confint(fit)[2, ]
  tibble(
    area    = area_label,
    n       = nrow(df),
    a       = exp(coef(fit)[1]),
    b       = coef(fit)[2],
    R2      = summary(fit)$r.squared,
    b_CI_lo = ci_b[1],
    b_CI_hi = ci_b[2],
    isometric = between(3, ci_b[1], ci_b[2])  # H0: b=3 within 95% CI?
  )
}

tab_lw <- bind_rows(
  lw_fit(lp |> filter(area == "Cadiz"),   "Cadiz"),
  lw_fit(lp |> filter(area == "Valencia"), "Valencia")
) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

print(tab_lw)
write_csv(tab_lw, here("RESULTS", "Table_LW.csv"))

## 6.2 Plot --------------------------------------------------------------------

fig_lw <- ggplot(lp, aes(x = TALLA, y = PESO, colour = area)) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_smooth(method = "nls",
              formula = y ~ a * x^b,
              method.args = list(start = list(a = 0.0005, b = 3)),
              se = FALSE, linewidth = 1) +
  scale_colour_manual(values = pal_area) +
  scale_x_log10() + scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.3) +
  labs(x = "Shell length (mm)", y = "Wet weight (g)",
       colour = "Region",
       caption = "log-log scale; curve fitted by NLS",
       title = expression("Length\u2013weight relationship \u2014 " * italic("D. trunculus"))) +
  theme_reclam

ggsave(here("FIG", "Fig_LW.jpeg"), fig_lw, width = 7, height = 5, dpi = 300)

## 6.3 Update Wbar and Lorenzen M with fitted a, b ----------------------------

a_c  <- tab_lw |> filter(area == "Cadiz")    |> pull(a)
b_c  <- tab_lw |> filter(area == "Cadiz")    |> pull(b)
a_v  <- tab_lw |> filter(area == "Valencia") |> pull(a)
b_v  <- tab_lw |> filter(area == "Valencia") |> pull(b)

Lmean_c <- mean(tallas |> filter(Area == "Cadiz")    |> pull(TALLA), na.rm = TRUE)
Lmean_v <- mean(tallas |> filter(Area == "Valencia") |> pull(TALLA), na.rm = TRUE)

Wbar_c <- a_c * Lmean_c^b_c
Wbar_v <- a_v * Lmean_v^b_v

# Re-calculate Lorenzen M with updated Wbar
M_l_c  <- M_lorenzen(Wbar_c)
M_l_v  <- M_lorenzen(Wbar_v)

M_mean_c <- mean(c(M_p_c, M_h_c, M_l_c))
M_mean_v <- mean(c(M_p_v, M_h_v, M_l_v))

message("Updated Lorenzen M  GoC: ", round(M_l_c, 3),
        "  GoV: ", round(M_l_v, 3))


# ─────────────────────────────────────────────────────────────────────────────
# 7. MASS-SPECIFIC GROWTH RATE (MGR) — Brey (2001)
# ─────────────────────────────────────────────────────────────────────────────
# Bioanalogical model for bivalves (Brey 2001):
#   log₁₀(P/B) = 0.8067 − 0.1883·log₁₀(AFDW) + 0.6918·log₁₀(T) − 0.3697·log₁₀(M)
#
# AFDW ≈ 0.065 · WW  (conversion factor for Donax; update with lab ash data)
# MGR reported as: mg AFDW · g AFDW⁻¹ · d⁻¹

afdw_factor <- 0.065   # AFDW/WW  — replace with measured ratio from Gandia

mgr_brey <- function(Wbar_WW, M_yr, T_C, afdw_f = afdw_factor) {
  AFDW   <- Wbar_WW * afdw_f
  logPB  <- 0.8067 - 0.1883 * log10(AFDW) +
            0.6918 * log10(T_C) -
            0.3697 * log10(M_yr)
  PB     <- 10^logPB                     # yr⁻¹
  MGR    <- PB / 365 * 1000              # mg AFDW · g AFDW⁻¹ · d⁻¹
  list(AFDW_g = AFDW, PB_yr = PB, MGR = MGR)
}

mgr_c <- mgr_brey(Wbar_c, M_mean_c, T_c)
mgr_v <- mgr_brey(Wbar_v, M_mean_v, T_v)

tab_mgr <- tibble(
  Area                         = c("Cadiz", "Valencia"),
  `Wbar_WW (g)`                = c(Wbar_c, Wbar_v),
  `AFDW_mean (g)`              = c(mgr_c$AFDW_g, mgr_v$AFDW_g),
  `M_mean (yr-1)`              = c(M_mean_c, M_mean_v),
  `P/B (yr-1)`                 = c(mgr_c$PB_yr, mgr_v$PB_yr),
  `MGR (mg AFDW g-1 d-1)` = c(mgr_c$MGR, mgr_v$MGR)
) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

print(tab_mgr)
write_csv(tab_mgr, here("RESULTS", "Table2_MGR.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# 8. POPULATION INDICATORS
# ─────────────────────────────────────────────────────────────────────────────

## 8.1 Recruitment index — monthly proportion of recruits (SL ≤ 8 mm) --------

recr_idx <- tallas |>
  group_by(Area, FECHA, year, month) |>
  summarise(
    N_total   = n(),
    N_recruit = sum(TALLA <= 8, na.rm = TRUE),
    RI        = N_recruit / N_total,
    .groups   = "drop"
  )

fig8a <- ggplot(recr_idx, aes(x = FECHA, y = RI * 100, colour = Area, fill = Area)) +
  geom_ribbon(aes(ymin = 0, ymax = RI * 100), alpha = 0.15) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values   = pal_area) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  labs(x = NULL, y = "Recruitment index (%)",
       colour = "Region", fill = "Region",
       title = "Proportion of recruits (SL \u2264 8 mm)") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## 8.2 CPUE monthly time series ------------------------------------------------

cpue_monthly <- cap_coq |>
  filter(!is.na(cpue_g_min), cpue_g_min >= 0) |>
  group_by(area, year, month, fecha) |>
  summarise(
    cpue_mean = mean(cpue_g_min, na.rm = TRUE),
    cpue_se   = sd(cpue_g_min,   na.rm = TRUE) / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  )

fig8b <- ggplot(cpue_monthly, aes(x = fecha, y = cpue_mean,
                                   colour = area, fill = area)) +
  geom_ribbon(aes(ymin = pmax(cpue_mean - cpue_se, 0),
                  ymax = cpue_mean + cpue_se), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values   = pal_area) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  labs(x = NULL, y = expression("CPUE (g\u00b7min"^{-1}*")"),
       colour = "Region", fill = "Region",
       title = "Catch-per-unit-effort") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## 8.3 Relative density (N per station) ----------------------------------------

density_ts <- tallas |>
  group_by(Area, FECHA, year, month, PUNTO) |>
  summarise(N = n(), .groups = "drop") |>
  group_by(Area, FECHA, year, month) |>
  summarise(
    N_mean = mean(N), N_se = sd(N) / sqrt(n()), .groups = "drop"
  )

fig8c <- ggplot(density_ts, aes(x = FECHA, y = N_mean,
                                 colour = Area, fill = Area)) +
  geom_ribbon(aes(ymin = pmax(N_mean - N_se, 0), ymax = N_mean + N_se),
              alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = pal_area) +
  scale_fill_manual(values   = pal_area) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  labs(x = NULL, y = expression("Density (N\u00b7station"^{-1}*")"),
       colour = "Region", fill = "Region",
       title = "Relative density") +
  theme_reclam +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## 8.4 Patchwork Figure 4 (population indicators panel) -----------------------

fig4 <- (fig8c / fig8b / fig8a) +
  plot_annotation(
    tag_levels = "a",
    title      = expression(italic("D. trunculus") ~
                              "\u2014 Population indicators by region")
  )

ggsave(here("FIG", "Fig4_PopIndicators.jpeg"), fig4, width = 10, height = 13, dpi = 300)

## 8.5 Monthly violin × region (size distribution) ----------------------------

fig_violin <- ggplot(tallas, aes(x = factor(month), y = TALLA,
                                  fill = Area, colour = Area)) +
  geom_violin(alpha = 0.35, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.75) +
  scale_fill_manual(values   = pal_area) +
  scale_colour_manual(values = pal_area) +
  scale_x_discrete(labels = month.abb) +
  labs(x = "Month", y = "Shell length (mm)",
       fill = "Region", colour = "Region",
       title = expression("Monthly size distribution \u2014 " * italic("D. trunculus"))) +
  theme_reclam

ggsave(here("FIG", "FigS3_SizeViolin.jpeg"), fig_violin, width = 12, height = 5, dpi = 300)


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
    tallas |> filter(Area == "Cadiz")    |> pull(TALLA),
    tallas |> filter(Area == "Valencia") |> pull(TALLA),
    "Shell length (mm)"
  ),
  run_wilcox(
    recr_idx |> filter(Area == "Cadiz")    |> pull(RI),
    recr_idx |> filter(Area == "Valencia") |> pull(RI),
    "Recruitment index"
  ),
  run_wilcox(
    cpue_monthly |> filter(area == "Cadiz")    |> pull(cpue_mean),
    cpue_monthly |> filter(area == "Valencia") |> pull(cpue_mean),
    "CPUE (g/min)"
  )
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
  tallas |> filter(Area == "Cadiz")    |> pull(TALLA),
  tallas |> filter(Area == "Valencia") |> pull(TALLA)
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

## 9.4 Comparison boxplot (Figure 5) ------------------------------------------

fig5 <- ggplot(tallas, aes(x = Area, y = TALLA, fill = Area)) +
  geom_violin(alpha = 0.5, trim = TRUE) +
  geom_boxplot(width = 0.10, outlier.shape = 21, outlier.size = 0.8,
               colour = "grey20") +
  ggpubr::stat_compare_means(method = "wilcox.test", label = "p.format",
                              label.x = 1.4, size = 3.5) +
  scale_fill_manual(values = pal_area) +
  labs(x = NULL, y = "Shell length (mm)", fill = NULL,
       title = expression("Size comparison \u2014 " * italic("D. trunculus"))) +
  theme_reclam + theme(legend.position = "none")

ggsave(here("FIG", "Fig5_SizeComparison.jpeg"), fig5, width = 6, height = 5, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 10. LINEAR MIXED-EFFECTS MODELS (LMM)
# ─────────────────────────────────────────────────────────────────────────────
# Response  : mean shell length at station × month level
# Fixed     : Area + month (seasonal signal)
# Random    : PUNTO (station identity)
# Correlation: AR(1) along time index within each station

## 10.1 Station-level dataset --------------------------------------------------

lmm_data <- tallas |>
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

ggsave(here("FIG", "FigS1_LMM_diag.jpeg"), fig_diag, width = 7, height = 5, dpi = 300)


# ─────────────────────────────────────────────────────────────────────────────
# 11. ENVIRONMENTAL DRIVERS — SST, MHW, GAM
# ─────────────────────────────────────────────────────────────────────────────
# Requires: SST_GoC_GoV.csv  in DATA/
# (date, sst, area)

if (!is.null(sst)) {

  ## 11.1 Marine heatwave detection — heatwaveR --------------------------------

  detect_mhw <- function(sst_df, area_nm) {
    ts_in <- sst_df |> filter(area == area_nm) |>
      arrange(date) |> dplyr::select(t = date, temp = sst)
    clim  <- ts2clm(ts_in, climatologyPeriod = c(
      format(min(ts_in$t), "%Y-%m-%d"),
      format(max(ts_in$t), "%Y-%m-%d")
    ))
    detect_event(clim)
  }

  mhw_c <- detect_mhw(sst, "Cadiz")
  mhw_v <- detect_mhw(sst, "Valencia")

  mhw_summary <- bind_rows(
    mhw_c$event |> mutate(area = "Cadiz"),
    mhw_v$event |> mutate(area = "Valencia")
  ) |>
    group_by(area) |>
    summarise(
      n_events         = n(),
      mean_duration_d  = round(mean(duration), 1),
      max_intensity_dC = round(max(intensity_max), 2),
      cum_MHW_days     = sum(duration),
      .groups          = "drop"
    )

  print(mhw_summary)
  write_csv(mhw_summary, here("RESULTS", "Table_MHW.csv"))

  ## 11.2 SST anomaly time series -----------------------------------------------

  sst_anom <- sst |>
    group_by(area) |>
    mutate(
      doy     = yday(date),
      clim    = ave(sst, doy, FUN = mean),
      anomaly = sst - clim
    ) |> ungroup()

  fig11a <- ggplot(sst_anom, aes(x = date, y = sst, colour = area)) +
    geom_line(linewidth = 0.5, alpha = 0.7) +
    geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1) +
    scale_colour_manual(values = pal_area) +
    labs(x = NULL, y = "SST (\u00b0C)", colour = "Region",
         title = "Sea surface temperature") +
    theme_reclam

  fig11b <- ggplot(sst_anom, aes(x = date, y = anomaly, fill = anomaly > 0)) +
    geom_col(width = 1) +
    facet_wrap(~ area, ncol = 1) +
    scale_fill_manual(values = c("TRUE" = "#D94F00", "FALSE" = "#1B7FC4"),
                      guide = "none") +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    labs(x = NULL, y = "SST anomaly (\u00b0C)", title = "SST anomaly") +
    theme_reclam

  fig11 <- (fig11a / fig11b) +
    plot_annotation(tag_levels = "a",
                    title = "Environmental time series \u2014 GoC vs. GoV")
  ggsave(here("FIG", "Fig5_SST_MHW.jpeg"), fig11, width = 10, height = 8, dpi = 300)

  ## 11.3 GAM: CPUE ~ Area + s(SST_anom) + s(month, bs="cc") + s(year) --------

  sst_monthly <- sst_anom |>
    mutate(year = year(date), month = month(date)) |>
    group_by(area, year, month) |>
    summarise(sst_mean  = mean(sst,     na.rm = TRUE),
              sst_anom  = mean(anomaly, na.rm = TRUE),
              .groups   = "drop")

  cpue_env <- cpue_monthly |>
    left_join(sst_monthly, by = c("area", "year", "month")) |>
    filter(!is.na(sst_mean), !is.na(cpue_mean), cpue_mean > 0)

  gam_cpue <- gam(
    log(cpue_mean) ~
      area +
      s(sst_anom, k = 5, by = area) +
      s(month,    k = 6, bs = "cc") +
      s(year,     k = 4),
    data   = cpue_env,
    method = "REML"
  )

  cat("\n--- GAM summary ---\n")
  print(summary(gam_cpue))

  write_csv(
    as.data.frame(summary(gam_cpue)$p.table),
    here("RESULTS", "Table5_GAM_parametric.csv")
  )
  write_csv(
    as.data.frame(summary(gam_cpue)$s.table),
    here("RESULTS", "Table5_GAM_smooth.csv")
  )

  # Partial effects (Figure 6)
  fig6_partials <- gratia::draw(gam_cpue, residuals = TRUE)
  ggsave(here("FIG", "Fig6_GAM_partials.jpeg"),
         fig6_partials, width = 10, height = 6, dpi = 300)

  # Diagnostics
  ggsave(here("FIG", "FigS2_GAM_diag.jpeg"),
         gratia::appraise(gam_cpue), width = 8, height = 6, dpi = 300)

} else {
  message("[INFO] Sections 11 skipped — SST file not found.")
}


# ─────────────────────────────────────────────────────────────────────────────
# 12. PUBLICATION-READY COMPOSITE FIGURES
# ─────────────────────────────────────────────────────────────────────────────

# Figure 2 — LFQ panel
fig2_panel <- (fig2a / fig2b) + plot_annotation(tag_levels = "a")
ggsave(here("FIG", "Fig2_LFQ_panel.jpeg"), fig2_panel,
       width = 14, height = 18, dpi = 300)

# Figure 3 — already saved above (fig3 = fig3a | fig3b)
# Figure 4 — already saved above (fig4)
# Figure 5 — size comparison already saved


# ─────────────────────────────────────────────────────────────────────────────
# 13. MASTER PARAMETER TABLE (flextable)
# ─────────────────────────────────────────────────────────────────────────────

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
  left_join(
    tab_mgr |> dplyr::select(Area, `MGR (mg AFDW g-1 d-1)`),
    by = "Area"
  )

ft <- tab_master |>
  flextable() |>
  set_header_labels(
    Area                       = "Region",
    Linf_mm                    = "L\u221e (mm)",
    K_yr                       = "K (yr\u207b\u00b9)",
    Phi_prime                  = "\u03a6\u2032",
    `M_Pauly (yr-1)`           = "M Pauly",
    `M_Hoenig (yr-1)`          = "M Hoenig",
    `M_Lorenzen (yr-1)`        = "M Lorenzen",
    `M_mean (yr-1)`            = "M\u0304 (yr\u207b\u00b9)",
    `MGR (mg AFDW g-1 d-1)` = "MGR"
  ) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  bg(i = 1, bg = "#D9E8F5") |>
  bg(i = 2, bg = "#F5E4D9") |>
  border_outer(border = officer::fp_border(color = "grey40", width = 1)) |>
  autofit()

save_as_docx(ft, path = here("RESULTS", "Table2_Master.docx"))
write_csv(tab_master, here("RESULTS", "Table2_Master.csv"))


# ─────────────────────────────────────────────────────────────────────────────
message("\n========================================")
message(" COMPARA_RECLAM.R — all sections done.")
message(" Figures : ", here("FIG"))
message(" Results : ", here("RESULTS"))
message("========================================\n")

