library(dplyr)
library(lubridate)
library(readr)
library(TropFishR)

set.seed(123)
tallas_proc <- tallasuni %>%
  filter(!is.na(TALLA)) %>%
  mutate(
    year  = year(FECHA),
    month = month(FECHA),
    talla_clase = floor(TALLA)   # clases de 1 mm
  ) %>% 
  mutate (especie = ifelse(especie == "CHs" , "CH", especie))

Ni_mensual <- tallas_proc %>%
  group_by(especie, Area, year, month, talla_clase) %>%
  summarise(
    Ni = n(),
    .groups = "drop"
  ) %>%
  arrange(Area, year, month, talla_clase) 


Ni_Cadiz <- Ni_mensual %>%
  filter(Area == "Cadiz")

Ni_Valencia <- Ni_mensual %>%
  filter(Area == "Valencia")


write.csv(Ni_Cadiz,
          "DATA/Ni_mensual_Cadiz.csv",
          row.names = FALSE)

write.csv(Ni_Valencia,
          "DATA/Ni_mensual_Valencia.csv",
          row.names = FALSE)

## PLOT

ggplot(Ni_mensual %>% filter(Area == "Cadiz"),
       aes(x = talla_clase, y = Ni, col=especie)) +
  geom_col(fill = NA) +
  facet_wrap(year~ month, ncol = 4,
             scale = "free_y") +
  theme_bw() +
  labs(title = "Cadiz - Frecuencias mensuales")

ggplot(Ni_mensual %>% filter(Area == "Valencia"),
       aes(x = talla_clase, y = Ni, col=especie)) +
  geom_col(fill = NA) +
  facet_wrap(year ~ month, ncol = 4,
             scale = "free_y") +
  theme_bw() +
  labs(title = "Valencia - Frecuencias mensuales")


ggplot(Ni_mensual,
       aes(x = talla_clase, y = month, fill = Ni)) +
  geom_tile() +
  facet_wrap(especie ~ Area) +
  scale_fill_viridis_c(option = "H") +
  theme_bw() +
  labs(
    x = "Clase de talla (mm)",
    y = "Mes",
    fill = "Ni"
  )
# CALCULO PARAMETROS TROPFISHR


tallas_proc <- tallasuni %>%
  filter(!is.na(TALLA)) %>%
  mutate(
    year  = year(FECHA),
    month = month(FECHA),
    talla_clase = floor(TALLA)
  )


crear_LFQ <- function(df_area){

  freq <- df_area %>%
    group_by(year, month, talla_clase) %>%
    summarise(N = n(), .groups = "drop")

  matriz <- freq %>%
    tidyr::pivot_wider(
      names_from = talla_clase,
      values_from = N,
      values_fill = 0
    ) %>%
    arrange(year, month)

  # Crear fechas reales
  fechas <- as.Date(paste(matriz$year,
                          matriz$month,
                          "15",
                          sep = "-"))

  # Extraer matriz numérica
  mat <- matriz[, -(1:2)]

  # ---- ORDENAR CLASES DE TALLA NUMÉRICAMENTE ----
  clases <- as.numeric(colnames(mat))
  orden  <- order(clases)

  mat <- as.matrix(mat[, orden])
  clases <- clases[orden]

  list(
    dates = fechas,
    midLengths = clases,
    catch = mat
  )
}



LFQ_Cadiz <- crear_LFQ(
  tallas_proc %>% filter(Area == "Cadiz")
)

LFQ_Valencia <- crear_LFQ(
  tallas_proc %>% filter(Area == "Valencia")
)



LFQ_Cadiz <- lfqRestructure(LFQ_Cadiz)
LFQ_Valencia <- lfqRestructure(LFQ_Valencia)



fit_Cadiz <- ELEFAN_GA(
  LFQ_Cadiz,
  popSize = 50,
  maxiter = 100,
  run = 5
)

fit_Valencia <- ELEFAN_GA(
  LFQ_Valencia,
  popSize = 80,
  maxiter = 150,
  run = 5,
  low_par = list(Linf = 30, K = 0.2),
  up_par  = list(Linf = 50, K = 1.5)
)



fit_Cadiz$par
fit_Valencia$par


par_C <- fit_Cadiz$par
par_V <- fit_Valencia$par



edad <- seq(0, 4.5, by = 0.01)


VB <- function(t, Linf, K, t0 = 0){
  Linf * (1 - exp(-K * (t - t0)))
}

Lt_C <- VB(edad,
           Linf = par_C$Linf,
           K    = par_C$K,
           t0   = par_C$t_anchor)

Lt_V <- VB(edad,
           Linf = par_V$Linf,
           K    = par_V$K,
           t0   = par_V$t_anchor)


plot(edad, Lt_C,
     type = "l",
     lwd = 2,
     col = "blue",
     xlab = "Edad (años)",
     ylab = "Longitud (mm)",
     ylim = c(0, max(Lt_C, Lt_V, na.rm = TRUE)))

lines(edad, Lt_V,
      lwd = 2,
      col = "red")

legend("bottomright",
       legend = c("Cadiz", "Valencia"),
       col = c("blue", "red"),
       lwd = 2)


###
library(dplyr)
library(lubridate)

tallas_proc <- tallasuni %>%
  mutate(
    FECHA = as.Date(FECHA),
    year_dec = decimal_date(FECHA)
  )

media_mensual <- tallas_proc %>%
  group_by(Area, FECHA) %>%
  summarise(
    Lmean = mean(TALLA, na.rm = TRUE),
    .groups = "drop"
  )



ajustar_vb <- function(df_area){
  
  df_area <- df_area %>%
    arrange(FECHA) %>%
    mutate(
      t = decimal_date(FECHA),
      t_rel = t - min(t)   # centrar tiempo
    )
  
  Lmax_obs <- max(df_area$Lmean)
  
  fit <- nls(
    Lmean ~ Linf * (1 - exp(-K * (t_rel - t0))),
    data = df_area,
    start = list(
      Linf = Lmax_obs * 1.1,
      K = 0.5,
      t0 = 0
    ),
    algorithm = "port",
    lower = c(Linf = Lmax_obs, K = 0.01, t0 = -5),
    upper = c(Linf = 60, K = 3, t0 = 5)
  )
  
  return(fit)
}



fit_cadiz <- ajustar_vb(
  media_mensual %>% filter(Area == "Cadiz")
)

fit_valencia <- ajustar_vb(
  media_mensual %>% filter(Area == "Valencia")
)


coef(fit_cadiz)
coef(fit_valencia)


phi_cadiz <- log10(coef(fit_cadiz)["K"]) +
  2 * log10(coef(fit_cadiz)["Linf"])

phi_valencia <- log10(coef(fit_valencia)["K"]) +
  2 * log10(coef(fit_valencia)["Linf"])


plot_vb <- function(fit, df_area, col){
  
  df_area <- df_area %>%
    arrange(FECHA) %>%
    mutate(
      t = decimal_date(FECHA),
      t_rel = t - min(t)
    )
  
  t_seq <- seq(min(df_area$t_rel),
               max(df_area$t_rel),
               length.out = 200)
  
  Linf <- coef(fit)["Linf"]
  K    <- coef(fit)["K"]
  t0   <- coef(fit)["t0"]
  
  Lt <- Linf * (1 - exp(-K * (t_seq - t0)))
  
  lines(t_seq, Lt, col = col, lwd = 2)
}

# base plot
cadiz_df <- media_mensual %>% filter(Area=="Cadiz") %>%
  arrange(FECHA) %>%
  mutate(t_rel = decimal_date(FECHA) - min(decimal_date(FECHA)))

plot(cadiz_df$t_rel,
     cadiz_df$Lmean,
     pch=16,
     xlab="Time (years)",
     ylab="Mean length (mm)")

plot_vb(fit_cadiz,
        media_mensual %>% filter(Area=="Cadiz"),
        "blue")

plot_vb(fit_valencia,
        media_mensual %>% filter(Area=="Valencia"),
        "red")

legend("bottomright",
       legend=c("Cadiz","Valencia"),
       col=c("blue","red"),
       lwd=2)
