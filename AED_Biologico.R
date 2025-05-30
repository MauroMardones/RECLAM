## ----setup1-----------------------------------------------------------------------------------------------
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = 'center',
                      dev = 'jpeg',
                      dpi = 300, 
                      fig.align='center')
#XQuartz is a mess, put this in your onload to default to cairo instead
options(bitmapType = "cairo") 
# (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------
library(tidyverse)
library(ggridges)
library(readxl)
library(here)
library(lubridate)
library(readr)
library(ggthemes)
library(hrbrthemes)
library(kableExtra)
library(gtsummary)
library(easystats)
library(sf)
library(egg)


## ----include=FALSE----------------------------------------------------------------------------------------
## Set path



## ----funcion----------------------------------------------------------------------------------------------
# Función para leer y combinar hojas de un archivo
leer_y_combinar <- function(archivo) {
  hojas <- excel_sheets(archivo)
  hojas_filtradas <- hojas[!hojas %in% c("Comercial", "Datos Lance")]
  datos <- lapply(hojas_filtradas, function(hoja) {
    read_excel(archivo, sheet = hoja) %>% 
      mutate(hoja = hoja) # Agrega el nombre de la hoja como columna
  }) %>% 
    bind_rows()
  
  return(datos)
}


## ---------------------------------------------------------------------------------------------------------
# Leo todos los archivos de Chirla
# Lista nombrada con las rutas de los archivos
archivos <- list(
  julio  = "DATA/Chirla 25_07_2024.xlsx",
  agosto = "DATA/Chirla 09_08_2024.xlsx",
  septiembre = "DATA/Chirla 16_09_2024.xlsx",
  octubre = "DATA/Chirla 23_10_2024.xlsx",
  noviembre = "DATA/Chirla 13_11_2024.xlsx",
  diciembre = "DATA/Chirla 10_12_2024.xlsx",
  enero = "DATA/Chirla 13_01_2025.xlsx",
  febrero = "DATA/Chirla 10_02_2025.xlsx"
  # marzo hasta junio
)
# Leer cada archivo y almacenarlo en una lista
datos_lista <- lapply(archivos, leer_y_combinar)
# Unir todos los datos en un solo data frame, agregando la columna "mes"
datos_totales <- bind_rows(datos_lista)
# Verificar resultado
glimpse(datos_totales)


## ---------------------------------------------------------------------------------------------------------
colSums(is.na(datos_totales))


## ---------------------------------------------------------------------------------------------------------
archivosco <- list(
  julio  = "DATA/Coquina 24_07_2024.xlsx",
  agosto = "DATA/Coquina 20_08_2024.xlsx",
  septiembre = "DATA/Coquina 20_09_2024.xlsx",
  octubre = "DATA/Coquina 18_10_2024.xlsx",
  noviembre = "DATA/Coquina 19_11_2024.xlsx",
  diciembre = "DATA/Coquina 18_12_2024.xlsx",
  enero = "DATA/Coquina 16_01_2025.xlsx",
  febrero = "DATA/Coquina 13_02_2025.xlsx"
  # marzo hasta junio
)
# Leer cada archivo y almacenarlo en una lista
datos_listaco <- lapply(archivosco, 
                        leer_y_combinar)
# Unir todos los datos en un solo data frame, agregando la columna "mes"
datos_totalesco <- bind_rows(datos_listaco)
# Verificar resultado
glimpse(datos_totalesco)


## ---------------------------------------------------------------------------------------------------------
# Verifica los cambios
tail(datos_totalesco)
dim(datos_totalesco)


## ---------------------------------------------------------------------------------------------------------
chplot <- ggplot(datos_totales %>%
                   mutate(ANO = year(FECHA),
                          MES = month(FECHA),
                          DIA = day(FECHA))) +
  geom_bar(aes(x = reorder(hoja, LONG), 
               fill = hoja), stat = "count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   size = 9),
        legend.position = "none") +
  facet_wrap(. ~ MES, ncol = 1) +
  #scale_fill_see_d(palette = "contrast") +
  labs(x = "") +
  ggtitle('registros RECLAM Chirla')


coplot_c <- ggplot(datos_totalesco %>%
                   mutate(ANO = year(Fecha),
                          MES = month(Fecha),
                          DIA = day(Fecha))) +
  geom_bar(aes(x = reorder(hoja, Longitud), 
               fill=hoja), stat = "count") +
  theme_bw()+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 9),
         legend.position = "none")+
  facet_wrap(.~MES, ncol=1)+
  #scale_fill_see_d(palette = "contrast")+
  labs(x="")+
  ggtitle('registros RECLAM Coquina')


ggarrange(chplot,
          coplot_c,
          ncol=2)


## ---------------------------------------------------------------------------------------------------------
chhist <- ggplot(datos_totales %>%
                     mutate(ANO = year(FECHA),
                            MES = month(FECHA),
                            DIA = day(FECHA))
                    )+
  geom_histogram(aes(x=LONG), bins = 60,
                 fill=NA,
                 col=1) +
  theme_bw()+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 9),
         legend.position = "none")+
  facet_wrap(.~MES, ncol=1)+
  labs(x="")+
  ggtitle('RECLAM Chirla')

cohist<- ggplot(datos_totalesco %>%
                     mutate(ANO = year(Fecha),
                            MES = month(Fecha),
                            DIA = day(Fecha))
                    )+
  geom_histogram(aes(x=Longitud), bins = 60,
                 fill=NA,
                 col=2) +
  theme_bw()+
   theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 9),
         legend.position = "none")+
  facet_wrap(.~MES, ncol=1)+
  labs(x="")+
  ggtitle('RECLAM Coquina')


ggarrange(chhist,
          cohist,
          ncol=2)


## ---------------------------------------------------------------------------------------------------------
tp_coquina <- read_excel(here("DATA",
                              "Muestreo 200 ejemplares.xlsx"), 
    sheet = "coquina") %>% 
  mutate(specie = "CQ")
tp_chirla <- read_excel(here("DATA",
                              "Muestreo 200 ejemplares.xlsx"), 
    sheet = "chirla") %>%
  mutate(specie = "CH")

tpcqch <- rbind(tp_chirla,
                tp_coquina)

#Crear el gráfico con los datos originales y la línea de regresión ajustada
TP <- ggplot(tpcqch) +
  geom_point(aes(Longitud, Peso, color = specie)) +
  geom_smooth(aes(Longitud, Peso),
              method = "gam",
              color = "black",
              se = TRUE) +
  facet_wrap(.~specie, scale = "free_y") +
  theme_few()

TP


## ---------------------------------------------------------------------------------------------------------
# Transformación logarítmica de Longitud y Peso
tpcqch_log <- tp_coquina %>%
  mutate(log_Longitud = log(Longitud),
         log_Peso = log(Peso))

# Ajustar un modelo lineal para todas las especies (combinadas)
modelo_completo <- lm(log_Peso ~ log_Longitud , data = tpcqch_log )

# Realizar el ANOVA para evaluar si las pendientes difieren entre especies
anova_resultados <- anova(modelo_completo)

# Ver los resultados del ANOVA
print(anova_resultados)

# Ver los coeficientes del modelo
coef(modelo_completo)

# Extraer los coeficientes
intercepto_log <- coef(modelo_completo)["(Intercept)"]  # Intercepto en log
pendiente_log <- coef(modelo_completo)["log(Longitud)"]  # Pendiente en log

# Calcular el intercepto en el espacio original (exponencial del intercepto logarítmico)
intercepto_absoluto <- exp(intercepto_log)

# Mostrar los resultados
cat("Intercepto (log):", intercepto_log, "\n")
cat("Pendiente (log):", pendiente_log, "\n")
cat("Intercepto (absoluto):", intercepto_absoluto, "\n")

# Crear el gráfico con los datos originales y la línea de regresión ajustada
TP <- ggplot(tp_coquina) +
  geom_point(aes(Longitud, Peso, color = specie)) +
  geom_smooth(aes(Longitud, Peso),
              method = "gam",
              color = "black",
              se = TRUE) +
  facet_wrap(.~specie, scale = "free_y") +
  theme_bw()

TP

# step-2
plot(parameters(modelo_completo))

# step-3

check <- check_normality(modelo_completo)
plot(check, 
     type="qq")


results <- summary(correlation(tpcqch_log))
plot(results, show_data = "points")


## ---------------------------------------------------------------------------------------------------------
coord_ch <- read_excel(here("DATA",
                    "Chirla 09_08_2024.xlsx"),
                    sheet = "Datos lance") %>% 
  dplyr::select(1, 2,3) %>% 
  rename(ZONA = Punto,
         Latitud =`Latitud inicio`,
         Longitud = `Longitud inicio`)

coord_co <- read_excel(here("DATA",
                    "Coquina 20_08_2024.xlsx"),
                    sheet = "Datos Lance") %>% 
  dplyr::select(c(2,4,5)) %>% 
  rename(ZONA = Réplica)

coord_bo <- rbind(coord_ch,
                  coord_co)


## ---------------------------------------------------------------------------------------------------------
total_c_geo <- merge(total_c, 
                     coord_bo,
                     by="ZONA")
head(total_c_geo)


## ---------------------------------------------------------------------------------------------------------
costandalucia <- st_read("~/IEO/IN_BENTOS/SHP_Chirla/costa_proyectada.shp") %>% 
  st_transform("+init=epsg:4326")
grilla <- st_read("~/IEO/IN_BENTOS/SHP_Chirla/cuadrกculas_definitivo.shp") %>% 
  st_transform("+init=epsg:4326")


## ---------------------------------------------------------------------------------------------------------
# Crear un data frame con las coordenadas y un atributo para el tamaño
puntos <- data.frame(
  lon = c(-7.2, -7.0, -6.8, -6.5),
  lat = c(36.8, 36.9, 37.1, 37.2),
  Longitud = c(10, 25, 50, 100)  # atributo para definir el tamaño
)

ggplot() +
  geom_point(data = puntos, 
             aes(x = lon,
                 y = lat, 
                 size = Longitud), 
             color = "blue", 
             alpha = 0.7) +
  geom_sf(data = costandalucia, 
          fill = "#fee8c8") + 
  geom_sf(data= grilla,
          fill=NA, col="red")+
  theme_few() +
  xlab(expression(paste(Longitude^o, ~'O'))) +
  ylab(expression(paste(Latitude^o, ~'S'))) +
  guides(size = guide_legend(title = "Talla Promedio (mm)")) +  # Título de la leyenda
  scale_size_continuous(range = c(3, 10)) +  # Ajuste del tamaño de los puntos
  theme(legend.position = "right") +  # Posición de la leyenda
  xlim(-7.6, -6.3) +
  ylim(36.65, 37.3)


