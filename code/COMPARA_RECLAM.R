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


## Leer --------------

archivo_cadiz     <- "DATA/Data_final/Template_Reclam_Cadiz.xlsx"
archivo_valencia  <- "DATA/Data_final/Template_Reclam_Valencia.xlsx"

hojas_cadiz <- excel_sheets(archivo_cadiz)
hojas_val   <- excel_sheets(archivo_valencia)

# uno dataframe de ambos excel por separado

## TALLAS ---------
# Cádiz
tallac <- read_excel(archivo_cadiz, sheet = "tallas") %>%
  select(-ZONA) %>%
  filter(!is.na(TALLA)) %>%          # elimina fila basura
  mutate(
    TALLA = as.numeric(TALLA),
    PUNTO = as.character(PUNTO),
    FECHA = as.Date(FECHA, origin = "1899-12-30"),
    Area = "Cadiz"
  )

# Valencia
tallav <- read_excel(archivo_valencia, sheet = "tallas") %>%
  mutate(
    TALLA = as.numeric(TALLA),
    FECHA = as.Date(FECHA),
    Area = "Valencia"
  )

# Unir
tallasuni <- bind_rows(tallac, tallav) 



## TALLA PESO ------------

# CÁDIZ 
tallapesoc <- read_excel(archivo_cadiz, sheet = "talla_peso") %>% 
  mutate(area = "Cadiz")

# VALENCIA 
tallapesov <- read_excel(archivo_valencia, sheet = "talla_peso") %>% 
  mutate(area = "Valencia")

tallapeso_uni <- bind_rows(tallapesoc, tallapesov)


## Captura Chirla--------------------
# CÁDIZ 
capturac <- read_excel(archivo_cadiz, sheet = "Captura_Chirla") %>% 
  mutate(area = "Cadiz") %>% 
  janitor::clean_names()

# VALENCIA 
capturav <- read_excel(archivo_valencia, sheet = "Captura_Chirla") %>% 
  mutate(area = "Valencia") %>% 
  janitor::clean_names()


captura_ch_uni <- bind_rows(capturac, capturav)


## Captura coquina --------------------

#  CÁDIZ 
captura_coq_c <- read_excel(archivo_cadiz, sheet = "Captura_coquina") %>% 
  mutate(area = "Cadiz") %>% 
  janitor::clean_names()

#  VALENCIA 
captura_coq_v <- read_excel(archivo_valencia, sheet = "Captura_coquina") %>% 
  mutate(area = "Valencia") %>% 
  janitor::clean_names()


captura_coq_c_std <- captura_coq_c %>%
  mutate(
    peso_total_con_cascajo_g = as.numeric(peso_total_con_cascajo_g),
    peso_muestreado_total_con_cascajo_g = as.numeric(peso_muestreado_total_con_cascajo_g),
    observaciones = as.character(observaciones)
  )

captura_coq_v_std <- captura_coq_v %>%
  mutate(
    peso_total_con_cascajo_g = as.numeric(peso_total_con_cascajo_g),
    peso_muestreado_total_con_cascajo_g = as.numeric(peso_muestreado_total_con_cascajo_g),
    observaciones = as.character(observaciones)
  )

captura_coq_uni <- bind_rows(captura_coq_c_std,
                             captura_coq_v_std)



