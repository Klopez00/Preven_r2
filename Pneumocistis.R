

################################
################################

##PNEUMOCISTIS

###############################
###############################


library(pacman)

pacman:: p_load(tidyverse, 
                googlesheets4, #leer pase de guardia
                gtsummary, # hacer tablas
                lubridate, # editar fechas
                flextable,  # edicion de tablas
                ggplot2, 
                readxl, 
                devtools, # para editar graficos
                writexl, # exportar excel
                janitor, # limpieza de datos avanzada
                rio)


###############

getwd()
setwd("C:/Users/kl_93/OneDrive/Escritorio/cruces/pneumocistis")
dir()

########

data <- import("data.xlsx") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  print()

data_2 <- data %>% 
  select(
    fecha, paciente, edad_anos, sexo, tipo_muestra, procedencia, servicio, centro_extraccion,
    no_sanitario, fecha_ingreso, pneumocystis_jiroveci_cuantificacion_adn, pneumocystis_jiroveci_cuantificacion_adn_resultado,
    pneumocystis_jiroveci_cuantificacion_adn_resultado_numerico, pneumocystis_jiroveci_cuantificacion_adn_comentario_resultado) %>% 
  
  # limpiar cada variable
  # 1.-fecha
  
  mutate(across(.cols = where(is.POSIXct), .fns = as.Date)) %>%
  
  
  
  # numeros
  rename(edad = edad_anos,
         cic = no_sanitario,
         resultado_numerico = pneumocystis_jiroveci_cuantificacion_adn_comentario_resultado,
         resultado_numerico2 = pneumocystis_jiroveci_cuantificacion_adn,
         
         resultado_3 =pneumocystis_jiroveci_cuantificacion_adn_resultado 
         
         ) %>% 
  
  mutate(
    edad = as.numeric(gsub(".*?([0-9]+).*", "\\1", edad)),
    cic = as.numeric(gsub(".*?([0-9]+).*", "\\1", cic)),
    resultado_numerico = as.numeric(gsub(".*?([0-9]+).*", "\\1", resultado_numerico)),
    fecha_ingreso = as.numeric(gsub(".*?([0-9]+).*", "\\1", fecha_ingreso))
  )

#########################

# INTENTANDO ENTENDER ESA SERIE Y VER SI BORRA LAS COMAS

data %>% mutate(a = "1,5")

a %>% mutate(a = as.numeric(gsub(".*?([0-9]+).*", "\\1", a)))

####################
# PARA EXTRAER

extraer <- data_2 %>% select(-c(fecha, paciente, edad, sexo, tipo_muestra, servicio, centro_extraccion, cic, procedencia)) %>% print()

extraer %>% select(-fecha_ingreso) %>% tbl_summary() %>% bold_labels() 

#######################################

data %>%
  select(pneumocystis_jiroveci_cuantificacion_adn , pneumocystis_jiroveci_cuantificacion_adn_resultado , 
         pneumocystis_jiroveci_cuantificacion_adn_resultado_numerico) %>% 
  print()   
  
  
  


view(data_2 %>% filter(resultado_numerico2 == "Positivo"))
  
