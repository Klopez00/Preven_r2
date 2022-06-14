

#descargar una tabla de access------------------------------------------------------------
#lo estoy sacando con Rcloud

#install.packages("pacman")
library(pacman)

pacman:: p_load(tidyverse,
                googlesheets4,           #leer pase de guardia
                gtsummary,               #hacer tablas
                lubridate,               #editar fechas
                flextable,               #edicion de tablas
                ggplot2,
                readxl,
                devtools,                #para editar graficos
                writexl,                 #exportar excel
                janitor,                 #limpieza de datos avanzada
                readxlsb,                #archivos de la torre
                openxlsx,
                skimr,
                rio,
                purrr,
                RODBC   
)

setwd("S:/Medicina Preventiva/LÓPEZ-GURIDI-KOLDO/R/Informes/Informe_centinela")

#CONSEGUIR LA TABLA DE CENTINELA-----------------------------------------------------

con <- odbcConnectAccess("//wx31166medpr010.elkarlan.euskadi.eus/Centinela/CENTINela.MDB")

table_names <- sqlTables(con) %>% select(TABLE_NAME)


vigilancia_tables <- table_names %>% 
  filter(grepl("koldo", TABLE_NAME)) %>% 
  unlist()

centinela <- vigilancia_tables %>% 
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>% 
  reduce(left_join, by="GlobalRecordID") 

centi <- centinela %>% 
  as_tibble() %>% 
  clean_names() %>% 
  arrange(cic)


#Y DESCARGAR---------------------------------------------------------------------

write_xlsx(centi, path = "koldo.xlsx")
