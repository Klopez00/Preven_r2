#library(pacman)
library(tidyverse)
library(gtsummary)
library(lubridate)
#library(flextable)
#library(ggplot2)
#library(readxl)
library(writexl)
library(janitor)
#library(readxlsb)
#library(openxlsx)
library(skimr)
library(rio)
library(RODBC)
library(gt)
#library(ggpubr)
#library(kableExtra)
#library(knitr)
#library(devtools)
library(ggpubr)
library(purrr)
library(scales)
#library(tidyquant)


'%ni%' <- Negate("%in%")

gt_tabla <- function(tbl) {
  tbl %>% 
    gt() %>% 
    #añadir bordes
    tab_style(
      style =  cell_borders(
        sides = c("left", "right", "top", "bottom"),
        color = "black",
        style = "solid"),
      locations = cells_body(
        columns = everything())) %>% 
    #enfatizar las titulos
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold"))) %>% 
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>% 
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    )
}


plantas_hospitalizacion <-  c("12",
                              "1A", "1B", "1C", "1D", "1E",
                              "2A", "2B", "2C", "2D", "2E", 
                              "3A", "3B", "3C", "3D", "3E", 
                              "4A", "4B", "4C", "4D", "4E") 


localizacion_funcion <-  function(tbl, col, output_col){
  
  mutate(tbl, 
         !!output_col := case_when(
           
           {{col}} ==  1	~ "INF.LUGAR QUIR.INCISIONAL SUPERFICIAL",
           {{col}} ==  2 ~	"INF.LUGAR QUIR.INCISIONAL PROFUNDA",
           {{col}} ==  3 ~	"INF.LUGAR QUIR.VISCERAL LOCAL",
           {{col}} ==  4	~ "URINARIA",
           {{col}} ==  5	~ "NEUMONIA",
           {{col}} ==  6	~ "INF. RESPIRATORIA INFERIOR",
           {{col}} ==  7	~ "FLEBITIS",
           {{col}} ==  8 ~	"BACTERIEMIA PRIMARIA",
           {{col}} ==  9	~ "BACTERIEMIA SECUNDARIA",
           {{col}} ==  10 ~	"BACTERIEMIA ASOCIADA A CATETER",
           {{col}} ==  11 ~	"OTROS",
           {{col}} ==  12 ~ "ESPUTOS",
           {{col}} ==  13 ~	"LAVADO BRONQUIOALVEOLAR",
           {{col}} ==  14 ~	"HERIDA",
           {{col}} ==  15 ~	"ESCARA",
           {{col}} ==  16 ~	"HEMOCULTIVO",
           {{col}} ==  17 ~	"UROCULTIVO",
           {{col}} ==  18 ~	"CULTIVO DE L.C.R.",
           {{col}} ==  19 ~	"CULTIVO DE LÍQUIDO ASCÍTICO",
           {{col}} ==  20 ~	"DRENAJE",
           {{col}} ==  21 ~	"CULTIVO DE CATETER",
           {{col}} ==  22 ~	"BIOPSIA PULMONAR",
           {{col}} ==  23 ~	"BIOPSIA",
           {{col}} ==  24 ~	"ABSCESO",
           {{col}} ==  25 ~	"BRONCOASPIRADO",
           {{col}} ==  26 ~	"NASAL",
           {{col}} ==  27 ~	"LESIÓN PIEL-MUCOSAS",
           {{col}} ==  28 ~	"QUEMADURA",
           {{col}} ==  29 ~	"CEPILLADO BRONQUIAL",
           {{col}} ==  30 ~	"OIDO DERECHO",
           {{col}} ==  31 ~	"OIDO IZQUIERDO",
           {{col}} ==   32 ~	"OIDO DERECHO E IZQUIERDO",
           {{col}} ==   33 ~	"EXUDADO FARINGOAMIGDALAR",
           {{col}} ==   34 ~	"LIQUIDO PERITONEAL",
           {{col}} ==   35 ~	"OSTEOARTICULAR",
           {{col}} ==   36 ~	"BILIAR",
           {{col}} ==   37 ~	"CONJUNTIVA",
           {{col}} ==   38 ~	"VALVULA",
           {{col}} ==   39 ~	"INSERCION VIA",
           {{col}} ==   40 ~	"ULCERA",
           {{col}} ==   41 ~	"NASAL Y FARINGEO",
           {{col}} ==   42 ~	"NASAL,FARINGEO Y RECTAL",
           {{col}} ==   43 ~	"NASAL Y RECTAL",
           {{col}} ==   44 ~	"FARINGEO Y RECTAL",
           {{col}} ==   45 ~	"EXUDADO URETRAL",
           {{col}} ==   46 ~	"COPROCULTIVO",
           {{col}} ==   47 ~	"EXUDADO TRAQUEOTOMIA",
           {{col}} ==   48 ~ "AXILAR",
           {{col}} ==   49 ~	"INGUINAL",
           {{col}} ==   50 ~	"LAVADO NASOFARINGEO",
           {{col}} ==   51 ~	"EXUDADO RECTAL",
           {{col}} ==   52 ~	"CABEZA",
           {{col}} ==   53 ~	"PUBIS",
           {{col}} ==   54 ~	"CABEZA Y PUBIS",
           {{col}} ==   55 ~	"PROTESIS DE RODILLA",
           {{col}} ==   56 ~	"NASAL (Positivo)",
           {{col}} ==   57 ~	"NASAL (Negativo)",
           {{col}} ==   58 ~	"PERINEAL",
           {{col}} ==   59 ~	"FARINGEO",
           {{col}} ==   60 ~	"PROTESIS DE CADERA",
           {{col}} ==   61 ~	"UMBILICAL",
           {{col}} ==   62 ~	"SALIVA",
           {{col}} ==   70 ~	"RECTAL",
           {{col}} ==   75 ~	"EXUDADO VAGINAL",
           {{col}} ==   79 ~	"LIQUIDO PLEURAL",
           {{col}} ==   85 ~	"AXILAR-INGUINAL",
           {{col}} ==   90 ~	"FARINGEO,AXILAR E INGUINAL",
           {{col}} ==   100 ~	"ASPIRADO ENDOTRAQUEAL",
           {{col}} ==   200	~"SANGRE",
           {{col}} ==   262	~"RESERVORIO",
           {{col}} ==   263	~"FARINGEO-AXILAR",
           {{col}} ==   280	~"INF.RESPIRATORIA",
           {{col}} ==  281	~"CONJUNTIVA Y NASAL",
           {{col}} ==  282	~"SEROLOGÍA",
           TRUE ~ NA_character_))               
  
}



#CARGAR DATOS------------------------------------------------------------------------------------

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS")

con <- odbcConnectAccess("S:/Medicina Preventiva/CENTINELA/CENTINela.MDB")

table_names <- sqlTables(con) %>% select(TABLE_NAME)


vigilancia_tables <- table_names %>% 
  filter(grepl("kold_admision", TABLE_NAME)) %>% 
  unlist()

data <- vigilancia_tables %>% 
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>% 
  reduce(left_join, by = "GlobalRecordID") %>% 
  clean_names() %>% 
  as_tibble() 


#---------------------------


admision <- data %>% 
  filter(is.na(fecha_final_aisl)) %>% 
  filter(is.na(fecha_de_alta)) %>% 
  filter(aislamiento == 1) %>% 
  mutate( 
    id_habitacion_copia = id_habitacion,
    id_habitacion = gsub(pattern = "-", replacement = ".", id_habitacion, fixed = TRUE),
    id_habitacion = gsub(pattern = ",", replacement = ".", id_habitacion, fixed = TRUE),
    
    
    #las Ts son 33
    id_habitacion = gsub(pattern = "N", replacement = "22", id_habitacion, fixed = TRUE),
    id_habitacion = gsub(pattern = "C", replacement = "12", id_habitacion, fixed = TRUE),
    id_habitacion = gsub(pattern = "T", replacement = "33", id_habitacion, fixed = TRUE),
    id_habitacion = gsub(pattern = "M", replacement = "11", id_habitacion, fixed = TRUE),
    id_habitacion = gsub(pattern = "H", replacement = "14", id_habitacion, fixed = TRUE),
    
    #esto para las camas de la 650
    id_habitacion = gsub(pattern = "A", replacement = "12", id_habitacion, fixed = TRUE),
    id_habitacion = gsub(pattern = "B", replacement = "12", id_habitacion, fixed = TRUE),
    
    #y aqui ya lo convierto en numerico
    id_habitacion_planta = id_habitacion,
    id_habitacion_planta = if_else( id_habitacion_planta == "68012", "680", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68112", "681", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68212", "682", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68312", "683", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68412", "684", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68512", "685", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68612", "686", id_habitacion_planta),
    id_habitacion_planta = if_else( id_habitacion_planta == "68712", "687", id_habitacion_planta),
    
    id_habitacion_planta = if_else( id_habitacion_planta == "65412", "654", id_habitacion_planta),
    
    
    id_habitacion_planta = if_else( id_habitacion_planta == "67612", "676", id_habitacion_planta)) %>% 
  
  #id_habitacion_planta = if_else(id_habitacion_planta > "70000", "0" , id_habitacion_planta)) %>% 
  
  
  #nombres de plantas
  mutate(
    cama = as.numeric(id_habitacion_planta),
    planta = case_when(
      cama > 99  & cama <125 ~ "1A",
      #las Ms
      cama > 1099  & cama <1150 ~ "1A",
      cama > 124 & cama <150 ~ "1B",
      #las Hs
      cama > 1399  & cama <1499 ~ "1A",
      cama > 149 & cama <175 ~ "1D",
      cama > 174 & cama <200 ~ "1E",
      cama > 199 & cama <225 ~ "2A",
      #las Ns
      cama > 2199  & cama <2250 ~ "2A",
      cama > 224 & cama <250 ~ "2B",
      cama > 249 & cama <275 ~ "2D",
      cama > 274 & cama <300 ~ "2E",
      cama > 299 & cama <325 ~ "3A",
      #las Ts
      cama > 3299  & cama <3350 ~ "3A",
      cama > 324 & cama <350 ~ "3B",
      cama > 349 & cama <375 ~ "3D",
      cama > 374 & cama <400 ~ "3E",
      cama > 399 & cama <425 ~ "4A",
      cama > 424 & cama <450 ~ "4B",
      cama > 449 & cama <475 ~ "4D",
      cama > 474 & cama <500 ~ "4E",
      
      
      cama > 499 & cama <512 ~ "Pasillo",
      cama > 511 & cama <517 ~ "GQx",
      cama > 520 & cama <535 ~ "UCI",
      
      cama > 534 & cama <549 ~ "Coro",
      #cama > 545 & cama <549 ~ "UCI",
      cama > 550 & cama <557 ~ "UCI-marti",
      cama > 549 & cama <575 ~ "5D",
      cama > 574 & cama <600 ~ "5E",
      
      
      cama > 600 & cama <614 ~ "R1",
      cama > 613 & cama <625 ~ "R2",
      cama > 633 & cama <642 ~ "RA",
      
      cama > 624 & cama <650 ~ "6B",
      cama > 649 & cama <675 ~ "6D",
      cama > 674 & cama <700 ~ "6E",
      cama > 699 & cama <750 ~ "7",
      cama > 1199 & cama <1250 ~ "12"),
    
    id_habitacion = id_habitacion_copia) %>% 
  
  mutate(
    today = today(),
    fecha_inicio_aisl = as.Date(fecha_inicio_aisl) + 1,
    nuevos = if_else(fecha_inicio_aisl == today, 1, 0)) %>% 
  
  
  localizacion_funcion(id_localizacion, "localiza1")  %>% 
  localizacion_funcion(idlocaliza2, "localiza2") %>% 
  localizacion_funcion(idlocaliza3, "localiza3") %>% 
  mutate(localiza1 = if_else(is.na(localiza1), "", localiza1),
         localiza2 = if_else(is.na(localiza2), "", localiza2),
         localiza3 = if_else(is.na(localiza3), "", localiza3))



admision_admision <- admision %>% 
  #aqui genero una nueva categoria------------------------------------------------------------------------------------------------
select(planta, cic, germen, precauciones, id_habitacion, nuevos, fecha_inicio_aisl) %>% 
  mutate(compartir = case_when(
    germen == "CORONAVIRUS SARS-Cov-2" ~ "Puede compartir habitación con otro covid",
    germen == "CORONAVIRUS CICLOS TARDÍOS" ~ "Preferentemente mantener en habitación individual hasta resultado de nueva PCR",
    germen == "CORONAVIRUS CONTACTO ESTRECHO" ~ "Mantener en habitación individual",
    TRUE ~ " ")) %>% 
  arrange(planta, id_habitacion)

#aqui introduZco el ver si tiene más de uno
covid_repetidos <- get_dupes(admision_admision, id_habitacion) %>% 
  filter(germen == "CORONAVIRUS SARS-Cov-2")


admision_admision <- admision_admision %>% 
  mutate(compartir = case_when(
    cic %ni% covid_repetidos$cic & compartir == "Puede compartir habitación con otro covid" ~ "Puede compartir habitación con otro covid",
    cic %in% covid_repetidos$cic & compartir == "Puede compartir habitación con otro covid" ~ "Habitación individual",
    TRUE ~ as.character(compartir))) %>% 
  group_by(id_habitacion)








#TABLA PARA ADMISION-----------------------------------

admision_tabla <- admision_admision %>% 
  
  gt_tabla() %>%
  
  #CONDICIONAL NUEVOS
  tab_style(
    style = list(
      cell_fill(color = "#F9E3D6"),
      cell_text(weight =  "bold")
    ),
    locations = cells_body(
      columns = nuevos,
      rows = nuevos > 0)
  ) %>% 
  
  tab_header(
    title = "Aislamientos en el Hospital de Cruces",
    subtitle = paste("En caso de no estar especificado, habitación individual", Sys.Date())
  ) 

#TABLA PARA ENFERMERIA--------------------------------

admision_enfermeria <- admision %>% 
  mutate(nombre = paste0(nombre, " ", apellidos)) %>% 
  select(planta, nombre, cic, germen, id_habitacion, fecha_inicio_aisl, localiza1:localiza3) %>% 
  mutate( 
    fin_aislamiento = "",
    alerta = " ",
  ) %>% 
  arrange(planta, id_habitacion) %>%
  select(id_habitacion, planta:germen, fecha_inicio_aisl:alerta)




admision_tabla_enfermeria_1 <- admision_enfermeria %>%
  filter(planta %in% plantas_hospitalizacion) %>% 
  group_by(cic, nombre) %>% 
  gt_tabla() %>% 
  
  tab_header(
    title = "Aislamientos en el Hospital de Cruces",
    subtitle = paste("Hospitalización", Sys.Date())
  )  

admision_tabla_enfermeria_2 <- admision_enfermeria %>%
  filter(planta %ni% plantas_hospitalizacion) %>% 
  group_by(cic, nombre) %>% 
  gt_tabla() %>% 
  
  tab_header(
    title = "Aislamientos en el Hospital de Cruces",
    subtitle = paste("Críticos", Sys.Date())
  )   

admision_tabla_enfermeria_3 <- admision_enfermeria %>%
  group_by(cic, nombre) %>% 
  gt_tabla() %>% 
  
  tab_header(
    title = "Aislamientos en el Hospital de Cruces, correción de cambios",
    subtitle = Sys.Date())




#------------------------------------------------------------------

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO/admision")

admision_tabla %>% 
  gtsave(., paste(Sys.Date(),"lista_admision.html")) 


admision_tabla_enfermeria_1 %>% 
  gtsave(., paste(Sys.Date(),"lista_enfermeria_HOSPITALIZACION.html"))

admision_tabla_enfermeria_2 %>% 
  gtsave(., paste(Sys.Date(),"lista_enfermeria_CRITICOS.html"))



admision_tabla_enfermeria_3 %>% 
  gtsave(., paste(Sys.Date(),"lista_enfermeria_TODO-CORRECIONES.html"))

#INDICADORES-------------------------------

q1 <- admision_admision %>% 
  ungroup() %>% 
  count() %>% 
  mutate(
    indicador = "diario_aislamientos",
    fecha = Sys.Date(),
    germen = "todos"
  )

q2 <- admision_admision %>% 
  ungroup() %>% 
  count(germen) %>% 
  mutate(
    indicador = "diario_microorganismo",
    fecha = Sys.Date()
  )

q3 <- admision_admision %>% 
  ungroup() %>%
  distinct(cic) %>% 
  count() %>% 
  mutate(
    indicador = "diario_pacientes",
    fecha = Sys.Date(),
    germen = "todos"
  )

#criticos----------

q1c <- admision_admision %>% 
  ungroup() %>%
  filter(planta %ni% plantas_hospitalizacion) %>% 
  count() %>% 
  mutate(
    indicador = "diario_aislamientos_criticos",
    fecha = Sys.Date(),
    germen = "todos"
  )

q2c <- admision_admision %>% 
  ungroup() %>% 
  filter(planta %ni% plantas_hospitalizacion) %>% 
  count(germen) %>% 
  mutate(
    indicador = "diario_microorganismo_criticos",
    fecha = Sys.Date())

q3c <- admision_admision %>% 
  ungroup() %>%
  filter(planta %ni% plantas_hospitalizacion) %>% 
  distinct(cic) %>% 
  count() %>% 
  mutate(
    indicador = "diario_pacientes_criticos",
    fecha = Sys.Date(),
    germen = "todos"
  )       

#no-criticos----------------------------


q1p <- admision_admision %>% 
  ungroup() %>%
  filter(planta %ni% plantas_hospitalizacion) %>% 
  count() %>% 
  mutate(
    indicador = "diario_aislamientos_planta",
    fecha = Sys.Date(),
    germen = "todos"
  )

q2p <- admision_admision %>% 
  ungroup() %>% 
  filter(planta %ni% plantas_hospitalizacion) %>% 
  count(germen) %>% 
  mutate(
    indicador = "diario_microorganismo_planta",
    fecha = Sys.Date())

q3p <- admision_admision %>% 
  ungroup() %>%
  filter(planta %ni% plantas_hospitalizacion) %>% 
  distinct(cic) %>% 
  count() %>% 
  mutate(
    indicador = "diario_pacientes_planta",
    fecha = Sys.Date(),
    germen = "todos"
  ) 

#-----------------------

importar_admision <- bind_rows(list(q1, q2, q3,
                           q1c, q2c, q3c,
                           q1p, q2p, q3p)) %>% 
  mutate_all(., str_to_lower) %>% 
  mutate(n = as.numeric(n),
         fecha = as.Date(fecha))

import("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO/admision/indicadores/indicadores_admision.xlsx") %>% 
  as_tibble() %>% 
  bind_rows(importar_admision) %>% 
  distinct() %>% 
  write_xlsx(., "S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO/admision/indicadores/indicadores_admision.xlsx")



#fin----------------

RODBC::odbcClose(con)
