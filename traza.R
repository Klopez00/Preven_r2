shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(tidyverse))
shhh(library(gtsummary))
shhh(library(lubridate))
shhh(library(flextable))
shhh(library(ggplot2))
shhh(library(readxl))
shhh(library(writexl))
shhh(library(janitor))
shhh(library(readxlsb))
shhh(library(openxlsx))
shhh(library(skimr))
shhh(library(rio))
shhh(library(purrr))
shhh(library(RODBC))
shhh(library(gt))

`%ni%` <- Negate(`%in%`)
currentDay <- weekdays(Sys.Date())


gt_tabla <- function(tbl){
  tbl %>% 
    gt()%>% 
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
    ) 
}


########################################
########################################
setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS-TRAZABILIDAD/nosocomial")
aa <- as_tibble(import("busqueda.xlsx"))
########################################
########################################


#carga multiple---------------------------------

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS-TRAZABILIDAD")

#los ultimos 15 dias
dias <- seq(as.Date(Sys.Date() - 15), as.Date(Sys.Date()), "days")

equises <- list.files(pattern = paste0(dias,  ".xlsx", collapse = "|"), full.names = F) 

names <- str_replace(equises, ".xlsx", "") %>%  unlist()

paso_1 <- equises %>%
  map(.x = ., .f = import) %>% 
  set_names(str_replace(equises, ".xlsx", "")) %>% 
  map(.x = ., .f = as_tibble) %>%
  map(.x = ., .f = clean_names) %>%
  map2(.x = ., .y = names, .f = ~ add_column(., dia = as.character(.y))) %>% 
  map(.x = ., ~ if (is.character(.$cic)) {
    .x %>% 
      extract(., col = cic, into = "cic", regex = "(.*),.*")} 
    else {.x }) %>%
  map(.x = ., ~ if ("cod_unico_pto_atencion_pto_at" %in% names(.)) {
    .x %>% 
      rename(., cama = cod_unico_pto_atencion_pto_at) %>% 
      mutate(., cama = gsub(pattern = "(^.*)(\\w{1}$)", "\\1\\.\\2", cama))} 
    else {.x }) %>%
  map(.x = ., .f = ~ mutate(., cic = as.numeric(cic))) %>% 
  map(.x = ., .f = ~ mutate(., cama_2 = cama)) %>% 
  map(.x = ., .f = ~ extract(., 
                             col = cama_2, 
                             into = "cama_3",
                             regex = "(.*)\\.")) %>%
  bind_rows() %>% 
  select(., cic, cama, cama_3, dia) %>% 
  print()

#en que camas ha estado
indice <- paso_1 %>% 
  filter(cic %in% aa$cic)

#que otros cic ha habido en esas camas
cic_ce <- paso_1 %>% 
  filter(cama_3 %in% indice$cama_3)


#me quedo con todos esos pacientes
tabla <- paso_1 %>% 
  filter(cic %in% cic_ce$cic) %>% 
  select(-cama_3) %>%  
  pivot_wider(names_from = dia, values_from = cama) %>% 
  gt_tabla() %>% 
  tab_style(style = list(cell_fill(color = 'grey90'), 
                         cell_text(weight = 'bold')), 
            locations = cells_body(
                                   #columns= 1:16, 
                                   rows = cic == aa$cic))

#######

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS-TRAZABILIDAD/nosocomial")
tabla %>% 
  gtsave(., paste(aa$cic, "_", Sys.Date(),"trazabilidad_nosocomial.html"))

