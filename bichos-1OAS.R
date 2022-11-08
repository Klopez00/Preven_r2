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



#CONSEGUIR LA TABLA DE CENTINELA-----------------------------------------------------

con <- odbcConnectAccess("S:/Medicina Preventiva/CENTINELA/CENTINela.MDB")

table_names <- sqlTables(con) %>% select(TABLE_NAME)


vigilancia_tables <- table_names %>% 
  filter(grepl("koldo", TABLE_NAME)) %>% 
  unlist()

centinela <- vigilancia_tables %>% 
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>% 
  reduce(left_join, by = "GlobalRecordID") 

centi <- centinela %>% 
  as_tibble() %>% 
  clean_names() %>% 
  arrange(cic)

print("Centinela cargado.")


#DESCARGAR LOS DATOS DIARIOS-----------------------------------------------------


setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO")


excel_1 <- list.files(pattern = "MIKRO.*.xlsx") %>% 
  read_excel()

excel_2 <- list.files(pattern = "CONSULTA.*.xlsx") %>% 
  read_excel()

excel_3 <- list.files(pattern = ".*.csv") %>% 
  read.csv(header = TRUE, sep = "\t", fileEncoding = "utf16")



# limpieza de datos
excel_11 <- excel_1 %>%
  clean_names() %>% 
  mutate(cic = as.numeric(cic))

excel_22 <- excel_2 %>%
  clean_names() %>%
  rename(
    microorganismo = prueba,
    marcador_resistencia = resultado
  )%>% 
  mutate(cic = as.numeric(cic))

excel_33 <- excel_3 %>%
  as_tibble() %>% 
  clean_names() %>% 
  rename(cama = cod_unico_pto_atencion_pto_at) %>% 
  select(cic, fecha_ingreso, servicio_hosp, cama) %>% 
  extract(col = cic, 
          into = "cic",
          regex = "(.*),.*") %>% 
  mutate(
    cama = gsub(pattern = "(^.*)(\\w{1}$)", "\\1\\.\\2", cama), 
    cic = as.numeric(cic)) 

print("Cargando...")

#autoguardado de trazabilidad

write_xlsx(excel_3,
           path = paste0("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS-TRAZABILIDAD/", Sys.Date(), ".xlsx"))


# joins
excel_1122 <- rbind(excel_11,excel_22)


excel_1234 <- left_join(excel_1122, excel_33, by = "cic",  all.x=TRUE) %>%
  rename(nombre = nombre_paciente,
         apellidos = apellidos_paciente) 




# RECUPERACION DE CICS PERDIDOS--------------------------------------------------------------

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS")

cic_perdidos <- read_excel("cic_clave-principal.xlsx") %>%
  #select(-cic) %>%                      #la segunda vez que corri esto tuve que eliminar la columna de cic
  #clean_names() %>%                     #he bloqueado todas estas filas porque las necesite cuando hice la extraccion
  #mutate(cic_inventado = cic) %>%
  #rename(cic_real = cic) %>%
  #filter(is.na(cic_inventado)) %>%
  #arrange(no_ha_ca) %>%
  #distinct() %>%
  #mutate(cic_real = row_number(), cic_inventado = row_number()) %>%
  #rename(fecha_nacimiento = fecha_de_nacimiento) %>%
  mutate(across(starts_with("cic"), as.numeric)) 


excel_union_1 <- excel_1234 %>%
  select(c(nombre,apellidos,cic,fecha_nacimiento)) %>%
  distinct() 


# union para recuperar cic-s
union_1 <- left_join(cic_perdidos, excel_union_1, by =c("nombre","apellidos","fecha_nacimiento"),all.x=TRUE) %>%
  mutate(    cic_real = if_else(
    is.na(cic), 
    cic_real,
    cic)) %>%
  
  select(-cic) %>%
  write.xlsx(file = "cic_clave-principal.xlsx")


# union para pasar al documento de hoy los cic-s falsos
union_2 <- left_join(excel_1234,cic_perdidos, by=c("nombre","apellidos","fecha_nacimiento"),all.x=TRUE) %>%
  mutate(
    cic = if_else(
      is.na(cic_inventado),
      cic,
      cic_inventado)) 

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO")

latorre <-  union_2 %>%
  rename(
    cama_micro = habitacion,
    ue = servicio,
    #servicio_micro = seccion_hosp,
    #habitacion = pto_atencion_pto_at
  ) %>%
  mutate(
    orden = case_when(
      !is.na(fecha_ingreso) ~ 0,
      is.na(fecha_ingreso) & ue == "URGENCIAS GENERALES (URG)" ~ 1,
      is.na(fecha_ingreso) & ue != "URGENCIAS GENERALES (URG)" ~ 2)) %>% 
  arrange(orden, microorganismo, marcador_resistencia, nombre) %>%
  filter(microorganismo != "Coronavirus SARS-CoV-2 (ARN)") %>% 
  
  mutate(
    # genero las variables "manuales"
    access = NA,
    precauciones = NA,
    tipo_nosocomial = NA,
    #aislamiento = NA,
    #volcado = NA,
    
    #editar las fechas
    across(.cols = where(is.POSIXct), .fns = as.Date)) %>% 
  select(-c(cic_real, cic_inventado, peticion, fecha_validacion, fecha_resultado))


#CONSULTA EN CENTINELA-----------------------------------------------------------------

#mirar si esta el cic
latorre$access <- latorre$cic %in% c(centi$cic)

#crear un nuevo dataframe con solo los ultimos ingresos
centi_ingreso <- centi %>% 
  arrange(desc(fecha_de_ingreso)) %>% 
  distinct(cic, germen, .keep_all = TRUE) %>% 
  unite(new,cic,fecha_de_ingreso)

#preparar una nueva variable en latorre
latorre <- latorre %>% mutate(
  cic2 = as.character(cic),
  fecha_ingreso2 = as.character(fecha_ingreso)) %>% 
  unite(new,cic2,fecha_ingreso2)

#preguntarle si ese cic con esa fecha esta
latorre$precauciones <- latorre$new %in% centi_ingreso$new

#juntado con si estan aislados y sin final de aislamiento
centi_aislados <- centi_ingreso %>%
  filter(is.na(fecha_final_aisl)) %>% 
  filter(aislamiento == "1") %>% 
  unite(new2, new, aislamiento) %>% 
  
  
  select(c(new2,germen)) %>% 
  mutate(germen_values = germen) %>% 
  pivot_wider(names_from = germen, values_from = germen_values) %>% 
  clean_names() %>% 
  unite(aislamientos, -"new2", sep = ", ", remove = T, na.rm = T)

latorre <- latorre %>% mutate(
  x = "1",
  cic2 = as.character(cic),
  fecha_ingreso2 = as.character(fecha_ingreso)) %>% 
  unite(new2 ,cic2, fecha_ingreso2, x)

latorre$tipo_nosocomial <- latorre$new2 %in% centi_aislados$new2

latorre <- left_join(latorre, centi_aislados, by = "new2", all.X = T)

latorre <- latorre %>% 
  mutate(
    access = case_when(
      access == "FALSE" ~ "El cic NO ESTA en centinela",
      access == "TRUE" & precauciones == "FALSE" ~ "Esta en centi, pero es ingreso NUEVO",
      access == "TRUE" & precauciones == "TRUE" & tipo_nosocomial == "TRUE" ~ as.character(aislamientos),
      access == "TRUE" & precauciones == "TRUE" & tipo_nosocomial == "FALSE" ~ "En centi EN ESTE INGRESO NO tiene aislamientos activos"))


#ORDENAR VARIABLES Y ULTIMOS DETALLES-------------------------------------------------


latorre <- latorre %>% 
  mutate(fecha_ingreso = as.Date(fecha_ingreso),
         dias_ingreso = fecha - fecha_ingreso,
         edad = (Sys.Date() - fecha_nacimiento)/ 365) %>% 
  separate(col = dias_ingreso,
           into = c("dias_ingreso", "x"),
           sep = " ") %>% 
  mutate(edad = as.numeric(edad),
         edad = round(edad, 0),
         cama_micro = gsub(pattern = "(^.*)(\\w{1}$)", "\\1\\.\\2", cama_micro)) %>% 
  select(nombre,
         apellidos,
         fecha,
         muestra,
         microorganismo,
         marcador_resistencia,
         servicio_hosp,
         cama,
         access,
         cic,
         fecha_ingreso,
         dias_ingreso,
         edad,
         sexo,
         ue,
         cama_micro,
         fecha_nacimiento)  %>% 
  
  mutate(
    marcador_resistencia = if_else(marcador_resistencia == "FECHA", " ", marcador_resistencia),
    cama = if_else(cama == "FECHA", " ", cama)
  ) %>% 
  distinct(cic, microorganismo, marcador_resistencia, .keep_all = T) 


#CARGA MULTIPLE TRAZABILIDAD---------------------------------


latorre_cama <- latorre %>% 
  select(cic, cama) %>% 
  distinct(cic, cama) %>% 
  mutate(dia = as.character(Sys.Date()))

latorre <- latorre %>%  select(-cama)

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS-TRAZABILIDAD")

dias <- seq(as.Date(Sys.Date() - 30), as.Date(Sys.Date()-1), "days")

equises <- list.files(pattern = paste0(dias,  ".xlsx", collapse = "|"), full.names = F) 

names <- str_replace(equises, ".xlsx", "") %>%  unlist()

df <- tibble (count = seq_len(6)) %>% 
  mutate(cic = 99,
         cama = "1.1",
         dia = as.character(Sys.Date())) %>% 
  select(cic, cama, dia, count)



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
  bind_rows() %>% 
  filter(cic %in% latorre_cama$cic) %>% 
  select(cic, cama, dia) %>%  
  bind_rows(latorre_cama) 


equises_2 <- paso_1 %>% 
  arrange(dia) %>% 
  distinct(cic, cama, .keep_all = T) %>% 
  arrange(desc(dia)) %>% 
  drop_na(cama) %>%  
  group_by(cic) %>% 
  mutate(count = row_number(cic)) %>% 
  ungroup() %>% 
  bind_rows(df) %>%    #lo meto justo aqui, antes del pivot_wider
  mutate(dia = format(as.Date(dia), "%b-%d")) %>% 
  unite("cama", cama:dia, sep = "_") %>% 
  mutate(count = as.character(count),
         count = case_when(
           count == "1" ~ "cama",
           count == "2" ~ "cama_2",
           count == "3" ~ "cama_3",
           count == "4" ~ "cama_4",
           count == "5" ~ "cama_5",
           count == "6" ~ "cama_6",
           T ~ "extra")) %>% 
  pivot_wider(names_from = count, values_from = cama) %>% 
  select( cic,
          cama,
          cama_2,
          cama_3,
          cama_4,
          cama_5,
          cama_6)



latorre <- latorre %>% 
  left_join(equises_2, by = "cic") %>% 
  select(nombre,
         apellidos,
         fecha,
         muestra,
         microorganismo,
         marcador_resistencia,
         servicio_hosp,
         cama,
         access,
         cic,
         fecha_ingreso,
         dias_ingreso,
         edad,
         ue,
         cama_2,
         cama_3,
         cama_4,
         cama_5,
         cama_6,
         sexo,
         cama_micro,
         fecha_nacimiento)  


setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO")


#CREACION DE EXCELS------------------------------

latorre_wb <- createWorkbook()

## Add a worksheet
addWorksheet(latorre_wb, "Sheet 1")

## set col widths-heights
setColWidths(latorre_wb, "Sheet 1", cols = 1:100 , widths = 15)
setColWidths(latorre_wb, "Sheet 1", cols = 9 , widths = 23.43)
setRowHeights(latorre_wb, "Sheet 1", rows = 1:500, heights = 75)
writeData(latorre_wb, sheet = 1, x = latorre)

# header_nombres de columnas
headerStyle <- createStyle(
  fontSize = 10, fontColour = "#FFFFFF", halign = "center",
  fgFill = "#4F81BD", border = "TopBottom", borderColour = "#4F81BD", wrapText = TRUE
)
addStyle(latorre_wb, sheet = 1, headerStyle, rows = 1, cols = 1:100, gridExpand = TRUE)

# cuerpo
bodyStyle <- createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#4F81BD", wrapText = TRUE)
addStyle(latorre_wb, sheet = 1, bodyStyle, rows = 2:500, cols = 1:100, gridExpand = TRUE)

access_style <- createStyle(fontSize = 8, border = c("top", "bottom", "left", "right"), borderColour = "#4F81BD", wrapText = TRUE)
addStyle(latorre_wb, sheet = 1, access_style, rows = 2:500, cols = 9, gridExpand = TRUE)

saveWorkbook(latorre_wb, "latorre.xlsx", overwrite = TRUE)







#GENERAR INFORME MIERCOLES--------------------------------------------------------------------------

if(currentDay == "miércoles"){
  source("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informes centinela/automatizar_informe.R")
  print("HAY INFORME AUTOMATIZADO EN LA CARPETA")
  
} else if(currentDay != "miércoles") {print("HOY NO TOCA INFORME")}



#REGISTRO-DE-VIRUS-----------------


#filtrar los de interes de hoy

viruses <- excel_1122 %>% 
  mutate(
    fecha = as.Date(fecha),
    registro = Sys.Date(),
    marcador_resistencia = case_when(
      marcador_resistencia != "FECHA" ~ as.character(marcador_resistencia),
      T ~ as.character(NA))) %>% 
  filter(str_detect(microorganismo, "A[DR]N"),
         microorganismo != "Coronavirus SARS-CoV-2 (ARN)")


#importar base de virus

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/primaria/virus")

base_virus <- import("base_virus.xlsx") %>% 
  bind_rows(viruses) %>% 
  distinct(cic, microorganismo, .keep_all = T) %>% 
  filter(registro > (Sys.Date() - 10))

write_xlsx(base_virus, "base_virus.xlsx")


#señalar los que hoy nos interesan


ss <- centi %>%
  filter(is.na(fecha_final_aisl),
         aislamiento == "1",
         fecha_de_ingreso > "2022-01-01") %>% 
  select(cic, germen) %>% 
  pivot_wider(names_from = germen, values_from = germen, values_fn = list) %>%
  mutate_all(as.character) %>% 
  replace(.=="NULL", NA) %>% 
  unite(aislamientos, -"cic", sep = ", ", remove = T, na.rm = T) %>% 
  mutate(aislamientos = str_to_lower(aislamientos),
         cic = as.numeric(cic))

virus_interes <- base_virus %>% 
  filter(cic %in% excel_33$cic) %>% 
  left_join(ss, by = "cic", all.x = T) %>% 
  as_tibble() %>% 
  select(-peticion, - marcador_resistencia) %>% 
  arrange(desc(aislamientos)) %>% 
  select(aislamientos, microorganismo, fecha:muestra, servicio:registro) %>% 
  mutate(microorganismo = case_when(
    microorganismo == "Virus respiratorio sincitial (ARN) rápida" &
      fecha_nacimiento < (Sys.Date() - years(18)) ~ "VRS en mayor de edad",
    T ~ microorganismo)) %>% 
  filter(microorganismo != "VRS en mayor de edad")
  
   


#---------------------


if(dim(virus_interes)[1] == 0) {
  
  print("No hay viruses del registro ingresados")
  
}else{
  
  setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO")
  
  tabla_virus_interes <- virus_interes %>% 
    gt_tabla() %>% 
    tab_style(
      style = list(
        cell_fill(color = "#F9E3D6"),
        cell_text(style = "italic")
      ),
      locations = cells_body(
        columns = c(aislamientos:microorganismo, nombre_paciente),
        rows = is.na(aislamientos)
      )
    ) %>% 
    gtsave(., paste(Sys.Date(),"virus_ingresados.html")) 
  
  
  print("Sí hay virus.")
  
  
}



#ALERTAS PRIMARIA---------------------

#guardamos los nuevos

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO")

#filtrar los de interes de hoy

tt <- list.files(pattern = "MIKRO.*.xlsx") %>% 
  import() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(
    cic = as.numeric(cic),
    peticion = as.numeric(peticion),
    fecha_muestra = fecha,
    marcador_resistencia = case_when(
      marcador_resistencia != "FECHA" ~ as.character(marcador_resistencia),
      T ~ as.character(NA)),
    servicio = gsub(".*\\((.*)\\).*", "\\1", servicio),
    germen = microorganismo,
    nueva = paste0(germen, marcador_resistencia )) %>% 
  filter(nueva != "Escherichia coliBETALACTAMASA DE ESPECTRO EXTENDIDO.") %>% 
  filter(!is.na(marcador_resistencia),
         germen != "Staphylococcus epidermidis",
         servicio != "HOS") 

#cargar la base de primaria
setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/primaria")

primaria <- import("primaria.xlsx") %>% 
  as_tibble() %>% 
  bind_rows(tt) %>% 
  distinct(cic, microorganismo, .keep_all = T)


#filtrar las alarmas de hoy

alerta_primaria <- primaria %>% 
  #esto es lo que hay que cambiar para la prueba
  filter(cic %in% excel_33$cic)



#quitar los de la alerta de la base de primaria
primaria_2 <- primaria %>% 
  filter(cic %ni% alerta_primaria$cic)

write_xlsx(primaria_2, "primaria.xlsx")




if(dim(alerta_primaria)[1] == 0) {
  
  print("No hay alertas")
  
}else{
  
  setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/OAS_DIARIO")
  
  alerta_primaria_tabla <- alerta_primaria %>% 
    gt_tabla() %>%  
    gtsave(., paste(Sys.Date(),"alertas primaria.html")) 
  
  print("Pues ya estaría. Sí hay alertas.")
  
  
}



#copias de seguridad-------------------------


if(currentDay == "viernes") {
  
write_xlsx(primaria_2, paste0(Sys.Date(),"copia_seguridad_primaria.xlsx"))
  
setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/primaria/virus")
write_xlsx(base_virus, paste0(Sys.Date(),"copia_seguridad_base_virus.xlsx"))
  
import("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informes centinela/indicadores/indicadores.xlsx") %>% 
write_csv(., "", file = paste0("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informes centinela/indicadores/copia_seguridad", Sys.Date(),"_indicadores.xlsx"))
  
}else{
  print("acabando...")
  
}






#fin----------------

RODBC::odbcClose(con)







