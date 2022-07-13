
#IMPORTACION DATOS ACCESS

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
                RODBC,                  #conexion con access
                ImportExport            #exportar a access
)

#CONEXION CON ACCESS---------------------------

con <- odbcConnectAccess("C:/Users/kl_93/OneDrive/Escritorio/Database1.mdb")
odbcClose(con)
odbcCloseAll()



table_names <- sqlTables(con) %>% 
  select(TABLE_NAME)


departamentos <- table_names %>% 
  filter(grepl("Departamentos",TABLE_NAME)) %>% 
  unlist()

vigilancia <- departamentos %>% 
  #LE PONGO EL NOMBRE DEOPARTAMENTOS A ESTA SELECCION (BO)
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>% 
  #con esta linea lo convierto en un data.frame (MUY LOCO)
  reduce(left_join, by="GlobalRecordId") %>% 
  as_tibble()

z <- as.numeric(max(vigilancia$auto))

  



#CREAR DATA FRAME-------------------------------

b1 <- data.frame (Departamento  = c("preven", "vacunas", "limpieza", "docencia"),
                  Jefe = c("marga", "jesus", "justi", "koldo"),
                  telefono = c("880226", "840240", "886666", "886666"),
                  auto = c(7, 8, NA, NA)) %>% 
  print()

b <- b1 %>% 
  #meto dos lineas de codigo para ordenarlo antes de nada
  mutate(auto  = if_else( is.na(auto), 0, auto)) %>% 
  print()

#telefono seria el cic, asi que si se repite el cic solo quiero uno
b_dupes <- b %>% 
  distinct(telefono, .keep_all = T) %>% 
  print()


b_dupes <- b_dupes %>% 
  arrange(auto) %>% 
  #genero un valor maximo y eso es el que vamos a superar por auto
  #EN SI EL VALOR MAXIMO NO ES DE LOS QUE METO YO HOY, SINO DE LA BASE DE DATOS
  mutate(
    #max = max(auto, na.rm = T),
    auto = if_else(auto == 0, row_number() + z, auto)) %>% 
  #select(-max) %>% 
  print()

#conseguir el cic que voy a querer copiar
pega <- b_dupes %>% 
  select(telefono, auto) %>% 
  print()

#jefe seria microorganismo y telefono cic
final <- anti_join(b, b_dupes, by = c("telefono", "Jefe")) %>% 
  select(-auto) %>% 
  left_join(pega, by = "telefono") %>% 
  print()

incredibile <- rbind(b_dupes, final) %>% 
  print()


sqlSave(channel = con,
        dat = incredibile, 
        tablename = "Departamentos", 
        append = T,
        rownames = F,
        colnames = F,
        verbose = T,
        fast = F)   
