carlos <- import("centrales.xlsx") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(-c(prioridad, unidad_enfermeria_uenf, ingresos)) 
  


carlos_2 <- left_join(carlos, b, by = "cic", all.x=TRUE)

carlos_3 <- carlos_2 %>% 
  filter(is.na(semana_pdia))

write_xlsx(carlos_3, path = "carlos_3.xlsx")
