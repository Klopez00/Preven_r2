
```{r}
h <- b1 %>%
  drop_na(cic) %>%  
  mutate(cic = as.numeric(unlist(cic)),
         contactos_estrechos = as.character(contactos_estrechos),
         contactos_estrechos = ifelse(contactos_estrechos == "NULL", "no", contactos_estrechos)) %>% 
  filter(indicadores == "inesp_durante_ingreso")

planta_registro <- read_sheet("https://docs.google.com/spreadsheets/d/1avqAm5Rau0YbpPr1G2470GVL2-DWzZBk32YmZhl7YY8/edit#gid=1128400221") %>% 
  clean_names() %>% 
  as_tibble() %>% 
  select(c(cic:fecha_pdia, planta, contactos_estrechos))

h_3 <- anti_join(h, planta_registro, by='cic', .keep.all = TRUE) %>% 
  select(c(cic:fecha_pdia, planta, contactos_estrechos))
 
h_4 <- rbind(planta_registro, h_3) %>% 
  arrange(fecha_pdia) %>% 
  distinct(cic, .keep_all = T) %>% 
  mutate(dias_hasta_positivo = (fecha_pdia - fecha_ingreso)/ 86400) %>% 
  separate(dias_hasta_positivo, into = c("dias_hasta_positivo","x")) %>%
  select(-c(contactos_estrechos, x)) %>% 
  
  #construir variable mes------------------------------------------------
mutate(
    mes_pdia = floor_date(fecha_pdia,
                        unit = "month")) 

format(h_4$mes_pdia,"%B")

```
