

```{r}

nosocomiales <- h_4 %>%
  mutate(planta2 = planta,
         planta = unlist(planta),
         planta = as.factor(planta)) %>% 
  mutate(planta2 = as.numeric(str_extract(planta2, "[0-9]+"))) %>% 
 # mutate(planta2 = as.numeric(planta2)) %>% 
  filter(planta2 == 1) %>% 
  drop_na(cic)


nosocomiales %>% count(planta, mes_pdia) %>%
  group_by(planta) %>% 
  ggplot(mapping = aes(x = mes_pdia)) +
  geom_line(stat = "count")


ggplot( data = nosocomiales) +
  geom_bar(
    mapping = aes(x = mes_pdia, fill = planta, color = planta),
    position = "dodge"
  )
  
  
  


view(nosocomiales)

c <- c1 %>% count(indicadores, semana_pdia) %>% 
  complete(indicadores, semana_pdia, fill = list (n=0)) %>% 
  ggplot(aes(x=semana_pdia, y=n, group = indicadores, color=indicadores)) + 
  geom_smooth(size=0.8, stat = "identity")+
    theme_bw() +
  xlab("Semana") + 
  ylab("Nº de casos") +
  labs(color="Indicadores", title = paste("Evolución de indicadores de interés")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(0,30)) +
  theme(axis.text.x = element_text(angle=0))+
  geom_point(size=2.5)
```
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
