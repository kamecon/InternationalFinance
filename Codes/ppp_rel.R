library(zoo)
library(BIS)
library(tidyverse)
library(ggpmisc)

datasets <- get_datasets()

#Datos de precios

precios <- get_bis(datasets$url[datasets$name == "Consumer prices"], quiet = TRUE)
tdc_usa <- get_bis(datasets$url[datasets$name == "US dollar exchange rates (monthly, quarterly and annual)"], quiet = TRUE)

inflaciones <- precios %>%
  dplyr::filter(freq=="A" & unit_of_measure == "Year-on-year changes, in per cent" & date > "1999-12") %>%
  dplyr::filter(ref_area %in% c("US", "CA", "XM", "AU", "JP", "CH")) %>% 
  select(ref_area, reference_area, unit_of_measure, date, obs_value) %>% 
  mutate(date = as.numeric(date)) %>% 
  select(ref_area, date, obs_value) %>% 
  rename(inflacion=obs_value)

#Datos de tipo de cambio

tdc_var <- tdc_usa %>% 
  dplyr::filter(freq=="A" & date > "1998" & collection == "A") %>%
  dplyr::filter(ref_area %in% c("US", "CA", "XM", "AU", "JP", "CH")) %>% 
  group_by(ref_area) %>%
  mutate(var_porct = (obs_value/lag(obs_value) -1)*100) %>% 
  dplyr::filter(date > "1999")%>% 
  select(ref_area, date, var_porct) %>% 
  mutate(date = as.numeric(date))

##Datos mensuales (prueba)

inflaciones_m <- precios %>%
  dplyr::filter(freq=="M" & unit_of_measure == "Year-on-year changes, in per cent" & date > "1999-12") %>%
  dplyr::filter(ref_area %in% c("US", "CA", "XM", "AU", "JP", "CH")) %>% 
  select(ref_area, reference_area, unit_of_measure, date, obs_value) %>% 
  mutate(date = as.Date(as.yearmon(date))) %>% 
  select(ref_area, date, obs_value) %>% 
  rename(inflacion=obs_value)


tdc_var_m <- tdc_usa %>% 
  dplyr::filter(freq=="M" & date > "1998" & collection == "A") %>%
  dplyr::filter(ref_area %in% c("US", "CA", "XM", "AU", "JP", "CH")) %>% 
  group_by(ref_area) %>%
  mutate(var_porct = (obs_value/lag(obs_value) -1)*100) %>% 
  dplyr::filter(date > "1999")%>% 
  select(ref_area, date, var_porct) %>% 
  mutate(date = as.Date(as.yearmon(date)))

#Join de ambos (aún no se ha usado)

tabla <-  dplyr::full_join(inflaciones, tdc_var, by=c("ref_area", "date"))


#Filtro por países

datos_lm <- inflaciones %>% 
  dplyr::filter(ref_area %in% c("US", "AU")) %>%
  spread(key=ref_area, value=inflacion)

colnames(datos_lm)[2:3] <- c("home","us")

datos_lm2 <- datos_lm %>% 
  mutate(diferencial=home - us) %>% 
  dplyr::full_join(tdc_var %>% dplyr::filter(ref_area=="AU"), by="date") %>% 
  select(-ref_area)

ggplot(datos_lm2, aes(x=diferencial, y=var_porct)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "right", label.y.npc = 0.15,
               formula = y ~ x, parse = TRUE, size = 3)

modelo <- lm(formula = var_porct ~ diferencial, data = datos_lm2)
summary(modelo)
