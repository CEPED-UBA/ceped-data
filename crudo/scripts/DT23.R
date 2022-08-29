#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
## Localidades####
poblacion_localidades <- read.xlsx("crudo/datos/Cuadro Todas las Localidades_23052016.xlsx",
                    sheet = "Todas las localidades",startRow = 7,cols = c(1:4,26:31),
                    skipEmptyRows = T)

poblacion_csv<- poblacion_localidades %>% 
  rename(Region = Región) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "Anio",values_to = "Poblacion Urbana")

readr::write_csv(poblacion_csv,file = "crudo/datos/urban_population_argentina.csv")

tiempo_censos <- readxl::read_xls("crudo/datos/tiempo_entre_censos.xls",
                                  range = "A9:D19") %>% 
  mutate(tiempo = as.numeric(tiempo)) %>% 
  select(Anio,tiempo)

## Para app censo ###
localidades<- poblacion_localidades %>% 
  rename(Region = Región) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "Anio",values_to = "poblacion_urbana") %>% 
  arrange(desc(Anio)) %>% 
  left_join(tiempo_censos) %>% 
  group_by(Localidad) %>% 
  mutate(tasa_crecim_intercensal = poblacion_urbana/lead(poblacion_urbana)-1,
         tasa_crecim_anualizada  = (poblacion_urbana/lead(poblacion_urbana))^(1/tiempo)-1) %>% 
  select(-tiempo)

regiones<- poblacion_localidades %>% 
  rename(Region = Región) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "Anio",values_to = "poblacion_urbana") %>% 
  group_by(Region,Anio) %>%
  summarise(poblacion_urbana = sum(poblacion_urbana,na.rm = T)) %>% 
  filter(!is.na(Region)) %>% 
  arrange(desc(Anio)) %>% 
  left_join(tiempo_censos) %>% 
  group_by(Region) %>%
  mutate(tasa_crecim_intercensal = poblacion_urbana/lead(poblacion_urbana)-1,
         tasa_crecim_anualizada  = (poblacion_urbana/lead(poblacion_urbana))^(1/tiempo)-1) %>% 
  rename(Localidad = Region)%>% 
  select(-tiempo)

provincias<- poblacion_localidades %>% 
  rename(Region = Región) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "Anio",values_to = "poblacion_urbana") %>% 
  group_by(Provincia,Anio) %>%
  summarise(poblacion_urbana = sum(poblacion_urbana,na.rm = T)) %>% 
  filter(!is.na(Provincia)) %>% 
  arrange(desc(Anio)) %>% 
  left_join(tiempo_censos) %>% 
  group_by(Provincia) %>%
  mutate(tasa_crecim_intercensal = poblacion_urbana/lead(poblacion_urbana)-1,
         tasa_crecim_anualizada  = (poblacion_urbana/lead(poblacion_urbana))^(1/tiempo)-1) %>% 
  rename(Localidad = Provincia)%>% 
  select(-tiempo)

saveRDS(localidades,file = "crudo/scripts/app_censo2010/localidades.RDS")
saveRDS(regiones,file = "crudo/scripts/app_censo2010/regiones.RDS")
saveRDS(provincias,file = "crudo/scripts/app_censo2010/provincias.RDS")
