#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
## Localidades####
poblacion_localidades <- read.xlsx("crudo/datos/Cuadro Todas las Localidades_23052016.xlsx",
                    sheet = "Todas las localidades",startRow = 7,cols = c(1:4,26:31),
                    skipEmptyRows = T)

saveRDS(poblacion_localidades,file = "data/Poblacion_localidades_urbanas.RDS")

poblacion_csv<- poblacion_localidades %>% 
  rename(Region = Región) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "Anio",values_to = "Poblacion Urbana")

readr::write_csv(poblacion_csv,file = "crudo/datos/urban_population_argentina.csv")
