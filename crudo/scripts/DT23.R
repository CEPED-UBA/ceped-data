#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
## Localidades####
poblacion_localidades <- read.xlsx("crudo/datos/Cuadro Todas las Localidades_23052016.xlsx",
                    sheet = "Todas las localidades",startRow = 7,cols = c(1:4,27:31),
                    skipEmptyRows = T) 

saveRDS(poblacion_localidades,file = "data/Poblacion_localidades_urbanas.RDS")
