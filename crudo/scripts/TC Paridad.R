# Cargamos librerías
library(openxlsx)
library(tidyverse)

# Leemos el Excel
TC_paridad <- read.xlsx("crudo/datos/Precio dolar_Argentina_v4.xlsx",
                           sheet = "data_sistematizada_cd",
                           startRow = 1,
                           cols = c(3:14),
                           skipEmptyRows = T)

# Convertimos de formato ancho a largo
TC_paridad <- TC_paridad %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "cod.variable",
               values_to = "valor")

# Agregamos columnas de país
TC_paridad <- TC_paridad %>%
  mutate(iso3c = "ARG",
         nombre.pais = "Argentina") %>% 
  select(nombre.pais,iso3c,everything())

# Guardamos en formato para R
saveRDS(TC_paridad,file ="www/data/base_tc_paridad.RDS")
