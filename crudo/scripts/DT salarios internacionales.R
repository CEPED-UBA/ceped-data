#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)
# Codigos ISO####
options(scipen = 999)
iso_codes_spa <- read.xlsx("crudo/datos/isocodes.xlsx")
iso_codes_spa <- iso_codes_spa %>% 
  select(iso3c,nombre.pais = pais)

iso_codes_eng <- read.xlsx("crudo/datos/isocodes.xlsx",
                           sheet = "INGLES")


# Series####
## Salarios e IPC ####
salarios_ppp2017 <- read.xlsx("crudo/datos/DT Salarios Internacionales_Cuadros y Gráficos.xlsx",
                             sheet =1,startRow = 3,skipEmptyCols = T
)

salarios_relativos <- read.xlsx("crudo/datos/DT Salarios Internacionales_Cuadros y Gráficos.xlsx",
                             sheet =2,startRow = 3,skipEmptyCols = T
)

names(salarios_ppp2017)[1] <- "ANO4"
names(salarios_relativos)[1] <- "ANO4"

codigos_iso<- salarios_ppp2017[2,2:ncol(salarios_ppp2017)] %>% 
  pivot_longer(cols = 1:ncol(.),
               names_to = "nombre.pais",
               values_to = "iso3c")

salarios_ppp_tidy<- salarios_ppp2017[7:nrow(salarios_ppp2017),]%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "salario_ppa_c_priv_real")

salarios_relativo_tidy<- salarios_relativos[7:nrow(salarios_relativos),]%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "salario_relativo_usa_c_priv")


base <- salarios_ppp_tidy %>% 
  left_join(salarios_relativo_tidy) %>% 
  left_join(codigos_iso) 
  
base_export_dt_28 <- base %>% 
  mutate(nombre.pais = str_replace(nombre.pais,"\\."," "),
         nombre.pais = str_replace(nombre.pais,"EEUU","Estados Unidos"),
         nombre.pais = str_to_title(nombre.pais),
         ANO4 = as.numeric(ANO4)
         ) %>% 
  select(iso3c,nombre.pais,everything()) %>% 
  pivot_longer(4:ncol(.),names_to = "cod.variable",values_to = "valor") %>% 
  ungroup() %>% 
  mutate(valor = as.numeric(valor)) %>% 
  filter(!is.na(valor),
         !iso3c %in% c("CHN","LUX"))

saveRDS(base_export_dt_28,file = "www/data/DT_salarios_internac.RDS")
