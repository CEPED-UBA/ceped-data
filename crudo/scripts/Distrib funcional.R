#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
participacion <- read.xlsx("crudo/datos/Puestos y salarios_para ceped.data.xlsx",
                           sheet = "Sintesis Participación",
                           startRow = 5,
                           skipEmptyRows = T)

puestos_masa_w <- read.xlsx("crudo/datos/Puestos y salarios_para ceped.data.xlsx",
                            sheet = "Síntesis Puestos, W y Masa W",
                            startRow = 4,
                            skipEmptyRows = T)
# Hago tidy las bases #####
##Participacion####
names(participacion)[1] <- "Anio"
names(participacion)[4:7] <- paste0("particip.vab.pb_",names(participacion)[4:7])
names(participacion)[8:11] <- paste0("particip.pib.pm_",names(participacion)[8:11])


participacion_tidy <-  data.frame(participacion) %>% 
  select(-(2:3)) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:ncol(.),names_to = "variable",values_to = "valor") %>% 
  separate(col = variable,into = c("variable","sector"),sep = "_")

##Puestos####
names(puestos_masa_w)[1] <- "Anio"
names(puestos_masa_w)[2:5] <- paste0("puestos_",names(puestos_masa_w)[2:5])
names(puestos_masa_w)[6:9] <- paste0("salario.nominal_",names(puestos_masa_w)[6:9])
names(puestos_masa_w)[10:13] <- paste0("masa.salarial_",names(puestos_masa_w)[10:13])
names(puestos_masa_w)[15:18] <- paste0("puestos.composicion_",names(puestos_masa_w)[15:18])
names(puestos_masa_w)[19:22] <- paste0("salario.real_",names(puestos_masa_w)[19:22])
names(puestos_masa_w)[23:26] <- paste0("masa.salarial.real_",names(puestos_masa_w)[23:26])

puestos_tidy <-  data.frame(puestos_masa_w) %>% 
  select(-14) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:ncol(.),names_to = "variable",values_to = "valor") %>% 
  separate(col = variable,into = c("variable","sector"),sep = "_")

#Base conjunta ####
base_completa_dt21<- puestos_tidy %>% 
  bind_rows(participacion_tidy)

saveRDS(base_completa_dt21,file ="www/data/base_dt24.RDS")

