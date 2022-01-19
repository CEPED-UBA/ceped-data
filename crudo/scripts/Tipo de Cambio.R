#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
## CEPED-CCNN####
TCN_anual <- read.xlsx("crudo/datos/Tipo de cambio_Argentina.xlsx",
                    sheet = "TCN_anual_series",startRow = 6,cols = 1:5,
                    skipEmptyRows = T) 

names(TCN_anual)[1] <- "ANO4"
names(TCN_anual)[2] <- "TCNA_DL_Ferreres"
names(TCN_anual)[3] <- "TCNA_EXPO_Ferreres"
names(TCN_anual)[4] <- "TCNA_JIC"
names(TCN_anual)[5] <- "TCNA_Arceo"

TCR_anual <- read.xlsx("crudo/datos/Tipo de cambio_Argentina.xlsx",
                       sheet = "TCR_anual_series",startRow = 6,cols = 1:5,
                       skipEmptyRows = T) 

names(TCR_anual)[1] <- "ANO4"
names(TCR_anual)[2] <- "TCRA_2007_DL_Ferreres"
names(TCR_anual)[3] <- "TCRA_2007_EXPO_Ferreres"
names(TCR_anual)[4] <- "TCRA_2007_JIC"
names(TCR_anual)[5] <- "TCRA_2007_Arceo"

valuac_anual_IPC <- read.xlsx("crudo/datos/Tipo de cambio_Argentina.xlsx",
                             sheet = "TCR y valuación_anual",startRow = 6,
                             cols = c(1,7:14),
                             skipEmptyRows = T) 

names(valuac_anual_IPC)[1] <- "ANO4"
names(valuac_anual_IPC)[2] <- "TCP_IPC_DL_Ferreres"
names(valuac_anual_IPC)[3] <- "TCP_IPC_EXPO_Ferreres"
names(valuac_anual_IPC)[4] <- "TCP_IPC_JIC"
names(valuac_anual_IPC)[5] <- "TCP_IPC_Arceo"
names(valuac_anual_IPC)[6] <- "GS_IPC_DL_Ferreres"
names(valuac_anual_IPC)[7] <- "GS_IPC_EXPO_Ferreres"
names(valuac_anual_IPC)[8] <- "GS_IPC_JIC"
names(valuac_anual_IPC)[9] <- "GS_IPC_Arceo"

valuac_anual_IPC_Product <- read.xlsx("crudo/datos/Tipo de cambio_Argentina.xlsx",
                                 sheet = "TCR y valuación_anual",startRow = 6,
                                 cols = c(1,16:23),
                                 skipEmptyRows = T) 

names(valuac_anual_IPC_Product)[1] <- "ANO4"
names(valuac_anual_IPC_Product)[2] <- "TCP_IPC_PROD_DL_Ferreres"
names(valuac_anual_IPC_Product)[3] <- "TCP_IPC_PROD_EXPO_Ferreres"
names(valuac_anual_IPC_Product)[4] <- "TCP_IPC_PROD_JIC"
names(valuac_anual_IPC_Product)[5] <- "TCP_IPC_PROD_Arceo"
names(valuac_anual_IPC_Product)[6] <- "GS_IPC_PROD_DL_Ferreres"
names(valuac_anual_IPC_Product)[7] <- "GS_IPC_PROD_EXPO_Ferreres"
names(valuac_anual_IPC_Product)[8] <- "GS_IPC_PROD_JIC"
names(valuac_anual_IPC_Product)[9] <- "GS_IPC_PROD_Arceo"







TCN_anual <- TCN_anual %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "cod.variable",
               values_to = "valor") 

TCR_anual <- TCR_anual %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "cod.variable",
               values_to = "valor") 

valuac_anual_IPC <- valuac_anual_IPC %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "cod.variable",
               values_to = "valor") 

valuac_anual_IPC_Product <- valuac_anual_IPC_Product %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "cod.variable",
               values_to = "valor") 


TC_anual <- TCR_anual %>% 
  bind_rows(TCN_anual)%>% 
  bind_rows(valuac_anual_IPC)%>% 
  bind_rows(valuac_anual_IPC_Product)%>% 
  filter(!is.na(valor)) %>% 
  mutate(iso3c = "ARG",
         nombre.pais = "Argentina") %>% 
  select(nombre.pais,iso3c,everything())


saveRDS(TC_anual,file = "data/Tipo_Cambio_Arg.RDS")
