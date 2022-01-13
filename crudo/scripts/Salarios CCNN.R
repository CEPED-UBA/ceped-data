#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
## CEPED-CCNN####
Paises <- read.xlsx("crudo/datos/Prod y Salarios.xlsx",
                    sheet = "Paises_Diccionario") %>% 
  select(nombre.pais,iso3c)


Salarios_UMN <- read.xlsx("crudo/datos/Prod y Salarios.xlsx",
                          sheet = "salario nominal - UMN"
                          ) %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "Salario.UMN") %>%
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))
 
IPC_2005 <- read.xlsx("crudo/datos/Prod y Salarios.xlsx",
                          sheet = "IPC (2005)") %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "IPC_2005") %>%
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))


# PPA####
PPA_WB <- read.csv("crudo/datos/All_PPA_WB_2017.csv") %>%
  rename(iso3c = Country.Code)

PPA_2017_Cpriv_Cefect<- PPA_WB %>%
  filter(Classification.Code == "PPPGlob",
         Series.Code %in% c(9260000,9020000)) %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Año",
               values_to = "PPA.2017") %>%
  mutate(ANO4 = as.numeric(str_extract(Año,"[[:digit:]]{4}")),
         PPA.2017 = as.numeric(as.character(PPA.2017))) %>% 
  filter(ANO4 == 2017) %>%
  select(iso3c,Series.Name,PPA.2017) %>% 
  pivot_wider(names_from = "Series.Name",values_from = "PPA.2017")

names(PPA_2017_Cpriv_Cefect)[2] <- "PPA_2017_c_actual"
names(PPA_2017_Cpriv_Cefect)[3] <- "PPA_2017_c_privado"


PPA_extrapolados <- IPC_2005 %>% 
  left_join(Paises) %>% 
  left_join(PPA_2017_Cpriv_Cefect) %>% 
  group_by(ANO4) %>% 
  mutate(IPC_2005_USA = IPC_2005[iso3c == "USA"]) %>% 
  group_by(nombre.pais) %>%
  mutate(PPA_c_actual_serie =
           case_when(
             ANO4 == 2017 ~ PPA_2017_c_actual,
             TRUE ~ PPA_2017_c_actual[ANO4 == 2017]*
               (IPC_2005/IPC_2005[ANO4 == 2017])/
               (IPC_2005_USA/IPC_2005_USA[ANO4 == 2017])),
         PPA_c_priv_serie =
           case_when(
             ANO4 == 2017 ~ PPA_2017_c_privado,
             TRUE ~ PPA_2017_c_privado[ANO4 == 2017]*
               (IPC_2005/IPC_2005[ANO4 == 2017])/
               (IPC_2005_USA/IPC_2005_USA[ANO4 == 2017])))
                     
  
PPA_series<- PPA_extrapolados %>% 
  select(iso3c,
         ANO4,
         PPA_c_actual_serie,
         PPA_c_priv_serie)


# Procesamiento ####
base <- Paises %>% 
  left_join(Salarios_UMN) %>% 
  left_join(IPC_2005) %>% 
  left_join(PPA_series) %>% 
  mutate(salario_real_2005 = Salario.UMN/IPC_2005*100,
         salario_ppa_c_priv_corr = Salario.UMN/PPA_c_priv_serie,
         salario_ppa_c_actual_corr = Salario.UMN/PPA_c_actual_serie) %>% 
  filter(!is.na(ANO4)) %>% 
  group_by(ANO4) %>% 
  mutate(salario_relativo_usa_c_priv = salario_ppa_c_priv_corr/salario_ppa_c_priv_corr[iso3c == "USA"],
         salario_relativo_usa_c_actual = salario_ppa_c_actual_corr/salario_ppa_c_actual_corr[iso3c == "USA"])

base_export <- base %>% 
  pivot_longer(4:ncol(.),
               names_to = "cod.variable",values_to = "valor") %>% 
  filter(!is.na(valor))

saveRDS(base_export,file = "data/salarios.RDS")
unique(base_export$cod.variable)
