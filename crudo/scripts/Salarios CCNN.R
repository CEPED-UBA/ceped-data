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
Salarios_UMN <- read.xlsx("crudo/datos/comparación internacional - salarios promedio PPP.xlsx",
                          sheet = "salario nominal - UMN"
) %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "iso3c",
               values_to = "Salario.UMN")

IPC_2005 <- read.xlsx("crudo/datos/comparación internacional - salarios promedio PPP.xlsx",
                      sheet = "IPC (2005 o 2010)") %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "iso3c",
               values_to = "IPC_2005")

IPC_WB <- readRDS(file = "crudo/datos/IPC_WORLD_BANK.RDS") %>% 
  select(iso3c,ANO4 = date,IPC_2010_WB = FP.CPI.TOTL)

IPC_corregido <- IPC_2005 %>%
  left_join(IPC_WB) %>% 
  group_by(iso3c) %>% 
  mutate(IPC_2005_corregido = 
           case_when(all(is.na(IPC_2005)) ~ IPC_2010_WB,
                     TRUE ~ IPC_2005)) %>% 
  group_by(iso3c) %>% 
  mutate(IPC_2005 = 100*IPC_2005_corregido/
           IPC_2005_corregido[ANO4 == 2005]) %>% 
  select(iso3c,ANO4,IPC_2005)

## Tipo de cambio####
Euro_area <- read.xlsx("crudo/datos/comparación internacional - salarios promedio PPP.xlsx",
                       sheet = "Euro Area")
tc <- readRDS("crudo/datos/series_tcn.RDS")
tcn_ocde <- readxl::read_excel("crudo/datos/TCN_OCDE_1955-2013.xlsx")
tcn_cepal <- readxl::read_excel("crudo/datos/TC Am Lat_CEPAL.xlsx") 
###OCDE####
tcn_ocde <- tcn_ocde %>% 
  mutate_at(.vars = 3:ncol(.),.funs = as.numeric) %>% 
  pivot_longer(cols = 3:ncol(.),names_to = "date",values_to = "tcn_ocde") %>% 
  mutate(date = as.double(date))

###CEPAL####
tcn_cepal <- tcn_cepal %>%   #mutate_at(.vars = 3:ncol(.),.funs = as.numeric) %>% 
  pivot_longer(cols = 3:ncol(.),names_to = "date",values_to = "tcn_cepal") %>% 
  mutate(date = as.double(date))


# Corrijo países en Euros
series_tcn_corregidas<- tc %>%
  group_by(date) %>%
  mutate(PA.NUS.FCRF = case_when(
    iso3c %in% Euro_area$`Alpha-3` ~ PA.NUS.FCRF[iso3c == "EMU"],
    TRUE ~ PA.NUS.FCRF)) %>% 
  left_join(tcn_ocde) %>% 
  left_join(tcn_cepal) %>% 
  mutate(valor = if_else(is.na(PA.NUS.FCRF),tcn_ocde,PA.NUS.FCRF),
         TCN = if_else(is.na(valor),tcn_cepal,valor)) %>% 
  rename(ANO4 = date) %>% 
  select(ANO4,iso3c,TCN)

## PPA####
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

PPA_2011_Cpriv_Cefect<- PPA_WB %>%
  filter(Classification.Code == "PPPGlob",
         Series.Code %in% c(9260000,9020000)) %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Año",
               values_to = "PPA.2011") %>%
  mutate(ANO4 = as.numeric(str_extract(Año,"[[:digit:]]{4}")),
         PPA.2011 = as.numeric(as.character(PPA.2011))) %>% 
  filter(ANO4 == 2011) %>%
  select(iso3c,Series.Name,PPA.2011) %>% 
  pivot_wider(names_from = "Series.Name",values_from = "PPA.2011")

names(PPA_2011_Cpriv_Cefect)[2] <- "PPA_2011_c_actual"
names(PPA_2011_Cpriv_Cefect)[3] <- "PPA_2011_c_privado"



PPA_extrapolados <- IPC_corregido %>% 
  filter(iso3c != c("VEN")) %>% 
  left_join(PPA_2017_Cpriv_Cefect) %>% 
  group_by(ANO4) %>% 
  mutate(IPC_2005_USA = IPC_2005[iso3c == "USA"]) %>% 
  group_by(iso3c) %>%
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
    
PPA_extrapolados_venezuela <- IPC_corregido %>% 
  left_join(PPA_2011_Cpriv_Cefect) %>% 
  filter(iso3c  %in%  c("VEN","USA")) %>% 
  group_by(ANO4) %>% 
  mutate(IPC_2005_USA = IPC_2005[iso3c == "USA"]) %>% 
  group_by(iso3c) %>%
  mutate(PPA_c_actual_serie =
           case_when(
             ANO4 == 2011 ~ PPA_2011_c_actual,
             TRUE ~ PPA_2011_c_actual[ANO4 == 2011]*
               (IPC_2005/IPC_2005[ANO4 == 2011])/
               (IPC_2005_USA/IPC_2005_USA[ANO4 == 2011])),
         PPA_c_priv_serie =
           case_when(
             ANO4 == 2011 ~ PPA_2011_c_privado,
             TRUE ~ PPA_2011_c_privado[ANO4 == 2011]*
               (IPC_2005/IPC_2005[ANO4 == 2011])/
               (IPC_2005_USA/IPC_2005_USA[ANO4 == 2011]))) %>% 
  filter(iso3c == "VEN")


                 
  
PPA_series<- PPA_extrapolados %>% 
  bind_rows(PPA_extrapolados_venezuela) %>% 
  select(iso3c,
         ANO4,
         PPA_c_actual_serie,
         PPA_c_priv_serie)


# Procesamiento ####
paises_trabajados<- Salarios_UMN %>% 
  select(iso3c) %>% 
  unique()

base <- paises_trabajados %>% 
  left_join(iso_codes_spa) %>% 
  left_join(Salarios_UMN) %>% 
  left_join(IPC_corregido) %>% 
  left_join(PPA_series) %>% 
  left_join(series_tcn_corregidas) %>% 
  mutate(salario_real_2005 = Salario.UMN/IPC_2005*100,
         salario_dolares = Salario.UMN/TCN,
         salario_ppa_c_priv_corr = Salario.UMN/PPA_c_priv_serie,
         salario_ppa_c_actual_corr = Salario.UMN/PPA_c_actual_serie) %>% 
  group_by(iso3c) %>% 
  mutate(indice_salario_real_2005 = 100*salario_real_2005/salario_real_2005[ANO4 == 2005]) %>% 
  filter(!is.na(ANO4)) %>% 
  group_by(ANO4) %>% 
  mutate(tcn_ppa_cpriv = TCN/PPA_c_priv_serie,
         salario_relativo_usa_dolares=salario_dolares/salario_dolares[iso3c == "USA"],
         salario_relativo_usa_c_priv = salario_ppa_c_priv_corr/salario_ppa_c_priv_corr[iso3c == "USA"],
         salario_relativo_usa_c_actual = salario_ppa_c_actual_corr/salario_ppa_c_actual_corr[iso3c == "USA"])

base_export <- base %>% 
  pivot_longer(4:ncol(.),
               names_to = "cod.variable",values_to = "valor") %>% 
  filter(!is.na(valor))

saveRDS(base_export,file = "opcion-modulos-paneles/www/data/salarios.RDS")
