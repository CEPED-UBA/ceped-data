
library(readxl)
library(tidyverse)
library(openxlsx)

base_ipc <- read_excel("opcion-modulos-paneles/www/data/ipc_argentina_ceped.xlsx") %>% 
  arrange(cod.variable,ANO4, MES)

unique(base_ipc$cod.variable)


variacion_ipc <- base_ipc %>% 
  mutate(aux = lag(valor))

variacion_ipc$aux[variacion_ipc$ANO4 == 1943 & variacion_ipc$MES == 1] <- NA


variacion_ipc <- variacion_ipc %>% 
  mutate(var = ((valor - aux)/aux)*100)

variacion_ipc <- variacion_ipc %>% 
  select(-aux)

write.xlsx(variacion_ipc, "opcion-modulos-paneles/www/data/ipc_argentina_ceped_var.xlsx")
