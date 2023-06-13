library(tidyverse)
library(readxl)


#####Nota: sacarle la tilde al nombre de la pestaña, dice cómputos

##unifico todas las variables de subperiodo como "sub"

IPC_Argentina <- read_excel("crudo/datos/IPC_Argentina.xlsx", 
                            sheet = "IPC para computos (2017=100)")


IPC_Argentina <- IPC_Argentina[5:nrow(IPC_Argentina),]

names(IPC_Argentina)[1] <- "ANO4"
names(IPC_Argentina)[11] <- "Mayo"
names(IPC_Argentina)[12] <- "Octubre"
names(IPC_Argentina)[13] <- "T1"
names(IPC_Argentina)[14] <- "T2"
names(IPC_Argentina)[15] <- "T3"
names(IPC_Argentina)[16] <- "T4"
names(IPC_Argentina)[20] <- "anual"

####EPH####

IPC_Argentina_EPH <- IPC_Argentina %>% 
  filter(ANO4 >= 1974) %>% 
  select(ANO4,Mayo,Octubre,T1,T2,T3,T4)

Ondas <- IPC_Argentina_EPH %>% 
  filter(ANO4 %in% c(1974:2003)) %>% 
  select(ANO4,Mayo,Octubre) %>% 
  gather(.,key="onda",value = "valor",-ANO4) %>% 
  filter(!(ANO4 == 2003 & onda == "Octubre")) %>% 
  arrange(ANO4,onda)%>% 
  filter(!is.na(valor))

Ondas <- Ondas %>% 
  rename("sub" = "onda") %>% 
  mutate(cod.variable = "Ondas_EPH_2017")

Ondas <- Ondas %>% 
  mutate(var = NA)

Ondas$valor <- as.numeric(Ondas$valor)

Continua <- IPC_Argentina_EPH %>% 
  filter(ANO4 >= 2003) %>% 
  select(ANO4,T1,T2,T3,T4) %>% 
  gather(.,key="trim",value = "valor",-ANO4) %>% 
  filter(!(ANO4 == 2003 & trim %in% c("T1","T2"))) %>% 
  arrange(ANO4,trim) %>% 
  filter(!is.na(valor))

Continua <- Continua %>% 
  rename("sub" = "trim")%>% 
  mutate(cod.variable = "Trimestres_EPH_2017")

Continua <- Continua %>% 
  mutate(var = NA)

Continua$valor <- as.numeric(Continua$valor)

###Anual####


IPC_Argentina_anual <- IPC_Argentina %>% 
  filter(ANO4 >= 1943) %>% 
  select(ANO4,"valor" = anual)

IPC_Argentina_anual <- IPC_Argentina_anual %>% 
  mutate(sub = NA)%>% 
  mutate(cod.variable = "IPC_Anual_2017")

IPC_Argentina_anual$valor <- as.numeric(IPC_Argentina_anual$valor)

variacion_ipc <- IPC_Argentina_anual %>% 
  mutate(aux = lag(valor))


variacion_ipc <- variacion_ipc %>% 
  mutate(var = ((valor - aux)/aux)*100)

IPC_Argentina_anual <- variacion_ipc %>% 
  select(-aux)


###Mensual####

IPC_Argentina_mensual <- read_excel("crudo/datos/IPC_Argentina.xlsx", 
                            sheet = "IPC mensual")

IPC_Argentina_mensual <- IPC_Argentina_mensual[4:nrow(IPC_Argentina_mensual),]

names(IPC_Argentina_mensual)[1] <- "ANO4"
names(IPC_Argentina_mensual)[2] <- "mes"
names(IPC_Argentina_mensual)[8] <- "valor"

IPC_Argentina_mensual <- IPC_Argentina_mensual %>% 
  select(ANO4,mes,valor) %>% 
  filter(!is.na(valor)) %>% 
  filter(!is.na(ANO4)) %>% 
  mutate(mes = as.numeric(mes)) %>% 
  arrange(ANO4, mes)

IPC_Argentina_mensual <- IPC_Argentina_mensual %>% 
  rename("sub" = "mes")%>% 
  mutate(cod.variable = "IPC_Mensual_2006")

IPC_Argentina_mensual$valor <- as.numeric(IPC_Argentina_mensual$valor)

variacion_ipc <- IPC_Argentina_mensual %>% 
  mutate(aux = lag(valor))


variacion_ipc <- variacion_ipc %>% 
  mutate(var = ((valor - aux)/aux)*100)

IPC_Argentina_mensual <- variacion_ipc %>% 
  select(-aux)
  
IPC_Argentina_mensual$sub <- as.character(IPC_Argentina_mensual$sub)

#####junto y guardo

base_ipc <- bind_rows(IPC_Argentina_anual, IPC_Argentina_mensual, Ondas, Continua)

base_ipc$ANO4 <- as.numeric(base_ipc$ANO4)

saveRDS(base_ipc,"www/data/base_ipc.RDS")

#base_ipc <- readRDS("opcion-modulos-paneles/www/data/base_ipc.RDS")

