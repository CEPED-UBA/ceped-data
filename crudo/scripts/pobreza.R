library(readxl)
library(tidyverse)

####Indigencia CEPA####

# base1_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza empalmadas.xlsx")
base1_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza.xlsx")

# base1 <- base1_cruda[36:103, c(2, 3, 6,7, 10, 11)]
base1 <- base1_cruda[36:107, c(2, 3, 6,7, 10, 11)]

colnames(base1) <- c("Indigentes EPH-puntual", "No indigentes EPH-puntual", "Indigentes EPH-continua", 
                     "No indigentes EPH-continua", "Indigentes Serie empalmada", "No indigentes  Serie empalmada")

base1 <- base1 %>% 
   mutate_if(is.character,as.numeric)

#Genero vector años
ANO4 <- c()
for (i in 1988:2023){#Automatizar detectando el año máximo
  ANO4 <- c(ANO4, rep(i, 2) )
}

#Genero vector semestre/onda
trim <- c()

for (i in 1988:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2023){#Automatizar detectando el año máximo
  trim <- c(trim, c("1º", "2º") )
}

base1 <- bind_cols(ANO4, trim, base1) %>% 
  rename_with(.cols = 1, ~"ANO4") %>% 
  rename_with(.cols = 2, ~"trim") %>% 
  mutate(Periodo=paste0(ANO4, "-", trim), 
         metodologia="CEPA") %>%   
  select(-trim) %>% 
  pivot_longer(2:7, names_to="Serie", values_to="valor")  

####Pobreza CEPA####

base2_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza.xlsx", 
                          sheet = "pobre (1985)")

base2 <- base2_cruda[9:107, c(2, 3, 6, 7, 10, 11)]

colnames(base2) <-  c("Pobres EPH-puntual", "No Pobres EPH-puntual", "Pobres EPH-continua", 
                      "No Pobres EPH-continua", "Pobres Serie empalmada", "No Pobres  Serie empalmada")
base2 <- base2 %>% 
  mutate_if(is.character,as.numeric)

#Genero vector años
ANO4 <- c()
for (i in 1974:2023){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:length(ANO4)]

#Genero vector semestre/onda
trim <- c()

for (i in 1974:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2023){
  trim <- c(trim, c("1º", "2º") )
}

trim <- trim[2:length(trim)]

base2 <- bind_cols(ANO4, trim, base2) %>% 
  rename_with(.cols = 1, ~"ANO4") %>% 
  rename_with(.cols = 2, ~"trim") %>% 
  mutate(Periodo=paste0(ANO4, "-", trim), 
         metodologia="CEPA") %>%  
  select(-trim) %>% 
  pivot_longer(2:7, names_to="Serie", values_to="valor")  

####Indigencia INDEC####

base3_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza.xlsx", 
                          sheet = "indig (2004)")


base3 <- base3_cruda[9:107, c(2, 3, 6, 7, 10, 11)]

base3 <- base3 %>% 
  mutate_if(is.character,as.numeric)

colnames(base3) <- c("Indigentes EPH-puntual", "No indigentes EPH-puntual", "Indigentes EPH-continua", 
                     "No indigentes EPH-continua", "Indigentes Serie empalmada", "No indigentes  Serie empalmada")

#Genero vector años
ANO4 <- c()
for (i in 1974:2023){ #Automatizar detectando el año máximo
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:length(ANO4)]

#Genero vector semestre/onda
trim <- c()

for (i in 1974:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2023){
  trim <- c(trim, c("1º", "2º") )
}

trim <- trim[2:length(trim)]

base3 <- bind_cols(ANO4, trim, base3) %>% 
  rename_with(.cols = 1, ~"ANO4") %>% 
  rename_with(.cols = 2, ~"trim") %>% 
  mutate(Periodo=paste0(ANO4, "-", trim), 
         metodologia="INDEC") %>%   
  select(-trim) %>% 
  pivot_longer(2:7, names_to="Serie", values_to="valor")   

####Pobreza INDEC####

base4_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza.xlsx",   
                          sheet = "pobre (2004)")

base4 <- base4_cruda[9:107, c(2, 3, 6, 7, 10, 11)]

base4 <- base4 %>% 
  mutate_if(is.character,as.numeric)

colnames(base4) <-  c("Pobres EPH-puntual", "No Pobres EPH-puntual", "Pobres EPH-continua", 
                      "No Pobres EPH-continua", "Pobres Serie empalmada", "No Pobres  Serie empalmada")

#Genero vector años
ANO4 <- c()
for (i in 1974:2023){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:length(ANO4)]

#Genero vector semestre/onda
trim <- c()

for (i in 1974:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2023){
  trim <- c(trim, c("1º", "2º") )
}

trim <- trim[2:length(trim)]

base4 <- bind_cols(ANO4, trim, base4) %>% 
  rename_with(.cols = 1, ~"ANO4") %>% 
  rename_with(.cols = 2, ~"trim") %>% 
  mutate(Periodo=paste0(ANO4, "-", trim), 
         metodologia="INDEC") %>%  
  select(-trim) %>% 
  pivot_longer(2:7, names_to="Serie", values_to="valor")        

base <- bind_rows(base1, base2, base3, base4) 

saveRDS(base, file = "www/data/pobreza.RDS")

####Tabla canastas####
##CBA
canastas_cba_cruda <- read_excel("crudo/datos/Canastas.xlsx",   
                          sheet = "CBA - GBA")

canastas_cba <- canastas_cba_cruda[3:nrow(canastas_cba_cruda),]

canastas_cba <- canastas_cba %>% 
  mutate_if(is.character,as.numeric) %>% 
  mutate(tipo_canasta= 'CBA-GBA')

colnames(canastas_cba) <-  c("periodo", "1985/6 hacia adelante", "2004/5 hacia atrás", "tipo_canasta")

##CBT
canastas_cbt_cruda <- read_excel("crudo/datos/Canastas.xlsx",   
                                 sheet = "CBT - GBA")

canastas_cbt <- canastas_cbt_cruda[3:nrow(canastas_cbt_cruda),]

canastas_cbt <- canastas_cbt %>% 
  mutate_if(is.character,as.numeric) %>% 
  mutate(tipo_canasta= 'CBT-GBA')

colnames(canastas_cbt) <-  c("periodo", "1985/6 hacia adelante", "2004/5 hacia atrás", "tipo_canasta")

canastas <- bind_rows(canastas_cba, canastas_cbt) 

saveRDS(canastas, file = "www/data/canastas.RDS")


# # PENDIENTE: revisar formato fecha para variable periodo

# df$nuevo_periodo <- format(df$periodo, "%b-%Y", locale = "es")
# 
# base_canastas$nuevo_periodo <- format(base_canastas$nuevo_periodo, "%b-%Y", locale = "es")










