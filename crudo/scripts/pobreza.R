library(readxl)
library(tidyverse)

#indigencia CEPA

base1_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza empalmadas.xlsx")

base1 <- base1_cruda[36:103, 10:11]

colnames(base1) <- c("Indigentes", "No indigentes")

#Genero vector años
ANO4 <- c()
for (i in 1988:2021){
  ANO4 <- c(ANO4, rep(i, 2) )
}

#Genero vector semestre/onda
trim <- c()

for (i in 1988:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2021){
  trim <- c(trim, c("1º", "2º") )
}

base1 <- data.frame(ANO4, trim, base1) %>% 
  mutate(ANO4.trim=paste0(ANO4, "-", trim), 
         Indigentes=as.numeric(Indigentes), 
         No.indigentes=as.numeric(No.indigentes),
         metodologia="CEPA") %>%   
  select(ANO4, ANO4.trim, metodologia, Indigentes, No.indigentes) %>% 
  pivot_longer(4:5, names_to="variable", values_to="valor")  

#pobreza CEPA

base2_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza empalmadas.xlsx", 
                          sheet = "pobre (1985)")

base2 <- base2_cruda[9:103, 10:11]

colnames(base2) <- c("Pobres", "No pobres")

#Genero vector años
ANO4 <- c()
for (i in 1974:2021){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:96]

#Genero vector semestre/onda
trim <- c()

for (i in 1974:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2021){
  trim <- c(trim, c("1º", "2º") )
}

trim <- trim[2:96]

base2 <- data.frame(ANO4, trim, base2) %>% 
  mutate(ANO4.trim=paste0(ANO4, "-", trim),
         Pobres=as.numeric(Pobres), 
         No.pobres=as.numeric(No.pobres),
         metodologia="CEPA") %>%   
  select(ANO4, ANO4.trim, metodologia, Pobres, No.pobres) %>% 
  pivot_longer(4:5, names_to="variable", values_to="valor")  



#indigencia INDEC

base3_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza empalmadas.xlsx", 
                          sheet = "indig (2004)")


base3 <- base3_cruda[9:103, 10:11]

colnames(base3) <- c("Indigentes", "No indigentes")

#Genero vector años
ANO4 <- c()
for (i in 1974:2021){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:96]

#Genero vector semestre/onda
trim <- c()

for (i in 1974:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2021){
  trim <- c(trim, c("1º", "2º") )
}

trim <- trim[2:96]

base3 <- data.frame(ANO4, trim, base3) %>% 
  mutate(ANO4.trim=paste0(ANO4, "-", trim), 
         Indigentes=as.numeric(Indigentes), 
         No.indigentes=as.numeric(No.indigentes),
         metodologia="INDEC")   %>%   
  select(ANO4, ANO4.trim, metodologia, Indigentes, No.indigentes) %>% 
  pivot_longer(4:5, names_to="variable", values_to="valor")  

#pobreza INDEC

base4_cruda <- read_excel("crudo/datos/Tasa de indigencia y pobreza empalmadas.xlsx",   
                          sheet = "pobre (2004)")

base4 <- base4_cruda[9:103, 10:11]

colnames(base4) <- c("Pobres", "No pobres")

#Genero vector años
ANO4 <- c()
for (i in 1974:2021){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:96]

#Genero vector semestre/onda
trim <- c()

for (i in 1974:2002){
  trim <- c(trim, c("may", "oct") )
}
for (i in 2003:2021){
  trim <- c(trim, c("1º", "2º") )
}

trim <- trim[2:96]

base4 <- data.frame(ANO4, trim, base4) %>% 
  mutate(ANO4.trim=paste0(ANO4, "-", trim), 
         Pobres=as.numeric(Pobres), 
         No.pobres=as.numeric(No.pobres),
         metodologia="INDEC") %>% 
  select(ANO4, ANO4.trim, metodologia, Pobres, No.pobres) %>% 
  pivot_longer(4:5, names_to="variable", values_to="valor")       


base <- bind_rows(base1, base2, base3, base4) %>% 
  mutate(variable=case_when(
    variable=="No.indigentes" ~ "No indigentes", 
    variable=="No.pobres"     ~ "No pobres", 
    TRUE                    ~ variable))


saveRDS(base, file = "www/data/pobreza.RDS")
