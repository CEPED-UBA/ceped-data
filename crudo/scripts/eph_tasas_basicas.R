
library(readxl)

#Script para levantar datos de archivos sobre EPH con formato CEPED horizontal

#### CONTINUA ####

base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "28 trim - tasas")

base <- base[7:12, ]

base <- base %>% 
  rename(index = 1)

#Traspongo y pongo nombre de variables
base2 <- data.frame( t(base[,-1]) )
colnames(base2) <- base$index

#Genero vector años
ANO4 <- c()
for (i in 2003:2021){
  ANO4 <- c(ANO4, rep(i, 4) )
}

base2[1] <- ANO4

#Genero vector trimestre
trimestre <- c()
for (i in 2003:2021){
  trimestre <- c(trimestre, seq(1, 4) )
}

base2[2] <- trimestre

colnames(base2) <- c("ANO4", "trimestre", "t_actividad_continua", "t_empleo_continua", "t_desocupacion_continua",  "t_subocupacion_continua")

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

rownames(base2) <- NULL

base2 <- base2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base3 <- gather(base2, key="cod.variable", value="valor", 2:5 )

base3 <- base3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG")

#### PUNTUAL ####

base_puntual <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED_Puntual y Continua.xls", 
                   sheet = "28 punt - tasas")

base_puntual2 <- base_puntual[6:11,1:17]

base_puntual2 <- base_puntual2 %>% 
  rename(index = 1)

#Traspongo y pongo nombre de variables
base_puntual2 <- data.frame( t(base_puntual2[,-1]) )
colnames(base_puntual2) <- base_puntual$index

#Genero vector años
ANO4 <- c()
for (i in 1995:2003){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:17]

base_puntual2[1] <- ANO4

colnames(base_puntual2) <- c("ANO4", "onda", "t_actividad_puntual", "t_empleo_puntual", "t_desocupacion_puntual",  "t_subocupacion_puntual")

base_puntual2 <- base_puntual2 %>% 
  mutate(t_actividad_puntual=as.numeric(t_actividad_puntual), 
        t_empleo_puntual=as.numeric(t_empleo_puntual),
        t_desocupacion_puntual=as.numeric(t_desocupacion_puntual),
        t_subocupacion_puntual=as.numeric(t_subocupacion_puntual))

rownames(base_puntual2) <- NULL

base_puntual2 <- base_puntual2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)

base_puntual3 <- gather(base_puntual2, key="cod.variable", value="valor", 2:5 )

base_puntual3 <- base_puntual3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG")

### Junto las bases ####

base_final <- bind_rows(base3, base_puntual3)

# Completo data frame con NAs

anios_incompletos_continua <- base_puntual3[, c(1, 2, 5, 6)]

t_actividad_continua <- c(rep("t_actividad_continua", 16))
t_empleo_continua <- c(rep("t_empleo_continua", 16))
t_desocupacion_continua <- c(rep("t_desocupacion_continua", 16))
t_subocupacion_continua <- c(rep("t_subocupacion_continua", 16))

codigos_continua <- c(t_actividad_continua, t_empleo_continua, t_desocupacion_continua, t_subocupacion_continua)

anios_incompletos_continua$cod.variable <- codigos_continua
anios_incompletos_continua$valor <- NA

anios_incompletos_puntual <- base3[, c(1, 2, 5, 6)]

t_actividad_puntual <- c(rep("t_actividad_puntual", 76))
t_empleo_puntual <- c(rep("t_empleo_puntual", 76))
t_desocupacion_puntual <- c(rep("t_desocupacion_puntual", 76))
t_subocupacion_puntual <- c(rep("t_subocupacion_puntual", 76))

codigos_puntual <- c(t_actividad_puntual, t_empleo_puntual, t_desocupacion_puntual, t_subocupacion_puntual)

anios_incompletos_puntual$cod.variable <- codigos_puntual
anios_incompletos_puntual$valor <- NA

base_final_final <- bind_rows(base_final, anios_incompletos_continua, anios_incompletos_puntual)

base_final_final <- base_final_final %>% 
  mutate(ANO4.trim= case_when(ANO4.trim=="2003.may" ~ "2003.1",
                              TRUE ~ ANO4.trim), 
         cod.variable= case_when(
           cod.variable == "t_actividad_continua" ~ "Tasa de actividad (EPH continua)", 
           cod.variable == "t_empleo_continua"~ "Tasa de empleo (EPH continua)", 
           cod.variable == "t_desocupacion_continua" ~"Tasa de desocupación (EPH continua)", 
           cod.variable == "t_subocupacion_continua"~"Tasa de subocupación  (EPH continua)", 
           cod.variable == "t_actividad_puntual"~ "Tasa de actividad (EPH puntual)" , 
           cod.variable == "t_empleo_puntual"~ "Tasa de empleo (EPH puntual)", 
           cod.variable == "t_desocupacion_puntual"~ "Tasa de desocupación (EPH puntual)", 
           cod.variable == "t_subocupacion_puntual" ~ "Tasa de subocupación (EPH puntual)"
         ))

saveRDS(base_final_final, file = "opcion-modulos-paneles/www/data/eph_tasas_basicas.RDS")
