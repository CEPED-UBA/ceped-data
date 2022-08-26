
library(readxl)

#Script para levantar datos de archivos sobre EPH con formato CEPED horizontal

#### CONTINUA ####

base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "28 trim - prec est")

base2 <- base[7:11, ]

base2 <- base2 %>% 
  rename(index = 1)

#Traspongo y pongo nombre de variables
base2 <- data.frame( t(base2[,-1]) )
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

colnames(base2) <- c("ANO4", "trimestre", "t_protegido_continua", "t_precario_continua", "t_desconocido_continua")

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

rownames(base2) <- NULL

base2 <- base2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base3 <- gather(base2, key="cod.variable", value="valor", 2:4 )

base3 <- base3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG")

#### PUNTUAL ####

base_puntual <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED_Puntual y Continua.xls", 
                           sheet = "28 punt - prec est")

base_puntual2 <- base_puntual[6:10,1:17]

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

colnames(base_puntual2) <- c("ANO4", "onda", "t_protegido_puntual", "t_precario_puntual", "t_desconocido_puntual")

base_puntual2[, 3:5] <- lapply(base_puntual2[, 3:5], function(x) as.numeric(as.character(x)))

rownames(base_puntual2) <- NULL

base_puntual2 <- base_puntual2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)

base_puntual3 <- gather(base_puntual2, key="cod.variable", value="valor", 2:4 )

base_puntual3 <- base_puntual3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG")

### Junto las bases ####

base_final <- bind_rows(base3, base_puntual3)

# Completo data frame con NAs

anios_incompletos_continua <- base_puntual3[, c(1, 2, 5, 6)]

t_protegido_continua <- c(rep("t_protegido_continua", 16))
t_precario_continua <- c(rep("t_precario_continua", 16))
t_desconocido_continua <- c(rep("t_desconocido_continua", 16))

codigos_continua <- c(t_protegido_continua, t_precario_continua, t_desconocido_continua)

anios_incompletos_continua$cod.variable <- codigos_continua
anios_incompletos_continua$valor <- NA

anios_incompletos_puntual <- base3[, c(1, 2, 5, 6)]

t_protegido_puntual <- c(rep("t_protegido_puntual", 76))
t_precario_puntual <- c(rep("t_precario_puntual", 76))
t_desconocido_puntual <- c(rep("t_desconocido_puntual", 76))

codigos_puntual <- c(t_protegido_puntual, t_precario_puntual, t_desconocido_puntual)

anios_incompletos_puntual$cod.variable <- codigos_puntual
anios_incompletos_puntual$valor <- NA

base_final_final <- bind_rows(base_final, anios_incompletos_continua, anios_incompletos_puntual)

base_final_final <- base_final_final %>% 
  mutate(ANO4.trim= case_when(ANO4.trim=="2003.may" ~ "2003.1",
                              TRUE ~ ANO4.trim))

saveRDS(base_final_final, file = "opcion-modulos-paneles/www/data/eph_precariedad.RDS")
