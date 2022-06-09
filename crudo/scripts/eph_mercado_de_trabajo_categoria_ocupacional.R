options(scipen = 999)
library(readxl)

setwd("~/GitHub/ceped-data")

#### VALORES ABSOLUTOS ####

base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "28 trim - cat ocup abs")

base <- base[7:14, ]

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

colnames(base2) <- c("ANO4", "trimestre", "patron", "cuenta_propia", "asalariado", "TFSS",   "desconocido", "total")

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

rownames(base2) <- NULL

base2 <- base2 %>% 
  mutate(ANO4= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base3 <- gather(base2, key="cod.variable", value="valor", 2:7 )

base3 <- base3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         tipo="Absoluto")

saveRDS(base3,file = "~/GitHub/ceped-data/opcion-modulos-paneles/www/data/eph_mercado_de_trabajo_categoria_ocupacional.RDS")




#### ESTRUCTURA ####

base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "28 trim - cat ocup est")

base <- base[7:14, ]

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

colnames(base2) <- c("ANO4", "trimestre", "porc_patron", "porc_cuenta_propia", "porc_asalariado", "porc_TFSS",   "porc_esconocido", "porc_total")

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

rownames(base2) <- NULL

base2 <- base2 %>% 
  mutate(ANO4= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base3 <- gather(base2, key="cod.variable", value="valor", 2:7 )

base3 <- base3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         tipo="Tasa")



#### JOIN DE DATAFRAMES ####

eph_mercado_de_trabajo_categoria_ocupacional <- readRDS(file = '~/GitHub/ceped-data/opcion-modulos-paneles/www/data/eph_mercado_de_trabajo_categoria_ocupacional.RDS')

eph_mercado_de_trabajo_categoria_ocupacional <- rbind(eph_mercado_de_trabajo_categoria_ocupacional, base3)

# Agrego identificador de tipo de variable (Absoluto/Tasa) para filtrar en la ui categoria_ocup_eph

absolutas <- c("patron" ,  "cuenta_propia" ,  "asalariado", "TFSS", "desconocido", "total")
tasas <- c("porc_patron", "porc_cuenta_propia", "porc_asalariado", "porc_TFSS",  "porc_esconocido", "porc_total" )

mercado_de_trabajo_arg <- mercado_de_trabajo_arg %>% 
  mutate(tipo=case_when(
    cod.variable %in% absolutas ~ 'Absoluto', 
    cod.variable %in% tasas ~ 'Tasa'))

saveRDS(eph_mercado_de_trabajo_categoria_ocupacional, file = "~/GitHub/ceped-data/opcion-modulos-paneles/www/data/eph_mercado_de_trabajo_categoria_ocupacional.RDS")
