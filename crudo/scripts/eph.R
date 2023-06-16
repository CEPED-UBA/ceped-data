library(readxl)
library(tidyverse)
library(sjlabelled)

# EPH Puntual - Total aglomerados ####

base_mayo <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                        sheet = "Mayo - 28 Aglo", skip = 1)

base_octubre <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                           sheet = "Oct - 28 Aglo", skip = 1)

base_puntual1 <- base_mayo[3:32, 1:13]

base_puntual2 <- base_octubre[3:31, 1:13]

base_puntual <- bind_rows(base_puntual1, base_puntual2)

colnames(base_puntual) <- c("ANO4", "onda", "Tasa de actividad (EPH puntual)", "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                            "Tasa de subocupación (EPH puntual)", "Tasa de desocupación (EPH puntual)","Patrón (EPH puntual)",
                            "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)", "Protegido (EPH puntual)", "Precario (EPH puntual)" )

base_puntual[, 3:ncol(base_puntual)] <- sapply(base_puntual[, 3:ncol(base_puntual)], as.numeric)

base_puntual <- base_puntual %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)   %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         modulo= case_when(
           cod.variable %in%  c("Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
                                "Tasa de desocupación (EPH continua)", "Tasa de subocupación (EPH continua)", "Tasa de actividad (EPH puntual)", 
                                "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                                "Tasa de desocupación (EPH puntual)", "Tasa de subocupación (EPH puntual)") ~ "tasas_basicas", 
           cod.variable %in%  c("Patrón (EPH continua)", "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", 
                                "Patrón (EPH puntual)", "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)")  ~ "categoria_ocupacional", 
           cod.variable %in%  c( "Protegido (EPH continua)", "Precario (EPH continua)",
                                 "Protegido (EPH puntual)", "Precario (EPH puntual)")  ~ "precariedad"
         )) 

remove(base_mayo, base_octubre, base_puntual1, base_puntual2)

#Ramas

base_puntual_ramas<- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED_Puntual y Continua.xls", 
                           sheet = "28 punt - rama est")

base_puntual_ramas2 <- base_puntual_ramas[c(6:9, 15:29),]

base_puntual_ramas2 <- base_puntual_ramas2 %>% 
  rename(index = 1)

#Traspongo y pongo nombre de variables
base_puntual_ramas2 <- data.frame( t(base_puntual_ramas2[,-1]) )
colnames(base_puntual_ramas2) <- base_puntual_ramas$index

#Genero vector años
ANO4 <- c()
for (i in 1995:2003){
  ANO4 <- c(ANO4, rep(i, 2) )
}

ANO4 <- ANO4[2:17]

base_puntual_ramas2[1] <- ANO4

col_names_ramas <- c("Actividades primarias",
                     "Industria Manufacturera",
                     # "Alimentos, bebidad y tabaco",                    #Saco estas que son un desagregado de Industria Manufacturera
                     # "Textiles, confecciones y calzado",
                     # "Productos químicos y de la ref. de petróleo",
                     # "Productos metálicos, maquinarias y equipos",
                     # "Otras industrias manufactureras",
                     "Electricidas, gas y agua",
                     "Construcción",
                     "Comercio y reparaciones",
                     "Restaurantes y hoteles",
                     "Transporte",
                     "Servicios de correo y telecomunicaciones",
                     "Intermediación finaciera",
                     "Actividades inmobiliarias",
                     "Servicios empresariales y de alquiler",
                     "Administración pública y defensa",
                     "Enseñanza",
                     "Servicios sociales y de salud",
                     "Otras servicios",
                     "Servicio doméstico",
                     "Desconocidos")



colnames(base_puntual_ramas2) <- c("ANO4", "onda", col_names_ramas) 


base_puntual_ramas2[, 3:19] <- lapply(base_puntual_ramas2[, 3:19], function(x) as.numeric(as.character(x)))

rownames(base_puntual_ramas2) <- NULL

base_puntual_ramas2 <- base_puntual_ramas2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)

base_puntual_ramas3 <- gather(base_puntual_ramas2, key="cod.variable", value="valor", 2:18 )

base_puntual_ramas3 <- base_puntual_ramas3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         ANO4=as.numeric(ANO4),
         modulo='empleo_ramas')

base_puntual <- bind_rows(base_puntual, base_puntual_ramas3)

remove(base_puntual_ramas, base_puntual_ramas2, base_puntual_ramas3)

# EPH Continua - Total aglomerados ####

## Tasas basicas ####

excel_tasas_basicas <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                       sheet = "28 trim - tasas")

#Datos 2003-2010 hipotesis b
base_hipotesis_b_2003_2010 <- excel_tasas_basicas[23:28, 4:33 ]
#Datos 2011-actual 
base_2011_actual <- excel_tasas_basicas [7:12, 34:ncol(excel_tasas_basicas) ]

base_tasas_basicas <- bind_cols(base_hipotesis_b_2003_2010, base_2011_actual)

base_tasas_basicas <- base_tasas_basicas[3:6,]

base_tasas_basicas <- t(base_tasas_basicas)

ANO4 <- c()
for (i in 2003:2022){
  ANO4 <- c(ANO4, rep(i, 4) )
}

ANO4 <- ANO4[3:length(ANO4)]

trimestre <- c()

for (i in 2003:2022){
  trimestre <- c(trimestre, c(1,2,3,4) )
}

trimestre <- trimestre[3:length(trimestre)]

base_tasas_basicas <- data.frame(ANO4, trimestre, base_tasas_basicas)


colnames(base_tasas_basicas) <-  c("ANO4", "trimestre", "Tasa de actividad (EPH continua)", 
                                           "Tasa de empleo (EPH continua)", "Tasa de desocupación (EPH continua)",
                                           "Tasa de subocupación (EPH continua)")

base_tasas_basicas[, 3:ncol(base_tasas_basicas)] <- sapply(base_tasas_basicas[, 3:ncol(base_tasas_basicas)], as.numeric)

base_tasas_basicas <- base_tasas_basicas %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_tasas_basicas) <- NULL

base_tasas_basicas <- base_tasas_basicas %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         modulo="tasas_basicas")

## Precariedad ####

excel_prec <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                             sheet = "28 trim - prec est")

base_prec <- excel_prec[9:11, 4:ncol(excel_prec) ]

base_prec  <- t(base_prec)

base_prec <- data.frame(ANO4, trimestre, base_prec)

colnames(base_prec) <- c("ANO4", "trimestre", "Protegido (EPH continua)", "Precario (EPH continua)", "s/d (EPH continua)" )

base_prec[, 3:ncol(base_prec)] <- sapply(base_prec[, 3:ncol(base_prec)], as.numeric)

base_prec <- base_prec %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_prec) <- NULL

base_prec <- base_prec %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         modulo="precariedad")

## Categoria ocupacional ####

excel_cat_ocup <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                         sheet = "28 trim - cat ocup est")

base_cat_ocup <- excel_cat_ocup[9:13, 4:ncol(excel_cat_ocup) ]

base_cat_ocup  <- t(base_cat_ocup)

base_cat_ocup <- data.frame(ANO4, trimestre, base_cat_ocup)

colnames(base_cat_ocup) <- c("ANO4", "trimestre","Patrón (EPH continua)", "Cuenta Propia (EPH continua)", 
                             "Asalariado (EPH continua)", "TFSS (EPH continua)", "Desconocido (EPH continua)" )

base_cat_ocup[, 3:ncol(base_cat_ocup)] <- sapply(base_cat_ocup[, 3:ncol(base_cat_ocup)], as.numeric)

base_cat_ocup <- base_cat_ocup %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_cat_ocup) <- NULL

base_cat_ocup <- base_cat_ocup %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         modulo="categoria_ocupacional")

## Categoria ocupacional Pok ####

excel_cat_ocup_pok <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                             sheet = "28 trim - cat pok est")

base_cat_ocup_pok <- excel_cat_ocup_pok[9:17, 4:ncol(excel_cat_ocup_pok) ]

base_cat_ocup_pok  <- t(base_cat_ocup_pok)

base_cat_ocup_pok <- data.frame(ANO4, trimestre, base_cat_ocup_pok)

colnames(base_cat_ocup_pok) <- c("ANO4", "trimestre",
                     "Patrón (s/planes)",
                     "Cuenta Propia (s/planes)",
                     "Asalariados públicos (s/planes)",
                     "Asalariados privados protegidos (s/ serv dom ni planes)",
                     "Asalariados privados precarios (s/ serv dom ni planes)",
                     "Serv dom (s/planes)",
                     "Trabajador familiar",
                     "Planes JJHD",
                     "Desconocidos")

base_cat_ocup_pok[, 3:ncol(base_cat_ocup_pok)] <- sapply(base_cat_ocup_pok[, 3:ncol(base_cat_ocup_pok)], as.numeric)

base_cat_ocup_pok <- base_cat_ocup_pok %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_cat_ocup_pok) <- NULL

base_cat_ocup_pok <- base_cat_ocup_pok %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         modulo="categoria_ocupacional_pok")

## Ramas ####

excel_ramas <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                             sheet = "28 trim - rama est")

base_ramas <- excel_ramas[c(9:10, 16:30), 4:ncol(excel_ramas) ]

base_ramas  <- t(base_ramas)

base_ramas <- data.frame(ANO4, trimestre, base_ramas)

colnames(base_ramas) <-  c("ANO4", "trimestre", col_names_ramas) 

base_ramas[, 3:ncol(base_ramas)] <- sapply(base_ramas[, 3:ncol(base_ramas)], as.numeric)

base_ramas <- base_ramas %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_ramas) <- NULL

base_ramas <- base_ramas %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         modulo="empleo_ramas")

##eph_total_aglos unificada ####

eph_total_aglos <- bind_rows(base_puntual, base_tasas_basicas, base_cat_ocup, base_cat_ocup_pok, base_prec, base_ramas)


# EPH Puntual GBA ####

base_gba_mayo <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                            sheet = "Mayo - GBA", skip = 1)

base_gba_octubre <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                               sheet = "Oct - GBA", skip = 1)

base_gba_puntual1 <- base_gba_mayo[3:32, 1:13]

base_gba_puntual2 <- base_gba_octubre[3:31, 1:13]

base_gba_puntual <- bind_rows(base_gba_puntual1, base_gba_puntual2)

colnames(base_gba_puntual) <- c("ANO4", "onda", "Tasa de actividad (EPH puntual)", "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                                "Tasa de subocupación (EPH puntual)", "Tasa de desocupación (EPH puntual)","Patrón (EPH puntual)",
                                "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)", "Protegido (EPH puntual)", "Precario (EPH puntual)" )

base_gba_puntual[,c(1, 3:ncol(base_gba_puntual))] <- sapply(base_gba_puntual[, c(1, 3:ncol(base_gba_puntual))], as.numeric)

base_gba_puntual <- base_gba_puntual %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)  %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="gba",
         modulo= case_when(
           cod.variable %in%  c("Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
                                "Tasa de desocupación (EPH continua)", "Tasa de subocupación (EPH continua)", "Tasa de actividad (EPH puntual)", 
                                "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                                "Tasa de desocupación (EPH puntual)", "Tasa de subocupación (EPH puntual)") ~ "tasas_basicas", 
           cod.variable %in%  c("Patrón (EPH continua)", "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", 
                                "Patrón (EPH puntual)", "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)")  ~ "categoria_ocupacional", 
           cod.variable %in%  c( "Protegido (EPH continua)", "Precario (EPH continua)",
                                 "Protegido (EPH puntual)", "Precario (EPH puntual)")  ~ "precariedad"
         )) 


# EPH Continua GBA ####

## Tasas basicas ####

excel_tasas_basicas <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                                  sheet = "Aglos trim - tasas")

base_tasas_basicas <- excel_tasas_basicas[c(10, 55, 100, 145 ), 5:ncol(excel_tasas_basicas) ]

base_tasas_basicas <- t(base_tasas_basicas)

ANO4 <- c()
for (i in 2003:2022){
  ANO4 <- c(ANO4, rep(i, 4) )
}

ANO4 <- ANO4[3:length(ANO4)]

trimestre <- c()

for (i in 2003:2022){
  trimestre <- c(trimestre, c(1,2,3,4) )
}

trimestre <- trimestre[3:length(trimestre)]

base_tasas_basicas <- data.frame(ANO4, trimestre, base_tasas_basicas)


colnames(base_tasas_basicas) <-  c("ANO4", "trimestre", "Tasa de actividad (EPH continua)", 
                                   "Tasa de empleo (EPH continua)", "Tasa de desocupación (EPH continua)",
                                   "Tasa de subocupación (EPH continua)")

base_tasas_basicas[, 3:ncol(base_tasas_basicas)] <- sapply(base_tasas_basicas[, 3:ncol(base_tasas_basicas)], as.numeric)

base_tasas_basicas <- base_tasas_basicas %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_tasas_basicas) <- NULL

base_tasas_basicas <- base_tasas_basicas %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="gba",
         modulo="tasas_basicas")

## Precariedad ####

excel_prec <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xlsx",
                         sheet = "28 trim - prec x aglom part")

base_prec <- excel_prec[c(11), 5:ncol(excel_prec) ]

base_prec  <- t(base_prec)

base_prec <- data.frame(ANO4, trimestre, protegidos = base_prec) %>% 
  mutate(proteg = 100-as.numeric(protegidos))

colnames(base_prec) <- c("ANO4", "trimestre", "Precario (EPH continua)", "Protegido (EPH continua)" )

base_prec[, 3:ncol(base_prec)] <- sapply(base_prec[, 3:ncol(base_prec)], as.numeric)

base_prec <- base_prec %>%
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
  select(-trimestre)

rownames(base_prec) <- NULL

base_prec <- base_prec %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="gba",
         modulo="precariedad")


## eph_gba unificada ####

eph_gba <- bind_rows(base_gba_puntual, base_tasas_basicas, base_prec) 

# base final eph ####

eph <- bind_rows(eph_total_aglos, eph_gba)

saveRDS(eph, file = "www/data/eph.RDS")

