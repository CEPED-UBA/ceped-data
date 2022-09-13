library(readxl)

# EPH Puntual - Total aglomerados ####

base_mayo <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                        sheet = "Mayo - 28 Aglo", skip = 1)

base_octubre <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                           sheet = "Oct - 28 Aglo", skip = 1)

base_puntual1 <- base_mayo[3:32, 1:13]

base_puntual2 <- base_octubre[3:31, 1:13]

base_puntual <- bind_rows(base_puntual1, base_puntual2)

colnames(base_puntual) <- c("ANO4", "onda", "Tasa de actividad (EPH puntual)", "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                            "Tasa de desocupación (EPH puntual)", "Tasa de subocupación (EPH puntual)","Patrón (EPH puntual)",
                            "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)", "Protegido (EPH puntual)", "Precario (EPH puntual)" )

base_puntual[, 3:ncol(base_puntual)] <- sapply(base_puntual[, 3:ncol(base_puntual)], as.numeric)

base_puntual <- base_puntual %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)

# EPH Continua - Total aglomerados ####


base_1er <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                        sheet = "1°trim - 28 Aglo", skip = 1)
base_1er <- base_1er[3:21, 1:13]

base_2do <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                           sheet = "2°trim - 28 Aglo", skip = 1)
base_2do <- base_2do[3:21, 1:13]

base_3er <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                       sheet = "3°trim - 28 Aglo", skip = 1)
base_3er <- base_3er[3:21, 1:13]

base_4to <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
                       sheet = "4°trim - 28 Aglo", skip = 1)
base_4to <- base_4to[3:21, 1:13]

base_continua <- bind_rows(base_1er, base_2do, base_3er, base_4to)

colnames(base_continua) <- c("ANO4", "trimestre", "Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
                            "Tasa de desocupación (EPH continua)", "Tasa de subocupación (EPH continua)","Patrón (EPH continua)",
                            "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", "Protegido (EPH continua)", "Precario (EPH continua)" )

base_continua[, 3:ncol(base_continua)] <- sapply(base_continua[, 3:ncol(base_continua)], as.numeric)

base_continua <- base_continua %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

# EPH total aglos unificada ####

eph_total_aglos <- bind_rows(base_puntual, base_continua) %>% 
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

# EPH Puntual GBA ####

base_gba_mayo <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                                       sheet = "Mayo - GBA", skip = 1)

base_gba_octubre <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                           sheet = "Oct - GBA", skip = 1)

base_gba_puntual1 <- base_gba_mayo[3:32, 1:13]

base_gba_puntual2 <- base_gba_octubre[3:31, 1:13]

base_gba_puntual <- bind_rows(base_gba_puntual1, base_gba_puntual2)

colnames(base_gba_puntual) <- c("ANO4", "onda", "Tasa de actividad (EPH puntual)", "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                    "Tasa de desocupación (EPH puntual)", "Tasa de subocupación (EPH puntual)","Patrón (EPH puntual)",
                    "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)", "Protegido (EPH puntual)", "Precario (EPH puntual)" )

base_gba_puntual[,c(1, 3:ncol(base_gba_puntual))] <- sapply(base_gba_puntual[, c(1, 3:ncol(base_gba_puntual))], as.numeric)

base_gba_puntual <- base_gba_puntual %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", onda)) %>% 
  select(-onda)

# EPH Continua GBA ####

base_gba_1er <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                       sheet = "1°trim - GBA", skip = 1)
base_gba_1er <- base_gba_1er[4:nrow(base_gba_1er), 1:13]

base_gba_2do <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                       sheet = "2°trim - GBA", skip = 1)
base_gba_2do <- base_gba_2do[4:nrow(base_gba_2do), 1:13]

base_gba_3er <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                       sheet = "3°trim - GBA", skip = 1)
base_gba_3er <- base_gba_3er[4:nrow(base_gba_3er), 1:13]

base_gba_4to <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
                       sheet = "4°trim - GBA", skip = 1)
base_gba_4to <- base_gba_4to[4:nrow(base_gba_4to), 1:13]

base_gba_continua <- bind_rows(base_gba_1er, base_gba_2do, base_gba_3er, base_gba_4to)

colnames(base_gba_continua) <- c("ANO4", "trimestre", "Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
                             "Tasa de desocupación (EPH continua)", "Tasa de subocupación (EPH continua)","Patrón (EPH continua)",
                             "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", "Protegido (EPH continua)", "Precario (EPH continua)" )

base_gba_continua[, c(1, 3:ncol(base_gba_continua))] <- sapply(base_gba_continua[, c(1, 3:ncol(base_gba_continua))], as.numeric)

base_gba_continua <- base_gba_continua %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

# EPH gba unificada ####

eph_gba <- bind_rows(base_gba_puntual, base_gba_continua) %>% 
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

eph <- bind_rows(eph_total_aglos, eph_gba)

saveRDS(eph, file = "www/data/eph.RDS")

#### Categorias ocupacionales POK | 28 aglomerados 2003-2021 ####

base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "28 trim - cat pok est")

base2 <- base[7:17,]

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

colnames(base2) <- c("ANO4", "trimestre", 
                     "Patrón (s/planes)",
                     "Cuenta Propia (s/planes)",
                     "Asalariados públicos (s/planes)",
                     "Asalariados privados protegidos (s/ serv dom ni planes)",
                     "Asalariados privados precarios (s/ serv dom ni planes)",
                     "Serv dom (s/planes)",
                     "Trabajador familiar",
                     "Planes JJHD",
                     "Desconocidos"
)

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

rownames(base2) <- NULL

base2 <- base2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base3 <- gather(base2, key="cod.variable", value="valor", 2:10 )

base3 <- base3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG") %>% 
  mutate(aglomerados="28_aglos", 
         modulo="categoria_ocupacional_pok")

#  Junto pok con la otra base (MODIFICAR LUEGO PARA QUE CORRA BIEN TODO DE UNA) ####

eph <- readRDS("www/data/eph.RDS") 

eph <- bind_rows(eph, base3)

saveRDS(eph, file = "www/data/eph.RDS")
