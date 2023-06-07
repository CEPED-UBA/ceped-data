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

# EPH Continua - Total aglomerados ####

## Versión vieja ####
# base_1er <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
#                         sheet = "1°trim - 28 Aglo", skip = 1)
# base_1er <- base_1er[3:21, 1:13]
# 
# base_2do <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
#                            sheet = "2°trim - 28 Aglo", skip = 1)
# base_2do <- base_2do[3:21, 1:13]
# 
# base_3er <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
#                        sheet = "3°trim - 28 Aglo", skip = 1)
# base_3er <- base_3er[3:21, 1:13]
# 
# base_4to <- read_excel("crudo/datos/03. EPH_Total Aglomerados_empleo_Total.xlsx", 
#                        sheet = "4°trim - 28 Aglo", skip = 1)
# base_4to <- base_4to[3:21, 1:13]
# 
# base_continua <- bind_rows(base_1er, base_2do, base_3er, base_4to)
# 
# colnames(base_continua) <- c("ANO4", "trimestre", "Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
#                             "Tasa de subocupación (EPH continua)", "Tasa de desocupación (EPH continua)","Patrón (EPH continua)",
#                             "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", "Protegido (EPH continua)", "Precario (EPH continua)" )
# 
# base_continua[, 3:ncol(base_continua)] <- sapply(base_continua[, 3:ncol(base_continua)], as.numeric)
# 
# base_continua <- base_continua %>% 
#   mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
#   select(-trimestre)

## Versión Nueva ####
### Descarga de bases con paquete eph | MODIFICAR ACA PARA ACTUALIZAR ####

# variables <- c("ANO4", "TRIMESTRE", "AGLOMERADO", "ESTADO", "INTENSI", "CH06", "PP07H",
#                "CAT_OCUP", "PP04A", "PONDERA", "PP04B_COD", "PP04B1", "PJ1_1")
# 
# base.eph2003_2010 <- eph::get_microdata(year = 2003:2010,trimester = 1:4, vars = variables )
# 
# variables <- c("ANO4", "TRIMESTRE","AGLOMERADO", "ESTADO", "INTENSI", "CH06", "PP07H",
#                "CAT_OCUP", "PP04A", "PONDERA", "PP04B_COD", "PP04B_CAES")
# 
# base.eph2011_2015 <- eph::get_microdata(year = 2011:2015,trimester = 1:4, vars = variables )
# 
# variables <- c("ANO4", "TRIMESTRE","AGLOMERADO", "ESTADO", "INTENSI", "CH06", "PP07H",
#                "CAT_OCUP", "PP04A", "PONDERA", "PP04B_COD")
# 
# base.eph2016_2021 <- eph::get_microdata(year = 2016:2022,trimester = 1:4, vars = variables )
# 
# base.eph <- bind_rows(base.eph2003_2010, base.eph2011_2015, base.eph2016_2021)
# 
# saveRDS(base.eph, "crudo/datos/eph_descargada.RDS")

### Procesamiento####

base.eph <- readRDS("crudo/datos/eph_descargada.RDS") 

base.eph$CAT_OCUP[is.na(base.eph$CAT_OCUP)] <- 0              #Corrección porque CAT_OCUP tiene NAs para 2017

base.eph <- base.eph %>%
  #  filter(AGLOMERADO %in% 32:33) %>% 
  mutate(
    ESTADO = case_when(
      PJ1_1==1 & ESTADO==1 ~ 2,                              # planes jjhd van como desocupados
      TRUE                ~ ESTADO)) %>% 
  eph::organize_caes() %>%
  eph::organize_labels() %>% 
  mutate(caes_seccion_cod=case_when(PP04B1==1 ~ "T",          #Corrección por problema en el codigo de actividad para servicio domestico para varios años
                                    TRUE ~ caes_seccion_cod))

base.eph <- base.eph   %>% 
  mutate(
    condicion = case_when(
      ESTADO == 1 & INTENSI !=1  ~ "ocupados.plenos",
      ESTADO == 1 & INTENSI ==1  ~ "subocupados",
      ESTADO == 2                ~ "desocupados",
      ESTADO %in%  c(0,3:4)|is.na(ESTADO) ~ "inactivos.y.menores"),
    calidad = case_when(PP07H==1~"Protegido",
                        PP07H==2~"Precario",
                        PP07H==0~"Ns/Nc"),
    cat.indec = case_when(
      PJ1_1== 1   ~ "planes jjhd",
      CAT_OCUP==1 ~ "patrones (s/planes)",
      CAT_OCUP==2 ~ "cuentapropistas (s/planes)",
      CAT_OCUP==3 & PP04A==1 & caes_seccion_cod !="T"~"asalariados publicos (s/planes)",
      CAT_OCUP==3 & PP04A==2 & PP07H==1 & caes_seccion_cod!="T"~"asalariados privados protegidos (s/serv dom ni planes)",
      CAT_OCUP==3 & PP04A==2 & PP07H==2 & caes_seccion_cod!="T"~"asalariados privados precarios (s/serv dom ni planes)",
      CAT_OCUP==3 & caes_seccion_cod=="T"~"servicio domestico (s/planes)",
      CAT_OCUP==4~ "trabajador sin salario",
      TRUE ~"Ns/Nc"))%>%
  mutate(cat.indec = factor(cat.indec,
                            levels = c("patrones (s/planes)",
                                       "cuentapropistas (s/planes)",
                                       "asalariados publicos (s/planes)",
                                       "asalariados privados protegidos (s/serv dom ni planes)",
                                       "asalariados privados precarios (s/serv dom ni planes)",
                                       "servicio domestico (s/planes)",
                                       "trabajador sin salario",
                                       "planes jjhd",
                                       "Ns/Nc"))) 
### Resultados ####

tabla <- base.eph %>% 
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(
    poblacion=sum(PONDERA),
    ocupados.plenos=sum(PONDERA[condicion == "ocupados.plenos"]), 
    subocupados=sum(PONDERA[condicion == "subocupados"]),
    desocupados=sum(PONDERA[condicion == "desocupados"]),
    PEA=ocupados.plenos + subocupados + desocupados,
    inactivos.y.menores=sum(PONDERA[condicion == "inactivos.y.menores"]),
    Protegido=sum(PONDERA[calidad == "Protegido"], na.rm = TRUE), 
    Precario=sum(PONDERA[calidad == "Precario"], na.rm = TRUE), 
    Patron=sum(PONDERA[CAT_OCUP == 1]), 
    Cuentapropia=sum(PONDERA[CAT_OCUP == 2]), 
    Asalariado=sum(PONDERA[CAT_OCUP == 3]),
    TFSS =sum(PONDERA[CAT_OCUP == 4]), 
    total_cat.indec=sum(PONDERA[cat.indec!="Ns/Nc"]),
    'Tasa de actividad (EPH continua)'                  = PEA/poblacion,
    'Tasa de empleo (EPH continua)'                     = (ocupados.plenos+subocupados)/poblacion,
    'Tasa de empleo pleno (EPH continua)'               = ocupados.plenos/poblacion,
    'Tasa de desocupación (EPH continua)'               = desocupados/PEA,
    'Tasa de subocupación (EPH continua)'               = subocupados/PEA,
    'Protegido (EPH continua)'                           = Protegido/(Precario + Protegido), 
    'Precario (EPH continua)'                           = Precario/(Precario + Protegido),
    'Patrón (EPH continua)'                             =  Patron /(Patron + Cuentapropia + Asalariado + TFSS) ,                         
    'Cuenta Propia (EPH continua)'                      =  Cuentapropia /(Patron + Cuentapropia + Asalariado + TFSS) ,     
    'Asalariado (EPH continua)'                         =  Asalariado /(Patron + Cuentapropia + Asalariado + TFSS) ,     
    'TFSS (EPH continua)'                               =  TFSS /(Patron + Cuentapropia + Asalariado + TFSS) ,
    'Patrón (s/planes)'                                 = sum(PONDERA[cat.indec=="patrones (s/planes)"]) / total_cat.indec,
    'Cuenta Propia (s/planes)'                          = sum(PONDERA[cat.indec=="cuentapropistas (s/planes)"]) / total_cat.indec,     
    'Asalariados públicos (s/planes)'                   = sum(PONDERA[cat.indec=="asalariados publicos (s/planes)"]) / total_cat.indec,
    'Asalariados privados protegidos (s/serv dom ni planes)'= sum(PONDERA[cat.indec=="asalariados privados protegidos (s/serv dom ni planes)"])  / total_cat.indec,
    'Asalariados privados precarios (s/serv dom ni planes)' = sum(PONDERA[cat.indec=="asalariados privados precarios (s/serv dom ni planes)"])  / total_cat.indec,
    'Serv dom (s/planes)'                               = sum(PONDERA[cat.indec=="servicio domestico (s/planes)"]) / total_cat.indec,    
    'Trabajador familiar'                               = sum(PONDERA[cat.indec=="trabajador sin salario"]) / total_cat.indec,     
    'Planes JJHD'                                       = sum(PONDERA[cat.indec=="planes jjhd"])/ total_cat.indec)

base_continua <- tabla %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", TRIMESTRE)) %>% 
  mutate(ANO4.trim=str_sub(ANO4.trim, end = 6)) %>% 
  select(-TRIMESTRE) %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  select(c(1,2, 16:ncol(.))) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         ANO4=as.numeric(ANO4), 
         valor=valor*100,
         modulo= case_when(
           cod.variable %in%  c("Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
                                "Tasa de desocupación (EPH continua)", "Tasa de subocupación (EPH continua)", "Tasa de actividad (EPH puntual)", 
                                "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                                "Tasa de desocupación (EPH puntual)", "Tasa de subocupación (EPH puntual)") ~ "tasas_basicas", 
           cod.variable %in%  c("Patrón (EPH continua)", "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", 
                                "Patrón (EPH puntual)", "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)")  ~ "categoria_ocupacional", 
           cod.variable %in%  c( "Protegido (EPH continua)", "Precario (EPH continua)",
                                 "Protegido (EPH puntual)", "Precario (EPH puntual)")  ~ "precariedad", 
           TRUE ~ "categoria_ocupacional_pok"
         )) %>% 
  ungroup() %>% 
  sjlabelled::remove_all_labels() %>% 
  tibble() 



### eph_total_aglos unificada ####

eph_total_aglos <- bind_rows(base_puntual, base_continua)

remove(base_puntual, base_continua, tabla)

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


remove(base_gba_mayo, base_gba_octubre, base_gba_puntual1, base_gba_puntual2)

# EPH Continua GBA ####
##Version vieja ####
# # base_gba_1er <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
# #                        sheet = "1°trim - GBA", skip = 1)
# # base_gba_1er <- base_gba_1er[4:nrow(base_gba_1er), 1:13]
# # 
# # base_gba_2do <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
# #                        sheet = "2°trim - GBA", skip = 1)
# # base_gba_2do <- base_gba_2do[4:nrow(base_gba_2do), 1:13]
# # 
# # base_gba_3er <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
# #                        sheet = "3°trim - GBA", skip = 1)
# # base_gba_3er <- base_gba_3er[4:nrow(base_gba_3er), 1:13]
# # 
# # base_gba_4to <- read_excel("crudo/datos/01. EPH_GBA_empleo_Total.xlsx", 
# #                        sheet = "4°trim - GBA", skip = 1)
# # base_gba_4to <- base_gba_4to[4:nrow(base_gba_4to), 1:13]
# # 
# # base_gba_continua <- bind_rows(base_gba_1er, base_gba_2do, base_gba_3er, base_gba_4to)
# # 
# # colnames(base_gba_continua) <- c("ANO4", "trimestre", "Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
# #                              "Tasa de subocupación (EPH continua)", "Tasa de desocupación (EPH continua)","Patrón (EPH continua)",
# #                              "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", "Protegido (EPH continua)", "Precario (EPH continua)" )
# # 
# # base_gba_continua[, c(1, 3:ncol(base_gba_continua))] <- sapply(base_gba_continua[, c(1, 3:ncol(base_gba_continua))], as.numeric)
# # 
# # base_gba_continua <- base_gba_continua %>% 
# #   mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
# #   select(-trimestre)
# 
# base_gba_continua <- readRDS("www/data/eph_gba.rds") %>% 
#   select(c("ANO4", "TRIMESTRE", "Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
#            "Tasa de subocupación (EPH continua)", "Tasa de desocupación (EPH continua)","Patrón (EPH continua)",
#            "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", 
#            "Protegido (EPH continua)", "Precario (EPH continua)" )) %>% 
#   mutate(TRIMESTRE2= case_when(                                         #Hay que quitarle etiquetas (ver si mejor saca etiquetas cuando se procesa el crudo)
#     TRIMESTRE==1 ~ 1, 
#     TRIMESTRE==2 ~ 2, 
#     TRIMESTRE==3 ~ 3, 
#     TRIMESTRE==4 ~ 4), 
#     ANO4=remove_var_label(ANO4)) %>% 
#   mutate(ANO4.trim= paste0(ANO4, ".", TRIMESTRE2)) %>% 
#   select(-c("TRIMESTRE", "TRIMESTRE2")) %>%
#   mutate(across(where(is.numeric), ~ .x * 100))

##Version nueva ####

tabla <- base.eph %>% 
  filter(AGLOMERADO %in% 32:33) %>%    #Filtro para GBA
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(
    poblacion=sum(PONDERA),
    ocupados.plenos=sum(PONDERA[condicion == "ocupados.plenos"]), 
    subocupados=sum(PONDERA[condicion == "subocupados"]),
    desocupados=sum(PONDERA[condicion == "desocupados"]),
    PEA=ocupados.plenos + subocupados + desocupados,
    inactivos.y.menores=sum(PONDERA[condicion == "inactivos.y.menores"]),
    Protegido=sum(PONDERA[calidad == "Protegido"], na.rm = TRUE), 
    Precario=sum(PONDERA[calidad == "Precario"], na.rm = TRUE), 
    Patron=sum(PONDERA[CAT_OCUP == 1]), 
    Cuentapropia=sum(PONDERA[CAT_OCUP == 2]), 
    Asalariado=sum(PONDERA[CAT_OCUP == 3]),
    TFSS =sum(PONDERA[CAT_OCUP == 4]), 
    total_cat.indec=sum(PONDERA[cat.indec!="Ns/Nc"]),
    'Tasa de actividad (EPH continua)'                  = PEA/poblacion,
    'Tasa de empleo (EPH continua)'                     = (ocupados.plenos+subocupados)/poblacion,
    'Tasa de empleo pleno (EPH continua)'               = ocupados.plenos/poblacion,
    'Tasa de desocupación (EPH continua)'               = desocupados/PEA,
    'Tasa de subocupación (EPH continua)'               = subocupados/PEA,
    'Protegido (EPH continua)'                           = Protegido/(Precario + Protegido), 
    'Precario (EPH continua)'                           = Precario/(Precario + Protegido),
    'Patrón (EPH continua)'                             =  Patron /(Patron + Cuentapropia + Asalariado + TFSS) ,                         
    'Cuenta Propia (EPH continua)'                      =  Cuentapropia /(Patron + Cuentapropia + Asalariado + TFSS) ,     
    'Asalariado (EPH continua)'                         =  Asalariado /(Patron + Cuentapropia + Asalariado + TFSS) ,     
    'TFSS (EPH continua)'                               =  TFSS /(Patron + Cuentapropia + Asalariado + TFSS) ,
    'Patrón (s/planes)'                                 = sum(PONDERA[cat.indec=="patrones (s/planes)"]) / total_cat.indec,
    'Cuenta Propia (s/planes)'                          = sum(PONDERA[cat.indec=="cuentapropistas (s/planes)"]) / total_cat.indec,     
    'Asalariados públicos (s/planes)'                   = sum(PONDERA[cat.indec=="asalariados publicos (s/planes)"]) / total_cat.indec,
    'Asalariados privados protegidos (s/serv dom ni planes)'= sum(PONDERA[cat.indec=="asalariados privados protegidos (s/serv dom ni planes)"])  / total_cat.indec,
    'Asalariados privados precarios (s/serv dom ni planes)' = sum(PONDERA[cat.indec=="asalariados privados precarios (s/serv dom ni planes)"])  / total_cat.indec,
    'Serv dom (s/planes)'                               = sum(PONDERA[cat.indec=="servicio domestico (s/planes)"]) / total_cat.indec,    
    'Trabajador familiar'                               = sum(PONDERA[cat.indec=="trabajador sin salario"]) / total_cat.indec,     
    'Planes JJHD'                                       = sum(PONDERA[cat.indec=="planes jjhd"])/ total_cat.indec)

base_gba_continua <- tabla %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", TRIMESTRE)) %>% 
  mutate(ANO4.trim=str_sub(ANO4.trim, end = 6)) %>% 
  select(-TRIMESTRE) %>% 
  relocate(ANO4.trim, .after=ANO4) %>% 
  select(c(1,2, 16:ncol(.))) %>% 
  gather(., key="cod.variable", value="valor", 3:ncol(.)) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         ANO4=as.numeric(ANO4), 
         valor=valor*100,
         modulo= case_when(
           cod.variable %in%  c("Tasa de actividad (EPH continua)", "Tasa de empleo (EPH continua)", "Tasa de empleo pleno (EPH continua)",
                                "Tasa de desocupación (EPH continua)", "Tasa de subocupación (EPH continua)", "Tasa de actividad (EPH puntual)", 
                                "Tasa de empleo (EPH puntual)", "Tasa de empleo pleno (EPH puntual)",
                                "Tasa de desocupación (EPH puntual)", "Tasa de subocupación (EPH puntual)") ~ "tasas_basicas", 
           cod.variable %in%  c("Patrón (EPH continua)", "Cuenta Propia (EPH continua)", "Asalariado (EPH continua)", "TFSS (EPH continua)", 
                                "Patrón (EPH puntual)", "Cuenta Propia (EPH puntual)", "Asalariado (EPH puntual)", "TFSS (EPH puntual)")  ~ "categoria_ocupacional", 
           cod.variable %in%  c( "Protegido (EPH continua)", "Precario (EPH continua)",
                                 "Protegido (EPH puntual)", "Precario (EPH puntual)")  ~ "precariedad", 
           TRUE ~ "categoria_ocupacional_pok"
         )) %>% 
  ungroup() %>% 
  sjlabelled::remove_all_labels() %>% 
  tibble() 




### eph_gba unificada ####

eph_gba <- bind_rows(base_gba_puntual, base_gba_continua) 

remove(base_gba_puntual, base_gba_continua)

eph <- bind_rows(eph_total_aglos, eph_gba)
saveRDS(eph, file = "www/data/eph.RDS")


# Categorias ocupacionales POK | 28 aglomerados 2003-2021 ####
## Version vieja (Ahora se calculan por paquete eph)
# base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls",
#                    sheet = "28 trim - cat pok est")
# 
# base2 <- base[7:17,]
# 
# base2 <- base2 %>%
#   rename(index = 1)
# 
# #Traspongo y pongo nombre de variables
# base2 <- data.frame( t(base2[,-1]) )
# colnames(base2) <- base$index
# 
# #Genero vector años
# ANO4 <- c()
# for (i in 2003:2021){
#   ANO4 <- c(ANO4, rep(i, 4) )
# }
# 
# base2[1] <- ANO4
# 
# #Genero vector trimestre
# trimestre <- c()
# for (i in 2003:2021){
#   trimestre <- c(trimestre, seq(1, 4) )
# }
# 
# base2[2] <- trimestre
# 
# colnames(base2) <- c("ANO4", "trimestre",
#                      "Patrón (s/planes)",
#                      "Cuenta Propia (s/planes)",
#                      "Asalariados públicos (s/planes)",
#                      "Asalariados privados protegidos (s/ serv dom ni planes)",
#                      "Asalariados privados precarios (s/ serv dom ni planes)",
#                      "Serv dom (s/planes)",
#                      "Trabajador familiar",
#                      "Planes JJHD",
#                      "Desconocidos"
# )
# 
# base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))
# 
# rownames(base2) <- NULL
# 
# base2 <- base2 %>%
#   mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>%
#   select(-trimestre)
# 
# base3 <- gather(base2, key="cod.variable", value="valor", 2:10 )
# 
# base3 <- base3 %>%
#   mutate(nombre.pais="Argentina",
#          iso3c="ARG") %>%
#   mutate(aglomerados="28_aglos",
#          modulo="categoria_ocupacional_pok")
# 
# Junto pok con la otra base
# 
# 
# eph <- bind_rows(eph, base3)




