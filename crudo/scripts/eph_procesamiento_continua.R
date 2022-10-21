
library(eph)
library(tidyverse)
library(openxlsx)

#### Descarga de datos desde paquete eph####

variables <- c("AGLOMERADO", "ESTADO", "INTENSI", "CH06", "PP07H",
               "CAT_OCUP", "PP04A", "PONDERA", "PP04B_COD", "ANO4", "TRIMESTRE", "PJ1_1")
base.eph2003_2010 <- eph::get_microdata(year = 2003:2010,trimester = 1:4, vars = variables )

variables <- c("AGLOMERADO", "ESTADO", "INTENSI", "CH06", "PP07H",
               "CAT_OCUP", "PP04A", "PONDERA", "PP04B_COD", "ANO4", "TRIMESTRE")
base.eph2011_2021 <- eph::get_microdata(year = 2011:2021,trimester = 1:4, vars = variables )

base.eph <- bind_rows(base.eph2003_2010, base.eph2011_2021)
 
saveRDS(base.eph, "crudo/datos/eph.RDS")

#### Procesamiento

base.eph <- readRDS("crudo/datos/eph.RDS") 

base.eph <- base.eph %>%
  filter(AGLOMERADO %in% 32:33) %>% 
   mutate(
     ESTADO = case_when(
       PJ1_1==1 & ESTADO==1 ~ 2,              # planes jjhd van como desocupados
       TRUE                ~ ESTADO)) %>% 
  eph::organize_caes() %>%
  eph::organize_labels()

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
#### resultados ####

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
            'Protegido (EPH puntual)'                           = Protegido/(Precario + Protegido), 
            'Precario (EPH continua)'                           = Precario/(Precario + Protegido),
            'Patrón (EPH continua)'                             =  Patron /(Patron + Cuentapropia + Asalariado + TFSS) ,                         
            'Cuenta Propia (EPH continua)'                      =  Cuentapropia /(Patron + Cuentapropia + Asalariado + TFSS) ,     
            'Asalariado (EPH continua)'                         =  Asalariado /(Patron + Cuentapropia + Asalariado + TFSS) ,     
            'TFSS (EPH continua)'                               =  TFSS /(Patron + Cuentapropia + Asalariado + TFSS) ,
            'Patrón (s/planes)'                                 = sum(PONDERA[cat.indec=="patrones (s/planes)"]) / total_cat.indec,
            'Cuenta Propia (s/planes)'                          = sum(PONDERA[cat.indec=="cuentapropistas (s/planes)"]) / total_cat.indec,     
            'Asalariados públicos (s/planes)'                   = sum(PONDERA[cat.indec=="asalariados publicos (s/planes)"]) / total_cat.indec,
            'Asalariados privados protegidos (s/ serv dom ni planes)'= sum(PONDERA[cat.indec==   "asalariados privados protegidos (s/serv dom ni planes)"])  / total_cat.indec,
            'Asalariados privados precarios (s/ serv dom ni planes)' = sum(PONDERA[cat.indec=="asalariados privados precarios (s/serv dom ni planes)"])  / total_cat.indec,
            'Serv dom (s/planes)'                               = sum(PONDERA[cat.indec=="servicio domestico (s/planes)"]) / total_cat.indec,    
            'Trabajador familiar'                               = sum(PONDERA[cat.indec=="trabajador sin salario"]) / total_cat.indec,     
            'Planes JJHD'                                       = sum(PONDERA[cat.indec=="planes jjhd"])/ total_cat.indec)

write.xlsx(tabla, "www/data/eph_gba.xlsx")  






