# EPH Continua - Total aglomerados ####
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

caes <- eph::caes # Defino tabla de CAES porque aveces el paquete eph no lo encuentra

base.eph <- base.eph %>%
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
                                       "Ns/Nc")), 
         rama.ceped = 
           case_when(
             caes_seccion_cod %in% c("A","B") ~ "Actividades primarias",
             PP04B_COD %in% c(10:12,1000:1299) ~ "Alimentos, bebidas y tabaco",
             PP04B_COD %in% c(13:15,1300:1599) ~ "Textiles y confecciones",
             PP04B_COD %in% c(19:22,1900:2299)  ~ "Productos químicos", 
             PP04B_COD %in% c(24:30,2400:3099) ~ "Productos metálicos, maquinaria y equipo",
             PP04B_COD %in% c(16:18,1600:1899,
                              23:23,2300:2399,
                              31:33,3100:3399,
                              58:58,5800:5899) ~ "Otras industrias manufactureras",
             PP04B_COD %in% c(35:39,3500:3900) ~ "Elec, Gas y Agua",
             PP04B_COD %in% c(45:48,4500:4899) ~ "Comercio y Reparaciones",
             PP04B_COD %in% c(59:61,5900:6199) ~ "Servicios de correo y telecomunicaciones",
             PP04B_COD %in% c(62:63,6200:6399,
                              69:75,6900:7599,
                              77:82,7700:8299) ~ "Servicios empresariales y de alquiler",
             PP04B_COD %in% c(90:96,9000:9699,
                              99,9900:9998) ~ "Otros servicios sociales y comunitarios", 
             TRUE ~ str_to_sentence(caes_seccion_label))) 

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


##Agregado información empleo por ramas ####
total_rama_ponderado <- base.eph %>% 
  group_by(ANO4, TRIMESTRE, rama.ceped) %>% 
  summarise(rama_ponderado= sum(PONDERA[!is.na(rama.ceped)]))

total_rama_trimestre <- base.eph %>% 
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(rama_total= sum(PONDERA[!is.na(rama.ceped)]))

tabla_rama <- left_join(total_rama_ponderado, total_rama_trimestre, by=c("ANO4", "TRIMESTRE")) %>% 
  ungroup() %>% 
  mutate(valor=(rama_ponderado/rama_total)*100) %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", TRIMESTRE)) %>% 
  mutate(ANO4.trim=str_sub(ANO4.trim, end = 6)) %>% 
  select(ANO4.trim, rama.ceped, valor) %>%           #Proximos 4 comandos para completar con valores NA     
  complete(ANO4.trim, rama.ceped) %>% 
  mutate(ANO4=str_sub(ANO4.trim, end = 4)) %>% 
  select(ANO4, ANO4.trim, rama.ceped, valor) %>% 
  rename( cod.variable= rama.ceped) %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG", 
         aglomerados="total_aglos",
         ANO4=as.numeric(ANO4),
         modulo='empleo_ramas') %>% 
  filter(!is.na(cod.variable))%>% 
  sjlabelled::remove_all_labels() %>% 
  tibble() 

base_continua <- bind_rows(base_continua, tabla_rama)
