#Librerias####
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)
library(shinydashboard)
library(openxlsx)
library(tidyverse)
library(ggthemes)
library(colorspace)
library(readxl)
library(ggplot2)
library(lubridate)

options(scipen=999)

paleta_colores <- c("#41bdcd","#7ccb29","#cce45b","#f9cb27","#fc941d","#e45335")

paleta_colores_extendida <- c(paleta_colores,"#cc0000","#e69138", "#4169e1" ,"#3d85c6","#000080", "#c576f6", "#3a7480", "#0c4f5d","#005c29","#7a1606","#ff9cd1")

paleta_colores_extendida2 <- c(paleta_colores,"#cc0000","#e69138", "#4169e1" ,"#3d85c6","#000080", "#c576f6", "#3a7480", "#0c4f5d","#005c29","#7a1606","#ff9cd1", "#FF00FF", "#FFFF00", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FF4500", "#00CED1", "#FF1493", "#8B4513", "#808000", "#008080", "#D2691E", "#7B68EE", "#4682B4", "#B0C4DE", "#FF00FF", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FF4500", "#00CED1", "#FF1493", "#8B4513", "#808000", "#008080", "#D2691E", "#7B68EE", "#4682B4", "#B0C4DE", "#FF00FF", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FF4500")



#Diccionario variables####
diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")

# Importacion de bases ####
## BP####
bop_arg_dolares <- readRDS("www/data/bop_arg_dolares.RDS") %>% 
  mutate(codigo_y_variable = paste0(Codigo," - ",cod.variable),
         Nivel = as.character(Nivel))
bop_sectores <- readRDS("www/data/bop_sectores.RDS") %>% 
  mutate(codigo_y_variable = paste0(Codigo," - ",cod.variable))
bop_dolares_diccionario <- readRDS("www/data/bop_dolares_diccionario.RDS") 
bop_sectores_diccionario <- readRDS("www/data/bop_sectores_diccionario.RDS") 
bop_dolares_diccionario_aclaracion <- readRDS("www/data/bop_dolares_diccionario_aclaracion.RDS") 
min_bop <- min(as.numeric(bop_sectores$ANO4))
max_bop <- max(as.numeric(bop_sectores$ANO4))

#serie_salarios2 <- readRDS("www/data/salarios.RDS")
serie_salarios <- readRDS("www/data/DT_salarios_internac.RDS")
tipo_cambio_argentina <- readRDS("www/data/Tipo_Cambio_Arg.RDS")
base_ipc <- readRDS("www/data/base_ipc.RDS")

eph <- readRDS("www/data/eph.RDS")
tabla_aglos <- readRDS("www/data/tabla_aglos.RDS")
pobreza <- readRDS("www/data/pobreza.RDS")
canastas <- readRDS("www/data/canastas.RDS")

## Funcional ####
diccionario_dt24 <- read.xlsx(xlsxFile = "www/data/diccionario_dt24.xlsx") 
base_dt24 <- readRDS(file = "www/data/base_dt24.RDS") %>% 
  mutate(valor = round(valor,digits = 2))
base_export_dt24 <- base_dt24 %>% 
  pivot_wider(names_from = sector,values_from = valor) %>% 
  arrange(variable)
periodo_min_dt24 <- min(base_export_dt24$Anio)
periodo_max_dt24 <- max(base_export_dt24$Anio)

## SALARIOS #####
# salarios <- serie_salarios %>% 
#   filter(cod.variable %in%  c("salario_relativo_usa_c_actual",
#                               "salario_relativo_usa_c_priv",
#                               "salario_ppa_c_priv_real",
#                               "salario_ppa_c_actual_real"))

salarios <- serie_salarios %>% 
  filter(cod.variable %in%  c("salario_relativo_usa_c_priv",
                              "salario_ppa_c_priv_real"))
min_salarios <- min(salarios$ANO4)
max_salarios <- max(salarios$ANO4)
##IPC_TIPO_CAMBIO####

v_ipc <- diccionario_variables %>% filter(base=="IPC_Argentina") %>%  pull(nombre.variable)      

min_ipc <- min(base_ipc$ANO4)
max_ipc <- max(base_ipc$ANO4)

min_trim <- min(base_ipc$ANO4[base_ipc$cod.variable == "Trimestres_EPH_2017"])
max_trim <- max(base_ipc$ANO4[base_ipc$cod.variable == "Trimestres_EPH_2017"])

min_ondas<- min(base_ipc$ANO4[base_ipc$cod.variable == "Ondas_EPH_2017"])
max_ondas <- max(base_ipc$ANO4[base_ipc$cod.variable == "Ondas_EPH_2017"])

## EPH ####

min_eph <- min(eph$ANO4)
max_eph <- max(eph$ANO4)

## Pobreza ####
max_pobreza <- max(pobreza$ANO4)
# Cómo citar? 
titulo_cita <- "Cómo cito estos datos?"
cita <- "CEPED (2022). Ceped.data. Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA), Universidad de Buenos Aires, Buenos Aires. Recuperado de www.ceped-data.shinyapps.io/"
cita_salarios <- "Cazón, F., Kennedy, D. y Weksler, G. (2023) Salario en paridad de poder adquisitivo internacional. Parte I: Procedimientos y resultados a partir de una metodología homogénea para Argentina y países de la OCDE en el largo plazo. Documento de Trabajo del CEPED N° 28, Buenos Aires, CEPED-FCE-UBA."
recuperado_de <- "Recuperado de www.ceped-data.shinyapps.io/ DOI: 10.5281/zenodo.7083025"
doi <- "DOI: 10.5281/zenodo.7083025"

# Notas aclaratorias eph

nota_aclaratoria_eph1 <- "El relevamiento de la EPH durante el año 2020 fue realizado con dificultades debido a la pandemia. Se recomienda consultar el capítulo 'Aspectos Metodológicos' del informe técnico del segundo trimestre de 2020."
nota_aclaratoria_eph2 <- "Asimismo, se advierte que las series estadísticas publicadas con posterioridad a enero 2007 y hasta diciembre 2015 deben ser consideradas con reservas, excepto las que ya hayan sido revisadas en 2016 y su difusión lo consigne expresamente. El INDEC, en el marco de las atribuciones conferidas por los decretos 181/15 y 55/16, dispuso las investigaciones requeridas para establecer la regularidad de procedimientos de obtención de datos, su procesamiento, elaboración de indicadores y difusión. (www.indec.gob.ar)"
metadata_eph <- "Estimación del CEPED en base a la Encuesta Permanente de Hogares (EPH-INDEC). Los datos de 1974-2003 corresponden a versiones anteriores de la EPH, mientras que el período 2003-actual corresponde a su versión continua. Por lo tanto, las series no son estrictamente comparables entre sí."
# Nota aclaratoria BP

#nota_aclaratoria_bp <- "La compatibilización de las series de las metodologías 2017 y 2007 del INDEC está conte Y su clasificación por sector institucional (público y privado) a partir de una reclasificación propia de las partidas se encuentra detallada en el Documento de Trabajo N°25 del CEPED. Para la presentación de los datos en dólares constantes se utilizó el Índice de Precios al Consumidor (CPI-U) publicado por el Bureau of Labor Statistics (BLS)." 

#### Notas aclaratorias Pobreza

nota_aclaratoria_pobreza1 <- "Estas series fueron construidas y actualizadas siguiendo la metodología expuesta en Arakaki (2018), en particular en lo que respecta a la obtención de las canastas para los períodos en los que no se dispone de un valor oficial y confiable. Si bien es posible encontrar mayores detalles en el paper citado anteriormente, a continuación se incluye una serie de notas aclaratorias" 
nota_aclaratoria_pobreza2<- "1. Hay ondas y semestres en los que no hay un dato, lo cual se explica por la ausencia de bases de microdatos de la EPH, excepto en la serie de indigencia para el período previo a 1988. En este último caso, la falta de datos está asociada a la ausencia de un índice de precios al consumidor desagregado por capítulos que permita llevar hacia atrás el valor de la canasta basica de alimentos"
nota_aclaratoria_pobreza3<- "2. Para las ondas de la EPH Puntual entre 1997 y 2003 se excluyen las áreas nuevas del Gran Buenos Aires"
nota_aclaratoria_pobreza4<- "3. Para el período de la EPH Continua, las tasas fueron calculadas a partir de las bases trimestrales, empalmadas y, luego, se calculó el valor semestral como el promedio de los dos trimestres correspondientes"
nota_aclaratoria_pobreza5<- "4. En relación a los ingresos no declarados, se utiliza el criterio utilizado por el INDEC en cada base de la EPH"
nota_aclaratoria_pobreza6<- "5. Para obtener la serie comparable se realiza un empalme hacia adelante"
nota_aclaratoria_pobreza7<- "6. Las series fueron nombradas a partir de los datos de la Encuesta de Ingresos y Gastos de los Hogares (ENGHo) utilizada para obtener los patrones de consumo de la población de referencia. La serie 1985/6 fue construida utilizando la metodología de CEPA (1993) y la serie 2004/5 fue construida utilizando la metodología de INDEC (2016)."
