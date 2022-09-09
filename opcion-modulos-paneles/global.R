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

options(scipen=999)

paleta_colores <- c("#41bdcd","#7ccb29","#cce45b","#f9cb27","#fc941d","#e45335")

paleta_colores_extendida <- c(paleta_colores,"#cc0000","#e69138", "#4169e1" ,"#3d85c6","#000080", "#c576f6", "#3a7480", "#0c4f5d","#005c29","#7a1606","#ff9cd1")


#Importacion####
diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")

bop_arg_dolares <- readRDS("www/data/bop_arg_dolares.RDS") %>% 
  mutate(codigo_y_variable = paste0(Codigo," - ",cod.variable),
         Nivel = as.character(Nivel))
bop_sectores <- readRDS("www/data/bop_sectores.RDS") %>% 
  mutate(codigo_y_variable = paste0(Codigo," - ",cod.variable))
bop_dolares_diccionario <- readRDS("www/data/bop_dolares_diccionario.RDS") 
bop_sectores_diccionario <- readRDS("www/data/bop_sectores_diccionario.RDS") 
bop_dolares_diccionario_aclaracion <- readRDS("www/data/bop_dolares_diccionario_aclaracion.RDS") 

serie_salarios <- readRDS("www/data/salarios.RDS")
tipo_cambio_argentina <- readRDS("www/data/Tipo_Cambio_Arg.RDS")
base_ipc <- readRDS("www/data/base_ipc.RDS")

precariedad_eph <- readRDS("www/data/eph_precariedad.RDS") 
tasas_basicas_eph <- readRDS("www/data/eph_tasas_basicas.RDS")
rama_eph <- readRDS("www/data/eph_rama.RDS") 
categoria_ocup_eph <- readRDS("www/data/eph_categoria_ocupacional.RDS")


# Funcional
diccionario_dt24 <- read.xlsx(xlsxFile = "www/data/diccionario_dt24.xlsx") 
base_dt24 <- readRDS(file = "www/data/base_dt24.RDS") %>% 
  mutate(valor = round(valor,digits = 2))
base_export_dt24 <- base_dt24 %>% 
  pivot_wider(names_from = sector,values_from = valor) %>% 
  arrange(variable)

# SALARIOS #####
salarios <- serie_salarios %>% 
  filter(cod.variable %in%  c("salario_relativo_usa_c_actual",
                              "salario_relativo_usa_c_priv",
                              "salario_ppa_c_priv_real",
                              "salario_ppa_c_actual_real"))
#IPC_TIPO_CAMBIO####
#base_binded <- bind_rows(serie_salarios,tipo_cambio_argentina)

v_ipc <- diccionario_variables %>% filter(base=="IPC_Argentina") %>%  pull(nombre.variable)      

min_ipc <- min(base_ipc$ANO4)
max_ipc <- max(base_ipc$ANO4)

min_trim <- min(base_ipc$ANO4[base_ipc$cod.variable == "Trimestres_EPH_2017"])
max_trim <- max(base_ipc$ANO4[base_ipc$cod.variable == "Trimestres_EPH_2017"])

min_ondas<- min(base_ipc$ANO4[base_ipc$cod.variable == "Ondas_EPH_2017"])
max_ondas <- max(base_ipc$ANO4[base_ipc$cod.variable == "Ondas_EPH_2017"])


# Cómo citar? 
titulo_cita <- "Cómo cito estos datos?"
cita <- "CEPED (2022). Ceped.data. Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA), Universidad de Buenos Aires, Buenos Aires. Recuperado de www.ceped-data.shinyapps.io/"

# Nota aclaratoria eph

nota_aclaratoria_eph1 <- "El relevamiento de la EPH durante el año 2020 fue realizado con dificultades debido a la pandemia. Se recomienda consultar el capítulo 'Aspectos Metodológicos' del informe técnico del segundo trimestre de 2020."
nota_aclaratoria_eph2 <- "Asimismo, se advierte que las series estadísticas publicadas con posterioridad a enero 2007 y hasta diciembre 2015 deben ser consideradas con reservas, excepto las que ya hayan sido revisadas en 2016 y su difusión lo consigne expresamente. El INDEC, en el marco de las atribuciones conferidas por los decretos 181/15 y 55/16, dispuso las investigaciones requeridas para establecer la regularidad de procedimientos de obtención de datos, su procesamiento, elaboración de indicadores y difusión. (www.indec.gob.ar)"

# Nota aclaratoria BP

#nota_aclaratoria_bp <- "La compatibilización de las series de las metodologías 2017 y 2007 del INDEC está conte Y su clasificación por sector institucional (público y privado) a partir de una reclasificación propia de las partidas se encuentra detallada en el Documento de Trabajo N°25 del CEPED. Para la presentación de los datos en dólares constantes se utilizó el Índice de Precios al Consumidor (CPI-U) publicado por el Bureau of Labor Statistics (BLS)." 
