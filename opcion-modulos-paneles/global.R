#Librerias####
library(shiny)
library(shinythemes)
library(shinycssloaders)
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

serie_salarios <- readRDS("www/data/salarios.RDS")
tipo_cambio_argentina <- readRDS("www/data/Tipo_Cambio_Arg.RDS")
base_ipc <- readRDS("www/data/base_ipc.RDS")

precariedad_eph <- readRDS("www/data/eph_precariedad.RDS") 
tasas_basicas_eph <- readRDS("www/data/eph_tasas_basicas.RDS")
rama_eph <- readRDS("www/data/eph_rama.RDS") 
categoria_ocup_eph <- readRDS("www/data/eph_categoria_ocupacional.RDS")
categoria_ocup_pok_eph <- readRDS("www/data/eph_categoria_ocupacional_pok.RDS")



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

#EPH_ Creacion de etiquetas para cod.variable ###

## Tasas básicas
v_tasas_basicas_eph <- diccionario_variables %>% filter(base=="eph_tasas_basicas") %>%  select(nombre.variable)
v_tasas_basicas_eph <- as.vector(v_tasas_basicas_eph[1])

# Cambio cod.variable por nombre de la variable
etiquetas_tasas_basicas <- diccionario_variables %>% filter(base=="eph_tasas_basicas") %>% select(nombre.variable, cod.variable)

tasas_basicas_eph <- left_join(tasas_basicas_eph, etiquetas_tasas_basicas, by=c("cod.variable")) %>%
  mutate(cod.variable=nombre.variable) %>%
  select(-nombre.variable)

## Categoria ocupacional
v_categoria_ocup_eph <- diccionario_variables %>% filter(base=="eph_categoria_ocupacional") %>%  select(nombre.variable)
v_categoria_ocup_eph <- as.vector(v_categoria_ocup_eph[1])
# Cambio cod.variable por nombre de la variable
etiquetas_catocup <- diccionario_variables %>% filter(base=="eph_categoria_ocupacional") %>% select(nombre.variable, cod.variable)
categoria_ocup_eph <- left_join(categoria_ocup_eph, etiquetas_catocup, by=c("cod.variable")) %>%
  mutate(cod.variable=nombre.variable) %>%
  select(-nombre.variable)

## Categoria ocupacional POK
v_categoria_ocup_pok_eph <- diccionario_variables %>% filter(base=="eph_categoria_ocupacional_pok") %>%  select(nombre.variable)
v_categoria_ocup_pok_eph <- as.vector(v_categoria_ocup_pok_eph[1])

# Cambio cod.variable por nombre de la variable
etiquetas_catocup_pok <- diccionario_variables %>% filter(base=="eph_categoria_ocupacional_pok") %>% select(nombre.variable, cod.variable)
categoria_ocup_pok_eph <- left_join(categoria_ocup_pok_eph, etiquetas_catocup_pok, by=c("cod.variable")) %>%
  mutate(cod.variable=nombre.variable) %>%
  select(-nombre.variable)

## Precariedad
v_precariedad_eph <- diccionario_variables %>% filter(base=="eph_precariedad") %>%  select(nombre.variable)
v_precariedad_eph <- as.vector(v_precariedad_eph[1])

# Cambio cod.variable por nombre de la variable
etiquetas_precariedad <- diccionario_variables %>% filter(base=="eph_precariedad") %>% select(nombre.variable, cod.variable)

precariedad_eph <- left_join(precariedad_eph, etiquetas_precariedad, by=c("cod.variable")) %>%
  mutate(cod.variable=nombre.variable) %>%
  select(-nombre.variable)

# Cómo citar? 
titulo_cita <- "Cómo cito estos datos?"
cita <- "CEPED (2022). Ceped.data. Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA), Universidad de Buenos Aires, Buenos Aires. Recuperado de www.ceped-data.shinyapps.io/"

# Nota aclaratoria eph

nota_aclaratoria_eph1 <- "El relevamiento de la EPH durante el año 2020 fue realizado con dificultades debido a la pandemia. Se recomienda consultar el capítulo 'Aspectos Metodológicos' del informe técnico del segundo trimestre de 2020."
nota_aclaratoria_eph2 <- "Asimismo, se advierte que las series estadísticas publicadas con posterioridad a enero 2007 y hasta diciembre 2015 deben ser consideradas con reservas, excepto las que ya hayan sido revisadas en 2016 y su difusión lo consigne expresamente. El INDEC, en el marco de las atribuciones conferidas por los decretos 181/15 y 55/16, dispuso las investigaciones requeridas para establecer la regularidad de procedimientos de obtención de datos, su procesamiento, elaboración de indicadores y difusión. (www.indec.gob.ar)"

# Nota aclaratoria BP

#nota_aclaratoria_bp <- "La compatibilización de las series de las metodologías 2017 y 2007 del INDEC está conte Y su clasificación por sector institucional (público y privado) a partir de una reclasificación propia de las partidas se encuentra detallada en el Documento de Trabajo N°25 del CEPED. Para la presentación de los datos en dólares constantes se utilizó el Índice de Precios al Consumidor (CPI-U) publicado por el Bureau of Labor Statistics (BLS)." 
