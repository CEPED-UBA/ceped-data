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

options(scipen=999)

#Importacion####
diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")
bop_arg_dolares <- readRDS("www/data/bop_arg_dolares.RDS")
serie_salarios <- readRDS("www/data/salarios.RDS")
tipo_cambio_argentina <- readRDS("www/data/Tipo_Cambio_Arg.RDS")
poblacion_eph <- readRDS("www/data/Poblacion_eph.RDS") 
trabajo_eph <- readRDS("www/data/Mercado_de_Trabajo_Arg.RDS") 
categoria_ocup_eph <- readRDS("www/data/eph_mercado_de_trabajo_categoria_ocupacional.RDS") %>% 
  mutate(ANO4.trim=ANO4,              ### Mover esto a script que crea la base desde raw data
         ANO4=substr(ANO4, 1, 4))


# SALARIOS #####
salarios <- serie_salarios %>% 
  filter(cod.variable %in%  c("salario_relativo_usa_c_actual",
                              "salario_relativo_usa_c_priv"))

#IPC_TIPO_CAMBIO####
base_binded <- bind_rows(serie_salarios,tipo_cambio_argentina)

v_bp <- c() #cuales?

v_salarios <- diccionario_variables %>% filter(base== "Serie_salarios") %>%  select(nombre.variable) %>% 
  filter(nombre.variable != "Indice de Precios al Consumidor (base 2005)")
v_monetario <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>%  select(cod.variable) 
v_ipc <- grep("IPC",v_monetario$cod.variable, value = T)
#v_tc <- v_monetario$cod.variable[!v_monetario$cod.variable %in% v_ipc]
v_ipc <- diccionario_variables %>% filter(cod.variable %in% v_ipc | nombre.variable == "Indice de Precios al Consumidor (base 2005)") %>%  select(nombre.variable)
#v_tc <- diccionario_variables %>% filter(cod.variable %in% v_tc) %>%  select(nombre.variable)
v_tc_cod <- unique(grep("TC",base_binded$cod.variable, value = T,ignore.case = T))

v_tc <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable %in% v_tc_cod]            

#EPH####
#Poblacion####

v_poblacion_eph <- diccionario_variables %>% filter(base=="Poblacion_eph") %>%  select(nombre.variable) 
v_poblacion_eph <- as.vector(v_poblacion_eph[1])

# Cambio cod.variable por nombre de la variable
etiquetas_poblacion <- diccionario_variables %>% filter(base=="Poblacion_eph") %>% select(nombre.variable, cod.variable)

poblacion_eph <- left_join(poblacion_eph, etiquetas_poblacion, by=c("cod.variable")) %>% 
  mutate(cod.variable=nombre.variable) %>% 
  select(-nombre.variable)

##Mercado de trabajo####
v_trabajo_eph <- diccionario_variables %>% filter(base=="Mercado_de_Trabajo_Arg") %>%  select(nombre.variable) 
v_trabajo_eph <- as.vector(v_trabajo_eph[1])

# Cambio cod.variable por nombre de la variable
etiquetas <- diccionario_variables %>% filter(base=="Mercado_de_Trabajo_Arg") %>% select(nombre.variable, cod.variable)

trabajo_eph <- left_join(trabajo_eph, etiquetas, by=c("cod.variable")) %>% 
  mutate(cod.variable=nombre.variable) %>% 
  select(-nombre.variable)

##Categoria OCUP####
v_categoria_ocup_eph <- diccionario_variables %>% filter(base=="eph_mercado_de_trabajo_categoria_ocupacional") %>%  select(nombre.variable) 
v_categoria_ocup_eph <- as.vector(v_categoria_ocup_eph[1])

# Cambio cod.variable por nombre de la variable
etiquetas_catocup <- diccionario_variables %>% filter(base=="eph_mercado_de_trabajo_categoria_ocupacional") %>% select(nombre.variable, cod.variable)

categoria_ocup_eph <- left_join(categoria_ocup_eph, etiquetas_catocup, by=c("cod.variable")) %>% 
  mutate(cod.variable=nombre.variable) %>% 
  select(-nombre.variable)


# Cómo citar? 
titulo_cita <- "Cómo cito estos datos?"
cita <- "CEPED (2022). Ceped.data. Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA), Universidad de Buenos Aires, Buenos Aires. Recuperado de www.ceped-data.shinyapps.io/"

# Nota aclaratoria eph

nota_aclaratoria_eph1 <- "El relevamiento de la EPH durante el año 2020 fue realizado con dificultades debido a la pandemia. Se recomienda consultar el capítulo 'Aspectos Metodológicos' del informe técnico del segundo trimestre de 2020."

nota_aclaratoria_eph2 <- "Asimismo, se advierte que las series estadísticas publicadas con posterioridad a enero 2007 y hasta diciembre 2015 deben ser consideradas con reservas, excepto las que ya hayan sido revisadas en 2016 y su difusión lo consigne expresamente. El INDEC, en el marco de las atribuciones conferidas por los decretos 181/15 y 55/16, dispuso las investigaciones requeridas para establecer la regularidad de procedimientos de obtención de datos, su procesamiento, elaboración de indicadores y difusión. (www.indec.gob.ar)"