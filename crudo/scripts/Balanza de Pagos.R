#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
bop_dolares_corrientes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ corr",startRow = 3,rows = 3:81,
                    skipEmptyRows = T) 

bop_dolares_constantes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ const",
            startRow = 3,rows = 3:81,
            skipEmptyRows = T)


bop_dolares_diccionario_aclaracion <-  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
                                        sheet = "Metadata BP",
                                        startRow = 3,rows = 3:6,cols = 2:3,
                                        skipEmptyRows = T)

bop_dolares_diccionario <-  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
                                      sheet = "Metadata BP",
                                      startRow = 9,rows = 9:78,cols = 2:4,
                                      skipEmptyRows = T)


saveRDS(bop_dolares_diccionario_aclaracion,
        file = "opcion-modulos-paneles/www/data/bop_dolares_diccionario_aclaracion.RDS")



bop_sectores_diccionario <-  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
                                      sheet = "Metadata BP",
                                      startRow = 9,rows = 9:33,cols = 6:8,
                                      skipEmptyRows = T)
saveRDS(bop_sectores_diccionario,
        file = "opcion-modulos-paneles/www/data/bop_sectores_diccionario.RDS")


  
bop_sectores_corrientes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ corr",
            rows = c(3,82:105),
            skipEmptyRows = T) 

bop_sectores_constantes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ corr",
            rows = c(3,82:105),
            skipEmptyRows = T) 

# Procesamiento de Datos####
## General####


bop_dol_corr <- bop_dolares_corrientes %>% 
  rename(cod.variable = Año,nombre.pais = País) %>% 
  mutate(cod.variable = str_trim(cod.variable,side = "both")) %>% 
  pivot_longer(cols = 6:ncol(.),
               names_to = "ANO4",
               values_to = "valor") %>% 
  mutate(iso3c = "ARG",
         valuacion = "U$ corrientes")


bop_dol_cons <- bop_dolares_constantes %>% 
  rename(cod.variable = Año,nombre.pais = País) %>% 
  mutate(cod.variable = str_trim(cod.variable,side = "both")) %>% 
  pivot_longer(cols = 6:ncol(.),
               names_to = "ANO4",
               values_to = "valor") %>% 
  mutate(iso3c = "ARG",
         valuacion = "U$ constantes")

bop_arg_dolares <- bop_dol_corr %>% 
  bind_rows(bop_dol_cons) %>% 
  mutate(cod.variable = factor(cod.variable,
                               levels = unique(cod.variable)))

saveRDS(bop_arg_dolares,file = "opcion-modulos-paneles/www/data/bop_arg_dolares.RDS")

## Sectores####


bop_sectores_corr <- bop_sectores_corrientes %>% 
  rename(cod.variable = Año,nombre.pais = País) %>% 
  mutate(cod.variable = str_trim(cod.variable,side = "both")) %>% 
  pivot_longer(cols = 6:ncol(.),
               names_to = "ANO4",
               values_to = "valor") %>% 
  mutate(iso3c = "ARG",
         valuacion = "U$ corrientes")


bop_sectores_const <- bop_sectores_constantes %>% 
  rename(cod.variable = Año,nombre.pais = País) %>% 
  mutate(cod.variable = str_trim(cod.variable,side = "both")) %>% 
  pivot_longer(cols = 6:ncol(.),
               names_to = "ANO4",
               values_to = "valor") %>% 
  mutate(iso3c = "ARG",
         valuacion = "U$ constantes")

bop_sectores <- bop_sectores_corr %>% 
  bind_rows(bop_sectores_const) %>% 
  mutate(cod.variable = factor(cod.variable,
                               levels = unique(cod.variable)))

saveRDS(bop_sectores,file = "opcion-modulos-paneles/www/data/bop_sectores.RDS")
