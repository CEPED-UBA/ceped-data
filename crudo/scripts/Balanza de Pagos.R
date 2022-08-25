#Librerias ####
library(openxlsx)
library(tidyverse)
library(stringr)

# Lectura de Datos####
bop_dolares_corrientes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ corr",startRow = 3,rows = 3:81,
                    skipEmptyRows = T) 


bop_spriv_dolares_corrientes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ corr",rows = c(3,83:96),
            skipEmptyRows = T) 

bop_spub_dolares_corrientes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ corr",rows = c(3,97:105),
            skipEmptyRows = T) 



bop_dolares_constantes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ const",
            startRow = 3,rows = 3:81,
            skipEmptyRows = T)

bop_spriv_dolares_constantes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ const",
            rows = c(3,83:96),
            skipEmptyRows = T) 

bop_spub_dolares_constantes <- 
  read.xlsx("crudo/datos/Balance de Pagos.xlsx",
            sheet = "BP_mill U$ const"
            ,rows = c(3,97:105),
            skipEmptyRows = T) 

# Procesamiento de Datos####



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
