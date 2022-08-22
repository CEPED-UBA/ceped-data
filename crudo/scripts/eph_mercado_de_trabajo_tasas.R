
library(readxl)


#### TASAS ####

base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "28 trim - tasas")

base <- base[7:12, ]

base <- base %>% 
  rename(index = 1)

#Traspongo y pongo nombre de variables
base2 <- data.frame( t(base[,-1]) )
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


colnames(base2) <- c("ANO4", "trimestre", "t_actividad", "t_empleo", "t_desocupacion",  "t_subocupacion")

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

rownames(base2) <- NULL

base2 <- base2 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base3 <- gather(base2, key="cod.variable", value="valor", 2:5 )

base3 <- base3 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG")

saveRDS(base3, file = "opcion-modulos-paneles/www/data/Mercado_de_Trabajo_Arg.RDS")
