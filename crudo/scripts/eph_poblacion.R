
library(readxl)
base <- read_excel("crudo/datos/Datos Mercado de Trabajo - CEPED (bases).xls", 
                   sheet = "pob trim")

base <- base[7:13, ]


#Traspongo 
base2 <- data.frame( t(base[,-1]) )

base2 <- base2[3:7]

colnames(base2) <- c("28_Aglomerados", "Resto_Urbano", "Total_Urbano", "Total_Rural", "Total_País" )

base2[] <- lapply(base2, function(x) as.numeric(as.character(x)))

base2[] <- lapply(base2, function(x) x*1000)

rownames(base2) <- NULL

base2 <- base2[1:76,]

#Genero vector años
ANO4 <- c()
for (i in 2003:2021){
  ANO4 <- c(ANO4, rep(i, 4) )
}


#Genero vector trimestre
trimestre <- c()
for (i in 2003:2021){
  trimestre <- c(trimestre, seq(1, 4) )
}

base3 <- cbind(ANO4, trimestre, base2)

base3 <- base3 %>% 
  mutate(ANO4.trim= paste0(ANO4, ".", trimestre)) %>% 
  select(-trimestre)

base4 <- gather(base3, key="cod.variable", value="valor", 2:6 )

base4 <- base4 %>% 
  mutate(nombre.pais="Argentina", 
         iso3c="ARG")

saveRDS(base4, file = "data/Poblacion_eph.RDS")
