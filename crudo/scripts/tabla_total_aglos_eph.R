tabla_aglos <- read_excel("crudo/datos/98. Aglomerados por onda.xlsx")

tabla_aglos <- tabla_aglos[c(2:nrow(tabla_aglos)), c(1, 2, 37) ]

colnames(tabla_aglos) <- c("Año", "Onda", "Cantidad de aglomerados")

saveRDS(tabla_aglos, "www/data/tabla_aglos.RDS")