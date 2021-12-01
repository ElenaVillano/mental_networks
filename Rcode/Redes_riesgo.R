############## Análisis con los datos del tamizaje ###############

# limpiamos escritorio
rm(list=ls())

# paqueterías necesarias
library("tidyverse")
library("psychTools") 
library("qgraph")
library("bootnet")

# fija tu directorio de trabajo en una carpeta específica
setwd("~/Documents/mental_networks")

# Base de datos
datos_raw_riesgo <- read_csv('data/BDCOVIDNivelRiesgo.csv')

# preguntas de sólo la psicopatologías
respuestas <- datos_raw_riesgo[,42:85]

# 1 conjunto de preguntas
pri_set <- respuestas[,1:17]

# Estimate network:
Network1 <- estimateNetwork(pri_set, default = "EBICglasso",
                           threshold=TRUE)
########## Network
# Estimated network:
plot(Network1, layout = 'spring')

# Medidas de centralidad para la red
centralityPlot(Network1, include = c("Betweenness", "Closeness", "Strength"))

########## A. intervalos de confianza para los pesos de las aristas
# Bootstrap 1000 values
boot_IC_ar <- bootnet(Network1, nBoots = 1000, nCores = 1)

summary(boot_IC_ar)

# Plot bootstrapped edge CIs:
plot(boot_IC_ar, labels = FALSE, order = "sample")

######### B. Bootstrap de subconjuntos para ver la estabilidad de las medidas de centralidad

boot_cen_estab <- bootnet(Network1, nBoots = 1000, type = 'case')

plot(boot_cen_estab, include = c("Betweenness", "Closeness", "Strength"))

corStability(boot_cen_estab)

######### C. Test for significan differences

differenceTest(boot_IC_ar, 3, 17,'strength')

plot(boot_IC_ar, 'edge', plot = "difference", onlyNonZero = TRUE, order='sample')

plot(boot_IC_ar, "strength")

