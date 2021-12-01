# Analisis de redes de psicolopatología
# codigo para Mariana

# limpia tu escritorio de trabajo
rm(list=ls())

# paqueterías necesarias
library("tidyverse")
library("psychTools") 
library("qgraph")
library("bootnet")


############# Análisis Básico #####################
# Cargamos datos de factores psicológicos
data(bfi)
bfiSub <- bfi[,1:25]

# vemos la base
bfiSub

# red con lasso
# Estimate network:
Network <- estimateNetwork(bfiSub, default = "EBICglasso",
                           threshold=TRUE)

# Estimated network:
plot(Network, layout = 'spring', labels=TRUE)

# medida de centralidad
centralityPlot(Network, include = c("Strength", "Betweenness", "Closeness"))

### Non-parametric bootstrap ###
# Bootstrap 100 values, using 8 cores:

Results1 <- bootnet(Network, nBoots = 100, nCores = 1)
Results1

# Plot bootstrapped edge CIs:
plot(Results1, labels = FALSE, order = "sample")

### Case-drop bootstrap ###
# Bootstrap 1000 values, using 8 cores:
Results2 <- bootnet(Network, nBoots = 100, nCores = 1,
                    type = "case")

plot(Results2)


# Plot significant differences (alpha = 0.05) of edges:
plot(Results1, "edge", plot = "difference",onlyNonZero = TRUE,
     order = "sample")
# Plot significant differences (alpha = 0.05) of node strength:
plot(Results1, "strength", plot = "difference")
# Test for difference in strength between node "A1" and "C2":
differenceTest(Results1, "A1", "C2", "strength")

# Plot centrality stability:
#plot(Results2)
# Compute CS-coefficients:
corStability(Results2)
## End(Not run)

############## Análisis con los datos del tamizaje ###############
# limpiamos escritorio
rm(list=ls())

# fija tu directorio de trabajo en una carpeta específica
setwd("~/Documents/mental_networks")

# Base de datos
datos_raw_riesgo <- read_csv('data/BDCOVIDNivelRiesgo.csv')

# preguntas de sólo la psicopatologías
respuestas <- datos_raw_riesgo[,42:85]

# 1 conjunto de preguntas
pri_set <- respuestas[,1:20]

# Estimate network:
Network <- estimateNetwork(pri_set, default = "EBICglasso",
                           threshold=TRUE)

# Estimated network:
plot(Network, layout = 'spring')

### Non-parametric bootstrap ###
# Bootstrap 1000 values, using 8 cores:
Results1 <- bootnet(Network, nBoots = 100, nCores = 1)

# Plot bootstrapped edge CIs:
plot(Results1, labels = FALSE, order = "sample")

# Plot significant differences (alpha = 0.05) of edges:
plot(Results1, "edge", plot = "difference",onlyNonZero = TRUE,
     order = "sample")